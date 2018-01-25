{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module CircuitBuilder where

-- import Debug.Trace
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Default.Class
import Data.Monoid
import Control.Monad.State.Lazy

import NanoLens
import RobodyCircuit
import qualified Data.FixedList as FL
import Data.FixedList (FixedList8)

data BuilderState = Bs {bpos:: Ptr, bState :: BState, bGates :: [Gate], bwires :: [Wire] }

type BState = FixedList8 GateState
type MainInput = FixedList8 Input

type Builder a = State BuilderState a

buildChunk :: Builder a -> MainChunk
buildChunk b = makeChunk $ execState b def

buildCircuit :: Builder a -> Circuit
buildCircuit b = makeCircuit $ execState b def

-- | Build a circuit and save it to a file
buildSaveCircuit :: FilePath -> Builder a -> IO ()
buildSaveCircuit file = saveCircuit file . buildCircuit

-- | The input/output of the entire circuit
mainInput :: BState -> MainInput
mainInput states = Input <$> FL.fromFoldable' [0..7] <*> states

defaultMainState :: BState
defaultMainState = pure O

makeChunk :: BuilderState -> MainChunk
makeChunk (Bs _ input gates wires) = MainChunk (mainInput input) (reverse gates) (reverse wires)

makeCircuit :: BuilderState -> Circuit
makeCircuit bs = Circuit (makeChunk bs) (makeMeta bs)

makeMeta :: BuilderState -> MetaData
makeMeta (Bs (Ptr pos) _ gates wires) = MetaData (findInOut gates wires) sizeTotal emptyDocs
  where
  gateSize = fromIntegral pos
  wireSize = size wires
  sizeTotal = 2 + gateSize + fromIntegral wireSize

findInOut :: [Gate] -> [Wire] -> [InOrOut]
findInOut gates wires = inOrOut <$> [0..7]
  where
    -- debug = trace "in" $ traceShow inputs $ trace "out" $ traceShow outputs $ undefined
    inOrOut n | elem (Ptr n) inputs  = In
              | elem (Ptr n) outputs = Out
              | otherwise            = Neither
    gateInputs = toListOf (traverse . _input . _input_ptr) gates
    gateOutputs = toListOf (traverse . _output . traverse) gates
    wireInputs = wireFrom <$> wires
    wireOutputs = concat $ wireTo <$> wires
    outputs = gateOutputs ++ wireOutputs
    inputs = gateInputs ++ wireInputs

emptyDocs :: Description
emptyDocs = (B8.replicate 18 ' ', [B8.pack (show n) <> B8.replicate 33 ' ' | n <- [1..8::Int]])

instance Default BuilderState where
  def = Bs (Ptr 8) defaultMainState [] []

newInput :: Int -> Builder Ptr
newInput nr = (+fromIntegral nr) <$> gets bpos

buildGate :: Gate -> Builder ()
buildGate gate = do
  (Bs (Ptr pos) inputs gates wires) <- get
  put $ Bs (Ptr $ pos + size gate) inputs (gate:gates) wires

buildWire :: Wire -> Builder ()
buildWire wire = do
  (Bs pos inputs gates wires) <- get
  put $ Bs pos inputs gates (wire:wires)

-- | Given list of output pointers, builds a not gate
--   and returns a pointer to its input
notGate :: GateState -> Output -> Builder Ptr
notGate istate out = do
  i1 <- newInput 1
  let gate = Not (Input i1 istate) out
  buildGate gate
  return i1

binaryGate :: (Input -> Input -> Output -> Gate)
           -> GateState -> GateState -> Output -> Builder (Ptr, Ptr)
binaryGate constructor i1state i2state out = do
  i1 <- newInput 1
  i2 <- newInput 2
  buildGate $ constructor (Input i1 i1state) (Input i2 i2state) out
  return (i1, i2)


andGate, orGate, xorGate :: GateState -> GateState -> Output -> Builder (Ptr, Ptr)
andGate = binaryGate And
orGate = binaryGate Or
xorGate = binaryGate Xor

flopGate :: Bool -> Output -> Output -> Builder (Ptr, Ptr)
flopGate isOn out1 out2 = do
  i1 <- newInput 1
  i2 <- newInput 2
  let (s1,s2) = if isOn then (1,0) else (0,1)
  let gate = Flop (Input i1 O) (Input i2 O) s1 s2 out1 out2
  buildGate gate
  return (i1, i2)

nestedCircuit :: Builder a -> Builder (FixedList8 Ptr)
nestedCircuit inner = do
  startPos <- newInput 0
  inputs <- mapM newInput $ FL.fromFoldable' [1..8]
  let gate = Nested startPos (buildChunk inner)
  buildGate gate
  return inputs

nestCircuit :: Circuit -> Builder (FixedList8 Ptr)
nestCircuit inner = do
  startPos <- newInput 0
  inputs <- mapM newInput $ FL.fromFoldable' [1..8]
  let gate = Nested startPos (cmain inner)
  buildGate gate
  return inputs

setInputState :: BState -> Builder ()
setInputState bstate = do
  modify (\(Bs n _ g w) -> Bs n bstate g w)

-- TODO: Nest, Put


newWire :: Ptr -> [Ptr] -> Builder ()
newWire input out = do
  buildWire $ Wire input out

main :: IO ()
main = printMainChunk $ buildChunk $ example

printBuildChunk :: Builder a -> IO ()
printBuildChunk = printMainChunk . buildChunk

printBuildCircuit :: Builder a -> IO ()
printBuildCircuit = printCircuit . buildCircuit

-- * Examples

-- | A clock that sends signals to specified output
clock :: Output -> Builder ()
clock out = mdo
  i1 <- notGate O (i1:out)
  return ()

-- | clockN n out genereates a clock of length 2*n+1 ticks
clockN :: Int -> Output -> Builder ()
clockN n out = mdo
  i1 <- notGate O (iN : out)
  iN <- foldM (\iK k -> notGate (if even k then O else I) [iK]) i1 [1..2*n]
  return ()

-- | Sends out one tick a pulse when the wire goes from O to I
edgeDetector :: Output -> Builder Output
edgeDetector out = mdo
  i1' <- notGate O [i1]
  (i1,i2) <- andGate I O out
  return [i1', i2]

-- | A toggle-flip-flop that switches state each time the input is pulsed
tFlipFlop :: Output -> Output -> Builder (Output, Output)
tFlipFlop outOff outOn = mdo
  (r2, reset) <- orGate O O [flopOff]
  toggle <- edgeDetector [andOff1, andOn1]
  (andOn1, andOn2) <- andGate O O [r2]
  (flopOn, flopOff) <- flopGate False (andOn2:outOn) (andOff2:outOff)
  (andOff1, andOff2) <- andGate O I [flopOn]

  return (toggle, [reset])

tFlipFlopCircuit :: Circuit
tFlipFlopCircuit = buildCircuit $ do
  (toggle,reset) <- tFlipFlop [4] [5]
  newWire 0 toggle
  newWire 1 reset
  setInputState $ FL.fromFoldable' [O,O,O,O,I,O,O,O]

-- | Makes a loop of or-gates that replays the sequence
--   of states entered
orLoop :: [GateState] -> Output -> Builder ()
orLoop [] _ = return ()
orLoop (s1:states) out = mdo
  i1 <- fst <$> orGate s1 O (iN:out)
  iN <- foldM orStep i1 states
  return ()

orStep :: Ptr -> GateState -> Builder Ptr
orStep out' st = fst <$> orGate st O [out']

orSequence :: [GateState] -> Output -> Builder Ptr
orSequence [] _ = error "Empty delay"
orSequence (s1:states) out = do
  i1 <- fst <$> orGate s1 O out
  foldM orStep i1 states

solveL5P3 :: IO ()
solveL5P3 = buildSaveCircuit "circuits/L5P3.CSV" $
  orLoop [I,O,O,I,I,O] [Ptr 0]

solveL5P4 :: IO ()
-- solveL5P4 = buildSaveCircuit "circuits/L5P4.CSV" $ do
solveL5P4 = buildSaveCircuit "c:\\Users\\skyio\\D-Fend Reloaded\\VirtualHD\\Robot Odyssey\\L5P4.CSV" $ do
  clockN 11 [Ptr 7]
  -- let right = Ptr 2
  -- let down = Ptr 0
  -- let left = Ptr 5
  -- ndown <- notGate O [down]
  -- -- right' <-
  -- orLoop (replicate 35 I ++ replicate 35 O) [right, ndown]



solveL5P6 :: IO ()
solveL5P6 = buildSaveCircuit "c:\\Users\\skyio\\D-Fend Reloaded\\VirtualHD\\Robot Odyssey\\L5P6.CSV" $ do
  _ <- orSequence (replicate 7 I ++ [O]) [Ptr 3]
  orLoop [I,I,I,O,O,O,O] [Ptr 7]

solveL5P7 :: IO ()
solveL5P7 = solveL5P7a >> solveL5P7b
solveL5P7a :: IO ()
solveL5P7a = buildSaveCircuit "c:\\Users\\skyio\\D-Fend Reloaded\\VirtualHD\\Robot Odyssey\\SOLUT1.CSV" $ do
  let right = [Ptr 7]
  let step = replicate . (8*)
  orLoop (step 1 O ++ step 4 I ++ step 2 O ++ step 2 O ++ step 2 O ++ step 5 I) right

solveL5P7b :: IO ()
solveL5P7b = buildSaveCircuit "c:\\Users\\skyio\\D-Fend Reloaded\\VirtualHD\\Robot Odyssey\\SOLUT2.CSV" $ do
  let left = [Ptr 7]
  let step = replicate . (8*)
  orLoop (step 1 O ++ step 4 O ++ step 2 O ++ step 2 I ++ step 2 O ++ step 5 O) left

  -- orLoop (step 8 O ++ step 2 I ++ step 6 O) left
  -- let hSquares n = replicate (16*n)
  -- let vSquares n = replicate (8*n)

  -- orLoop (vSquares 0 I ++ replicate 30 O) [Ptr 6]
  -- orLoop (hSquares 1 I ++ replicate 30 O) [Ptr 7]



getOrig :: IO MainChunk
getOrig = do
  [_,b,_,_] <- map snd . toListOf (_mainChunk._gates.traverse._nested) . getRight <$> parseFile "circuits/builtin/4BITCNTR.CSV"
  return b

nestedChunks :: Traversal' Circuit (Ptr,MainChunk)
nestedChunks = (_mainChunk._gates.traverse._nested)

{-
-- Original
[Input (Ptr 0) I,Input (Ptr 1) O,Input (Ptr 2) O,Input (Ptr 3) O,Input (Ptr 4) I,Input (Ptr 5) O,Input (Ptr 6) O,Input (Ptr 7) O]
Or (Input (Ptr 9) O) (Input (Ptr 10) O) [Ptr 35]
Not (Input (Ptr 15) I) [Ptr 20]
And (Input (Ptr 20) O) (Input (Ptr 21) I) [Ptr 49,Ptr 28]
And (Input (Ptr 28) O) (Input (Ptr 29) O) [Ptr 9]
Flop (Input (Ptr 34) O) (Input (Ptr 35) O) 0 1 [Ptr 29,Ptr 5] [Ptr 50,Ptr 4]
And (Input (Ptr 49) O) (Input (Ptr 50) I) [Ptr 34]
Wire {wireFrom = Ptr 0, wireTo = [Ptr 15,Ptr 21]}
Wire {wireFrom = Ptr 1, wireTo = [Ptr 10]}
-}

example :: Builder ()
example = do
  i1 <- notGate O [0]
  newWire 3 [i1]
  return ()
