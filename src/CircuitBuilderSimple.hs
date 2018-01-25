{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall #-}
-- {-# OPTIONS -WUnusedImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module CircuitBuilderSimple where

-- import Debug.Trace
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Char8 as B8
import Data.Default.Class
-- import Data.Monoid
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)
import Control.Monad.State.Lazy

-- import NanoLens
import RobodyCircuit
-- import qualified CircuitBuilder

-- import qualified Data.FixedList as FL
import Data.FixedList (FixedList8)

newtype SPtr = SPtr Int deriving (Eq, Show, Enum, Num, Ord)

type BState = FixedList8 GateState
type MainInput = FixedList8 Input

data BuilderStateSimple = Bs {bpos:: SPtr, bState :: BState, bGates :: GateMap, bwires :: [Wire] } deriving (Eq, Show)

instance Default BuilderStateSimple where
  def = Bs (SPtr 0) defaultMainState Map.empty []

defaultMainState :: BState
defaultMainState = pure O

defBuild :: BuilderStateSimple
defBuild = def

-- type GateMap = Map SPtr (SGate SPtr)
-- data SGate a = SAnd (Binop a) | SOr (Binop a) | SXor (Binop a) | SNot (Unop a) deriving (Show, Eq)

-- data P2 a = P2 a a deriving (Show, Eq)

-- type Binop a = Op (P2 a)
-- type Unop a = Op a

-- data Op a = Op GateState a deriving (Show, Eq)
type GateMap = Map SPtr SGate
data SInput = External Int | None | GatePtr SPtr deriving (Show, Eq)
data SGate =
    SAnd GateState SInput SInput
  | SOr GateState SInput SInput
  | SXor GateState SInput SInput
  | SNot GateState SInput
  | SFlipFlop GateState GateState SInput SInput
  deriving (Show, Eq)

type Builder a = State BuilderStateSimple a

newOutput :: Builder SPtr
newOutput = state $
  \bs -> (bpos bs, bs {bpos = bpos bs + 1})

newGate :: SPtr -> SGate -> Builder ()
newGate n g = modify $ \bs -> bs {bGates = Map.insert n g (bGates bs)}

binaryGate :: (GateState -> SInput -> SInput -> SGate) ->
               GateState -> SInput -> SInput -> Builder SPtr
binaryGate gateType s i1 i2 = do
  pos <- newOutput
  newGate pos $ gateType s i1 i2
  return pos

andGate, orGate, xorGate :: GateState -> SInput -> SInput -> Builder SPtr
andGate = binaryGate SAnd
orGate = binaryGate SOr
xorGate = binaryGate SXor

notGate :: GateState -> SInput -> Builder SPtr
notGate s i1 = do
  pos <- newOutput
  newGate pos $ SNot s i1
  return pos

instance Num (Builder SInput) where
  fromInteger = pure . External . fromInteger
  x + y    = fmap GatePtr . join $ orGate  O <$> x <*> y
  x * y    = fmap GatePtr . join $ andGate O <$> x <*> y
  negate x = fmap GatePtr . join $ notGate I <$> x
  abs      = error "Not applicable"
  signum   = error "Not applicable"

liftJoin1 f a = f >>= a
liftJoin2 f a b = join $ f <$> a <*> b

-- | Given two gate builders, return an and-gate with initial out-state hot
(&+&) :: Builder SPtr -> Builder SPtr -> Builder SPtr
a &+& b = do
  i1 <- a
  i2 <- b
  andGate I (GatePtr i1) (GatePtr i2)
-- a &&- b = andGate I <$> a <*> b




{-



buildChunk :: Builder a -> MainChunk
buildChunk b = makeChunk $ execState b def

buildCircuit :: Builder a -> Circuit
buildCircuit b = makeCircuit $ execState b def

-- | The input/output of the entire circuit
mainInput :: BState -> MainInput
mainInput states = Input <$> FL.fromFoldable' [0..7] <*> states

makeChunk :: BuilderState -> MainChunk
makeChunk (Bs _ input gates wires) = MainChunk (mainInput input) (reverse gates) (reverse wires)

makeCircuit :: BuilderState -> Circuit
makeCircuit bs = Circuit (makeChunk bs) (makeMeta bs)

makeMeta :: BuilderState -> MetaData
makeMeta (Bs (Ptr pos) _ gates wires) = MetaData (findInOut gates wires) sizeTotal emptyDocs
  where
  gateSize = fromIntegral pos
  wireSize = size wires
  sizeTotal = gateSize + fromIntegral wireSize

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


newInput :: Int -> Builder Ptr
newInput nr = (+fromIntegral nr) <$> gets bpos

putGate :: Gate -> Builder ()
putGate gate = do
  (Bs (Ptr pos) inputs gates wires) <- get
  put $ Bs (Ptr $ pos + size gate) inputs (gate:gates) wires

putWire :: Wire -> Builder ()
putWire wire = do
  (Bs (Ptr pos) inputs gates wires) <- get
  put $ Bs (Ptr $ pos + size wire) inputs gates (wire:wires)

-- | Given list of output pointers, builds a not gate
--   and returns a pointer to its input
notGate :: GateState -> Output -> Builder Ptr
notGate istate out = do
  i1 <- newInput 1
  let gate = Not (Input i1 istate) out
  putGate gate
  return i1

binaryGate :: (Input -> Input -> Output -> Gate)
           -> GateState -> GateState -> Output -> Builder (Ptr, Ptr)
binaryGate constructor i1state i2state out = do
  i1 <- newInput 1
  i2 <- newInput 2
  putGate $ constructor (Input i1 i1state) (Input i2 i2state) out
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
  putGate gate
  return (i1, i2)

nestedCircuit :: Builder a -> Builder (FixedList8 Ptr)
nestedCircuit inner = do
  startPos <- newInput 0
  inputs <- mapM newInput $ FL.fromFoldable' [1..8]
  let gate = Nested startPos (buildChunk inner)
  putGate gate
  return inputs

-- TODO: Nest, Put


newWire :: Ptr -> [Ptr] -> Builder ()
newWire input out = do
  putWire $ Wire input out

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

-- | Sends out a pulse when the newWire goes from O to I
edgeDetector :: Output -> Builder Output
edgeDetector out = mdo
  i1' <- notGate O [i1]
  (i1,i2) <- andGate O I out
  return [i1', i2]

tFlipFlop :: Output -> Output -> Builder (Output, Output)
tFlipFlop outOff outOn = mdo
  (r2, reset) <- orGate O O [flopOff]
  toggle <- edgeDetector [andOff1, andOn1]
  (andOn1, andOn2) <- andGate O O [r2]
  (flopOn, flopOff) <- flopGate False (andOn2:outOn) (andOff2:outOff)
  (andOff1, andOff2) <- andGate O O [flopOn]

  return (toggle, [reset])

tFlipFlopCircuit :: MainChunk
tFlipFlopCircuit = buildChunk $ do
  (toggle,reset) <- tFlipFlop [4] [5]
  newWire 0 toggle
  newWire 1 reset

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

-}
