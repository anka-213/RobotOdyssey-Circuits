
{-|

A monadic circuit builder where each component return a pointer
to its output. This is reversed from 'RobotOdyssey.Circuit.Builder'
where each component takes a list of output and return its inputs.

The easiest way to build a circuit is using the '(&+)', etc. combinators.
(see section ???)
The "+"/"-" signs says if the output is initially hot or cold.

For example:

> circuit = connectOut 3 $ i 1 &- i 2

creates a circuit that connects the first and second input of the circuit
to the inputs of an and-gate and the third one to its output.

-----

# Cyclic circuits

To create a cyclic circuit, use the RecursiveDo extension
and then connect the inputs of a later circuit to an earlier one.

Example:

> {-# LANGUAGE RecursiveDo #-}
> -- | A 3-clock made of not-gates.
> notClock :: Builder SPtr
> notClock = mdo
>   out <- notOn . notOff . notOn $ out
>   return out


-}

-- TODO: Screenshots of examples

{-# OPTIONS -Wall #-}
{-# OPTIONS -Wno-unused-matches #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module RobotOdyssey.Circuit.BuilderSimple where


-- import Debug.Trace
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Char8 as B8
import Data.Default.Class
-- import Data.Monoid
import Data.Foldable (toList)
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)
import Control.Monad.State.Lazy

import NanoLens
import RobotOdyssey.Circuit
import qualified RobotOdyssey.Circuit.Builder as CB

import qualified Data.FixedList as FL
import Data.FixedList (FixedList8)

newtype SPtr = SPtr Int deriving (Eq, Show, Enum, Ord)

type BState = FixedList8 GateState
type MainInput = FixedList8 Input

data BuilderStateSimple = Bss {bpos:: SPtr, bState :: BState, bGates :: GateMap, bwires :: FixedList8 SInput } deriving (Eq, Show)

_bwires :: Lens' BuilderStateSimple (FixedList8 SInput)
_bwires k (Bss p s g w) = Bss p s g <$> k w

_bgates :: Lens' BuilderStateSimple GateMap
_bgates k (Bss p s g w) = Bss p s <$> k g <$$> w

instance Default BuilderStateSimple where
  def = Bss (SPtr 0) defaultMainState Map.empty defWires

defWires :: FixedList8 SInput
defWires = pure None

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
type OutputIndex = Int
data SInput = External Int | None | GatePtr SPtr OutputIndex deriving (Show, Eq)

_sinputGPtr :: Traversal' SInput SPtr
_sinputGPtr k (GatePtr p n) = GatePtr <$> k p <$$> n
_sinputGPtr _ x           = pure x

data SGate =
    SAnd GateState SInput SInput
  | SOr GateState SInput SInput
  | SXor GateState SInput SInput
  | SNot GateState SInput
  | SFlipFlop GateState GateState SInput SInput
  deriving (Show, Eq)

-- | A 'Traversal\'' to each input in a gate.
_sgateInput :: Traversal' SGate SInput
_sgateInput k (SAnd      gs      i1 i2) = SAnd gs <$> k i1 <*> k i2
_sgateInput k (SOr       gs      i1 i2) = SOr  gs <$> k i1 <*> k i2
_sgateInput k (SXor      gs      i1 i2) = SXor gs <$> k i1 <*> k i2
_sgateInput k (SNot      gs      i1   ) = SNot gs <$> k i1
_sgateInput k (SFlipFlop gs1 gs2 i1 i2) = SFlipFlop gs1 gs2 <$> k i1 <*> k i2

-- | 'gateState n gate' returns the state of the 'n'th output of 'gate'
gateState :: OutputIndex -> SGate -> GateState
gateState 0 (SAnd      gs      i1 i2) = gs
gateState 0 (SOr       gs      i1 i2) = gs
gateState 0 (SXor      gs      i1 i2) = gs
gateState 0 (SNot      gs      i1   ) = gs
gateState 0 (SFlipFlop gs1 gs2 i1 i2) = gs1
gateState 1 (SFlipFlop gs1 gs2 i1 i2) = gs2
gateState _ _ = error "GateState: Invalid index"


type Builder a = State BuilderStateSimple a

-- * Use builder

-- | Extract the list of circuits from a SimpleBuilder
runBuilder :: Builder a -> (a, BuilderStateSimple)
runBuilder x = runState x def

-- | Extract the list of circuits from a SimpleBuilder
execBuilder :: Builder a -> BuilderStateSimple
execBuilder x = execState x def

-- | Build a chunk
buildChunk :: Builder a -> MainChunk
buildChunk = CB.buildChunk . generateABuilder . execBuilder

-- | Build a circuit
buildCircuit :: Builder a -> Circuit
buildCircuit = CB.buildCircuit . generateABuilder . execBuilder

-- | Convert a simple builder into a builder
generateABuilder :: BuilderStateSimple -> CB.Builder ()
generateABuilder (Bss _pos _state gates wires) = mdo
    ans <- Map.traverseWithKey (generateGate gates . allPtrsFrom ans) gates
    mapM_ (uncurry CB.newWire) $ externalTo ans
    return ()
  where
    -- | A list of pointers from a gate
    sptrsFrom :: SInput -> [(SPtr,Int)]
    sptrsFrom ptr = [ (k,n) |
                      (k, v) <- Map.toList gates,
                      (n,inN) <- zip [0,1] $ toListOf (_sgateInput) v,
                      inN == ptr]

    -- | List of all pointers from a specific gate
    allPtrsFrom :: Map SPtr [Ptr] -> SPtr -> ([Ptr],[Ptr])
    allPtrsFrom mp key = (ptrsFrom mp (GatePtr key 0), ptrsFrom mp (GatePtr key 1))

    ptrsFrom :: Map SPtr [Ptr] -> SInput -> [Ptr]
    ptrsFrom ans key = map (findPtr ans) (sptrsFrom key) ++ externalFrom key

    -- | External pointers from a gate
    externalFrom :: SInput -> [Ptr]
    externalFrom a = [ n | (n,x) <- zip [0..] $ toList wires, a == x]

    -- | Connections from an external input
    externalTo :: Map SPtr [Ptr] -> [(Ptr, [Ptr])]
    externalTo ans =
      [ (inp, out)
      | ext <- [0 .. 7]
      , let ex1 = External ext
      , let inp = fromIntegral ext
      , let out = ptrsFrom ans ex1
      , not $ null out
      ]

    findPtr :: Map SPtr [Ptr] -> (SPtr, Int) -> Ptr
    findPtr mp (k, ix) = (mp Map.! k) !! ix

  -- TODO: External output state (BState)

-- | Given a 'SGate' and some additional data, builds a 'Gate' and returns
--   a list of pointers to its inputs
generateGate :: GateMap
             -> ([Ptr],[Ptr])  -- ^ A list of all gates that point from this gate
             -> SGate          -- ^ The gate to generate
             -> CB.Builder [Ptr]
generateGate gm (o1,[]) (SAnd _ p1 p2) = makeGate CB.andGate gm o1 p1 p2
generateGate gm (o1,[]) (SOr _ p1 p2)  = makeGate CB.orGate gm o1 p1 p2
generateGate gm (o1,[]) (SXor _ p1 p2) = makeGate CB.xorGate gm o1 p1 p2
generateGate gm (o1,[]) (SNot _ p1) = do
  let gs1 = findState gm p1
  i1 <- CB.notGate gs1 o1
  return [i1]
generateGate gm (o1,o2) (SFlipFlop s1 s2 p1 p2) = do
  let gs1 = findState gm p1
  let gs2 = findState gm p2
  let s1' = toEnum $ fromEnum s1
  let s2' = toEnum $ fromEnum s2
  (i1, i2) <- CB.flopGate' gs1 gs2 s1' s2' o1 o2
  return [i1,i2]
generateGate _ _ _ = error "generateGate: Invalid output"

type GateMaker = GateState -> GateState -> Output -> CB.Builder (Ptr, Ptr)

makeGate :: GateMaker -> GateMap -> Output -> SInput -> SInput -> CB.Builder [Ptr]
makeGate gateBuilder gm outs p1 p2 = do
  let gs1 = findState gm p1
  let gs2 = findState gm p2
  (i1, i2) <- gateBuilder gs1 gs2 outs
  return [i1,i2]


-- | Find an input state of a gate by looking at the output
--   state of the gate it is connected to
findState :: GateMap -> SInput -> GateState
findState gm (GatePtr p1 n) = gateState n $ gm Map.! p1
findState _ (External _)  = O
findState _ None          = O

-- * Internal stuff

newOutput :: Builder SPtr
newOutput = state $
  \bs -> (bpos bs, bs {bpos = succ $ bpos bs})

newGate :: SPtr -> SGate -> Builder ()
newGate n g = _bgates %= Map.insert n g

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

flopGate :: GateState -> GateState -> SInput -> SInput -> Builder (SInput,SInput)
flopGate s1 s2 i1 i2 = do
  pos <- newOutput
  newGate pos $ SFlipFlop s1 s2 i1 i2
  return (GatePtr pos 0, GatePtr pos 1)

gatePtr :: SPtr -> SInput
gatePtr x = GatePtr x 0

-- instance Num (Builder SInput) where
--   fromInteger = pure . External . fromInteger
--   -- x + y    = fmap gatePtr $ liftJoin2 (orGate O) x y
--   x + y    = fmap gatePtr . join $ orGate  O <$> x <*> y -- ^ Or-gate
--   x * y    = fmap gatePtr . join $ andGate O <$> x <*> y -- ^ And-gate
--   negate x = fmap gatePtr . join $ notGate I <$> x       -- ^ Not-gate
--   abs      = error "Not applicable"
--   signum   = error "Not applicable"

liftJoin1 :: Monad m => m a -> (a -> m b) -> m b
liftJoin1 f a = f >>= a

liftJoin2 :: Monad m => (a1 -> a2 -> m a3) -> m a1 -> m a2 -> m a3
liftJoin2 f a b = join $ f <$> a <*> b

-- | An 'IsInput' is either the output from builder or a builder itself
class IsInput x where
  toInputBuilder :: x -> Builder SInput

instance IsInput (Builder SInput)  where
  toInputBuilder = id

instance IsInput (Builder SPtr) where
  toInputBuilder = fmap gatePtr

instance IsInput SInput where
  toInputBuilder = pure

instance IsInput SPtr where
  toInputBuilder = pure . gatePtr

-- | Binary gate with generic inputs (allows combining gates without explicit monad operations)
binaryGateG :: (IsInput a, IsInput b) =>  (GateState -> SInput -> SInput -> Builder c) ->
               GateState -> a -> b -> Builder c
binaryGateG gateConstructor s i1 i2 = do
  i1' <- toInputBuilder i1
  i2' <- toInputBuilder i2
  gateConstructor s i1' i2'

andGateG, orGateG, xorGateG :: (IsInput a, IsInput b) => GateState -> a -> b -> Builder SPtr
andGateG = binaryGateG andGate
orGateG  = binaryGateG orGate
xorGateG = binaryGateG xorGate

notGateG :: IsInput a => GateState -> a -> Builder SPtr
notGateG s i1 = do
  i1' <- toInputBuilder i1
  notGate s i1'

flopGateG :: (IsInput a, IsInput b) => GateState -> GateState -> a -> b -> Builder (SInput, SInput)
flopGateG = binaryGateG . flopGate

-- * Infix Builder operators

infixl 7 &+
infixl 7 &-
infixl 6 |+
infixl 6 |-
infixl 4 \-\
infixl 4 /-/

-- | Build an and-gate with hot output
(&+) :: (IsInput a, IsInput b) => a -> b -> Builder SPtr
(&+) = andGateG I

-- | Build an and-gate with cold output
(&-) :: (IsInput a, IsInput b) => a -> b -> Builder SPtr
(&-) = andGateG O

-- | Build an or-gate with hot output
(|+) :: (IsInput a, IsInput b) => a -> b -> Builder SPtr
(|+) = orGateG I

-- | Build an or-gate with cold output
(|-) :: (IsInput a, IsInput b) => a -> b -> Builder SPtr
(|-) = orGateG O

-- | Build an xor-gate with hot output
(^+) :: (IsInput a, IsInput b) => a -> b -> Builder SPtr
(^+) = xorGateG I

-- | Build an xor-gate with cold output
(^-) :: (IsInput a, IsInput b) => a -> b -> Builder SPtr
(^-) = xorGateG O

-- | Build a not-gate with hot output
notOn :: (IsInput a) => a -> Builder SPtr
notOn = notGateG I

-- | Build a not-gate with cold output
notOff :: (IsInput a) => a -> Builder SPtr
notOff = notGateG O

-- | Build a flip-flop-gate with the left output on
(\-\) :: (IsInput a, IsInput b) => a -> b -> Builder (SInput, SInput)
(\-\) = flopGateG I O

-- | Build a flip-flop-gate with the right output on
(/-/) :: (IsInput a, IsInput b) => a -> b -> Builder (SInput, SInput)
(/-/) = flopGateG O I

-- | Create a pointer to an external input
i :: Int -> SInput
i = External

-- | Connect a connector to an external output.
-- In case of multiple conectors connecting to the same wire,
-- the last one will be used
connectOut :: IsInput a => Int -> a -> Builder ()
connectOut n i1 = do
  i1' <- toInputBuilder i1
  _bwires . FL.ix n .= i1'

connectOut2 :: Int -> Int -> Builder (SInput, SInput) -> Builder ()
connectOut2 n1 n2 b = do
  (i1,i2) <- b
  connectOut n1 i1
  connectOut n2 i2

-- * Examples

-- | Two alternative ways of writing (x & y) | z
xAndYOrZ :: (IsInput x, IsInput y, IsInput z) => x -> y -> z -> Builder SPtr
xAndYOrZ x y z = do
  xy <- x &- y
  xy |- z

xAndYOrZ' :: (IsInput x, IsInput y, IsInput z) => x -> y -> z -> Builder SPtr
xAndYOrZ' x y z = ( x &- y ) |- z

-- | A 3-clock made of not-gates.
-- Example of gate cycles
notClock :: Builder SPtr
notClock = mdo
  out <- notOn . notOff . notOn $ out
  return out


-- | Sends out one tick a pulse when the wire goes from O to I
edgeDetector :: IsInput x => x -> Builder SPtr
edgeDetector i1 = notOn i1 &- i1

-- | A toggle-flip-flop that switches state each time the input is pulsed
tFlipFlop :: (IsInput x, IsInput y) => x -> y -> Builder (SInput, SInput)
-- tFlipFlop outOff outOn = mdo
tFlipFlop toggle reset = mdo
  flopOff <- r2 |- reset
  pulse <- edgeDetector toggle
  r2 <- pulse &- outOn
  (outOn, outOff) <- flopOn /-/ flopOff
  flopOn <- pulse &- outOff

  return (outOff, outOn)


-- | A toggle-flip-flop that switches state each time the input is pulsed
tFlipFlop' :: (IsInput x, IsInput y) => x -> y -> Builder (SInput, SInput)
-- tFlipFlop outOff outOn = mdo
tFlipFlop' toggle reset = mdo
  pulse <- edgeDetector toggle
  flopOff <- (pulse &- outOn) |- reset
  flopOn <- pulse &- outOff
  (outOn, outOff) <- flopOn /-/ flopOff

  return (outOff, outOn)

printTFlop :: IO ()
printTFlop = printCircuit $ buildCircuit  $  connectOut2 3 4 $ tFlipFlop (i 1) (i 2)


-- * Pretty printing
