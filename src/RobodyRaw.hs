module RobodyRaw where

import Control.Monad
import Control.Monad.Identity
-- import Control.Monad.Constant
import Control.Applicative
import Data.Word
import Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Debug.Trace
import NanoLens

-- * Implementation

-- | An integrated circuit file
data Circuit = Circuit {cmain :: MainChunk, cmeta :: MetaData} deriving Show

_mainChunk :: Lens' Circuit MainChunk
_mainChunk k (Circuit main meta) = (\x -> Circuit x meta) <$> k main

-- | Meta data about the circuit file
data MetaData = MetaData
     { inouts :: [InOrOut]
     , mlength :: Int
     , description :: Description
     } deriving Show

type Description = (ByteString, [ByteString])

data InOrOut = Neither | In | Out deriving (Enum, Show)

-- | The chunk containing the grinds
data MainChunk = MainChunk
     { inputs :: [Input] -- ^ The initial state of the inputs and outputs
     , grinds :: [Grind]
     , wires  :: [Wire]
     }
        deriving Show

_grinds :: Lens' MainChunk [Grind]
_grinds k chnk = (\x -> chnk {grinds = x}) <$> k (grinds chnk)

_inputs :: Lens' MainChunk [Input]
_inputs k chnk = (\x -> chnk {inputs = x}) <$> k (inputs chnk)

-- | A traversal to all input connections (excluding nested) for a block
_deep_input :: Traversal' MainChunk Input
_deep_input k (MainChunk iput grinds wires) = let
    iput' = traverse k iput
    grind' = (traverse . _input) k grinds
  in MainChunk <$> iput' <*> grind' <$$> wires

data Grind =
       And Input Input Output
     | Or  Input Input Output
     | Xor Input Input Output
     | Not Input Output
     | Flop Input Input Word8 Word8 Output Output
     | Nested Ptr MainChunk
     deriving Show

_input :: Traversal' Grind Input
_input k (And i1 i2 out) = And <$> k i1 <*> k i2 <$$> out
_input k (Or i1 i2 out) = Or <$> k i1 <*> k i2 <$$> out
_input k (Not i1 out) = Not <$> k i1 <$$> out
-- _input k (Flop i1 i1 c1 c2 out1 out2) = (\i1' i2' -> Flop i1' i2' c1 c2 out1 out2) <$> k i1 <*> k i2
_input k (Flop i1 i2 c1 c2 out1 out2) = Flop <$> k i1 <*> k i2 <$$> c1 <$$> c2 <$$> out1 <$$> out2
_input k nst@(Nested _ _) = pure nst


_nested :: Traversal' Grind (Ptr, MainChunk)
_nested k (Nested ptr chnk) = uncurry Nested <$> k (ptr, chnk)
_nested k x                 = pure x

nested :: Ptr -> MainChunk -> Grind
nested ptr@(Ptr blockStart) chnk = Nested ptr $ over (_deep_input . _input_ptr) shift chnk
  where
    shift :: Ptr -> Ptr
    shift (Ptr x) = Ptr (x-blockStart)

-- | An input bundled with it's position in the file
data Input = Input Ptr Word8 deriving Show

_input_ptr :: Lens' Input Ptr
_input_ptr k (Input ptr s) = Input <$> k ptr <$$> s

-- | A pointer to an absolute position in the file.
-- This represents an input for a grind or an output
-- for the whole circuit (if n is in [0..7])
newtype Ptr = Ptr Word16 deriving (Eq, Show)


type Output = [Ptr]

data Wire = Wire {wireFrom ::Ptr, wireTo :: [Ptr]} deriving Show

-- * Debug

getData :: IO ByteString
getData = B.readFile "OR123.CSV"

printCircuit :: Circuit -> IO ()
printCircuit = printMainChunk . cmain

printMainChunk :: MainChunk -> IO ()
printMainChunk (MainChunk inputs grinds wires) = do
  print inputs
  mapM_ print grinds
  mapM_ print wires

-- * Handling data

getNestedBlocks :: Either String Circuit -> [(Ptr, MainChunk)]
getNestedBlocks = toListOf (_right . _mainChunk . _grinds . traverse . _nested)

-- * Helpers

-- | Print a value or a error message
printErr :: Show a => Either String a -> IO ()
printErr (Right a) = print a
printErr (Left err) = Prelude.putStrLn $ "Error: " ++ err

-- | Convert an either value into an exception
getRight :: String -> Either String b -> b
getRight str = either (\err -> error $ str ++ err) id

-- | Find the offset of a bytestring
getBSOffset :: ByteString -> Int
getBSOffset (BI.PS _ off _) = off

traceShowIdM :: (Monad m, Show b) => m b -> m b
traceShowIdM m = do
    x <- m
    traceShowM x
    return x
