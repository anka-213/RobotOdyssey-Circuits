{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RobodyCircuit where

import Control.Monad
import Control.Applicative
import Data.Foldable
import Data.Word
import Data.Monoid
import Data.Serialize
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Internal as BI
import Debug.Trace

import NanoLens
import Data.FixedList


-- | This module is a parser and generator for saved circuits for the game Robot Odyssey

-- * Main interface

-- | Parse a .CSV file into a Circuit
parseFile :: String -> IO (Either String Circuit)
parseFile file = parseByteString <$> B.readFile file

-- | Parse a bytestring containing a .CSV file into a Circuit
parseByteString :: ByteString -> Either String Circuit
parseByteString = parseCircuit


-- | Print the meta-data for a file
describeFile :: FilePath -> IO ()
describeFile f = do
  Right (Circuit _ meta) <- parseFile f
  printMeta meta

printFile :: FilePath -> IO ()
printFile f = printErrCircuit =<< parseFile f


-- | Serialize a circuit into a bytestring
makeByteString :: Circuit -> ByteString
makeByteString = serializeCircuit

-- | Save a circuit as a .CSV file
saveCircuit :: FilePath -> Circuit -> IO ()
saveCircuit file = B.writeFile file . serializeCircuit

-- * Types

-- | A parsed integrated circuit file
data Circuit = Circuit {cmain :: MainChunk, cmeta :: MetaData} deriving (Show, Eq)

_mainChunk :: Lens' Circuit MainChunk
_mainChunk k (Circuit main meta) = Circuit <$> k main <$$> meta

-- | Meta data about the circuit file
data MetaData = MetaData
     { meta_inouts :: [InOrOut]
     , meta_length :: Int
     , meta_description :: Description
     } deriving (Show, Eq)

type Description = (ByteString, [ByteString])

data InOrOut = Neither | In | Out deriving (Enum, Show, Eq)

-- | The chunk containing the gates
data MainChunk = MainChunk
     { main_inputs :: FixedList8 Input -- ^ The initial state of the inputs and outputs
     , main_gates :: [Gate]
     , main_wires  :: [Wire]
     }
        deriving (Show, Eq)


_gates :: Lens' MainChunk [Gate]
_gates k chnk = (\x -> chnk {main_gates = x}) <$> k (main_gates chnk)

_inputs :: Lens' MainChunk (FixedList8 Input)
_inputs k chnk = (\x -> chnk {main_inputs = x}) <$> k (main_inputs chnk)

-- | A traversal to all input connections (excluding nested) for a block
_deep_input :: Traversal' MainChunk Input
_deep_input k (MainChunk iput gates wires) = let
    iput' = traverse k iput
    gate' = (traverse . _input) k gates
  in MainChunk <$> iput' <*> gate' <$$> wires

data Gate =
       And Input Input Output
     | Or  Input Input Output
     | Xor Input Input Output
     | Not Input Output
     | Flop Input Input Word8 Word8 Output Output
     | Nested Ptr MainChunk
     deriving (Show, Eq)

_input :: Traversal' Gate Input
_input k (And i1 i2 out) = And <$> k i1 <*> k i2 <$$> out
_input k (Or i1 i2 out)  = Or  <$> k i1 <*> k i2 <$$> out
_input k (Xor i1 i2 out) = Xor <$> k i1 <*> k i2 <$$> out
_input k (Not i1 out)    = Not <$> k i1 <$$> out
-- _input k (Flop i1 i1 c1 c2 out1 out2) = (\i1' i2' -> Flop i1' i2' c1 c2 out1 out2) <$> k i1 <*> k i2
_input k (Flop i1 i2 c1 c2 out1 out2) = Flop <$> k i1 <*> k i2 <$$> c1 <$$> c2 <$$> out1 <$$> out2
_input _ nst@(Nested _ _) = pure nst

_output :: Traversal' Gate Output
_output k (And i1 i2 out) = And i1 i2 <$> k out
_output k (Or i1 i2 out)  = Or i1 i2 <$> k out
_output k (Xor i1 i2 out) = Xor i1 i2 <$> k out
_output k (Not i1 out) = Not i1 <$> k out
_output k (Flop i1 i2 c1 c2 out1 out2) = Flop i1 i2 c1 c2 <$> k out1 <*> k out2
_output _ nst@(Nested _ _) = pure nst

_nested :: Traversal' Gate (Ptr, MainChunk)
_nested k (Nested ptr chnk) = uncurry Nested <$> k (ptr, chnk)
_nested _ x                 = pure x

-- | Correct the pointers inside nested blocks
--   so that they are relative to the start of the nested block
nested :: Ptr -> MainChunk -> Gate
nested ptr@(Ptr blockStart) chnk = Nested ptr $ over (_deep_input . _input_ptr) shift chnk
  where
    shift :: Ptr -> Ptr
    shift (Ptr x) = Ptr (x - blockStart)

-- | An input bundled with it's position in the file
data Input = Input Ptr GateState deriving (Show, Eq)

-- | The state of a gate input. O = unpowered, I = powered
data GateState = O | I deriving (Enum, Show, Eq)

_input_ptr :: Lens' Input Ptr
_input_ptr k (Input ptr s) = Input <$> k ptr <$$> s

-- | A pointer to an absolute position in the file.
-- This represents an input for a gate or an output
-- for the whole circuit (if n is in [0..7])
newtype Ptr = Ptr Word16 deriving (Show, Eq, Num, Enum)

type Output = [Ptr]

data Wire = Wire {wireFrom ::Ptr, wireTo :: Output} deriving (Show, Eq)

-- * Size
class Sized a where
  size :: a -> Word16

instance Sized a => Sized [a] where
  size = getSum . foldMap (Sum . size)

instance Sized Input where
  size _ = 1

instance Sized Wire where
  size (Wire input out) = 1 + size input + size out

instance Sized Ptr where
  size _ = 2

instance Sized MainChunk where
  size (MainChunk inputs gates wires) = 2 + size inputs + size gates + size wires

instance (FixedList f, Sized (f a), Sized a) => Sized (Cons f a) where
  size xs = size $ toList xs

instance Sized (Nil a) where
  size Nil = 0

instance Sized MetaData  where
  size _ = 0x135

-- A circuit file is always fixed size
instance Sized Circuit where
  size _ = 0x535

instance Sized Gate where
  size (And _ _ o) = 4 + size o
  size (Or  _ _ o) = 4 + size o
  size (Xor _ _ o) = 4 + size o
  size (Not _   o) = 3 + size o
  size (Flop _ _ _ _ o1 o2) = 7 + size o1 + size o2
  size (Nested _ nest) = 1 + size nest

-- * Serializing



-- | This is the main parser for a whole file
serializeCircuit :: Circuit -> ByteString
serializeCircuit = runPut . putCircuit


putCircuit :: Putter Circuit
putCircuit (Circuit main meta) = do
  putMain main
  let padding = 0x400 - meta_length meta
  when (padding <= 0 ) $ error $ "Circuit too large: " ++ show padding
  skipPut padding
  putMeta meta

skipPut :: Putter Int
skipPut n = putByteString $ B.replicate n 0x00

putMeta :: Putter MetaData
putMeta (MetaData inorout len descr) = do
  mapM_ put inorout
  putWord16le $ fromIntegral len
  putDescription descr

putDescription :: Putter Description
putDescription (header, descr) = do
  putCString header
  mapM_ putCString descr

putCString :: Putter ByteString
putCString str = do
  putByteString str
  putWord8 0x00


putMain :: Putter MainChunk
putMain (MainChunk istate gates wires) = do
   mapM_ putInput istate
   mapM_ putGate gates
   putWord8 0x07
   mapM_ putWire wires
   putWord8 0xFF

putGate :: Putter Gate
putGate (And i1 i2 os) = putWord8 0x01 *> putInput i1 *> putInput i2 *> putOutput os
putGate (Or  i1 i2 os) = putWord8 0x02 *> putInput i1 *> putInput i2 *> putOutput os
putGate (Xor i1 i2 os) = putWord8 0x03 *> putInput i1 *> putInput i2 *> putOutput os
putGate (Not i1    os) = putWord8 0x04 *> putInput i1 *> putOutput os
putGate (Flop i1 i2 s1 s2 o1 o2) = do
  putWord8 0x05
  putInput i1
  putInput i2
  put s1
  put s2
  putOutput o1
  putOutput o2
putGate (Nested _ nstd) = putWord8 0x06 *> putMain nstd

putInput :: Putter Input
putInput (Input _ state) = put state

putOutput :: Putter Output
putOutput xs = mapM_ putPtr xs *> putWord8 0xFF

putPtr :: Putter Ptr
putPtr (Ptr x) = putWord16be x

putWire :: Putter Wire
putWire (Wire from to) = do
  putPtr from
  putOutput to

-- * Parsing

-- | This is the main parser for a whole file
parseCircuit :: ByteString -> Either String Circuit
parseCircuit = runGet getCircuit


getCircuit :: Get Circuit
getCircuit = do
      meta <- lookAhead $ skip 0x400 >> getMeta
      let len = meta_length meta
      main <- isolate len getMain
      return $ Circuit main meta

getMeta :: Get MetaData
getMeta = label "meta" $ do
   inorout <- replicateM 8 get
   len <- fromIntegral <$> getWord16le
   descr <- getDescription
   void $ mfilter id isEmpty -- Check that there is no extra data at the end
   return $ MetaData inorout len descr

getDescription :: Get Description
getDescription = label "description" $ do
    header <- getCString 18
    description_lines <- replicateM 8 $ getCString 34
    return (header, description_lines)

instance Serialize InOrOut where
    get = toEnum . fromIntegral <$> getWord8
    put = putWord8 . fromIntegral . fromEnum

instance Serialize GateState where
    get = toEnum . fromIntegral <$> getWord8
    put = putWord8 . fromIntegral . fromEnum

getMain :: Get MainChunk
getMain = label "main" $ do
   istate <- createM getInput
   gates <- many $ getGate -- <* skipByteEq 0xFF
   -- traceShowM gates
   skipByteEq 0x07 -- Start of wires
   wires <- manyTill 0xFF getWire
   -- void $ label "isEOF" $ mfilter id isEmpty -- Check that there is no extra data at the end
   remains <- getRemaining
   unless (B.null remains) $
     traceM $ "Warning: " ++ show remains
   return $ MainChunk istate gates wires

-- | A version of getMain for nested chunks
getMain' :: Get MainChunk
getMain' = label "nest" $ do
   istate <- createM getInput
   gates <- many getGate
   skipByteEq 0x07 -- Start of wires
   wires <- manyTill 0xFF getWire
   return $ MainChunk istate gates wires

getGate :: Get Gate
getGate = -- traceShowIdM $
        And <$ skipByteEq 0x01 <*> getInput <*> getInput <*> getOutput
    <|> Or  <$ skipByteEq 0x02 <*> getInput <*> getInput <*> getOutput
    <|> Xor <$ skipByteEq 0x03 <*> getInput <*> getInput <*> getOutput
    <|> Not <$ skipByteEq 0x04 <*> getInput <*> getOutput
    <|> Flop <$ skipByteEq 0x05 <*> getInput <*> getInput <*> get <*> get <*> getOutput <*> getOutput
    <|> nested <$ skipByteEq 0x06 <*> getOffset <*> getMain'

getInput :: Get Input
getInput = do
    pos <- getOffset
    state <- get
    return $ Input pos state


getPtr :: Get Ptr
getPtr = Ptr <$> getWord16be

getOutput :: Get Output
getOutput = manyTill 0xFF getPtr

getWire :: Get Wire
getWire = -- traceShowIdM $
  Wire <$> getPtr <*> getOutput
-- getWire = Wire <$> getPtr <*> getOutput

manyTill :: Word8 -> Get x -> Get [x]
manyTill x getX = go
  where
    go = [] <$ skipByteEq x <|> (:) <$> getX <*> go

getCString :: Int -> Get ByteString
getCString n = do
    str <- getByteString n
    skipByteEq 0x00
    return str

skipByteEq :: Word8 -> Get ()
skipByteEq = void . getByteEq

getByteEq :: Word8 -> Get Word8
-- getByteEq c = mfilter (== c) getWord8
getByteEq c = do
          x <- getWord8
          if x == c
             then return x
             else fail $ "Found: '\\" ++ show x ++ "', Expected: '\\" ++ show c ++ "'"

-- | Get all remaining bytes
getRemaining :: Get ByteString
getRemaining = do
   len <- remaining
   getBytes len

-- * Debug

fetchData :: IO ByteString
fetchData = B.readFile "circuits/builtin/4BITCNTR.CSV"

getparse :: IO (Either String Circuit)
getparse = parseCircuit <$> fetchData

printErrCircuit :: Either String Circuit -> IO ()
printErrCircuit (Left err) = print $ "Error: " ++ err
printErrCircuit (Right circt) = printCircuit circt


printCircuit :: Circuit -> IO ()
printCircuit crc = do
  printMainChunk . cmain $ crc
  printMeta . cmeta $ crc

printMeta :: MetaData -> IO ()
printMeta (MetaData io len (hd,descr)) = do
  putStrLn "\nMeta:"
  print io
  putStr "Length = "
  print len
  putStrLn ""
  B8.putStrLn hd
  mapM_ B8.putStrLn descr

printMainChunk :: MainChunk -> IO ()
printMainChunk (MainChunk inputs gates wires) = do
  print inputs
  mapM_ printGate gates
  mapM_ print wires

printGate :: Gate -> IO ()
printGate (Nested ptr chunk) = do
  putStr "\nNested gate at: "
  print ptr
  printMainChunk chunk
  putStr "End of gate: "
  print ptr
  putStrLn ""
printGate other = print other

-- * Handling data

getNestedBlocks :: Either String Circuit -> [(Ptr, MainChunk)]
getNestedBlocks = toListOf (_right . _mainChunk . _gates . traverse . _nested)

-- * Helpers

-- | Print a value or a error message
printErr :: Show a => Either String a -> IO ()
printErr (Right a) = print a
printErr (Left err) = Prelude.putStrLn $ "Error: " ++ err

-- | Get right or throw error in left
getRight :: Either String b -> b
getRight = either error id

-- | Convert an either value into an exception
expect :: String -> Either String b -> b
expect str = either (\err -> error $ str ++ err) id

-- | Find the offset of a bytestring
getBSOffset :: ByteString -> Int
getBSOffset (BI.PS _ off _) = off

-- | Find current offset in input. Warning: this is very unstable and may depend on how the bytestring was created
getOffset :: Get Ptr
getOffset = Ptr . fromIntegral . getBSOffset <$> getBytes 0

traceShowIdM :: (Monad m, Show b) => m b -> m b
traceShowIdM m = do
    x <- m
    traceShowM x
    return x
