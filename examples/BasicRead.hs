import qualified RobotOdyssey.Circuit as C

main :: IO ()
main = do
  readPrintFile "resources/circuits/builtin/RSFLOP.CSV"

readPrintFile :: FilePath -> IO ()
readPrintFile file = do
  circuit <- C.parseFile file
  C.printErrCircuit circuit
