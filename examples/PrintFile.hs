import qualified RobotOdyssey.Circuit as C
import System.Environment (getArgs,getProgName)

main :: IO ()
main = do
  args <- getArgs
  me   <- getProgName
  case args of
    [file] -> printFile file
    _      -> putStrLn $ "Usage: " ++ me ++ " CIRCUIT.CSV"

printFile :: FilePath -> IO ()
printFile file = do
  circuit <- C.parseFile file
  C.printErrCircuit circuit
