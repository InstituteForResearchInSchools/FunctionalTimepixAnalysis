import qualified TimepixData.Clusterer as Clusterer
import qualified TimepixFiles.XYCHandler as XYCHandler
import System.IO
import System.Environment

main = do (arg:args) <- getArgs
          contents <- readFile arg
          let linesOfFile = lines contents
              pixels = XYCHandler.formatLines linesOfFile
              clusters = Clusterer.queueCluster pixels
          putStrLn ("done.")                   