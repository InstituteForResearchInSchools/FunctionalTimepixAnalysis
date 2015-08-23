--this is the import block, it's pretty much like any other import block
--
--TimepixData is a directory which will contain all modules that work on, Timepix data
--
--TimepixData.Clusterer is imported and then given the name "Clusterer" to save typing
--time, inside this module currently is one clustering function "queueCluster" but more
--should be added so clustering implementations can be selectively chosen
--
--TimepixFiles is another directory, this will contain all modules that work on Timepix
--files themselves rather than the scientific data, the actual IO should probably be moved
--into here at some point
--
--TimepixFiles.XYCHandler is a module that handles the XYC format files, currently all it
--can do is a function called "formatLines" which a String list is passed into and it
--returns a 3-tuple of (x,y,c). This should probably be generalised at some point so any
--string list is passed in and it returns a tuple as this could be used for matrix files
--
--TimepixFiles.MHandler is a module that handles the pixel matrix format files, not sure
--if this is the correct term but basically the files where its a 256 x 256 matrix of c
--values, this module will generate a 3-tuple list of non-zero pixels
--
--System.IO ships with Haskell and contains functions needed for I/O
--
--System.Environment also ships with Haskell and is needed to get arguments from terminal
import qualified TimepixData.Clusterer as Clusterer
import qualified TimepixFiles.XYCHandler as XYCHandler
import qualified TimepixFiles.MHandler as MHandler
import TimepixData.Classify
import System.IO
import System.Environment

--this is the main block, it runs when ./FunctionalTimepixAnalysis is called from terminal
--
--the do block ties a load of actions together into one new action (yes, I realise it
--looks horribly like imperative code but I promise it isn't!)
--
--getArgs pulls the arguments out of the command line and binds the result to the
--fileType:filePath:[] as we expect two arguments, the first telling us what type of
--Timepix file we expect to be dealing with and the second telling us the file path to the
--file.
--
--readFile is then called with the argument supplied in call of program, this should be
--a file path to XYC format file, the result is bound to contents
--
--a let expression is used to bind a load of stuff; we call lines on contents to map the
--String of contents to a String list "linesOfFile", pixels is a function that returns a
--3-tuple list, essentially a list of pixels, the first argument that called
--FunctionalTimepixAnalysis tells us the file type, "-x" being an xyc file and "-m" being
--a pixel matrix file, this function then uses this to toggle which handler it uses to
--format the lines into a pixel list. Clusters is then bound to calling the
--queueCluster function in the Clusterer module and this is a list of lists of these
--3-tuples. Essentially a list of clusters where each list of clusters is a list of
--pixels. It is obvious that a recursive datatype system needs to be implemented at some
--stage!
--
--finally we just put a String to the terminal to confirm code has executed
main = do (fileType:filePath:[]) <- getArgs
          contents <- readFile filePath
          let
              linesOfFile = lines contents
              pixels :: String -> [String] -> [(Int,Int,Float)]
              pixels t l
                  | t == "-x" = XYCHandler.formatLines l
                  | t == "-m" = MHandler.formatLines 0 l
              clusters = Clusterer.queueCluster (pixels fileType linesOfFile)
              classifications = map classifyCluster clusters
          putStrLn $ show classifications
