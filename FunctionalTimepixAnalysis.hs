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
--System.IO ships with Haskell and contains functions needed for I/O
--
--System.Environment also ships with Haskell and is needed to get arguments from terminal  
import qualified TimepixData.Clusterer as Clusterer
import qualified TimepixFiles.XYCHandler as XYCHandler
import System.IO
import System.Environment

--this is the main block, it runs when ./FunctionalTimepixAnalysis is called from terminal
--
--the do block ties a load of actions together into one new action (yes, I realise it 
--looks horribly like imperative code but I promise it isn't!)
--
--getArgs pulls the arguments out of the command line and binds the result to the arg:args
--pattern. All being well, args should just be an empty list as we only expect one 
--argument currently - obviously as the toolkit expands this will change
--
--readFile is then called with the argument supplied in call of program, this should be 
--a file path to XYC format file, the result is bound to contents
--
--a let expression is used to bind a load of stuff; we call lines on contents to map the
--String of contents to a String list "linesOfFile", pixels is then bound to calling the
--formatLines function in the XYCHandler on these lines to return an (Int,Int,Float) 
--3-tuple list of active pixels in the XYC file, clusters is then bound to calling the 
--queueCluster function in the Clusterer module and this is a list of lists of these 
--3-tuples. Essentially a list of clusters where each list of clusters is a list of 
--pixels. It is obvious that a recursive datatype system needs to be implemented at some 
--stage!
--
--finally we just put a String to the terminal to confirm code has executed
main = do (arg:args) <- getArgs
          contents <- readFile arg
          let linesOfFile = lines contents
              pixels = XYCHandler.formatLines linesOfFile
              clusters = Clusterer.queueCluster pixels
          putStrLn ("done.")                   