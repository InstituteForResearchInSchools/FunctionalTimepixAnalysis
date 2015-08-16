# FunctionalTimepixAnalysis
Haskell toolkit being developed for Timepix data analysis

**WARNING: This code is very much in development, in fact this is the very first upload of the code, it does not contain full functionality yet, in fact it's pretty much a bare skeleton of what we intend to build. Use it with caution!**

## How to use
Before or after downloading the code to your machine but definitely before running it you need to install a Haskell compiler, I recommend the Glasgow Haskell Compiler (GHC).

After installing GHC, navigate to the folder on your Linux or Mac machine and run the following two commands:

	[computer-name]:FunctionalTimepixAnalysis [username]$ ghc --make FunctionalTimepixAnalysis.hs
	
	[computer-name]:FunctionalTimepixAnalysis [username]$ ./FunctionalClusterAnalysis -x [path-to-xyc-file]
	OR
	[computer-name]:FunctionalTimepixAnalysis [username]$ ./FunctionalClusterAnalysis -m [path-to-pixel-matrix-file]
	
This will give no output as currently everything is calculated internally...

## To Do

* Add error handling to whole toolkit
* Prove correctness and performance of queueCluster
* Decide what metrics need to be calculated from the clusters to allow for particle identification
* Decide on an output standard
* Print nicer things to command line
* Many other things
