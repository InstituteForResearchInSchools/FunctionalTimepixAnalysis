--this is the XYC file format handler module
--
--first comes the functions that this module exports, currently this is just the 
--formatLines function which takes a String list and returns a list of (Int,Int,Float)
--3-tuples which is essentially a list of pixels, type system needs to be implemented
module TimepixFiles.XYCHandler
( formatLines
) where

--this is the formatLines function
--
--the type definition takes a string list and returns what is essentially a hit pixel list
--
--there are two patterns
--
--the first is the case where formatLines takes an empty list 
--which signals it has reached end of string list and so it terminates recursion by giving
--empty list
--
--the second pattern must be a non-empty list, t uses a let expression to bind a string 
--list "x:y:c:[]" to the words of the first element of the list passed to formatLines this
--forms a list where each element is the non-whitespace of the line. The variables x', y'
--and c' are bound to the value of calling read with x, y and c respectively, this is like
--a type cast. Finally in the in part of the expression a 3-tuple is formed and then 
--constructed with a recursive call on the remainder of the original list
formatLines :: [String] -> [(Int,Int,Float)]
formatLines []     = []
formatLines (l:ls) = let 
                         (x:y:c:[]) = words l
                         x' = read x
                         y' = read y
                         c' = read c
                     in   
                         (x',y',c') : (formatLines ls)                                  