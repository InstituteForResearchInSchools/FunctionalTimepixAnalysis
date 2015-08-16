--this module handles pixel matrix files
--
--currently only one function is exported which is "formatLines", this recurses over each
--line of a file and then recurses along each element, binding each non-zero pixel to a 
--3-tuple with that pixels value and its x and y coordinates.
module TimepixFiles.MHandler
( formatLines
) where

--format lines is called with two arguments, the first is the Int type y coordinate and 
--the second is a String list of lines, the first function call should just pass 0 in as 
--the y coordinate initially.
--
--it terminates at the end of the list
--
--inside it rowRecurse is used to move along each column in each line which it generates
--by calling words on the line to create a String list delimited by whitespace, it then
--bind all non zero pixels to a 3-tuple of (x,y,c) using read to generate the actual Float
--value of each pixel, when rowRecurse terminates it starts the next recursive call of 
--formatLines
--
--this should be improved to check valid Timepix files are being passed in, so looking for
--256 x/y coordinates etc. 
formatLines :: Int -> [String] -> [(Int,Int,Float)]
formatLines _ []     = []
formatLines r (l:ls) = let
                         rowRecurse :: Int -> Int -> [String] -> [(Int,Int,Float)]
                         rowRecurse x y [] = (formatLines (r+1) ls)
                         rowRecurse x y (p:ps) 
                             | c /= 0    = (x,y,c) : (rowRecurse (x+1) y ps)
                             | otherwise = (rowRecurse (x+1) y ps)
                             where c = read p 
                       in
                         rowRecurse 0 r (words l)
                                        