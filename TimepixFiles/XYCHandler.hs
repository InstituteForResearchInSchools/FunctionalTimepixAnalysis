module TimepixFiles.XYCHandler
( formatLines
) where

formatLines :: [String] -> [(Int,Int,Float)]
formatLines []     = []
formatLines (l:ls) = let 
                         (x:y:c:[]) = words l
                         x' = read x
                         y' = read y
                         c' = read c
                     in   
                         (x',y',c') : (formatLines ls)