module Clusterer
( queueCluster
) where

queueCluster :: [(Int,Int,Float)] -> [[(Int,Int,Float)]]
queueCluster []     = []
queueCluster (p:ps) = let
                          mag :: Int -> Int
                          mag x = if (x < 0) then x * (-1) else x
                          canCluster :: [(Int,Int,Float)] -> (Int,Int,Float) -> Bool
                          canCluster [] (x,y,c) = False
                          canCluster ((xc,yc,cc):cs) (x,y,c) 
                              | xdiff <= 1 && ydiff <= 1 = True
                              | otherwise                = (canCluster cs (x,y,c))
                              where    xdiff = (mag (xc - x))
                                       ydiff = (mag (yc - y))
                          makeCluster :: [(Int,Int,Float)] -> [(Int,Int,Float)] -> [(Int,Int,Float)] -> [[(Int,Int,Float)]]  
                          makeCluster cs [] bs     = (cs:(queueCluster bs))
                          makeCluster cs (f:fs) bs = if (canCluster cs f)
                                                           then
                                                               (makeCluster (f:cs) (fs ++ bs) [])
                                                           else 
                                                               (makeCluster cs fs (f:bs))
                      in
                          makeCluster [p] ps []