--the Clusterer module is used to form clusters from (Int,Int,Float) lists which are 
--essentially hit pixel lists
--
--this module exports one function, "queueCluster", but this should be expanded
module TimepixData.Clusterer
( queueCluster
) where

--queueCluster is a clustering function which the type definition shows takes a hit pixel
--list and then returns a list of hit pixel lists (aka a list of clusters)
--
--this just treats clusters as simple mathematical entities whereby a cluster is defined 
--as either a grouping of particles where each pixel in the cluster is within one x and
--y coordinate of at least one other pixel in the cluster or the cluster contains only
--one pixel and this pixel is not within 1 x and y coordinate of any other in the file
--
--it will be up to another function(s) to map these simple entities to the physical 
--particles
--
--queueCluster works by taking a hit pixel list, each recursive call of queueCluster will
--have constructed one cluster and removed every pixel in this cluster from the list of 
--unclustered pixels which for its single argument. Inside there is a "let in" expression
--this has a function definition "mag" which just strips the negative sign of integers, 
--the canCluster function is a boolean value function which takes a pixel list (a cluster) 
--and a pixel it then checks to see if the pixel can cluster into this list, finally the 
--makeCluster function takes a cluster, a queue of hit pixels (hence the name!) and then
--another queue which non-clustered pixels get put in, it's output is a cluster list.
--
--it's difficult to explain this code via comments as there are three levels of recursion 
--going on and lots of forming and reforming queues... I will try and improve the 
--documentation in due course...
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