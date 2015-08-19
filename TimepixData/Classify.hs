module TimepixData.Classify
( classifyCluster
) where

import TimepixData.ClusterProperties

-- TODO encode this data in a more readable way...
-- [radius, numberPixels, density, squiggliness]; (lowerBound, upperBound)
particleDefinitions :: [(String, [(Double, Double)])]
particleDefinitions = [
  ("BETA", [(0.0, 20.0), (11, 49), (0.0, 0.75), (0.9, 65536.0) ] ),
  ("BETA", [(0.0, 3.6), (5, 10), (0.0, 65536.0), (0.0, 65536.0) ] ),
  ("BETA", [(0.71, 65536.0), (4, 4), (0.0, 65536.0), (0.0, 65536.0) ] ),
  ("BETA", [(10.0, 70.0), (50.0, 1000.0), (0.0, 1000.0), (0.0, 65536.0) ] ),
  ("BETA", [(0.75, 65536.0), (3, 3), (0.0, 65536.0), (0.0, 65536.0) ] ),
  ("BETA", [(3.0, 9.99), (11, 49), (0.0, 0.35), (0.0, 0.9) ] ),
  ("BETA", [(0.0, 4.2), (11, 49), (0.35, 1.01), (0.0, 0.9) ] ),
  ("GAMMA", [(0.0, 0.71), (4, 4), (0.0, 65536.0), (0.0, 65536.0) ] ),
  ("GAMMA", [(0.0, 65536.0), (2, 2), (0.0, 65536.0), (0.0, 65536.0) ] ),
  ("GAMMA", [(0.0, 0.75), (3, 3), (0.0, 65536.0), (0.0, 65536.0) ] ),
  ("GAMMA", [(0.0, 65536.0), (1, 1), (0.0, 65536.0), (0.0, 65536.0) ] ),
  ("ALPHA", [(1.0, 2.85), (11, 24), (0.84, 65536.0), (0.74, 65536.0) ] ),
  ("ALPHA", [(3.5, 9.0), (90, 120), (0.0, 2.0), (0.0, 65536.0) ] ),
  ("ALPHA", [(2.5, 6.0), (25, 90), (0.75, 1.5), (1.05, 2.5) ] )
  ]


classifyCluster cluster
  | null classifications = "OTHER"
  | otherwise = head classifications
    where
      properties = clusterProperties cluster -- returns (centroid, radius, numberPixels, density, squiggliness)
      classifications = [ name | (name, definition) <- particleDefinitions, isMatch properties definition ]

isMatch :: ((Double, Double), Double, Int, Double, Double) -> [(Double, Double)] -> Bool
isMatch (centroid, radius, numberPixels, density, squiggliness) definition =
  and $ map checkProperty [ (radius, definition !! 0),
                            (fromIntegral numberPixels, definition !! 1),
                            (density, definition !! 2),
                            (squiggliness, definition !! 3) ]
  where
    checkProperty (value, (lower, upper)) = (value >= lower) && (value <= upper)
