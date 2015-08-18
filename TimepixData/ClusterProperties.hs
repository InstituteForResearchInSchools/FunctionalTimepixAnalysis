-- A set of functions for determining metrics of a cluster in the format [(x, y, c)]
-- Depends on the module statistics-linreg for linear regression algorithm; hackage.haskell.org/package/statistics-linreg
-- Real documentation coming soon...

module TimepixData.ClusterProperties
( clusterProperties
) where

import qualified Data.Vector.Unboxed as U
import Statistics.LinearRegression

-- Returns a tuple of all of a cluster's properties:
-- (centroid, radius, numberPixels, density, squiggliness)
clusterProperties :: [(Int, Int, Double)] -> ((Double, Double), Double, Int, Double, Double)
clusterProperties cluster =
  let pixels = [ (fromIntegral x, fromIntegral y) | (x, y, c) <- cluster ] in
    (findCentroid pixels, findRadius pixels, findNumPixels pixels, findDensity pixels, findSquiggliness pixels)


-- A few helper fuctions
distance (x1, y1) (x2, y2) = sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
pointLineDistance (x, y) (m, c) = abs (m * x - y + c) / sqrt (1 + m ^ 2)
mean xs = sum xs / fromIntegral (length xs)

-- Definitions of various properties of a cluster
findCentroid pixels = (mean (map fst pixels), mean (map snd pixels))

findRadius pixels = maximum [ distance pixel centroid | pixel <- pixels ]
  where centroid = findCentroid pixels

findNumPixels pixels = length pixels

findDensity pixels
  | area == 0 = 1
  | otherwise = fromIntegral numPixels / area
  where
    area = radius^2 * pi
    radius = findRadius pixels
    numPixels = findNumPixels pixels

findSquiggliness pixels
  -- If all x values or all y values are the same, the blob is a straight line, so has 0 squiggliness
  | all (== head xs) (tail xs) = 0.0
  | all (== head ys) (tail ys) = 0.0
  -- Otherwise, calculate a line of best fit...
  | otherwise = let
      -- x and y values need to be in a vector form for the regression algorithm to work
      vxs = U.fromList xs
      vys = U.fromList ys
      (intercept, gradient) = linearRegressionTLS vxs vys in
        -- Find the mean distance between hit pixels and the LoBF
        mean [ pointLineDistance pixel (gradient, intercept) | pixel <- pixels ]
  where
    (xs, ys) = (map fst pixels, map snd pixels)
