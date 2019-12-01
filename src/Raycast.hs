module Raycast (intersect, findHits, raycast) where

import           Control.Monad (guard)
import           Data.Maybe (isJust, fromJust, listToMaybe)
import Types
import Ray (createHit)
import Numeric.Vector
import Numeric.Scalar

-- https://courses.cs.washington.edu/courses/csep557/10au/lectures/triangle_intersection.pdf
intersect :: Ray -> Object -> Tri -> Maybe RayHit
intersect ray@(Ray pos dir) obj tri = do
  let a = v_getPos $ t_getP0 tri
      b = v_getPos $ t_getP1 tri
      c = v_getPos $ t_getP2 tri
      n = normalized $ (b - a) × (c - a)
      d = n · a
      t = (d - n · pos) / (n · dir)
      q = pos + dir * (fromScalar t)

      areaQAB = ((b - a) × (q - a)) · n
      areaQBC = ((c - b) × (q - b)) · n
      areaQCA = ((a - c) × (q - c)) · n
      areaABC = ((b - a) × (c - a)) · n
      
  guard $ areaQAB >= 0
  guard $ areaQBC >= 0
  guard $ areaQCA >= 0
  guard $ t > 0
  -- Only counts as a hit if the ray is traveling towards the front of the triangle
  -- (when direction and normal are closer to antiparallel)
  guard $ (dir · n) < 0

  let alpha = unScalar $ areaQBC / areaABC
      beta = unScalar $ areaQCA / areaABC
      gamma = unScalar $ areaQAB / areaABC

  let hitPos = pos + dir * (fromScalar t)

  return $ createHit obj tri ray (unScalar t) (alpha, beta, gamma) hitPos
                                             
findHits :: Scene -> Ray -> [RayHit]
findHits s ray = let objs = s_getObjects s
                     intersections = concatMap (\o -> map (intersect ray o) $ o_getTris o) objs
                 -- It's safe to use `fromJust` here because the filter guarantees all values are `Just`
                 in  map fromJust $ filter isJust intersections

minimumBy :: (Ord b) => (a -> b) -> [a] -> a
minimumBy f xs = foldr1 (\m x -> if f x < f m then x else m) xs

raycast :: Scene -> Ray -> Maybe RayHit
raycast s ray@(Ray origin _) = let hs = findHits s ray
                               in  if   length hs == 0 then Nothing
                                   else Just $ minimumBy rh_getDistance hs
