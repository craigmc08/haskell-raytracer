module Raycast where

import           Control.Monad (guard)
import           Data.Maybe (isJust, fromJust)
import Types
import Ray (createHit)
import Numeric.Vector
import Numeric.Scalar

-- · ×
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
