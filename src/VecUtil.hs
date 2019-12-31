{-# LANGUAGE MagicHash, UnboxedTuples #-}

module VecUtil (unpack2, unpack3, tangentBitangent, vec3Of, vec2Of) where

import Numeric.Vector

unpack2 :: Vec2d -> (Double, Double)
unpack2 v = case unpackV2# v of
              (# x, y #) -> (x, y)

unpack3 :: Vec3d -> (Double, Double, Double)
unpack3 v = case unpackV3# v of
              (# x, y, z #) -> (x, y, z)

tangentBitangent :: Vec3d -> (Vec3d, Vec3d)
tangentBitangent normal =
  -- Cross product with an arbitrary vector to produce the tangent. The if is for the rare case that
  -- the normal is the arbitrary vector. Is there a better approach?
  let tangent = normalized $ normal × (if normal == (vec3 1 0 0) then vec3 0 1 0 else vec3 1 0 0)
      bitangent = normal × tangent -- Don't need to normalize, magnitude is mathematically 1
  in (tangent, bitangent)

vec3Of :: Double -> Vec3d
vec3Of = realToFrac

vec2Of :: Double -> Vec2d
vec2Of = realToFrac
