{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Render (render, parRender) where

import Control.Parallel.Strategies

import Numeric.Vector
import Numeric.Scalar (fromScalar, scalar)
import Types
import Control.Monad.State
import Camera (rayFrom)
import Raycast (raycast)
import Shader (reflectance, sampleBRDF, probability)
import Scene (makeContext)
import VecUtil (unpack3)
import System.Random
import SceneRandom
import Lights

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f = map (map f)

vec23 :: Vec2d -> (Double, Double, Double)
vec23 v = case unpackV2# v of
            (# u, v #) -> (u, v, 1)

possibly :: (a -> b) -> b -> Maybe a -> b
possibly _ def Nothing = def
possibly f _ (Just x) = f x

shade :: State SceneContext Vec3d
shade = do
  ctx <- get
  let hit = ss_getHit ctx
  let brdf = rh_getShader $ hit
  col <- reflectance brdf
  let scene = ss_getScene ctx
  let lights = s_getLights scene
  let norm = rh_getNormal hit
  let newRayPos = (rh_getPos hit) + (norm * (vec3 0.01 0.01 0.01))
  lighting <- sequence $ map (sampleLight newRayPos norm) lights
  let lighting' = filter (>0) lighting
  let lit = col * ((sum lighting') + (s_getSkyColor scene * (vec3 0.1 0.1 0.1)))
  return lit

samplePixel :: Scene -> (Int, Int) -> Vec3d
samplePixel s (x, y) = let w = s_getWidth s
                           h = s_getHeight s
                           cam = s_getCamera s
                           skyCol = s_getSkyColor s
                           -- Random seed is an arbitrary formula based on pixel location to form distinct results
                           gen = mkStdGen $ x + y * w + x * (x + y) + (s_getSeed s)
                       in  possibly (fst . runState shade . makeContext s gen) skyCol $
                           raycast s $
                           rayFrom cam $
                           (fromIntegral x / fromIntegral w * 2.0 - 1.0, (fromIntegral h / fromIntegral w) - fromIntegral y / fromIntegral w * 2.0)
  

render :: Scene -> [[(Double, Double, Double)]]
render s = let w = s_getWidth s
               h = s_getHeight s
               cam = s_getCamera s
               skyCol = s_getSkyColor s
               seed = 1
               gen = mkStdGen seed
          in  map2 unpack3 $
              map2 (samplePixel s) $
              [ [ (x, y) | x <- [1..w] ] | y <- [1..h] ]
          --  in  map2 unpack3 $
          --      map2 (possibly (fst . runState shade . makeContext s gen) skyCol) $
          --      map2 (raycast s) $
          --      map2 (rayFrom cam) $
          --      [[(fromIntegral x / fromIntegral w * 2.0 - 1.0, (fromIntegral h / fromIntegral w) - fromIntegral y / fromIntegral w * 2.0) | x <- [1..w]] | y <- [1..h]]

-- Found how to evaluate the list in parallel from
-- https://stackoverflow.com/a/5606176
parRender :: Scene -> [[(Double, Double, Double)]]
parRender s = (render s) `using` parList rdeepseq
