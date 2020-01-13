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
import VecUtil (unpack3, vec3Of)
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

shade :: Int -> State SceneContext Vec3d
shade bounceCount = get >>= (\ctx -> if bounceCount > s_getBounces (ss_getScene ctx) then return (vec3Of 0) else do
  let hit = ss_getHit ctx
  let brdf = rh_getShader $ hit
  col <- reflectance brdf
  let scene = ss_getScene ctx
  let lights = s_getLights scene
  let norm = rh_getNormal hit

  mReflectRay <- sampleBRDF brdf
  (reflectColor, reflectProbability) <- case mReflectRay of
    Nothing -> return (0, 0)
    Just reflectRay -> do
      prob <- probability brdf (r_getDir reflectRay)
      case raycast scene reflectRay of
        Nothing -> return $ (s_getSkyColor scene, )prob
        Just hit -> do
          let (v, s) = runState (shade (bounceCount + 1)) $ makeContext scene (ss_getGen ctx) hit
          put s
          return (v, prob) -- 1 / (2 * PI)

  let newRayPos = (rh_getPos hit) + (norm * (vec3Of 0.01))

  lighting <- sequence $ map (sampleLight newRayPos norm) lights
  let lighting' = filter ((>0) . fst) lighting
  lightProbabilities <- sequence $ map (probability brdf . snd) lighting'
  let lightColors = map fst lighting'

  let probabilitySum = reflectProbability + sum lightProbabilities

  let adjustedReflectColor = reflectColor * vec3Of reflectProbability
  let adjustedIncomingLighting = sum $ zipWith (*) lightColors (map vec3Of lightProbabilities)
  
  let incomingLight = adjustedReflectColor + adjustedIncomingLighting
  
  let lit = col * incomingLight
  
  reflectColor <- reflectance brdf
  return lit
  )

samplePixel :: Scene -> (Int, Int) -> Int -> Vec3d
samplePixel s (x, y) sample = let w = s_getWidth s
                                  h = s_getHeight s
                                  cam = s_getCamera s
                                  skyCol = s_getSkyColor s
                                  -- Random seed is an arbitrary formula based on pixel location to form distinct results
                                  gen = mkStdGen $ (x + y * w + x * (x + y) + (s_getSeed s)) * sample
                              in  possibly (fst . runState (shade 0) . makeContext s gen) (skyCol / (2 * pi)) $
                                  raycast s $
                                  rayFrom cam $
                                  (fromIntegral x / fromIntegral w * 2.0 - 1.0, (fromIntegral h / fromIntegral w) - fromIntegral y / fromIntegral w * 2.0)
  
renderPixel :: Scene -> (Int, Int) -> Vec3d
renderPixel s p = let numSamples = s_getSamples s
                      samples = map (samplePixel s p)  [1..numSamples]
                  -- Multiply by 2pi for monte-carlo integration because the volume of the integration bounds is 2pi
                  -- (integration bounds form a hemisphere)
                  in  sum samples / fromIntegral numSamples * vec3Of (2 * pi)

render :: Scene -> [[(Double, Double, Double)]]
render s = let w = s_getWidth s
               h = s_getHeight s
               cam = s_getCamera s
               skyCol = s_getSkyColor s
               seed = 1
               gen = mkStdGen seed
          in  map2 unpack3 $
              map2 (renderPixel s) $
              [ [ (x, y) | x <- [1..w] ] | y <- [1..h] ]

-- Found how to evaluate the list in parallel from
-- https://stackoverflow.com/a/5606176
parRender :: Scene -> [[(Double, Double, Double)]]
parRender s = (render s) `using` parList rdeepseq
