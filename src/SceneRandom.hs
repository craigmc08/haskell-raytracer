module SceneRandom where

import Numeric.Vector
import Numeric.Scalar (fromScalar, scalar)
import Control.Monad.State
import System.Random
import Types
import VecUtil

randDouble :: State SceneContext Double
randDouble = do
  ss <- get
  let (v, gen) = random (ss_getGen ss) :: (Double, StdGen)
  put $ ss { ss_getGen = gen }
  return v

randRange :: Double -> Double -> State SceneContext Double
randRange l h = randDouble >>= (\v -> return $ v * (h - l) + l)

randNeg1To1 :: State SceneContext Double
randNeg1To1 = randRange (-1) 1

randVecSphere :: State SceneContext Vec3d
randVecSphere = do
  x <- randNeg1To1
  y <- randNeg1To1
  z <- randNeg1To1
  return $ normalized $ vec3 x y z

randVecHemisphere :: Vec3d -> State SceneContext Vec3d
randVecHemisphere normal = do
  let (tangent, bitangent) = tangentBitangent normal

  theta <- randRange 0 (2 * pi)
  phi <- randRange 0 (pi / 2)

  let t = fromScalar $ scalar $ sin theta * cos phi
  let b = fromScalar $ scalar $ cos theta * cos phi
  let n = fromScalar $ scalar $ sin phi
  return $ tangent * t + bitangent * b + normal * n

randVecDisc :: Vec3d -> Vec3d -> Double -> State SceneContext Vec3d
randVecDisc normal center radius = do
  let (tangent, bitangent) = tangentBitangent normal
  theta <- randRange 0 (2 * pi)
  let t = fromScalar $ scalar $ sin theta * radius
  let b = fromScalar $ scalar $ cos theta * radius
  return $ tangent * t + bitangent * b + center

-- https://math.stackexchange.com/a/182936
randVecCone :: Double -> Vec3d -> State SceneContext Vec3d
randVecCone angle vector = do
  let (tangent, bitangent) = tangentBitangent vector
  z <- randRange (cos angle) 1
  phi <- randRange 0 (2 * pi)
  let theta = acos z
  let t = fromScalar $ scalar $ cos phi
  let b = fromScalar $ scalar $ sin phi
  let v = fromScalar $ scalar $ cos theta
  let sinTheta = fromScalar $ scalar $ sin theta
  return $ sinTheta * (t * tangent + b * tangent) + v * vector

randColor :: State SceneContext Vec3d
randColor = do
  r <- randDouble
  g <- randDouble
  b <- randDouble
  return $ vec3 r g b
