module Lights where

import Types
import Numeric.Vector
import Numeric.Scalar (fromScalar, scalar)
import SceneRandom
import Control.Monad.State
import Raycast

sampleLight :: Vec3d -> Vec3d -> Light -> State SceneContext (Vec3d, Vec3d)

sampleLight start normal (Directional angle strength direction color)  = do
  dir <- randVecCone (angle * pi / 180) direction
  let incidence = normal · negate dir
  ss <- get
  let scene = ss_getScene ss
  case raycast scene (Ray start (negate dir)) of
    Just _ -> return $ (vec3 0 0 0, negate dir)
    Nothing -> return $ (color * (fromScalar $ incidence * scalar strength), negate dir)
  
-- TODO: Point light sampling
sampleLight _ _ _ = return $ (vec3 0 0 0, vec3 0 0 0)
