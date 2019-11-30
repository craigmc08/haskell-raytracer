module Shader.SColor (SColor (..), sample) where

import Types
import Control.Monad.Reader
import Numeric.Vector
import qualified Shader.SVector as Vec
import qualified Shader.SValue as Val

sample :: SColor -> Reader SceneContext Vec3d

sample ColVertex = do
  ss <- ask
  return $ rh_getColor $ ss_getHit ss


sample (ColVector v) = Vec.sample v

sample (ColConst vr vg vb) = do
  r <- Val.sample vr
  g <- Val.sample vg
  b <- Val.sample vb
  return $ vec3 r g b
