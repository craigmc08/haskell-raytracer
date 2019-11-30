module Shader.SValue (SValue (..), sample) where

import Types
import Control.Monad.Reader

sample :: SValue -> Reader SceneContext Double

sample (ValConst x) = return x

sample RayLength = do
  ss <- ask
  return $ rh_getDistance $ ss_getHit ss
