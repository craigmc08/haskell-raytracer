module Shader.BRDF (BRDF (..), sample) where

import Types
import Control.Monad.Reader

sample :: BRDF -> Reader SceneContext (Maybe Ray)
sample BRDFEmpty = return Nothing
