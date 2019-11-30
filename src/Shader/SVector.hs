{-# LANGUAGE UnboxedTuples, MagicHash #-}

module Shader.SVector (SVector (..), sample) where

import Types
import Numeric.Vector
import Control.Monad.Reader

sample :: SVector -> Reader SceneContext Vec3d

sample UV = do
  ss <- ask
  case unpackV2# (rh_getTexCoord $ ss_getHit ss) of
    (# u, v #) -> return $ vec3 u v 0

sample Position = do
  ss <- ask
  return $ rh_getPos $ ss_getHit ss

sample Normal = do
  ss <- ask
  return $ rh_getNormal $ ss_getHit ss

sample (VecConst v) = return v
