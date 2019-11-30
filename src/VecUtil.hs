{-# LANGUAGE MagicHash, UnboxedTuples #-}

module VecUtil (unpack2, unpack3) where

import Numeric.Vector

unpack2 :: Vec2d -> (Double, Double)
unpack2 v = case unpackV2# v of
              (# x, y #) -> (x, y)

unpack3 :: Vec3d -> (Double, Double, Double)
unpack3 v = case unpackV3# v of
              (# x, y, z #) -> (x, y, z)
