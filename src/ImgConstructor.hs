{-# LANGUAGE MagicHash, UnboxedTuples #-}

module ImgConstructor where

import Numeric.Vector
import Types
import Control.Monad.Reader
import Camera (rayFrom)
import Raycast (findHits)

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f = map (map f)

vec23 :: Vec2d -> (Double, Double, Double)
vec23 v = case unpackV2# v of
            (# u, v #) -> (u, v, 1)

render :: Scene -> [[(Double, Double, Double)]]
render s = let w = s_getWidth s
               h = s_getHeight s
               cam = s_getCamera s
           in  map2 (\hs -> if length hs > 0 then vec23 (rh_getTexCoord (head hs)) else (0, 0, 0)) $
               map2 (findHits s) $
               map2 (rayFrom cam) $
               [[(fromIntegral x / fromIntegral w * 2.0 - 1.0, (fromIntegral h / fromIntegral w) - fromIntegral y / fromIntegral w * 2.0) | x <- [1..w]] | y <- [1..h]]
