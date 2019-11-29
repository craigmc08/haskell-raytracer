module Img where

import Graphics.Image.IO (writeImage)
import Graphics.Image (Image, RGB, fromLists, Pixel (PixelRGB) )
import Graphics.Image.Interface.Vector (VS)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

writeImg :: FilePath -> [[(Double, Double, Double)]] -> IO ()
writeImg fp pss = writeImage fp $ (fromLists $ map (map (uncurry3 PixelRGB)) pss :: Image VS RGB Double)
