module Main where

import Lib

main :: IO ()
main = writeImg "testimg.png" [[(fromIntegral x / 300, fromIntegral y / 200, 1) | x <- [1..300]] | y <- [1..200]]
