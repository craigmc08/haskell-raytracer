module TestWavefront where

import Wavefront.Wavefront

main :: IO ()
main = do
  text <- readFile "/home/craig/haskell/haskell-raytracer/app/cube.obj"
  let tris = readObj "cube.obj" text
  print tris
