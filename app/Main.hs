module Main where

import Lib
import Types
import Numeric.Vector
import Wavefront.Wavefront

cam :: Camera
cam = Camera { c_getPos = vec3 3 2 (-5)
             , c_getForward = vec3 0 0 (1)
             , c_getUp = vec3 0 1 0
             , c_getRight = vec3 1 0 0
             , c_getFOV = 90
             }

checkerShader :: BRDF
checkerShader = let spacing = VecConst 1 1 1
                    quarterSpacing = VecMath VDiv spacing (VecConst 4 4 4)
                    cs = VecMath
                           VSub
                           (VecConst 1 1 1)
                           (VecMath
                             VMul
                             (VecConst 2 2 2) 
                             (VecMath
                               VDiv
                               (VecMath
                                 VMod
                                 (VecMath
                                   VAdd
                                   quarterSpacing
                                   (VecMath VAbs (VecMath VAdd UV quarterSpacing) (VecConst 0 0 0))
                                 )
                                 spacing
                               )
                               spacing
                             )
                           )
                    lt = ValMath LessThan (ValMath Mul (SeparateX cs) (SeparateY cs)) (ValConst 0)
                    value = ValMath Add (ValConst 0.7) (ValMath Mul (ValConst 0.3) lt)
                    color = CombineRGB value value value
                in Diffuse color

shader :: BRDF
shader = Diffuse $ ColConst 0.8 0.8 0.8

sun :: Light
sun = Directional { l_getAngle = 2.5
                  , l_getStrength = 1
                  , l_getDirection = normalized $ vec3 0.5 (-1) (-0.4)
                  , l_getColor = vec3 1 1 1
                  }

scene :: Scene
scene = Scene { s_getCamera = cam
              , s_getObjects = []
              , s_getLights = [sun]
              , s_getSkyColor = vec3 0.6 0.7 0.9
              , s_getWidth = 640
              , s_getHeight = 480
              , s_getSeed = 1
              , s_getSamples = 128
              , s_getBounces = 2
              }

main :: IO ()
main = do
  maybeCube <- readObj "cube.obj" <$> readFile "/home/craig/haskell/raytracer/app/cube.obj"
  case maybeCube of
    Left e -> print e
    Right cube -> do
      -- print cube
      let scene' = scene { s_getObjects = [Object cube shader] }
      writeImg "testimg.png" $ parRender scene'
