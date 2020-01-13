module Main where

import Lib
import Types
import Object.Vertex (createVertex)
import Numeric.Vector

cam :: Camera
cam = Camera { c_getPos = vec3 0 0.2 (-5)
             , c_getForward = vec3 0 0 (1)
             , c_getUp = vec3 0 1 0
             , c_getRight = vec3 1 0 0
             , c_getFOV = 90
             }

shader :: BRDF
shader = let spacing = VecConst 1 1 1
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
-- shader = Diffuse $ ColVector $ VecMath VAdd UV $ VecConst 0 0 1

fstTri :: Tri
fstTri = Tri { t_getP0 = createVertex (vec3 (-1) (-1) 0) (vec3 1 1 1) (vec2 0 0) (vec3 0 0 (-1)) (vec3 1 0 0)
             , t_getP1 = createVertex (vec3 (-1) 1 0) (vec3 1 1 1) (vec2 0 1) (vec3 0 0 (-1)) (vec3 1 0 0)
             , t_getP2 = createVertex (vec3 1 1 0) (vec3 1 1 1) (vec2 1 1) (vec3 0 0 (-1)) (vec3 1 0 0)
             }

sndTri :: Tri
sndTri = Tri { t_getP0 = createVertex (vec3 1 1 0) (vec3 1 1 1) (vec2 1 1) (vec3 0 0 (-1)) (vec3 1 0 0)
             , t_getP1 = createVertex (vec3 1 (-1) 0) (vec3 1 1 1) (vec2 1 0) (vec3 0 0 (-1)) (vec3 1 0 0)
             , t_getP2 = createVertex (vec3 (-1) (-1) 0) (vec3 1 1 1) (vec2 0 0) (vec3 0 0 (-1)) (vec3 1 0 0)
             }

thdTri :: Tri
thdTri = Tri { t_getP0 = createVertex (vec3 (-1) (-1) 0) (vec3 1 1 1) (vec2 0 0) (vec3 0 1 0) (vec3 1 0 0)
             , t_getP1 = createVertex (vec3 1 (-1) (-1)) (vec3 1 1 1) (vec2 1 1) (vec3 0 1 0) (vec3 1 0 0)
             , t_getP2 = createVertex (vec3 (-1) (-1) (-1)) (vec3 1 1 1) (vec2 0 1) (vec3 0 1 0) (vec3 1 0 0)
             }

fourTri :: Tri
fourTri = Tri { t_getP0 = createVertex (vec3 (-1) (-1) 0) (vec3 1 1 1) (vec2 0 0) (vec3 0 1 0) (vec3 1 0 0)
              , t_getP1 = createVertex (vec3 1 (-1) 0) (vec3 1 1 1) (vec2 1 0) (vec3 0 1 0) (vec3 1 0 0)
              , t_getP2 = createVertex (vec3 1 (-1) (-1)) (vec3 1 1 1) (vec2  1 1) (vec3 0 1 0) (vec3 1 0 0)
              }

quad :: Object
quad = Object { o_getTris = [fstTri, sndTri, thdTri, fourTri]
              , o_getShader = shader
              }

sun :: Light
sun = Directional { l_getAngle = 2.5
                  , l_getStrength = 1
                  , l_getDirection = normalized $ vec3 0.5 (-1) (-0.4)
                  , l_getColor = vec3 1 1 1
                  }

scene :: Scene
scene = Scene { s_getCamera = cam
              , s_getObjects = [quad]
              , s_getLights = [sun]
              , s_getSkyColor = vec3 0.6 0.7 0.9
              , s_getWidth = 360
              , s_getHeight = 240
              , s_getSeed = 1
              , s_getSamples = 32
              , s_getBounces = 0
              }

main :: IO ()
main = do
  writeImg "testimg.png" $ parRender scene
