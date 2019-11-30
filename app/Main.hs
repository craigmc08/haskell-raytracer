module Main where

import Lib
import Types
import Object.Vertex (createVertex)
import Numeric.Vector
import Camera (rayFrom)
import Raycast (intersect, findHits)

cam = Camera { c_getPos = vec3 0 0 (-5)
                  , c_getForward = vec3 0 0 1
                  , c_getUp = vec3 0 1 0
                  , c_getRight = vec3 1 0 0
                  , c_getFOV = 90
                  }

shader = BRDFEmpty

fstTri = Tri { t_getP0 = createVertex (vec3 (-1) (-1) 0) (vec3 1 1 1) (vec2 0 0) (vec3 0 0 (-1)) (vec3 1 0 0)
             , t_getP1 = createVertex (vec3 (-1) 1 0) (vec3 1 1 1) (vec2 0 1) (vec3 0 0 (-1)) (vec3 1 0 0)
             , t_getP2 = createVertex (vec3 1 1 0) (vec3 1 1 1) (vec2 1 1) (vec3 0 0 (-1)) (vec3 1 0 0)
             }

sndTri = Tri { t_getP0 = createVertex (vec3 1 1 0) (vec3 1 1 1) (vec2 1 1) (vec3 0 0 (-1)) (vec3 1 0 0)
             , t_getP1 = createVertex (vec3 1 (-1) 0) (vec3 1 1 1) (vec2 1 0) (vec3 0 0 (-1)) (vec3 1 0 0)
             , t_getP2 = createVertex (vec3 (-1) (-1) 0) (vec3 1 1 1) (vec2 0 0) (vec3 0 0 (-1)) (vec3 1 0 0)
             }

quad = Object { o_getTris = [fstTri, sndTri]
              , o_getShader = shader
              }

scene = Scene { s_getCamera = cam
              , s_getObjects = [quad]
              , s_getLights = []
              , s_getSkyColor = vec3 0 0 0
              , s_getWidth = 300
              , s_getHeight = 200
              }

main :: IO ()
main = do
  print $ rayFrom cam (-1, -1)
  print $ rayFrom cam (-1, 1)
  print $ rayFrom cam (1, 1)
  print $ rayFrom cam (1, -1)
  writeImg "testimg.png" $ render scene
