module Ray where

import Numeric.Vector
import Numeric.Scalar
import Types
import Object.Tri (asList)

createHit :: Object -> Tri -> Ray -> Double -> (Double, Double, Double) -> Vec3d -> RayHit
createHit obj tri ray dist (a, b, c) pos = let abc3 = map (fromScalar . scalar) [a, b, c] :: [Vec3d]
                                               abc2 = map (fromScalar . scalar) [a, b, c] :: [Vec2d]
                                               tex = sum $ zipWith (*) abc2 $ map v_getTexCoord $ asList tri
                                               normal = sum $ zipWith (*) abc3 $ map v_getNormal $ asList tri
                                               tangent = sum $ zipWith (*) abc3 $ map v_getTangent $ asList tri
                                               bitangent = tangent × normal
                                               color = sum $ zipWith (*) abc3 $ map v_getColor $ asList tri

                                           in  RayHit { rh_getPos = pos
                                                      , rh_getRay = ray
                                                      , rh_getDistance = dist
                                                      , rh_getTri = tri
                                                      , rh_getObj = obj
                                                      , rh_getShader = o_getShader obj
                                                      , rh_getTexCoord = tex
                                                      , rh_getNormal = normal
                                                      , rh_getTangent = tangent
                                                      , rh_getBitangent = bitangent
                                                      , rh_getColor = color
                                                      }
