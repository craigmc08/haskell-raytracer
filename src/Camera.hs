module Camera where

import Types
import Numeric.Vector
import Numeric.Scalar
import VecUtil (unpack3)

rayFrom :: Camera -> (Double, Double) -> Ray
rayFrom (OrthoCamera p forward up right size) (x, y) = let dir = forward
                                                           x' = right * fromScalar (scalar (x * size))
                                                           y' = up * fromScalar (scalar (y * size))
                                                           origin = p + x' + y'
                                                       in  Ray origin dir
rayFrom (Camera p forward up right fov) (x, y) = let mul = tan $ fov * 0.00872664625 -- Magic number is to convert degrees to radians and divide by two
                                                     (r, u, f) = unpack3 $ normalized $ vec3 (x * mul) (y * mul) 1
                                                     dir = forward * (fromScalar $ scalar f) + right * (fromScalar $ scalar r) + up * (fromScalar $ scalar u)
                                                 in  Ray p dir
