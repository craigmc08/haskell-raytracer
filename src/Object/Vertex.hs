module Object.Vertex (Vertex (..), createVertex) where

import Numeric.Vector
import Types

createVertex :: Vec3d -> Vec3d -> Vec2d -> Vec3d -> Vec3d -> Vertex
createVertex pos color texcoord normal tangent = Vertex { v_getPos = pos
                                                        , v_getColor = color
                                                        , v_getTexCoord = texcoord
                                                        , v_getNormal = normal
                                                        , v_getTangent = tangent
                                                        , v_getBitangent = tangent × normal
                                                        }
