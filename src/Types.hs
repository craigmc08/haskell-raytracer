module Types where

import System.Random
import Numeric.Vector

data Camera = Camera { c_getPos :: Vec3d
                     , c_getForward :: Vec3d
                     , c_getUp :: Vec3d
                     , c_getRight :: Vec3d
                     , c_getFOV :: Double
                     }
            | OrthoCamera { c_getPos :: Vec3d
                          , c_getForward :: Vec3d
                          , c_getUp :: Vec3d
                          , c_getRight :: Vec3d
                          , c_getSize :: Double
                          } deriving Show

data Scene = Scene { s_getCamera :: Camera
                   , s_getObjects :: [Object]
                   , s_getLights :: [Light]
                   , s_getSkyColor :: Vec3d
                   , s_getWidth :: Int
                   , s_getHeight :: Int
                   , s_getSeed :: Int
                   , s_getSamples :: Int
                   , s_getBounces :: Int
                   } deriving Show

data SceneContext = SceneContext { ss_getScene :: Scene
                                 , ss_getGen :: StdGen
                                 , ss_getHit :: RayHit
                                 } deriving Show

data Ray = Ray Vec3d Vec3d deriving Show
r_getOrigin :: Ray -> Vec3d
r_getOrigin (Ray o _) = o
r_getDir :: Ray -> Vec3d
r_getDir (Ray _ d) = d

data RayHit = RayHit { rh_getPos :: Vec3d 
                     , rh_getRay :: Ray
                     , rh_getDistance :: Double
                     , rh_getTri :: Tri
                     , rh_getObj :: Object
                     , rh_getShader :: BRDF
                     , rh_getTexCoord :: Vec2d
                     , rh_getNormal :: Vec3d
                     , rh_getTangent :: Vec3d
                     , rh_getBitangent :: Vec3d
                     , rh_getColor :: Vec3d
                     } deriving Show

data SVOp = VAdd | VSub | VDot | VCross | VMul | VDiv | VAbs | VMod deriving Show
data SOp = Add | Sub | Mul | Div | Abs | Mod | LessThan | GreaterThan deriving Show
data SVector = UV
             | Position
             | Normal
             | VecConst Double Double Double
             | VecMath SVOp SVector SVector
             | CombineXYZ SValue SValue SValue
             deriving Show
data SValue = ValConst Double
            | RayLength
            | ValMath SOp SValue SValue
            | SeparateX SVector | SeparateY SVector | SeparateZ SVector
            | SeparateR SColor | SeparateG SColor | SeparateB SColor
            deriving Show
data SColor = ColVertex
            | ColVector SVector
            | ColConst Double Double Double
            | CombineRGB SValue SValue SValue
            deriving Show
data BRDF = Emission SColor SValue
          | Diffuse SColor
          | Glossy SColor SValue
          | BRDFEmpty
          deriving Show

data Light = Directional { l_getAngle :: Double
                         , l_getStrength :: Double
                         , l_getDirection :: Vec3d
                         , l_getColor :: Vec3d
                         }
           | Point { l_getRadius :: Double
                   , l_getStrength :: Double
                   , l_getColor :: Vec3d
                   } deriving Show

data Object = Object { o_getTris :: [Tri]
                     , o_getShader :: BRDF
                     } deriving Show

data Tri = Tri { t_getP0 :: Vertex
               , t_getP1 :: Vertex
               , t_getP2 :: Vertex
               } deriving Show

data Vertex = Vertex { v_getPos :: Vec3d
                     , v_getColor :: Vec3d
                     , v_getTexCoord :: Vec2d
                     , v_getNormal :: Vec3d
                     , v_getTangent :: Vec3d
                     , v_getBitangent :: Vec3d
                     } deriving Show
