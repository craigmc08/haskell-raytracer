module Shader ( sampleBRDF, probability, reflectance
              , sampleVec
              , sampleCol
              , sampleVal
              ) where

import Types
import Numeric.Vector
import Numeric.Scalar (fromScalar, scalar)
import Control.Monad.State
import VecUtil (unpack3, unpack2)

-- UTIL STUFF

fmod :: (RealFrac a) => a -> a -> a
fmod n d = n - (fromIntegral $ truncate $ n / d) * d

-- END UTIL STUFF

-- BRDF STUFF

sampleBRDF :: BRDF -> State SceneContext (Maybe Ray)
sampleBRDF BRDFEmpty = return Nothing

probability :: BRDF -> Vec3d -> State SceneContext Double
probability BRDFEmpty _ = return 0
probability (Diffuse _) _ = return 0.15915494309 -- 1 / (2 * PI)

reflectance :: BRDF -> State SceneContext Vec3d
reflectance BRDFEmpty = return $ vec3 0 0 0

reflectance (Diffuse col) = do
  col' <- sampleCol col
  return col'

reflectance (Emission col str) = do
  col' <- sampleCol col
  str' <- sampleVal str
  return $ col' * (fromScalar (scalar str'))

reflectance (Glossy col roughness) = do
  col' <- sampleCol col
  return col'
  
-- END BRDF STUFF

-- VECTOR STUFF

sampleVec :: SVector -> State SceneContext Vec3d

sampleVec UV = do
  ss <- get
  let (u, v) = unpack2 $ rh_getTexCoord $ ss_getHit ss
  return $ vec3 u v 0

sampleVec Position = do
  ss <- get
  return $ rh_getPos $ ss_getHit ss

sampleVec Normal = do
  ss <- get
  return $ rh_getNormal $ ss_getHit ss

sampleVec (VecConst x y z) = return $ vec3 x y z

sampleVec (VecMath op l r) = do
  l' <- sampleVec l
  r' <- sampleVec r
  case op of
    VAdd -> return $ l' + r'
    VSub -> return $ l' - r'
    VMul -> return $ l' * r'
    VDiv -> return $ l' / r'
    VMod -> let (x,y,z) = unpack3 l'; (i,j,k) = unpack3 r' in return $ vec3 (fmod x i) (fmod y j) (fmod z k)
    VAbs -> return $ abs l'
    -- VDot -> return $ l' · r'
    -- VCross -> return $ l' × r'

sampleVec (CombineXYZ x y z) = do
  x' <- sampleVal x
  y' <- sampleVal y
  z' <- sampleVal z
  return $ vec3 x' y' z'

-- END VECTOR STUFF

-- COLOR STUFF

sampleCol :: SColor -> State SceneContext Vec3d

sampleCol ColVertex = do
  ss <- get
  return $ rh_getColor $ ss_getHit ss

sampleCol (ColVector v) = sampleVec v

sampleCol (ColConst r g b) = return $ vec3 r g b

sampleCol (CombineRGB vr vg vb) = do
  r <- sampleVal vr
  g <- sampleVal vg
  b <- sampleVal vb
  return $ vec3 r g b

-- END COLOR STUFF

-- VALUE STUFF
sampleVal :: SValue -> State SceneContext Double

sampleVal (ValConst x) = return x

sampleVal RayLength = do
  ss <- get
  return $ rh_getDistance $ ss_getHit ss

sampleVal (ValMath op l r) = do
  l' <- sampleVal l
  r' <- sampleVal r
  case op of
    Add -> return $ l' + r'
    Sub -> return $ l' - r'
    Mul -> return $ l' * r'
    Div -> return $ l' / r'
    Abs -> return $ abs l'
    Mod -> return $ fmod l' r'
    LessThan -> return $ if l' < r' then 1 else 0
    GreaterThan -> return $ if l' > r' then 1 else 0

sampleVal (SeparateX v) = sampleVec v >>= (return . unpack3) >>= \(x,_,_) -> return x
sampleVal (SeparateY v) = sampleVec v >>= (return . unpack3) >>= \(_,y,_) -> return y
sampleVal (SeparateZ v) = sampleVec v >>= (return . unpack3) >>= \(_,_,z) -> return z

sampleVal (SeparateR v) = sampleCol v >>= (return . unpack3) >>= \(r,_,_) -> return r
sampleVal (SeparateG v) = sampleCol v >>= (return . unpack3) >>= \(_,g,_) -> return g
sampleVal (SeparateB v) = sampleCol v >>= (return . unpack3) >>= \(_,_,b) -> return b

-- END VALUE STUFF
