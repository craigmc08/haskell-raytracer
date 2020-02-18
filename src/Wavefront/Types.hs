module Wavefront.Types where

import Numeric.Vector

data WavefrontLine = Comment String
                   | Vertex Double Double Double
                   | TexCoord Double Double
                   | Normal Double Double Double
                   | Face Int Int Int Int Int Int Int Int Int
                   deriving (Show)

isComment :: WavefrontLine -> Bool
isComment (Comment _) = True
isComment _ = False

isVertex :: WavefrontLine -> Bool
isVertex (Vertex _ _ _) = True
isVertex _ = False

isTexCoord :: WavefrontLine -> Bool
isTexCoord (TexCoord _ _) = True
isTexCoord _ = False

isNormal :: WavefrontLine -> Bool
isNormal (Normal _ _ _) = True
isNormal _ = False

isFace :: WavefrontLine -> Bool
isFace (Face _ _ _ _ _ _ _ _ _) = True
isFace _ = False

data Vertex_ = Vertex_ Double Double Double deriving (Show)
data TexCoord_ = TexCoord_ Double Double deriving (Show)
data Normal_ = Normal_ Double Double Double deriving (Show)
data Face_ = Face_ Int Int Int Int Int Int Int Int Int deriving (Show)

vertexFromLine :: WavefrontLine -> Vertex_
vertexFromLine (Vertex a b c) = Vertex_ a b c
vertexFromLine _ = error "vertexFromLine is a partial function: it only accepts vertex lines"

texCoordFromLine :: WavefrontLine -> TexCoord_
texCoordFromLine (TexCoord a b) = TexCoord_ a b
texCoordFromLine _ = error "texCoordFromLine is a partial function: it only accepts tex coord lines"

normalFromLine :: WavefrontLine -> Normal_
normalFromLine (Normal a b c) = Normal_ a b c
normalFromLine _ = error "normalFromLine is a partial function: it only accepts normal lines"

faceFromLine :: WavefrontLine -> Face_
faceFromLine (Face a b c d e f g h i) = Face_ a b c d e f g h i
faceFromLine _ = error "vertexFromLine is a partial function: it only accepts face lines"

vertexToVec :: Vertex_ -> Vec3d
vertexToVec (Vertex_ a b c) = vec3 a b c

texCoordToVec :: TexCoord_ -> Vec2d
texCoordToVec (TexCoord_ a b) = vec2 a b

normalToVec :: Normal_ -> Vec3d
normalToVec (Normal_ a b c) = vec3 a b c
