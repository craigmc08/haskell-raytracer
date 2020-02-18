module Wavefront.Converter where

import qualified Types as T
import Wavefront.Types
import Numeric.Vector
import VecUtil

face2Tri :: [Vertex_] -> [TexCoord_] -> [Normal_] -> Face_ -> T.Tri
face2Tri vs tcs ns (Face_ p1 tc1 n1 p2 tc2 n2 p3 tc3 n3) =
  let (t1, b1) = tangentBitangent $ normalToVec $ ns !! n1
      (t2, b2) = tangentBitangent $ normalToVec $ ns !! n2
      (t3, b3) = tangentBitangent $ normalToVec $ ns !! n3
      v1 = T.Vertex (vertexToVec $ vs !! p1) (vec3 1 1 1) (texCoordToVec $ tcs !! tc1) (normalToVec $ ns !! n1) t1 b1
      v2 = T.Vertex (vertexToVec $ vs !! p2) (vec3 1 1 1) (texCoordToVec $ tcs !! tc2) (normalToVec $ ns !! n2) t2 b2
      v3 = T.Vertex (vertexToVec $ vs !! p3) (vec3 1 1 1) (texCoordToVec $ tcs !! tc3) (normalToVec $ ns !! n3) t3 b3
  in  T.Tri v1 v2 v3

obj2Mesh :: [WavefrontLine] -> [T.Tri]
obj2Mesh ls = let verts = map vertexFromLine $ filter isVertex ls
                  texCoords = map texCoordFromLine $ filter isTexCoord ls
                  normals = map normalFromLine $ filter isNormal ls
                  faces = map faceFromLine $ filter isFace ls
              in  map (face2Tri verts texCoords normals) faces
