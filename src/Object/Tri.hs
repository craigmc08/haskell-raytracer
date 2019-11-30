module Object.Tri (Tri (..), asList, asTuple) where

import Types

asList :: Tri -> [Vertex]
asList t = map ($t) [t_getP0, t_getP1, t_getP2]

asTuple :: Tri -> (Vertex, Vertex, Vertex)
asTuple t = (t_getP0 t, t_getP1 t, t_getP2 t)
