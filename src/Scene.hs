module Scene (Scene (..), SceneContext (..), makeContext) where

import Types
import System.Random

makeContext :: Scene -> StdGen -> RayHit -> SceneContext
makeContext s g rh = SceneContext { ss_getScene = s
                                  , ss_getGen = g
                                  , ss_getHit = rh
                                  }
