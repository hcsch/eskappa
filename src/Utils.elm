module Utils exposing (elMul)

import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY)


{-
   utils that come in handy but were not 'worthy' of their own file / didn't belong in another one
-}


elMul : Vec2 -> Vec2 -> Vec2
elMul v1 v2 =
    vec2 (getX v1 * (getX v2)) (getY v1 * (getY v2))
