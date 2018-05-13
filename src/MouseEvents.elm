module MouseEvents exposing (onRelMouseMove, onRelMouseDown)

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Json
import Math.Vector2 as Vec2 exposing (vec2, Vec2)


{-
   Custom mouse event handling to get target relative coordinates

   (uses evt.offsetX and evt.offsetY, which are marked experimental in the MDN web docs)

   - hcsch
-}


onRelMouseMove : (Vec2 -> msg) -> Attribute msg
onRelMouseMove tagger =
    on "mousemove" <|
        Json.map
            tagger
            offsetDecoder


onRelMouseDown : (Vec2 -> msg) -> Attribute msg
onRelMouseDown tagger =
    on "mousedown" <|
        Json.map
            tagger
            offsetDecoder


offsetDecoder : Json.Decoder Vec2
offsetDecoder =
    Json.map2 vec2 (Json.field "offsetX" Json.float) (Json.field "offsetY" Json.float)
