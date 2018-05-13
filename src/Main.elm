module Main exposing (main)

{-
   WebGL GUI for the Game with some simple html around it
-}

import AnimationFrame
import Html exposing (Html, div, h1, text, span, ul, li)
import Html.Attributes exposing (width, height, style)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector2 as Vec2 exposing (vec2, Vec2, getX, getY, add, sub)
import Time exposing (Time, second)
import WebGL exposing (Mesh, Shader, antialias, clearColor)
import List exposing (map, foldl, foldr)
import MouseEvents exposing (onRelMouseMove, onRelMouseDown)
import Game exposing (Game, init, Entity, Enemy, start, step, movePlayer, hit)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


type Msg
    = Step Time
    | MouseDown Vec2
    | MouseMove Vec2


type alias Model =
    { game : Game
    , mouseOffset : Vec2
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.none
    )


initialModel : Model
initialModel =
    Model
        Game.init
        (vec2 0 0)


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.game.running then
        AnimationFrame.diffs Step
    else
        Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step timeDiff ->
            ( { model | game = step model.game timeDiff }, Cmd.none )

        MouseDown pos ->
            if
                not model.game.running
                    && hit pos model.game.player
            then
                ( { model
                    | game = start model.game
                    , mouseOffset = sub pos model.game.player.position
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        MouseMove pos ->
            if model.game.running then
                ( { model | game = movePlayer model.game (sub pos model.mouseOffset) }
                , Cmd.none
                )
            else
                ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text "Sκ "
            , span
                [ style
                    [ ( "fontSize", "0.4em" )
                    , ( "color", "rgba(0,0,0,0.4)" )
                    ]
                ]
                [ text "(eskappa)" ]
            ]
        , WebGL.toHtmlWith
            [ clearColor 0.125 0.125 0.125 1, antialias ]
            [ width 450
            , height 450
            , style [ ( "display", "block" ), ( "position", "relative" ) ]
            , onRelMouseDown MouseDown
            , onRelMouseMove MouseMove
            ]
            [ WebGL.entity
                vertexShader
                fragmentShader
                (mesh (model.game.player :: (map (.entity) model.game.enemies)) model.game.elapsedTime)
                ()
            ]
        , div []
            [ text
                ("Current time: "
                    ++ (if model.game.running then
                            (toString (model.game.elapsedTime / second)) ++ "s"
                        else
                            "not running"
                       )
                )
            ]
        , div [] [ text ("Personal highscore: " ++ (toString ((foldr (max) 0 model.game.pastTimes) / second)) ++ "s") ]
        , div []
            [ text "Previous times:"
            , ul []
                (foldr (::) [] (map (\t -> li [] [ text ((toString (t / second)) ++ "s\n") ]) model.game.pastTimes))
            ]
        ]



-- Mesh


type alias Vertex =
    { position : Vec2
    , color : Vec3
    }


mesh : List Entity -> Time -> Mesh Vertex
mesh entities elapsedTime =
    WebGL.triangles
        ([ ( Vertex (pxToGL (vec2 50 50)) (vec3 1 1 1)
           , Vertex (pxToGL (vec2 400 50)) (vec3 1 1 1)
           , Vertex (pxToGL (vec2 50 400)) (vec3 1 1 1)
           )
         , ( Vertex (pxToGL (vec2 50 400)) (vec3 1 1 1)
           , Vertex (pxToGL (vec2 400 50)) (vec3 1 1 1)
           , Vertex (pxToGL (vec2 400 400)) (vec3 1 1 1)
           )
         ]
            ++ (foldl (\entity acc -> acc ++ (coloredRect entity.dimensions entity.position entity.bgColor)) [] entities)
        )


pxToGL : Vec2 -> Vec2
pxToGL v =
    vec2 ((getX v) / 450 * 2 - 1) -((getY v) / 450 * 2 - 1)


coloredRect : Vec2 -> Vec2 -> Vec3 -> List ( Vertex, Vertex, Vertex )
coloredRect dims pos rectColor =
    [ ( Vertex (pxToGL pos) rectColor
      , Vertex (pxToGL (add (vec2 (getX dims) 0) pos)) rectColor
      , Vertex (pxToGL (add (vec2 0 (getY dims)) pos)) rectColor
      )
    , ( Vertex (pxToGL (add (vec2 0 (getY dims)) pos)) rectColor
      , Vertex (pxToGL (add (vec2 (getX dims) 0) pos)) rectColor
      , Vertex (pxToGL (add pos dims)) rectColor
      )
    ]



-- Shaders


vertexShader : Shader Vertex () { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec2 position;
        attribute vec3 color;
        varying vec3 vcolor;
        void main () {
            gl_Position = vec4(position, 0.0, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : Shader {} () { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]
