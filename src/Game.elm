module Game exposing (Game, init, Entity, Enemy, start, step, movePlayer, hit)

import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, setX, setY, add, sub, scale)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Time exposing (Time, second)
import List exposing (any, map)
import Utils exposing (elMul)


{-
   elm port of the game at http://members.iinet.net.au/~pontipak/redsquare.html
   should be comparable to the original as it is using the same game mechanics
-}


type alias Game =
    { enemies : List Enemy
    , player : Entity
    , running : Bool
    , elapsedTime : Time
    , pastTimes : List Time
    }


type alias Entity =
    { dimensions : Vec2
    , position : Vec2
    , bgColor : Vec3
    }


type alias Enemy =
    { entity : Entity

    {- in px/s -}
    , baseSpeed : Vec2

    {- direction of movement on the x- and y-axis, either -1 or 1 -}
    , direction : Vec2
    }


init : Game
init =
    Game
        [ Enemy (Entity (vec2 60 50) (vec2 270 60) (vec3 0 0 0.6)) (vec2 125 150) (vec2 -1 1)
        , Enemy (Entity (vec2 100 20) (vec2 300 330) (vec3 0 0 0.6)) (vec2 150 250) (vec2 -1 -1)
        , Enemy (Entity (vec2 30 60) (vec2 70 320) (vec3 0 0 0.6)) (vec2 187.5 162.5) (vec2 1 -1)
        , Enemy (Entity (vec2 60 60) (vec2 70 70) (vec3 0 0 0.6)) (vec2 212.5 137.5) (vec2 1 1)
        ]
        (Entity (vec2 40 40) (vec2 205 205) (vec3 0.6 0 0))
        False
        0
        []


start : Game -> Game
start game =
    { game | running = True }


step : Game -> Time -> Game
step game timeDiff =
    if not game.running then
        game
    else
        let
            newElapsedTime =
                game.elapsedTime + timeDiff

            updatedEnemies =
                updateEnemies game.enemies (speedMultiplier newElapsedTime) timeDiff

            updatedGame =
                { game
                    | elapsedTime = game.elapsedTime + timeDiff
                    , enemies = updatedEnemies
                }
        in
            if any (collide game.player) (map (.entity) updatedEnemies) then
                stop updatedGame
            else
                updatedGame


stop : Game -> Game
stop game =
    if not game.running then
        game
    else
        { init | pastTimes = game.elapsedTime :: game.pastTimes }


movePlayer : Game -> Vec2 -> Game
movePlayer game newPos =
    if not game.running then
        game
    else
        let
            player =
                game.player

            playerMinCoords =
                player.position

            playerMaxCoords =
                add player.position player.dimensions
        in
            if
                (getX playerMinCoords <= 50)
                    || (getY playerMinCoords <= 50)
                    || (getX playerMaxCoords >= 400)
                    || (getY playerMaxCoords >= 400)
                    && not (any (collide game.player) (map (.entity) game.enemies))
            then
                stop { game | player = { player | position = newPos } }
            else
                { game
                    | player = { player | position = newPos }
                }


updateEnemies : List Enemy -> Float -> Time -> List Enemy
updateEnemies enemies speedMul timeDiff =
    map
        (\enemy ->
            let
                eSpeed =
                    scale (speedMul * timeDiff / second) enemy.baseSpeed

                ePos =
                    enemy.entity.position

                entity =
                    enemy.entity

                eDir =
                    vec2
                        (if getX ePos >= 450 - (getX entity.dimensions) then
                            -1
                         else if getX ePos <= 0 then
                            1
                         else
                            getX enemy.direction
                        )
                        (if getY ePos >= 450 - (getY entity.dimensions) then
                            -1
                         else if getY ePos <= 0 then
                            1
                         else
                            getY enemy.direction
                        )
            in
                { enemy
                    | entity =
                        { entity
                            | position = add ePos (elMul eSpeed eDir)
                        }
                    , direction = eDir
                }
        )
        enemies


collide : Entity -> Entity -> Bool
collide e1 e2 =
    getX e1.position
        < (getX e2.position + (getX e2.dimensions))
        && getX e1.position
        + (getX e1.dimensions)
        > (getX e2.position)
        && getY e1.position
        < (getY e2.position + (getY e2.dimensions))
        && getY e1.position
        + (getY e1.dimensions)
        > (getY e2.position)


hit : Vec2 -> Entity -> Bool
hit p e =
    (getX p > (getX e.position))
        && (getX p < (getX e.position + (getX e.dimensions)))
        && (getY p > (getY e.position))
        && (getY p < (getY e.position + (getY e.dimensions)))



{- computes the base speed unit in px/ms for the given elapsed time -}


speedMultiplier : Time -> Float
speedMultiplier elapsedTime =
    if elapsedTime < 8 * second then
        1
    else if elapsedTime < 14 * second then
        1 + 1 / 3
    else if elapsedTime < 18 * second then
        2
    else if elapsedTime < 21 * second then
        2 + 2 / 3
    else if elapsedTime < 23 * second then
        4
    else
        8
