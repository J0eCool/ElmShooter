module Bullet exposing (..)

import Constants exposing (worldSize)
import Vec2 exposing (Vec2, (/+/))


type alias Model =
    Vec2.Vec2f


type Msg
    = Tick Float


init : Vec2.Vec2f -> Model
init pos =
    pos


update : Msg -> Model -> Maybe Model
update msg pos =
    case msg of
        Tick dT ->
            let
                speed =
                    1200

                toMove =
                    Vec2 0 (dT * speed)

                newPos =
                    pos /+/ toMove
            in
                if newPos.y < toFloat worldSize.h / 2 + 100 then
                    Just newPos
                else
                    Nothing
