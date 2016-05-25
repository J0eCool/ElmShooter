module Bullet exposing (..)

import Collage
import Color


---

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
update msg model =
    case msg of
        Tick dT ->
            let
                speed =
                    1200

                toMove =
                    Vec2 0 (dT * speed)

                newPos =
                    model /+/ toMove
            in
                if newPos.y < toFloat worldSize.h / 2 + 100 then
                    Just newPos
                else
                    Nothing


view : Model -> Collage.Form
view model =
    Collage.rect size.x size.y
        |> Collage.filled Color.yellow
        |> Collage.move (Vec2.toTuple model)


size =
    { x = 22
    , y = 34
    }
