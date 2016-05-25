module Rect exposing (..)

import Vec2 exposing (Vec2)


type alias Rect a =
    { x : a
    , y : a
    , w : a
    , h : a
    }


type alias Rectf =
    Rect Float


init : Vec2 a -> Vec2 a -> Rect a
init size pos =
    { x = pos.x
    , y = pos.y
    , w = size.x
    , h = size.y
    }


collides : Rectf -> Rectf -> Bool
collides r1 r2 =
    let
        xSize =
            (r1.w + r2.w) / 2

        ySize =
            (r1.h + r2.h) / 2

        inX =
            r2.x >= r1.x - xSize && r2.x <= r1.x + xSize

        inY =
            r2.y >= r1.y - ySize && r2.y <= r1.y + ySize
    in
        inX && inY
