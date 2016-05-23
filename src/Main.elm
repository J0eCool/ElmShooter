module Main exposing (..)

import Collage
import Color
import Element
import Html exposing (div, text, button, ul, li)
import Html.App as App
import Http
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing ((:=))
import Mouse
import String
import Task
import Time
import Vec2 exposing (Vec2, (/+/), (/-/), (.*/))


type Msg
    = Nop
    | Tick Time.Time
    | MouseMoved Mouse.Position


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


model =
    { lastTime = 0
    , mousePos = Vec2.init
    , shipPos = Vec2.init
    , lastShotTime = 0
    , bullets = []
    }


init =
    let
        timeCmd =
            Task.perform (always Nop) Tick Time.now
    in
        ( model, timeCmd )


view model =
    let
        form =
            Collage.collage worldSize.w worldSize.h elements

        body =
            Element.toHtml form

        toTuple point =
            ( point.x, point.y )

        elements =
            ([ Collage.rect (toFloat worldSize.w) (toFloat worldSize.h)
                |> Collage.filled Color.gray
             , Collage.rect 40 55
                |> Collage.filled Color.blue
                |> Collage.move (toTuple model.shipPos)
             ]
                ++ (List.map
                        (\pos ->
                            Collage.rect 22 34
                                |> Collage.filled Color.yellow
                                |> Collage.move (toTuple pos)
                        )
                        model.bullets
                   )
            )
    in
        div []
            [ body
            , text <| "Num bullets: " ++ toString (List.length model.bullets)
              --, Element.toHtml <| Element.show model
            ]


update message model =
    case message of
        Nop ->
            model ! []

        Tick rawTime ->
            let
                t =
                    rawTime / 1000

                dT =
                    t - model.lastTime

                speed =
                    400

                delta =
                    model.mousePos /-/ model.shipPos

                dir =
                    Vec2.normalize delta

                vel =
                    (dT * speed) .*/ dir

                toMove =
                    Vec2.lengthMin delta vel

                newShipPos =
                    model.shipPos
                        /+/ toMove
                        |> Vec2.max (Vec2 (-350) (-350))
                        |> Vec2.min (Vec2 350 150)

                fireRate =
                    1.5

                didShoot =
                    t - model.lastShotTime > 1 / fireRate

                ( shotBullets, newShotTime ) =
                    if didShoot then
                        ( [ model.shipPos ], t )
                    else
                        ( [], model.lastShotTime )

                bulletSpeed =
                    1500

                bulletToMove =
                    Vec2 0 (dT * bulletSpeed)

                bulletUpdate pos =
                    let
                        newPos =
                            pos /+/ bulletToMove
                    in
                        if newPos.y < toFloat worldSize.h / 2 + 100 then
                            Just newPos
                        else
                            Nothing

                newBullets =
                    model.bullets
                        ++ shotBullets
                        |> List.filterMap bulletUpdate
            in
                { model
                    | lastTime = t
                    , shipPos = newShipPos
                    , lastShotTime = newShotTime
                    , bullets = newBullets
                }
                    ! []

        MouseMoved rawPos ->
            let
                pos =
                    { x = toFloat <| rawPos.x - worldSize.w // 2
                    , y = toFloat <| worldSize.h // 2 - rawPos.y
                    }
            in
                { model | mousePos = pos } ! []


subscriptions model =
    let
        fps =
            60
    in
        Sub.batch
            [ Time.every (Time.second / fps) Tick
            , Mouse.moves MouseMoved
            ]


worldSize =
    { w = 800
    , h = 800
    }
