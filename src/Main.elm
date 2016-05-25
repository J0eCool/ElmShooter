module Main exposing (..)

import Collage
import Color
import Element
import Html exposing (div, text, button, ul, li)
import Html.App as App
import Html.Events exposing (onClick)
import Mouse
import Task
import Time


---

import Bullet
import Constants exposing (worldSize)
import Enemy
import Vec2 exposing (Vec2, (/+/), (/-/), (.*/))


type Msg
    = Nop
    | Tick Float
    | MouseMoved Mouse.Position


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init =
    let
        model =
            { lastTime = 0
            , mousePos = Vec2.init
            , shipPos = Vec2.init
            , lastShotTime = 0
            , bullets = []
            , enemies = []
            }

        timeCmd =
            Task.perform (always Nop) (timeToSecond >> Tick) Time.now
    in
        ( model, timeCmd )


view model =
    let
        form =
            Collage.collage worldSize.w worldSize.h elements

        body =
            Element.toHtml form

        elements =
            ([ Collage.rect (toFloat worldSize.w) (toFloat worldSize.h)
                |> Collage.filled Color.gray
             , Collage.rect 40 55
                |> Collage.filled Color.blue
                |> Collage.move (Vec2.toTuple model.shipPos)
             ]
                ++ (List.map Bullet.view model.bullets)
                ++ (List.map Enemy.view model.enemies)
            )
    in
        div []
            [ body
            , div [] [ text <| "Num bullets: " ++ toString (List.length model.bullets) ]
            , div [] [ text <| "Num enemies: " ++ toString (List.length model.enemies) ]
              --, Element.toHtml <| Element.show model
            ]


update message model =
    case message of
        Nop ->
            model ! []

        Tick t ->
            let
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
                    2

                didShoot =
                    t - model.lastShotTime > 1 / fireRate

                ( shotBullets, spawnedEnemies, newShotTime ) =
                    if didShoot then
                        ( [ model.shipPos ]
                        , [ { x = worldSize.w * cos (model.lastTime ^ 1.2) / 2
                            , y = worldSize.h / 2
                            }
                          ]
                        , t
                        )
                    else
                        ( [], [], model.lastShotTime )

                bulletUpdate bullet =
                    Bullet.update (Bullet.Tick dT) bullet

                newBullets =
                    model.bullets
                        ++ shotBullets
                        |> List.filterMap bulletUpdate

                enemyUpdate enemy =
                    Enemy.update (Enemy.Tick dT) model.bullets enemy

                newEnemies =
                    model.enemies
                        ++ spawnedEnemies
                        |> List.filterMap enemyUpdate
            in
                { model
                    | lastTime = t
                    , shipPos = newShipPos
                    , lastShotTime = newShotTime
                    , bullets = newBullets
                    , enemies = newEnemies
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
            [ Time.every (Time.second / fps) (timeToSecond >> Tick)
            , Mouse.moves MouseMoved
            ]


timeToSecond rawTime =
    rawTime / 1000
