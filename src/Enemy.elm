module Enemy exposing (..)

import Collage
import Color


---

import Bullet
import Constants exposing (worldSize)
import Rect exposing (collides)
import Vec2 exposing (Vec2, (/+/))


type alias Model =
    Vec2.Vec2f


type Msg
    = Tick Float


init : Vec2.Vec2f -> Model
init pos =
    pos



--update : Msg -> Model -> Maybe Model


update msg bullets model =
    case msg of
        Tick dT ->
            let
                speed =
                    120

                toMove =
                    Vec2 0 (-dT * speed)

                newPos =
                    model /+/ toMove

                inBounds =
                    newPos.y > -(toFloat worldSize.h / 2 + 100)

                colliding =
                    List.any (\bullet -> collidesWithBullet bullet model) bullets
            in
                if inBounds && not colliding then
                    Just newPos
                else
                    Nothing


view : Model -> Collage.Form
view model =
    Collage.rect size.x size.y
        |> Collage.filled Color.red
        |> Collage.move (Vec2.toTuple model)


size =
    { x = 40
    , y = 40
    }


collidesWithBullet : Bullet.Model -> Model -> Bool
collidesWithBullet bullet enemy =
    let
        enemyRect =
            Rect.init size enemy

        bulletRect =
            Rect.init Bullet.size bullet
    in
        Rect.collides enemyRect bulletRect
