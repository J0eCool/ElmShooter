module Vec2 exposing (..)


type alias Vec2 a =
    { x : a
    , y : a
    }


type alias Vec2f =
    Vec2 Float


type alias Vec2i =
    Vec2 Int


init : Vec2 number
init =
    { x = 0
    , y = 0
    }


map : (a -> a) -> Vec2 a -> Vec2 a
map f vec =
    { x = f vec.x
    , y = f vec.y
    }


map2 : (a -> a -> a) -> Vec2 a -> Vec2 a -> Vec2 a
map2 f v1 v2 =
    { x = f v1.x v2.x
    , y = f v1.y v2.y
    }


map3 : (a -> a -> a -> a) -> Vec2 a -> Vec2 a -> Vec2 a -> Vec2 a
map3 f v1 v2 v3 =
    { x = f v1.x v2.x v3.x
    , y = f v1.y v2.y v3.y
    }


(/+/) : Vec2 number -> Vec2 number -> Vec2 number
(/+/) v1 v2 =
    map2 (+) v1 v2


(/-/) : Vec2 number -> Vec2 number -> Vec2 number
(/-/) v1 v2 =
    map2 (-) v1 v2


(.*/) : number -> Vec2 number -> Vec2 number
(.*/) scalar vec =
    map ((*) scalar) vec


(/*/) : Vec2 number -> Vec2 number -> Vec2 number
(/*/) v1 v2 =
    map2 (*) v1 v2


normalize : Vec2f -> Vec2f
normalize vec =
    let
        l =
            length vec
    in
        if l == 0 then
            vec
        else
            (1 / l) .*/ vec


length : Vec2f -> Float
length vec =
    length2 vec
        |> sqrt


length2 : Vec2 number -> number
length2 vec =
    vec.x ^ 2 + vec.y ^ 2


min v1 v2 =
    map2 Basics.min v1 v2


max v1 v2 =
    map2 Basics.max v1 v2


clamp vLo vHi vec =
    map3 Basics.clamp vLo vHi vec


lengthMin v1 v2 =
    if length2 v1 < length2 v2 then
        v1
    else
        v2
