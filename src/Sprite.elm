module Sprite exposing (..)


type alias Sprite =
  { position : (Float, Float)
  , rotate : Float
  }


detectCollision : (Float, Float) -> (Float, Float) -> Bool
detectCollision (ax, ay) (bx, by) =
  let
    xOverlap =
      ((bx <= ax) && (ax <= bx + 25)) || ((bx <= ax + 5) && (ax + 5 <= bx + 25))
    yOverlap =
      ((by <= ay) && (ay <= by + 25)) || ((by <= ay + 5) && (ay + 5 <= by + 25))
  in
     xOverlap && yOverlap

moveSpriteX : Sprite -> Float -> Sprite
moveSpriteX sprite distance =
  let
    (x_, y)  = (\(x,y) -> (x + distance, y) ) sprite.position
    rotate =
       case distance > 0 of
         True -> degrees 180
         False -> degrees 0
  in
   { sprite
   |  position = ( clamp -240 240 x_, y)
   , rotate = rotate
    }

moveSpriteY : Sprite -> Float -> Sprite
moveSpriteY sprite distance =
  let
    (x, y_)  = (\(x,y) -> (x, y + distance ) ) sprite.position
    rotate =
      case distance > 0 of
        True -> degrees 270
        False -> degrees 90
  in
    { sprite | position = (x, clamp -40 40 y_ ), rotate = rotate }
