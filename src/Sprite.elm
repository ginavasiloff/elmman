module Sprite exposing (..)


type alias Sprite =
  { position : (Float, Float)
  , dimensions : (Float, Float)
  , rotate : Float
  }


food : (Float, Float) -> Sprite
food position =
  { position = position
  , dimensions = (5,5)
  , rotate = 0
  }


detectCollision : Sprite -> Sprite -> Bool
detectCollision sprite1 sprite2 =
  let
    (ax, ay) = sprite1.position
    (bx, by) = sprite2.position
    (heightA, widthA) = sprite1.dimensions
    (heightB, widthB) = sprite2.dimensions
    xOverlap1 = (bx <= ax) && (ax <= bx + widthB)
    xOverlap2 = (bx <= ax + widthA) && (ax + widthA <= bx + widthB)
    yOverlap1 = (by <= ay) && (ay <= by + 25)
    yOverlap2 = (by <= ay + heightA) && (ay + heightA <= by + heightB)
  in
    (xOverlap1 || xOverlap2) && (yOverlap1 || yOverlap2)


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
