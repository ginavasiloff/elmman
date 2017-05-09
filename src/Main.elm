module Main exposing (..)

import Color
import Keyboard
import Html exposing (Html)

import Collage
import Element

type alias Model =
  { pacman : Sprite
  , score : Int
  , food : List (Float, Float)
  }

type alias Sprite =
  { position : (Float, Float)
  , rotate : Float
  }

type Msg
  = KeyMsg Keyboard.KeyCode


update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    KeyMsg code ->
      let
        pacman =
          case code of
            39 -> movePacmanX model.pacman 10
            37 -> movePacmanX model.pacman -10
            38 -> movePacmanY model.pacman 10
            40 -> movePacmanY model.pacman -10
            _ -> model.pacman

        (eaten, notEaten) =
          List.partition (checkFoodCollision model.pacman.position) model.food

      in
        { model
        | pacman = pacman
        , score = model.score + List.length eaten
        , food = notEaten
        } ! []

checkFoodCollision : (Float, Float) -> (Float, Float) -> Bool
checkFoodCollision (foodX, foodY) (pacX, pacY) =
  let
    xOverlap =
      ((pacX <= foodX) && (foodX <= pacX + 25)) || ((pacX <= foodX + 5) && (foodX + 5  <= pacX + 25 ))
    yOverlap =
      ((pacY <= foodY) && (foodY <= pacY + 25)) || ((pacY  <= foodY + 5) && (foodY + 5 <= pacY + 25))
  in
     xOverlap && yOverlap

movePacmanX : Sprite -> Float -> Sprite
movePacmanX pacman distance =
  let
    (x_, y)  = (\(x,y) -> (x + distance, y) ) pacman.position
    rotate =
       case distance > 0 of
         True -> degrees 180
         False -> degrees 0
  in
   { pacman
   |  position = ( clamp -240 240 x_, y)
   , rotate = rotate
    }

movePacmanY : Sprite -> Float -> Sprite
movePacmanY pacman distance =
  let
    (x, y_)  = (\(x,y) -> (x, y + distance ) ) pacman.position
    rotate =
      case distance > 0 of
        True -> degrees 270
        False -> degrees 90
  in
    { pacman | position = (x, clamp -40 40 y_ ), rotate = rotate }

init : (Model, Cmd msg)
init =
  { pacman = { position = (230, 0), rotate = 0 }
  , score = 0
  , food = [(50, 0), (25, 0), (0,0), (-25, 0) ]
  } ! []


view : Model -> Html msg
view model =
  Html.div []
    [ Html.span [] [ Html.text <| "score: " ++ toString model.score ]
    , Element.toHtml
      <| Collage.collage 500 500
      [ Collage.rect 500 100 |> Collage.filled Color.blue
      , Collage.rotate model.pacman.rotate <| Collage.move model.pacman.position pacman
      , Collage.group <| List.map displayFood model.food
      ]
    ]

pacman : Collage.Form
pacman =
  Element.image 25 25 "assets/Original_PacMan.png" |> Collage.toForm

displayFood : (Float, Float) -> Collage.Form
displayFood (x,y) =
  Collage.move (x,y) ( Collage.circle 5 |> Collage.filled Color.white )

main : Program Never Model Msg
main =
  Html.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.downs KeyMsg ]
