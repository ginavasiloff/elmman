module Main exposing (..)

import Color
import Keyboard
import Html exposing (Html)

import Collage
import Element

type alias Model =
  { pacman : (Float, Float)
  , score : Int
  , food : List (Float, Float)
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
            39 ->
              movePacmanX model.pacman 10
            37 ->
              movePacmanX model.pacman -10
            38 ->
              movePacmanY model.pacman 10
            40 ->
              movePacmanY model.pacman -10
            _ -> model.pacman
      in
        { model | pacman = pacman } ! []

movePacmanX : (Float, Float) -> Float -> (Float, Float)
movePacmanX initialPos distance =
  let
    (x_, y)  = (\(x,y) -> (x + distance, y) ) initialPos
  in
    ( clamp -240 240 x_, y)

movePacmanY : (Float, Float) -> Float -> (Float, Float)
movePacmanY initialPos distance =
  let
    (x, y_)  = (\(x,y) -> (x, y + distance ) ) initialPos
  in
    (x, clamp -40 40 y_ )

init : (Model, Cmd msg)
init =
  { pacman = (230, 0)
  , score = 0
  , food = [(50, 0)]
  } ! []


view : Model -> Html msg
view model =
  Html.div []
    [ Html.span [] [ Html.text <| "score: " ++ toString model.score ]
    , Element.toHtml
      <| Collage.collage 500 500
      [ Collage.rect 500 100 |> Collage.filled Color.blue
      , Collage.move model.pacman pacman
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
