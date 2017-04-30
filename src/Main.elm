module Main exposing (..)

import Color
import Keyboard
import Html exposing (Html)

import Collage
import Element

type alias Model =
  { pacman : (Float, Float)
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
              (\(x, y) ->
                let
                  x_ =
                    clamp -250 240 (x + 10)
                in
                  ( x_ , y)
              ) model.pacman
            37 ->
              (\(x, y) ->
                let
                  x_ =
                    clamp -240 240 (x - 10)
                in
                  (x_, y)
              ) model.pacman
            38 ->
              (\(x, y) ->
                let
                  y_ =
                    clamp -40 40 (y+10)
                in
                  (x, y_) ) model.pacman
            40 ->
              (\(x, y) ->
                let
                  y_ =
                    clamp -40 40 ( y - 10 )
                in
                  (x, y_)
               ) model.pacman
            _ -> model.pacman
      in
        { model | pacman = pacman } ! []



init : (Model, Cmd msg)
init =
  { pacman = (230, 0)
  } ! []


view : Model -> Html msg
view model =
  Element.toHtml
  <| Collage.collage 500 500
    [ Collage.rect 500 100 |> Collage.filled Color.blue
    , Collage.move model.pacman pacman
    ]

pacman : Collage.Form
pacman =
  Element.image 25 25 "assets/Original_PacMan.png" |> Collage.toForm

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
