module Main exposing (..)

import Keyboard
import Html exposing (Html)

import Data exposing (Model, Msg(..))
import Draw
import Sprite


update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    KeyMsg code ->
      let
        pacman =
          case code of
            39 -> Sprite.moveSpriteX model.pacman 10
            37 -> Sprite.moveSpriteX model.pacman -10
            38 -> Sprite.moveSpriteY model.pacman 10
            40 -> Sprite.moveSpriteY model.pacman -10
            _ -> model.pacman

        (eaten, notEaten) =
          List.partition (Sprite.detectCollision model.pacman) model.food

      in
        { model
        | pacman = pacman
        , score = model.score + List.length eaten
        , food = notEaten
        } ! []


init : (Model, Cmd msg)
init =
  { pacman = { position = (230, 0), rotate = 0, dimensions = (25,25) }
  , score = 0
  , food = List.map Sprite.food [(50, 0), (25, 0), (0,0), (-25, 0) ]
  , barriers = []
  } ! []


main : Program Never Model Msg
main =
  Html.program
  { init = init
  , update = update
  , view = Draw.screen
  , subscriptions = subscriptions
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.downs KeyMsg ]
