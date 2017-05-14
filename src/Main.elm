module Main exposing (..)

import Color
import Keyboard
import Html exposing (Html)

import Collage
import Element

import Sprite exposing (Sprite)

type alias Model =
  { pacman : Sprite
  , score : Int
  , food : List Sprite
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

displayFood : Sprite -> Collage.Form
displayFood sprite =
  Collage.move sprite.position ( Collage.circle 5 |> Collage.filled Color.white )

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
