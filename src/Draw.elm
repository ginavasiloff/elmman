module Draw exposing (..)

import Collage exposing (..)
import Color
import Element exposing (Element)
import Html exposing (Html)
import Sprite exposing (Sprite)

import Data exposing (Model)

screen : Model -> Html msg
screen model =
  Html.div []
    [ Html.span [] [ Html.text <| "score: " ++ toString model.score ]
      , Collage.collage 500 500
      [ Collage.rect 500 200 |> Collage.filled Color.blue
      , model.pacman |> pacman
      , model.food |> foods
      , model.barriers |> barriers
      ] |> Element.toHtml
    ]

barriers : List Path -> Collage.Form
barriers paths =
  paths
    |> List.map (Collage.traced barrierStyle)
    |> Collage.group

foods : List Sprite -> Collage.Form
foods sprites =
  sprites
    |> List.map food
    |> Collage.group

food : Sprite -> Collage.Form
food sprite =
  Collage.circle 5
   |> Collage.filled Color.white
   |> Collage.move sprite.position


pacman : Sprite -> Collage.Form
pacman sprite =
  Element.image 25 25 "assets/Original_PacMan.png"
    |> Collage.toForm
    |> Collage.move sprite.position
    |> Collage.rotate sprite.rotate


barrierStyle : Collage.LineStyle
barrierStyle =
  { color = Color.blue
  , width = 1
  , cap = Flat
  , join = Smooth
  , dashing = []
  , dashOffset = 0
  }
