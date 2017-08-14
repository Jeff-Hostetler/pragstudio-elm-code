module Bingo exposing(..)

import Html exposing (..)
import Html.Attributes exposing (..)

playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " - game #" ++ (toString gameNumber)

viewPlayer : String -> Int -> Html msg
viewPlayer name gameNumber =
    let
        playerInfoText =
            playerInfo name gameNumber
                |> String.toUpper
                |> Html.text
    in
        h2 [id "info", class "classy" ]
            [ playerInfoText ]

viewHeader : String -> Html msg
viewHeader title =
    header []
        [ h1 [] [text title] ]

viewFooter : Html msg
viewFooter =
    footer []
        [ a [ href "http://elm-lang.org"]
            [ text "Go to Elm"]
        ]

view : Html msg
view =
    div [ class "content" ]
        [viewHeader "Elm Tutorial Game"
        , viewPlayer "Jeff" 4
        , viewPlayer "Kelcey" 5
        , viewFooter]

main =
    view