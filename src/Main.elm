module Main exposing (..)

import Html exposing (Html, text, div, h1, img, input, button)
import Html.Attributes as Attr exposing (src, type_, value)
import Html.Events exposing (onInput, onClick)
import Random


---- MODEL ----


type alias Model =
    { numberOfDices : Int
    , rollsOfDice : Int
    , dices : List Int
    }


init : ( Model, Cmd Msg )
init =
    { numberOfDices = 1
    , rollsOfDice = 6
    , dices = []
    }
        ! []



---- UPDATE ----


type Msg
    = NoOp
    | UpdateNumberOfDices Int
    | UpdateRollsOfDice Int
    | CastDices
    | UpdateDices (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateNumberOfDices n ->
            { model | numberOfDices = n, dices = [] } ! []

        UpdateRollsOfDice n ->
            { model | rollsOfDice = n, dices = [] } ! []

        CastDices ->
            model ! [ castDices model ]

        UpdateDices l ->
            { model | dices = l } ! []


castDices : Model -> Cmd Msg
castDices model =
    Random.generate UpdateDices <|
        randomDices model.numberOfDices model.rollsOfDice


randomDices : Int -> Int -> Random.Generator (List Int)
randomDices dices rolls =
    Random.list dices <|
        Random.int 1 rolls



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewSetting model
        , viewDices model.dices
        ]


viewSetting : Model -> Html Msg
viewSetting model =
    div []
        [ text "サイコロの数 : "
        , input
            [ type_ "number"
            , Attr.min "1"
            , value (toString model.numberOfDices)
            , onInput (updateNumber UpdateNumberOfDices)
            ]
            []
        , text "サイコロの目の数 : "
        , input
            [ type_ "number"
            , Attr.min "1"
            , value (toString model.rollsOfDice)
            , onInput (updateNumber UpdateRollsOfDice)
            ]
            []
        , button [ onClick CastDices ] [ text "サイコロを振る" ]
        ]


viewDices : List Int -> Html Msg
viewDices l =
    div []
        [ text <| toString l ]


updateNumber : (Int -> Msg) -> String -> Msg
updateNumber toMsg s =
    case String.toInt s of
        Ok n ->
            toMsg n

        _ ->
            NoOp



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
