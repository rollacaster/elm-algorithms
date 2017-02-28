module Main exposing (..)

import Html exposing (Html, h3, div, text)
import Html.Attributes exposing (style)
import Time exposing (Time)
import Animation exposing (px)
import InsertionSort exposing (InsertionList, insertionSort)
import Point exposing (Point)
import MergeSort exposing (mergeSort)


type alias Model =
    { insertionSort : InsertionList, merge : List Point }


init : ( Model, Cmd Msg )
init =
    let
        unsorted =
            Point.init [ 40, 30, 60, 10, 40, 30, 60, 10, 40, 30, 60, 10, 40, 30, 60, 10 ]
    in
        ( { insertionSort = InsertionSort.init unsorted
          , merge = MergeSort.mergeSort unsorted
          }
        , Cmd.none
        )


type Msg
    = UpdateArray Time
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateArray _ ->
            ( { model
                | insertionSort = InsertionSort.update model.insertionSort
                , merge = MergeSort.update model.merge
              }
            , Cmd.none
            )

        Animate time ->
            ( { model
                | insertionSort = InsertionSort.animate time model.insertionSort
                , merge = MergeSort.animate time model.merge
              }
            , Cmd.none
            )


view : Model -> Html msg
view model =
    div [ (style [ ( "margin", "20px" ), ( "width", "320px" ) ]) ]
        ((InsertionSort.view model.insertionSort) ++ (MergeSort.view model.merge))


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.insertionSort.done == False then
        Sub.batch
            [ Time.every (Time.second / 3) UpdateArray
            , Animation.subscription Animate (List.map .style (InsertionSort.toList model.insertionSort))
            ]
    else
        Animation.subscription Animate (List.map .style (InsertionSort.toList model.insertionSort))


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
