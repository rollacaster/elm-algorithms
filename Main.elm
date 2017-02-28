module Main exposing (..)

import Html exposing (Html, h3, div, text)
import Html.Attributes exposing (style)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (height, width, x, fill, y)
import Time exposing (Time)
import Color
import Animation exposing (px)
import InsertionSort exposing (InsertionList, insertionSort, CurrentList, currentListToList)
import Point exposing (Point)
import MergeSort exposing (mergeSort)


type alias Model =
    { insertionSort : InsertionList, merge : List Point }


init : ( Model, Cmd Msg )
init =
    let
        unsorted =
            List.indexedMap
                (\i x ->
                    Point
                        x
                        (20 * i)
                        (Animation.style
                            [ Animation.x (toFloat (20 * i))
                            , Animation.y (toFloat (100 - x))
                            , Animation.fill (Color.hsl ((toFloat x) * 0.05) 20.0 20.0)
                            , Animation.height (px (toFloat x))
                            ]
                        )
                )
                [ 40, 30, 60, 10, 40, 30, 60, 10, 40, 30, 60, 10, 40, 30, 60, 10 ]
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
            let
                newModel =
                    insertionSort model.insertionSort.sorted model.insertionSort.unsorted

                newStyles =
                    InsertionSort.update newModel.sorted
            in
                ( { model
                    | insertionSort = { sorted = newStyles, unsorted = newModel.unsorted, done = newModel.done }
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
    let
        { merge } =
            model
    in
        div [ (style [ ( "margin", "20px" ), ( "width", "320px" ) ]) ]
            [ (svg [ height "100", width "320" ]
                (List.map (\element -> rect (Animation.render element.style ++ [ width "10" ]) [])
                    ((currentListToList model.insertionSort.sorted) ++ model.insertionSort.unsorted)
                )
              )
            , (h3 [ style [ ( "textAlign", "center" ) ] ] [ text "Insertion Sort" ])
            , (svg [ height "100", width "320" ]
                (List.map (\element -> rect (Animation.render element.style ++ [ width "10" ]) []) merge)
              )
            , (h3 [ style [ ( "textAlign", "center" ) ] ] [ text "Merge Sort" ])
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.insertionSort.done == False then
        Sub.batch
            [ Time.every (Time.second / 3) UpdateArray
            , Animation.subscription Animate (List.map .style (currentListToList model.insertionSort.sorted))
            ]
    else
        Animation.subscription Animate (List.map .style (currentListToList model.insertionSort.sorted))


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
