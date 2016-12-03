module Main exposing (..)

import Html exposing (Html)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (height, width, x, fill, y)
import Time


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init =
    ( { sorted = SortedList []
      , unsorted = [ 40, 30, 60, 10, 40, 30, 60, 10, 40, 30, 60, 10, 40, 30, 60, 10, 40, 30, 60, 10, 40, 30, 60, 10 ]
      , done = False
      }
    , Cmd.none
    )


type CurrentList
    = SortBuffer (List Int) Int (List Int)
    | SortedList (List Int)


type Msg
    = UpdateArray Time.Time


update msg model =
    case msg of
        UpdateArray _ ->
            let
                { sorted, unsorted } =
                    model
            in
                ( (insertionSort sorted unsorted), Cmd.none )


view model =
    let
        svgHeight =
            100

        { sorted, unsorted } =
            model

        list =
            case sorted of
                SortedList list ->
                    list ++ unsorted

                SortBuffer head element sorted ->
                    head ++ (element :: sorted) ++ unsorted
    in
        svg [ height (toString svgHeight), width "1000" ]
            (List.indexedMap
                (\index element ->
                    let
                        margin =
                            20

                        colWidth =
                            10
                    in
                        rect
                            [ y (toString (svgHeight - element))
                            , fill ("#" ++ (toString (15 * element)))
                            , width (toString colWidth)
                            , x (toString ((2 * index * colWidth) + margin))
                            , height (toString element)
                            ]
                            []
                )
                list
            )


insert =
    insertHelper []


insertHelper head element sorted =
    case sorted of
        [] ->
            SortedList [ element ]

        [ x ] ->
            if x < element then
                SortedList (head ++ [ x, element ])
            else
                SortedList (head ++ [ element, x ])

        x :: xs ->
            if x < element then
                SortBuffer (head ++ [ x ]) element xs
            else
                SortedList (head ++ (element :: sorted))


insertionSort sorted list =
    case sorted of
        SortBuffer head element sorted ->
            { sorted = insertHelper head element sorted, unsorted = list, done = False }

        SortedList sortedList ->
            case list of
                [] ->
                    { sorted = sorted, unsorted = [], done = True }

                [ x ] ->
                    { sorted = insert x sortedList, unsorted = [], done = True }

                x :: xs ->
                    { sorted = insert x sortedList, unsorted = xs, done = False }


subscriptions model =
    if model.done == False then
        Time.every (Time.second / 5) UpdateArray
    else
        Sub.none
