module Main exposing (..)

import Html exposing (Html)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (height, width, x, fill, y)
import Time
import Debug exposing (log)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init =
    ( ( [], [ 40, 30, 60, 10, 40, 30, 60, 10, 40, 30, 60, 10 ] ), Cmd.none )


type Msg
    = UpdateArray Time.Time


update msg model =
    case msg of
        UpdateArray _ ->
            let
                ( sorted, unsorted ) =
                    model
            in
                ( (insertionSort sorted unsorted), Cmd.none )


view model =
    let
        svgHeight =
            100

        ( sorted, unsorted ) =
            model
    in
        svg [ height (toString svgHeight), width "300" ]
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
                (List.append sorted unsorted)
            )


insert =
    insertHelper []


insertHelper head element sorted =
    case sorted of
        [] ->
            [ element ]

        [ x ] ->
            if x < element then
                List.append head [ x, element ]
            else
                List.append head [ element, x ]

        x :: xs ->
            if x < element then
                insertHelper (List.append head [ x ]) element xs
            else
                List.append head (element :: sorted)


insertionSort sorted list =
    case list of
        [] ->
            ( sorted, [] )

        [ x ] ->
            ( insert x sorted, [] )

        x :: xs ->
            ( (insert x sorted), xs )


subscriptions model =
    Time.every Time.second UpdateArray
