module MergeSort exposing (mergeSort, update, animate, view)

import Svg exposing (svg, rect)
import Svg.Attributes exposing (height, width, x, fill, y)
import Html exposing (Html, h3, div, text)
import Html.Attributes exposing (style)
import Point exposing (Point, updateStyles, updatePosition, animateElement)
import Animation


update : List Point -> List Point
update unsorted =
    List.map updateStyles (List.indexedMap (\i -> updatePosition i) unsorted)


animate : Animation.Msg -> List Point -> List Point
animate time newList =
    List.map (animateElement time) newList


view : List Point -> List (Html msg)
view list =
    [ (svg [ height "100", width "320" ]
        (List.map (\element -> rect (Animation.render element.style ++ [ width "10" ]) []) list)
      )
    , (h3 [ style [ ( "textAlign", "center" ) ] ] [ text "Merge Sort" ])
    ]


mergeSort : List Point -> List Point
mergeSort unsorted =
    if (List.length unsorted) <= 1 then
        unsorted
    else
        let
            middle =
                (List.length unsorted) // 2

            left =
                List.take middle unsorted

            right =
                List.drop middle unsorted
        in
            merge (mergeSort left) (mergeSort right) []


merge : List Point -> List Point -> List Point -> List Point
merge left right sorted =
    case left of
        [] ->
            sorted ++ right

        x :: xs ->
            case right of
                [] ->
                    sorted ++ left

                y :: ys ->
                    if x.value < y.value then
                        merge xs right (sorted ++ [ x ])
                    else
                        merge left ys (sorted ++ [ y ])
