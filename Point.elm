module Point
    exposing
        ( Point
        , updatePosition
        , updateStyles
        , animateElement
        , init
        , renderPoints
        )

import Svg exposing (svg, rect)
import Svg.Attributes exposing (height, width, x, fill, y)
import Html exposing (h3, text, Html)
import Html.Attributes exposing (style)
import Color
import Animation exposing (px)


type alias Point =
    { value : Int, xPosition : Int, style : Animation.State }


init : List Int -> List Point
init xs =
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
        xs


animateElement : Animation.Msg -> Point -> Point
animateElement time element =
    { element | style = Animation.update time element.style }


updatePosition : Int -> Point -> Point
updatePosition newPosition point =
    { point | xPosition = newPosition * 20 }


updateStyles : Point -> Point
updateStyles element =
    { element
        | style =
            Animation.interrupt
                [ Animation.to
                    [ Animation.x (toFloat element.xPosition) ]
                ]
                element.style
    }


renderPoints : String -> List Point -> List (Html msg)
renderPoints label list =
    [ (svg [ height "100", width "320" ]
        (List.map (\element -> rect (Animation.render element.style ++ [ width "10" ]) []) list)
      )
    , (h3 [ style [ ( "textAlign", "center" ) ] ] [ text label ])
    ]
