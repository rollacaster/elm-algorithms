module Point exposing (Point, updatePosition, updateStyles, animateElement)

import Animation


type alias Point =
    { value : Int, xPosition : Int, style : Animation.State }


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
