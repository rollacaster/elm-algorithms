module Main exposing (..)

import Html exposing (Html, h3, div, text)
import Html.Attributes exposing (style)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (height, width, x, fill, y)
import Time
import Color
import Animation exposing (px)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type CurrentList
    = SortBuffer (List Point) Point (List Point)
    | SortedList (List Point)


type Msg
    = UpdateArray Time.Time
    | Animate Animation.Msg


type alias Point =
    { value : Int, xPosition : Int, style : Animation.State }


type alias Model =
    { sorted : CurrentList, unsorted : List Point, done : Bool }


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
        ( { sorted = SortedList []
          , unsorted = unsorted
          , done = False
          }
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateArray _ ->
            let
                { sorted, unsorted } =
                    model

                newModel =
                    insertionSort sorted unsorted

                newStyles =
                    case newModel.sorted of
                        SortedList list ->
                            SortedList (List.map updateStyles (list))

                        SortBuffer head element unsorted ->
                            SortBuffer
                                (List.map updateStyles head)
                                (updateStyles element)
                                (List.map updateStyles unsorted)
            in
                ( { sorted = newStyles
                  , unsorted = newModel.unsorted
                  , done = newModel.done
                  }
                , Cmd.none
                )

        Animate time ->
            let
                animateWithNewTime =
                    animateElement time
            in
                case model.sorted of
                    SortedList list ->
                        ( { model | sorted = SortedList (List.map animateWithNewTime (list)) }
                        , Cmd.none
                        )

                    SortBuffer head element unsorted ->
                        ( { model
                            | sorted =
                                SortBuffer
                                    (List.map animateWithNewTime head)
                                    (animateWithNewTime element)
                                    (List.map animateWithNewTime unsorted)
                          }
                        , Cmd.none
                        )


updateStyles element =
    { element
        | style =
            Animation.interrupt
                [ Animation.to
                    [ Animation.x (toFloat element.xPosition) ]
                ]
                element.style
    }


animateElement time element =
    { element | style = Animation.update time element.style }


view : Model -> Html msg
view model =
    let
        { sorted, unsorted } =
            model
    in
        div [ (style [ ( "margin", "20px" ), ( "width", "320px" ) ]) ]
            [ (svg [ height "100", width "320" ]
                (List.map (\element -> rect (Animation.render element.style ++ [ width "10" ]) []) ((currentListToList sorted) ++ unsorted))
              )
            , (h3 [ style [ ( "textAlign", "center" ) ] ] [ text "Insertion Sort" ])
            ]


currentListToList : CurrentList -> List Point
currentListToList currentList =
    case currentList of
        SortBuffer sorted element unsorted ->
            sorted ++ (element :: unsorted)

        SortedList list ->
            list


insertionSort : CurrentList -> List Point -> Model
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


insert : Point -> List Point -> CurrentList
insert =
    insertHelper []


insertHelper : List Point -> Point -> List Point -> CurrentList
insertHelper head element sorted =
    case sorted of
        [] ->
            SortedList [ element ]

        [ x ] ->
            if x.value < element.value then
                SortedList
                    (head
                        ++ [ updatePosition (List.length head) x
                           , updatePosition (List.length head + 1) element
                           ]
                    )
            else
                SortedList
                    (head
                        ++ [ updatePosition (List.length head) element
                           , updatePosition (List.length head + 1) x
                           ]
                    )

        x :: xs ->
            if x.value < element.value then
                SortBuffer
                    (head ++ [ updatePosition (List.length head) x ])
                    (updatePosition (List.length head + 1) element)
                    (List.indexedMap (\i x -> updatePosition (List.length head + 2 + i) x) xs)
            else
                SortedList (head ++ ((updatePosition (List.length head) element) :: (List.indexedMap (\i x -> updatePosition (List.length head + 1 + i) x) sorted)))


updatePosition : Int -> Point -> Point
updatePosition newPosition point =
    { point | xPosition = newPosition * 20 }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.done == False then
        Sub.batch
            [ Time.every (Time.second / 3) UpdateArray
            , Animation.subscription Animate (List.map .style (currentListToList model.sorted))
            ]
    else
        Animation.subscription Animate (List.map .style (currentListToList model.sorted))
