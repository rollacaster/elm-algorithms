module Main exposing (..)

import Html exposing (Html)
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


type alias Position =
    { x : Int, y : Int }


type alias Point =
    { value : Int, position : Position, style : Animation.State }


type alias Model =
    { sorted : CurrentList, unsorted : List Point, done : Bool }


type alias ModelList =
    { sorted : CurrentList, unsorted : List Point, done : Bool }


init : ( Model, Cmd Msg )
init =
    let
        values =
            List.indexedMap
                (\i x ->
                    Point
                        x
                        (Position (20 * i) (100 - x))
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
          , unsorted = values
          , done = False
          }
        , Cmd.none
        )


mapCurrentListToList : CurrentList -> List Point
mapCurrentListToList currentList =
    case currentList of
        SortBuffer sorted element unsorted ->
            sorted ++ (element :: unsorted)

        SortedList list ->
            list


updateStyles element =
    { element
        | style =
            Animation.interrupt
                [ Animation.to [ Animation.x (toFloat element.position.x) ] ]
                element.style
    }


animateElement element time =
    { element | style = Animation.update time element.style }


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
                            SortBuffer (List.map updateStyles head) (updateStyles element) (List.map updateStyles unsorted)
            in
                ( { sorted = newStyles
                  , unsorted = newModel.unsorted
                  , done = newModel.done
                  }
                , Cmd.none
                )

        Animate time ->
            case model.sorted of
                SortedList list ->
                    ( { model | sorted = SortedList (List.map (\e -> animateElement e time) (list)) }, Cmd.none )

                SortBuffer head element unsorted ->
                    ( { model | sorted = SortBuffer (List.map (\e -> animateElement e time) head) (animateElement element time) (List.map (\e -> animateElement e time) unsorted) }, Cmd.none )


view : Model -> Html msg
view model =
    let
        { sorted, unsorted } =
            model
    in
        svg [ height "100", width "1000" ]
            (List.map (\element -> rect (Animation.render element.style ++ [ width "10" ]) []) ((mapCurrentListToList sorted) ++ unsorted))


insert : Point -> List Point -> CurrentList
insert =
    insertHelper []


updatePosition : Int -> Point -> Point
updatePosition newPosition point =
    let
        { position } =
            point
    in
        { point | position = { position | x = newPosition * 20 } }


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


insertionSort : CurrentList -> List Point -> ModelList
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


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.done == False then
        Sub.batch
            [ Time.every (Time.second / 3) UpdateArray
            , Animation.subscription Animate (List.map .style (mapCurrentListToList model.sorted) ++ List.map .style model.unsorted)
            ]
    else
        Animation.subscription Animate (List.map .style (mapCurrentListToList model.sorted) ++ List.map .style model.unsorted)
