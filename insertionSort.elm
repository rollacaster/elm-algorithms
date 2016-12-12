module Main exposing (..)

import Html exposing (Html)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (height, width, x, fill, y)
import Time
import Color
import Animation exposing (px)

main: Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Point =
    { value : Int, position : Int }


type alias Model =
    { sorted : CurrentList, unsorted : List Int, done : Bool, styles : List Animation.State }


type alias ModelList =
    { sorted : CurrentList, unsorted : List Int, done : Bool }


init : ( Model, Cmd Msg )
init =
    let
        values =
            [ 40, 30, 60, 10, 40, 30, 60, 10, 40, 30, 60, 10, 40, 30, 60, 10, 40, 30, 60, 10, 40, 30, 60, 10 ]
    in
        ( { sorted = SortedList []
          , unsorted = values
          , done = False
          , styles =
                List.map
                    Animation.style
                    (List.indexedMap
                        (\index element ->
                            [ Animation.x (20 * (toFloat index) + 20)
                            , Animation.y (100 - element)
                            , Animation.fill (Color.hsl (element * 0.05) 20.0 20.0)
                            , Animation.height (px element)
                            ]
                        )
                        values
                    )
          }
        , Cmd.none
        )


type CurrentList
    = SortBuffer (List Int) Int (List Int)
    | SortedList (List Int)


type Msg
    = UpdateArray Time.Time
    | Animate Animation.Msg


mapCurrentListToList : CurrentList -> List Int
mapCurrentListToList currentList =
    case currentList of
        SortBuffer sorted element unsorted ->
            sorted ++ (element :: unsorted)

        SortedList list ->
            list


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
                    List.map3
                        (\i style element ->
                            Animation.interrupt
                                [ Animation.to
                                    [ Animation.x (20 * (toFloat i) + 20)
                                    , Animation.y (100 - (toFloat element))
                                    , Animation.height (px (toFloat element))
                                    , Animation.fill (Color.hsl ((toFloat element) * 0.05) 20.0 20.0)
                                    ]
                                ]
                                style
                        )
                        (List.range 0 (List.length model.styles))
                        model.styles
                        ((mapCurrentListToList newModel.sorted) ++ newModel.unsorted)
            in
                ( { sorted = newModel.sorted
                  , unsorted = newModel.unsorted
                  , done = newModel.done
                  , styles = newStyles
                  }
                , Cmd.none
                )

        Animate time ->
            ( { model | styles = List.map (Animation.update time) model.styles }, Cmd.none )


view : Model -> Html msg
view model =
    let
        { sorted, unsorted, styles } =
            model

        list =
            case sorted of
                SortedList list ->
                    list ++ unsorted

                SortBuffer head element sorted ->
                    head ++ (element :: sorted) ++ unsorted
    in
        svg [ height "100", width "1000" ]
            (List.map (\style -> rect (Animation.render style ++ [ width "10" ]) []) styles)


insert : Int -> List Int -> CurrentList
insert =
    insertHelper []


insertHelper : List Int -> Int -> List Int -> CurrentList
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


insertionSort : CurrentList -> List Int -> ModelList
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
            [ Time.every (Time.second / 2) UpdateArray
            , Animation.subscription Animate model.styles
            ]
    else
        Animation.subscription Animate model.styles
