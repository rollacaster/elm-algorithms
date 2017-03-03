module InsertionSort
    exposing
        ( InsertionList
        , insertionSort
        , init
        , update
        , animate
        , view
        , subscribe
        )

import Html exposing (Html)
import Point exposing (Point, updatePosition, updateStyles, animateElement, renderPoints)
import Animation


type alias InsertionList =
    { sorted : CurrentList, unsorted : List Point }


type CurrentList
    = SortBuffer (List Point) Point (List Point)
    | SortedList (List Point)


init : List Point -> InsertionList
init unsorted =
    { unsorted = unsorted, sorted = SortedList [] }


update : InsertionList -> InsertionList
update oldList =
    let
        newModel =
            insertionSort oldList.sorted oldList.unsorted
    in
        case newModel.sorted of
            SortedList list ->
                { newModel | sorted = SortedList (List.map updateStyles (list)) }

            SortBuffer head element unsorted ->
                { newModel
                    | sorted =
                        SortBuffer
                            (List.map updateStyles head)
                            (updateStyles element)
                            (List.map updateStyles unsorted)
                }


animate : Animation.Msg -> InsertionList -> InsertionList
animate time newList =
    case newList.sorted of
        SortedList list ->
            { sorted = SortedList (List.map (animateElement time) list)
            , unsorted = newList.unsorted
            }

        SortBuffer head element unsorted ->
            { sorted =
                SortBuffer
                    (List.map (animateElement time) head)
                    ((animateElement time) element)
                    (List.map (animateElement time) unsorted)
            , unsorted = newList.unsorted
            }


view : InsertionList -> List (Html msg)
view list =
    renderPoints "Insertion Sort" ((currentListToList list.sorted) ++ list.unsorted)


subscribe : (Animation.Msg -> msg) -> InsertionList -> Sub msg
subscribe msg list =
    Animation.subscription msg (List.map .style ((currentListToList list.sorted) ++ list.unsorted))


currentListToList : CurrentList -> List Point
currentListToList currentList =
    case currentList of
        SortBuffer sorted element unsorted ->
            sorted ++ (element :: unsorted)

        SortedList list ->
            list


insertionSort : CurrentList -> List Point -> InsertionList
insertionSort sorted list =
    case sorted of
        SortBuffer head element sorted ->
            { sorted = insertHelper head element sorted, unsorted = list }

        SortedList sortedList ->
            case list of
                [] ->
                    { sorted = sorted, unsorted = [] }

                [ x ] ->
                    { sorted = insert x sortedList, unsorted = [] }

                x :: xs ->
                    { sorted = insert x sortedList, unsorted = xs }


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
