module InsertionSort
    exposing
        ( InsertionList
        , insertionSort
        , CurrentList
        , init
        , update
        , animate
        , currentListToList
        )

import Point exposing (Point, updatePosition, updateStyles, animateElement)
import Animation


type alias InsertionList =
    { sorted : CurrentList, unsorted : List Point, done : Bool }


type CurrentList
    = SortBuffer (List Point) Point (List Point)
    | SortedList (List Point)


init : List Point -> InsertionList
init unsorted =
    { unsorted = unsorted, sorted = SortedList [], done = False }


update : CurrentList -> CurrentList
update sorted =
    case sorted of
        SortedList list ->
            SortedList (List.map updateStyles (list))

        SortBuffer head element unsorted ->
            SortBuffer
                (List.map updateStyles head)
                (updateStyles element)
                (List.map updateStyles unsorted)


animate : Animation.Msg -> InsertionList -> InsertionList
animate time newList =
    let
        animateWithNewTime =
            animateElement time
    in
        case newList.sorted of
            SortedList list ->
                { sorted = SortedList (List.map animateWithNewTime (list))
                , unsorted = newList.unsorted
                , done = newList.done
                }

            SortBuffer head element unsorted ->
                { sorted =
                    SortBuffer
                        (List.map animateWithNewTime head)
                        (animateWithNewTime element)
                        (List.map animateWithNewTime unsorted)
                , unsorted = newList.unsorted
                , done = newList.done
                }


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
