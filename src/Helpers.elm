module Helpers exposing (..)

{-| This module contains functions that don't fit into any other module but are
generally usefull.


# List Helpers

@docs segmentEvery


# Set Helpers

@docs flip


# Composition

@docs curry, uncurry


# To String

@docs fromBool


# Result

@docs isErr, isOk

-}

import Set exposing (Set)


{-| Convert a List into a List of Lists where each list has a the specified
length (except the last one which can be shorter).

    segmentEvery 2 [ 1, 2, 3, 4, 5, 6, 7 ]

    [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ], [ 7 ] ]

    segmentEvery 0 [ 1, 2, 3 ]

    []

    segmentEvery -1 [ 1, 2, 3 ]

    []

-}
segmentEvery : Int -> List a -> List (List a)
segmentEvery n xs =
    let
        segment =
            List.take n xs

        rest =
            List.drop n xs
    in
    if n <= 0 || xs == [] then
        []

    else if List.length segment < n then
        [ segment ]

    else
        segment :: segmentEvery n rest


{-| Add the element to the set if it doesn't already exist, in that case remove
it instead.

TODO: Add tests!!

-}
flip : comparable -> Set comparable -> Set comparable
flip a ass =
    if Set.member a ass then
        Set.remove a ass

    else
        Set.insert a ass


curry : (( a, b ) -> c) -> a -> b -> c
curry f a b =
    f ( a, b )


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


{-| From Bool to string
-}
fromBool : Bool -> String
fromBool b =
    if b then
        "True"

    else
        "False"


{-| Check if a result is an Error
-}
isErr : Result a b -> Bool
isErr v =
    case v of
        Ok _ ->
            False

        _ ->
            True


{-| Check if a result is an Ok
-}
isOk : Result a b -> Bool
isOk =
    not << isErr
