module JsonApi.Base.Utility exposing
    ( dropSecond
    , find
    , findSpot
    , tuplicate
    , pair
    )

import Json.Decode as Decode exposing (Decoder)
import List exposing (filter, sort)
import Maybe exposing (withDefault)


find : (a -> Bool) -> List a -> Maybe a
find test list =
    case list of
        [] ->
            Nothing

        h :: rest ->
            if test h then
                Just h

            else
                find test rest


findWithOneIndex : (Int -> a -> Bool) -> List a -> Maybe ( Int, a )
findWithOneIndex test list =
    let
        next count l =
            case l of
                [] ->
                    Nothing

                h :: rest ->
                    if test count h then
                        Just ( count, h )

                    else
                        next (count + 1) rest
    in
    next 1 list


findSpot : List Int -> Int
findSpot list =
    list
        |> filter ((\b a -> (>) a b) 0)
        |> sort
        |> findWithOneIndex (/=)
        |> Maybe.map Tuple.first
        |> withDefault 1


tuplicate : a -> ( a, a )
tuplicate a =
    ( a, a )


dropSecond : (a -> c) -> (a -> b -> c)
dropSecond simpleF =
    let
        function a b =
            simpleF a
    in
    function


pair : a -> b -> (a,b)
pair  a b = (a, b)

