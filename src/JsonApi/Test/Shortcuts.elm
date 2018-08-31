module JsonApi.Test.Shortcuts exposing (b, f, i, l, linkage, linkages, n, o, s, takeId, p)

import Json.Encode as E exposing (Value, encode)

import JsonApi.Base.Utility exposing (pair)

o =
    E.object


i =
    E.int


s =
    E.string


b =
    E.bool


f =
    E.float


l =
    E.list


n =
    E.null

p = pair


takeId : List ( String, Value ) -> List ( String, Value )
takeId pairs =
    let
        isIdField ( key, _ ) =
            List.member key [ "type", "id", "meta" ]
    in
    List.filter isIdField pairs


linkage : List ( String, Value ) -> ( String, Value )
linkage pairs =
    pair "data" (o (takeId pairs))


linkages : List (List ( String, Value )) -> ( String, Value )
linkages pairList =
     List.map (o << takeId) pairList
     |> E.list 
    |> (\a -> p "data"  a )
