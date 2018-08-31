module JsonApi.Relationship exposing
    ( setLocality, rename
    , createSingle, createOneMore, relateSingle, incorporate, relateOneMore, unrelate
    , swapData
    , getIdr, getIdrs, links, meta, reach, reachMany, isLocal, find
    )

{-| #Configuration of json api `relationships`.


## Configuration

@docs setLocality, rename


## Setter

@docs createSingle, createOneMore, relateSingle, incorporate, relateOneMore, unrelate
@docs swapData


# Getter

@docs getIdr, getIdrs, links, meta, reach, reachMany, isLocal, find

-}

import Boxed exposing (Boxed(..))
import JsonApi as JsonApi
import JsonApi.Base.Core as Core
    exposing
        ( Identifier
        , Object
        , TopLevel
        , getRelationshipLinks
        )
import JsonApi.Base.Definition as Definition
import JsonApi.Base.Guide as Guide
    exposing
        ( Guide
        , setDoc
        , updateDoc
        , updateIdr
        )
import JsonApi.Base.Link exposing (linkTranslator)
import JsonApi.Base.Utility as Utility
import List exposing (map, singleton)
import Maybe exposing (andThen, withDefault)


{-| Set relationship either as local or outbound. Local relationships do not get
sent to the back end.
-}
setLocality : String -> Bool -> Guide g c -> Guide g c
setLocality field locality g =
    Core.setRelationLocality g.idr field locality g.doc
        |> Guide.setDoc g


{-| Find if relationship is local or outbound. Local relationships do not get
sent to the back end.
-}
isLocal : String -> Guide g c -> Bool
isLocal field g =
    Core.getRelationLocality g.idr field g.doc


{-| Change relationship's name.
-}
rename : String -> String -> Guide g c -> Guide g c
rename field newName g =
    Core.renameRelationship g.idr field newName g.doc
        |> Guide.setDoc g


{-| Remove relationship. If this relationship is not currently present in the
document, it has no effect. In one-to-one relationships, the value of this
relationship will be encoded as `null` if the user sends an http `POST` request
and the relationship was present before a call to `unrelate`.
-}
unrelate : String -> Identifier -> Guide g a -> Guide g a
unrelate field source g =
    Core.unrelate g.idr field source g.doc
        |> Guide.setDoc g


{-| Creates a brand new resource of given type, added as a one-to-one
relationship with given name. Returns a `Guide` pointing to the newly
created resource.

    childGuide = createSingle "rerouce-type" "field-name" someGuide

    updateDoc childGuide.doc someGuide
    |> getIdr "field-name"
    |> Maybe.map (JsonApi.equal childGuide.idr)

-}
createSingle : String -> String -> Guide g a -> Guide g a
createSingle type_ field g =
    let
        ( newIdr, newDoc ) =
            Core.createOne g.idr field True type_ g.doc
    in
    { g | idr = newIdr, doc = newDoc }


{-| Creates a brand new resource of given type, added as part of a one-to-many
relationship with given name. Returns a `Guide` pointing to the newly
created resource.

    childGuide = createOneMore "resource-type" "field-name" someGuide

    updateDoc childGuide.doc someGuide
    |> getIdrs "field-name"
    |> List.any (JsonApi.equal childGuide.idr)

-}
createOneMore : String -> String -> Guide g a -> Guide g a
createOneMore type_ field g =
    let
        ( newIdr, newDoc ) =
            Core.createOne g.idr field False type_ g.doc
    in
    { g | idr = newIdr, doc = newDoc }


{-| Establish a one-to-one relationship.
-}
relateSingle : String -> Identifier -> Guide g a -> Guide g a
relateSingle field source g =
    Core.relate g.idr field True source g.doc
        |> setDoc g


{-| Adds a single part of a one-to-many relationship.
-}
relateOneMore : String -> Identifier -> Guide g a -> Guide g a
relateOneMore field source g =
    Core.relate g.idr field False source g.doc
        |> setDoc g


{-| Adds the primary data of a source document to existing data of a
relationship in the target document, keeping the new data if any conflict
arises. It succeeds only if both source and target data match in being either
single resource or array of resources.
-}
incorporate : String -> TopLevel a -> Guide g a -> Guide g a
incorporate targetField source g =
    Core.incorporate g.idr targetField source g.doc
        |> setDoc g


{-| Get the links from a json api `relationship object`.
-}
links : String -> JsonApi.Guide g c -> List ( String, JsonApi.Link c )
links field g =
    Core.getRelationshipLinks g.idr field g.doc
        |> map linkTranslator


{-| Get `meta` member from a json api `relationship object`.
-}
meta : String -> Guide.Guide g a -> Definition.GeneralPairList a
meta field g =
    Core.getRelationshipMeta g.idr field g.doc


{-| -}
getIdr : String -> Guide g a -> Maybe Identifier
getIdr field g =
    Core.reach g.idr [ field ] g.doc


{-| -}
getIdrs : String -> Guide g a -> List Identifier
getIdrs field g =
    Core.reachMany g.idr [ field ] g.doc


{-| Follow given relationships. All relationships named in the list must be
one-to-one.
-}
reach : List String -> Guide g a -> Maybe Identifier
reach fields g =
    Core.reach g.idr fields g.doc


{-| Follow given relationships. Either penultimate relationship or the last
relationship must be one-to-many, but not both at the same time. The first `n-2`
relationships named in the list must be one-to-one.
-}
reachMany : List String -> Guide g a -> List Identifier
reachMany fields g =
    Core.reachMany g.idr fields g.doc


{-| Follow given relationships and return element, representative of the last
relationship, and that passes given test. Either penultimate relationship or
the last relationship may be one-to-many, or both can be one-to-one, but not
one-to-many. The first `n-2` relationships named in the list must be one-to-one.
-}
find : (Object (Boxed c) -> Bool) -> List String -> JsonApi.Guide g c -> Maybe Identifier
find objectTest fields g =
    let
        test idr =
            Core.getObject idr g.doc
                |> Maybe.map objectTest
                |> withDefault False
    in
    case
        reachMany fields g
            |> Utility.find test
    of
        Nothing ->
            reach fields g
                |> Maybe.map singleton
                |> andThen (Utility.find test)

        something ->
            something


{-| Swap the resources `data` pertaining to these relationships. It will keep
any other configuration the same.
-}
swapData : String -> String -> Guide g c -> Guide g c
swapData one two g =
    Core.swapData g.idr one two g.doc
        |> Guide.setDoc g
