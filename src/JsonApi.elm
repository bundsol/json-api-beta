module JsonApi exposing
    ( Identifier, TopLevel, Guide
    , Object, Method(..), DocType(..), Primary(..)
    , Link(..), Href
    , Error
    , emptyIdr, trail, track
    , getId, idOf, isNew, typeOf, getType, equal
    , basicGuide, rebuildId
    , updateIdr, updateDoc, setIdr, setDoc
    , docDecoder
    , request
    )

{-| This main module includes functions that interact directly with the
`Identifier` and the `TopLevel` types. Shared also are other types and functions
needed to initialize a `TopLevel` document. All of the Elm primitives (`Int`,
`Bool`, etc) will be stored and retrieved by means of the `Boxed` type and
library.

Many words and concepts from the Json Api v1.0 specification will have a direct
counterpart in the types and operations provided by this library. In that light,
concepts like `primary data`, `resource linkage`, etc., will be freely used in
the documentation provided for this set of mudules.

Any use of the term `the specification`, will be a reference to the Json Api
v1.0 standard.


# Main types

@docs Identifier, TopLevel, Guide


# Helper types

@docs Object, Method, DocType, Primary


# Link

@docs Link, Href


# Error

@docs Error


# Identifer constructor

@docs emptyIdr, trail, track


# Identifier query functions

@docs getId, idOf, isNew, typeOf, getType, equal


# Guide constructor

@docs basicGuide, rebuildId


# Guide transformation

@docs updateIdr, updateDoc, setIdr, setDoc


# Decoder

@docs docDecoder


# Http

@docs request

-}

import Boxed exposing (..)
import Boxed.Json
import Dict exposing (Dict)
import Http exposing (Header)
import Json.Decode as Decode exposing (Decoder, decodeValue, oneOf)
import Json.Encode as Encode exposing (Value)
import JsonApi.Base.Core as Core
import JsonApi.Base.Definition as Definition
    exposing
        ( DocTypeTaggers
        )
import JsonApi.Base.Guide as Guide exposing (Guide)
import JsonApi.Base.Http as JAHttp
import List exposing (map)


{-| This type plays the role of the `resource linkage` concept in the Json Api
specification. It identifies every `resource` held by the `json api document`.
And, as so, it has a `type` and `id` as part of its components, and they are
of type `String`. Unlike the `resource linkage` in the Json Api specification,
an `Identifier` will also have the ability to point to a `new resource`. And
new resources will be allowed to be part of the `compound document`.
-}
type alias Identifier =
    Core.Identifier


{-| In the Json Api specification, the `top level` object effectively contains
all of the document's information, so a direct port of the concept was made, in
the shape of the `TopLevel` type.
-}
type alias TopLevel c =
    Core.TopLevel (Boxed c)


{-| This library operates around the concept of an Elm record that must have
a member named `doc` of type `TopLevel`, and a member named `idr` of type
`Identifier`. Even though the record may have other members, this library
recommends storing values as part of the json api document. Measures have been
provided to keep some of the values from being sent to the backend if they only
belong ot the front end. Regardless of how this record is named in an actual
application, we will refer to said record as the `Guide`. The `c` in the
definition of Guide stands for a custom type that the user might also want to
store in a `Boxed`.
-}
type alias Guide g c =
    Guide.Guide g (Boxed c)


{-| -}
type alias Href c =
    Definition.Href (Boxed c)


{-| -}
type Link c
    = UrlLink String
    | HrefLink (Href c)


type alias LinkList c =
    List ( String, Link c )


{-| -}
type alias Error c =
    Definition.GeneralError (Boxed c)
        { links : List ( String, Link c )
        }


{-| You pass a value of this type to build an http request. `POST`, `PUT` and
`PATCH` must include the document heading the backend's way. GET and DELETE
don't need any payload.
-}
type Method c
    = GET
    | DELETE
    | POST (TopLevel c)
    | PUT (TopLevel c)
    | PATCH (TopLevel c)


{-| This type is used to describe and share the contents of the `primary data`.
Primary data will only be defined in terms of `resource linkage`, as allowed by
the specification. The rest of any primary resource information (e.g. its
`attributes`) will be stored as `included` data.
-}
type Primary
    = Single (Maybe Identifier)
    | Multiple (List Identifier)


{-| A value of this type is returned by the decoder provided in this library.
A DataDoc instance will share the contents of the primary data. This will be
the only time the user will have access to said contents. This library does not
provide a function to query the contents of the primary data.
-}
type DocType
    = DataDoc Primary
    | ErrorDoc
    | MetaDoc


{-| This record represents a resource, sharing only type, id, the attributes,
links, and the meta member. It also has its associated `Identifer`, named `idr`.
-}
type alias Object c =
    Core.Object (Boxed c)


docTypeTaggers : DocTypeTaggers DocType Core.Identifier
docTypeTaggers =
    { data =
        { single = DataDoc << Single
        , multiple = DataDoc << Multiple
        }
    , errors = ErrorDoc
    , meta = MetaDoc
    }


decoder =
    Boxed.Json.decoder


encoder =
    Boxed.Json.encoder


{-| Decode json data as a `TopLevel`. Some functionality in this library is not
contemplated in the Json Api specification. Although the specification allows
the absence of the `id` member only if the resource originates at the client
side, we will accept a single top level resource that is new from back end if
`id` is missing, empty, null or has the value "new". In addition to that, the
top level meta member must have the entry `{is-new: true}`.
If 'meta' says that is new but the other conditions for 'id' are not met, then
it will proceed to decode the top level single resource as a regular persisted
one.
-}
docDecoder : Decoder ( DocType, TopLevel c )
docDecoder =
    Core.docDecoder decoder docTypeTaggers


{-| An `Identifier` constructor. To create an `Identifier`, given some `type`
and `id`, we need to verify that we had access to such data in the first place.
This is to minimize access to other resources that are out of the scope of the
current application component. That is why the first argument to this
constructor is a path to related resources, similiar to the one given to
`reachMany`. (See documentation for the `reachMany` function). The `Identifier`
created with this constructor gets verified by any of the functions 'track',
`relateSingle`, `relateOneMore`, or `unrelate`.
-}
trail : List String -> String -> String -> Identifier
trail =
    Core.trail


{-| Constructor for an `Identifier` that doesn't point to any resource, and it
does not have a `type` either.
-}
emptyIdr : Identifier
emptyIdr =
    Core.emptyId


{-| Verify validity of an `Identifier`. It only tests identifiers created by
the `trail` function. Any other identifiers get returned just as is.
-}
track : Identifier -> Identifier -> TopLevel a -> Maybe Identifier
track =
    Core.track


{-| Return json api `type` of `Identifier`.
-}
typeOf : Identifier -> String
typeOf =
    Core.typeOf


{-| Possibly return json api `type` of `Identifier`.
-}
getType : Identifier -> Maybe String
getType =
    Core.getType


{-| Return json api `id` of `Identifier`.
-}
idOf : Identifier -> String
idOf =
    Core.idOf


{-| Possibly return json api `id` of `Identifier`.
-}
getId : Identifier -> Maybe String
getId =
    Core.getId


{-| Test if `Identifier` points to a resource created by the current
application, that it has not been saved to the back end yet.
-}
isNew : Identifier -> Bool
isNew =
    Core.isNew


{-| It will verify validity of an `String` id value (See documentation for
`trail`). If it succeeds it will return a `Guide` with its `idr` member set to
the newly created and verified `Identifier`, otherwise it will simply return
the `Guide` unchanged.
-}
rebuildId : List String -> String -> String -> Guide g a -> Guide g a
rebuildId =
    Guide.rebuildId


{-| Test equality between two `Identifier` values.
-}
equal : Identifier -> Identifier -> Bool
equal =
    Core.equal


{-| Guide constructor with just the two minimal required members.
-}
basicGuide : TopLevel c -> Identifier -> { doc : TopLevel c, idr : Identifier }
basicGuide doc idr =
    { doc = doc, idr = idr }


{-| Build an http request with the json api headers already set.
### Example 
    request (POST someGuide.doc) [] "/orders" 
    |> Http.send SomeMsg
-}
request : Method c -> List Header -> String -> Http.Request ( DocType, TopLevel c )
request method headers url =
    case method of
        POST doc ->
            JAHttp.request encoder decoder docTypeTaggers (JAHttp.Post doc) headers url

        GET ->
            JAHttp.request encoder decoder docTypeTaggers JAHttp.Get headers url

        PUT doc ->
            JAHttp.request encoder decoder docTypeTaggers (JAHttp.Put doc) headers url

        PATCH doc ->
            JAHttp.request encoder decoder docTypeTaggers (JAHttp.Patch doc) headers url

        DELETE ->
            JAHttp.request encoder decoder docTypeTaggers JAHttp.Delete headers url


{-| Update value of doc member
-}
updateDoc : TopLevel a -> Guide g a -> Guide g a
updateDoc =
    Guide.updateDoc


{-| Update value of idr member
-}
updateIdr : Identifier -> Guide g a -> Guide g a
updateIdr =
    Guide.updateIdr


{-| Set idr member
-}
setIdr : Guide g a -> Identifier -> Guide g a
setIdr =
    Guide.setIdr


{-| Set doc member
-}
setDoc : Guide g a -> TopLevel a -> Guide g a
setDoc =
    Guide.setDoc
