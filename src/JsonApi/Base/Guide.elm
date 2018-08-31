module JsonApi.Base.Guide exposing
    ( Guide
    , correlate
    , rebuildId
    , setDoc
    , setIdr
    , updateDoc
    , updateIdr
    )

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, decodeString)
import Json.Encode as Encode exposing (Value)
import JsonApi.Base.Core as Core
    exposing
        ( Identifier
        , TopLevel
        , modifyAttributes
        , modifyLocal
        , relate
        , track
        , trail
        )
import JsonApi.Base.Decode as Decode exposing (generalDictionaryDecoder)
import JsonApi.Base.Definition as Definition exposing (..)
import List exposing (map)
import Maybe exposing (andThen, withDefault)
import String exposing (trim)
import Tuple exposing (mapFirst, mapSecond)


type alias Guide g a =
    { g | idr : Identifier, doc : TopLevel a }


updateDoc : TopLevel a -> Guide g a -> Guide g a
updateDoc doc g =
    { g | doc = doc }


setDoc : Guide g a -> TopLevel a -> Guide g a
setDoc g doc =
    { g | doc = doc }


updateIdr : Identifier -> Guide g a -> Guide g a
updateIdr idr g =
    { g | idr = idr }


setIdr : Guide g a -> Identifier -> Guide g a
setIdr g idr =
    { g | idr = idr }


rebuildId : List String -> String -> String -> Guide g a -> Guide g a
rebuildId fields type_ id g =
    track g.idr (trail fields type_ id) g.doc
        |> Maybe.map (setIdr g)
        |> withDefault g


correlate : Guide g a -> Guide g a -> Guide g a
correlate target source =
    { target | idr = source.idr, doc = source.doc }
