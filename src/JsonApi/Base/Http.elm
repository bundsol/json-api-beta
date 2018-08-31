module JsonApi.Base.Http exposing (Method(..), request)



import Dict exposing (Dict)
import Http exposing (Body, Expect, Header, emptyBody, header)
import Json.Decode exposing (Decoder)
import Json.Encode as Encode
import JsonApi.Base.Core as Core
    exposing
        ( Identifier
        , TopLevel
        , docDecoder
        , docEncoder
        , updateMethod
        )
import JsonApi.Base.Definition as Definition
    exposing
        ( DocTypeTaggers
        , Encoder
        )
import JsonApi.Base.Utility exposing (find)
import List exposing (filter, filterMap, foldl, isEmpty, map, partition, unzip)
import Maybe exposing (andThen, withDefault)
import Set exposing (Set)


type alias RequestDef t a =
    { method : String
    , headers : List Header
    , url : String
    , body : Body
    , expect : Expect ( t, TopLevel a )
    , timeout : Maybe Float
    , withCredentials : Bool
    }





type Method a
    = Get
    | Delete
    | Post (TopLevel a)
    | Put (TopLevel a)
    | Patch (TopLevel a)


request :
    Encoder a
    -> Decoder a
    -> DocTypeTaggers t Identifier
    -> Method a
    -> List Header
    -> String
    -> Http.Request ( t, TopLevel a )
request encoder decoder taggers method headers url =
    let
        ( strMethod, value ) =
            case method of
                Post doc ->
                    ( "POST", docEncoder encoder doc )

                Get ->
                    ( "GET", Encode.string "" )

                Put doc ->
                    ( "PUT", docEncoder encoder (updateMethod doc (Just "PUT")) )

                Patch doc ->
                    ( "PUT", docEncoder encoder (updateMethod doc (Just "PATCH")) )

                Delete ->
                    ( "POST", Encode.object [ ( "_method", Encode.string "DELETE" ) ] )

        
        stringValue =
            Encode.encode 0 value

        body =
            Http.stringBody "application/vnd.api+json" stringValue

        requestDef =
            RequestDef
                strMethod
                ([ header "Accept" "application/vnd.api+json" ] ++ headers)
                url
                body
                (Http.expectJson (docDecoder decoder taggers))
                Maybe.Nothing
                False
    in
    Http.request requestDef
