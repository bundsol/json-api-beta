module JsonApi.Base.Link exposing (linkTranslator)

import Boxed exposing (Boxed(..))
import JsonApi
    exposing
        ( DocType(..)
        , Href
        , Link(..)
        )
import JsonApi.Base.Definition as Definition


linkTranslator : ( String, Definition.Link (Boxed c) ) -> ( String, JsonApi.Link c )
linkTranslator link =
    case link of
        ( key, Definition.UrlLink str ) ->
            ( key, JsonApi.UrlLink str )

        ( key, Definition.HrefLink hl ) ->
            ( key, JsonApi.HrefLink hl )
