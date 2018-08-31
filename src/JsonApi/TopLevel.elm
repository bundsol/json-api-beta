module JsonApi.TopLevel exposing
    ( emptyDocument
    , setMeta
    , getMeta, links, errors, allMeta
    )

{-|


# Functions that deal mostly with the top level object.


## Constructor

@docs emptyDocument


## Setter

@docs setMeta


## Getter

@docs getMeta, links, errors, allMeta

-}

import Boxed exposing (Boxed(..))
import JsonApi
    exposing
        ( Error
        , TopLevel
        )
import JsonApi.Base.Core as Core
import JsonApi.Base.Definition as Definition
import JsonApi.Base.Link exposing (linkTranslator)
import List exposing (map)


{-| Set a member of the top level `meta` json array.
-}
setMeta : String -> Boxed c -> TopLevel c -> TopLevel c
setMeta key value doc =
    Core.setDocMeta key value doc


{-| Get a member of the top level `meta` json array.
-}
getMeta : String -> TopLevel c -> Maybe (Boxed c)
getMeta key doc =
    Core.getDocMeta key doc


{-| Get the whole top level `meta` member.
-}
allMeta : TopLevel c -> Definition.GeneralPairList (Boxed c)
allMeta doc =
    Core.getAllDocMeta doc


{-| Only the top level `meta` member accepts new data in an empty document.
-}
emptyDocument : TopLevel a
emptyDocument =
    Core.emptyDocument






{-| Obtain full list of errors.
-}
errors : TopLevel c -> List (JsonApi.Error c)
errors doc =
    let
        transform e =
            { id = e.id
            , links = map linkTranslator e.links
            , status = e.status
            , code = e.code
            , title = e.title
            , detail = e.detail
            , source = e.source
            , meta = e.meta
            }
    in
    Core.getErrors doc
        |> map transform


{-| Get top level links.
-}
links : TopLevel c -> List ( String, JsonApi.Link c )
links doc =
    Core.getDocLinks doc
        |> map linkTranslator
