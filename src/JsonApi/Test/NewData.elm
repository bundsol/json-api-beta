module JsonApi.Test.NewData exposing (newObject, newPurchaseGuide)

import Json.Decode as D exposing (decodeValue)
import Json.Encode as E exposing (encode)
import JsonApi
    exposing
        ( DocType(..)
        , Primary(..)
        , TopLevel
        , docDecoder
        , emptyIdr
        )
import JsonApi.Base.Utility exposing (tuplicate)
import JsonApi.Test.Data.Bogus exposing (..)
import JsonApi.Test.Data.Brand exposing (..)
import JsonApi.Test.Data.Choice exposing (..)
import JsonApi.Test.Data.Establishment exposing (..)
import JsonApi.Test.Data.Phony exposing (fetchedJson)
import JsonApi.Test.Data.Product exposing (..)
import JsonApi.Test.Data.Purchase exposing (..)
import JsonApi.Test.Data.Stock exposing (..)
import JsonApi.Test.Data.Want exposing (..)
import JsonApi.TopLevel exposing (emptyDocument)
import Maybe exposing (andThen, withDefault)
import Tuple exposing (first, mapSecond, second)


newObject =
    E.object
        [ ( "data"
          , E.object new_purchase
          )
        , ( "included"
          , [ E.object new_purchase_option_pack
            , E.object favoriteStore
            , E.object twinTown
            , E.object bogusOne
            , E.object bogusTwo
            , E.object newComer
            ]
            |> E.list 
          )
        , ( "meta"
          , E.object
                [ ( "is-new", E.bool True )
                ]
          )
        ]


wrongNew =
    E.object
        [ ( "data"
          , E.object new_purchase
          )
        ]


wrongObject =
    E.object
        [ ( "data"
          , E.object wrong_purchase
          )
        ]


decodedNewObject =
    decodeValue docDecoder newObject


newPurchaseGuide =
    case decodedNewObject of
        Ok ( DataDoc (Single (Just idr)), topLevel ) ->
            { doc = topLevel, idr = idr }

        _ ->
            { doc = emptyDocument, idr = emptyIdr }
