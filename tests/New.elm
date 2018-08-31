module New exposing (..)

import Expect exposing (Expectation)

import Test exposing (..)
import JsonApi.Test.NewData exposing
  ( newPurchaseGuide
  , newObject
  )
import JsonApi exposing
 ( isNew, idOf
 , Primary(..)
 , DocType(..)
 , emptyIdr
 , docDecoder
 )


import JsonApi.Base.Core exposing
  (docEncoder)
  
import JsonApi.TopLevel exposing 
  ( emptyDocument
  )
  
import JsonApi.Relationship exposing
  ( getIdr
  )
import JsonApi.Getter exposing
  ( getString
  , getInt
  , getProperty
  , getLocalString
  , getLocalInt
  , getLocal
  , getSubfield
  , isTrue
  , isLocalTrue
  , reachObject
  , getObject
  , reachIdOrEmpty
  , reachDefaultString
  , localDefaultString
  , defaultInt
  )
import JsonApi.Setter exposing
  ( setLocal
  , setBool
  , setLocalBool
  , setSubfield
  , setProperties
  , setProperty
  , setLocals
  , Varname(..)
  , swap
  )
import JsonApi.Base.Utility exposing
  ( tuplicate
  )
import Boxed exposing (Boxed(..))
import Boxed.Dictionary as Dictionary
import Boxed.Json
import Boxed.Lst
import Dict
import Maybe exposing (andThen)
import Tuple exposing (mapFirst,mapSecond)
import Json.Decode exposing (decodeString, decodeValue)
import Json.Encode as Encode



intoJson doc = 
  docEncoder Boxed.Json.encoder doc 
  --|> Encode.encode 0
  
  
  
decodeGuide json =
  let 
    decodedResult = 
      decodeValue docDecoder json
  in 
    case  decodedResult of 
      Ok (DataDoc (Single (Just idr)), topLevel) -> 
        { doc=topLevel, idr = idr}
      _ -> {doc=emptyDocument,idr=emptyIdr}      
      
      

suite : Test
suite =
  describe "TopLevel Decoding"
  [ test "verify you got the right new object" 
      (  \_ -> 
          reachObject ["loser-brand"] newPurchaseGuide
          |> Maybe.map .attributes
          |> andThen (Dict.get "name")
          |> Expect.equal (Just (Str "New Comer"))
      )
      
  , test "new object survive re-decoding" <|
    \_ -> 
      intoJson newPurchaseGuide.doc
      |> decodeGuide 
      |> reachDefaultString "WRONG GGG"  ["loser-brand"] "name"
      |> Expect.equal "New Comer"
     
    
  ]
  
