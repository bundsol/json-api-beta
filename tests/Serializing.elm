module Serializing exposing (suite)



import Expect 
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import JsonApi.Base.Guide exposing(Guide)

import JsonApi.Base.Core exposing
  (docEncoder)

import JsonApi.Relationship exposing
  ( incorporate
  , getIdrs, getIdr
  , createOneMore
  , createSingle
  , setLocality
  )

import JsonApi.Getter exposing
  ( reachFilterMap
  )

import JsonApi.Test.SampleData exposing
  ( fetched
  , sampleObject
  )  
  
import JsonApi.TopLevel exposing (emptyDocument)
import JsonApi exposing 
  ( emptyIdr
  , Primary(..)
  , DocType(..)
  , docDecoder
  )  
  
import Json.Encode as Encode
import Json.Decode exposing (decodeValue, decodeString)

  
import Boxed.Json   

import Boxed exposing (asString)
import String exposing (contains, join)
import Maybe exposing (andThen)
import Dict

import List exposing (map, intersperse, sort)

suite : Test 
suite = 
  describe "Serialize and deserialize over and over"
    [ serialize "Feedback"
    , setAsLocal "Do not encode local relationships"
    ]
  
  
decodeGuide json =
  let 
    decodedObject = 
      decodeString docDecoder json
  in 
    case decodedObject of 
      Ok (DataDoc (Single (Just idr)), topLevel) -> 
        { doc=topLevel, idr = idr}
      _ -> {doc=emptyDocument,idr=emptyIdr}      


catenate g = 
  let 
    transform item = 
      Dict.get "name" item.attributes
      |> andThen asString
  in
    reachFilterMap transform ["phonies"] g
    |> sort 
    |> join " - "
  
  
serialize name =
  test name <|
    \_ -> 
      let
        originalGuide = 
          Encode.encode 0 sampleObject
          |> decodeGuide 
        intoJson doc = 
          docEncoder Boxed.Json.encoder doc 
          |> Encode.encode 0
        didItHave = 
          intoJson originalGuide.doc 
          |> contains "\"phonies\":{\"data\":[{\"type\":\"phony\"" 
        grownGuide = 
          incorporate "phonies"  fetched originalGuide
        grownString =   
          intoJson grownGuide.doc
        nowHasPhonies =
          contains "\"phonies\":{\"data\":[{\"type\":\"phony\"" grownString
        iterated = 
          decodeGuide grownString
          |> intoJson << .doc
          |> decodeGuide 
          |> intoJson << .doc 
          |> decodeGuide
        catenated = catenate iterated
      in
        Expect.equal 
          (didItHave, nowHasPhonies, catenated)
          (False,     True,         "Awful - Much better - Worse")
  

setAsLocal name =
  test name <|
    \_ -> 
      let
        grownGuide = 
          Encode.encode 0 sampleObject
          |> decodeGuide 
          |> incorporate "phonies"  fetched 
        intoJson doc = 
          docEncoder Boxed.Json.encoder doc 
          |> Encode.encode 0  
        outbound =
          intoJson grownGuide.doc
          |> decodeGuide
          |> catenate
        localGuide =
          setLocality "phonies" True grownGuide
        local =
          intoJson localGuide.doc
          |> decodeGuide
          |> catenate
        restored  =
          setLocality "phonies" False localGuide
          |> intoJson << .doc 
          |> decodeGuide
          |> catenate
      in
        Expect.equal 
          (outbound, local, restored)
          ( "Awful - Much better - Worse"
          , ""
          , "Awful - Much better - Worse"
          )


  