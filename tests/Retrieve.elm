module Retrieve exposing (suite)


import Expect 
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import JsonApi.Base.Guide exposing(Guide)
import JsonApi.Getter as Getter exposing
  ( getProperty, getLocal
  , getString, getInt, getFloat, getBool
  , getLocalString, getLocalInt, getLocalFloat, getLocalBool
  )
  
import JsonApi.Setter as Setter exposing
  ( setString, setInt, setFloat, setBool
  , setLocalString, setLocalInt, setLocalFloat, setLocalBool
  )
  

import JsonApi.Relationship as Relationship
import JsonApi.Test.SampleData exposing
  ( userGuide
  )
import JsonApi.TopLevel exposing
  ( emptyDocument
  )
import JsonApi exposing 
  ( emptyIdr, setIdr
  )

import List exposing(map)
import Maybe exposing(withDefault)



type alias Retrieval g a c = 
  { primitive: String 
  , value : a 
  , setter : String -> a -> Guide g c -> Guide g c
  , getter : String -> Guide g c -> Maybe a
  }


type alias BoxedGetter g c = 
  { functionName: String
  , function : String -> Guide g c -> Maybe c
  }



suite :  Test
suite =
  describe "Fuzz the field name for inserts and retrievals of different types"
    (List.concat [primaryPublic, relatedLocal])
  
  
primaryPublic =
  let 
    bg = BoxedGetter "getProperty" getProperty
  in 
    map ((|>) userGuide)
    [ expectRetrieved bg
      ( Retrieval 
         "Int" 
         777
         Setter.setInt
         Getter.getInt
      )
    , expectRetrieved bg
      ( Retrieval 
         "Float" 
         5.7777
         setFloat
         getFloat
      )
    , expectRetrieved bg
      ( Retrieval 
         "String" 
         "Charvar example"
         setString
         getString
      )
    , expectRetrieved bg
      ( Retrieval 
         "Bool" 
         True
         setBool
         getBool
      )
    ]

relatedGuide = 
  userGuide
  |> Relationship.getIdrs "stocks"
  |> List.head
  |> Maybe.map (setIdr userGuide)
  |> withDefault {doc=emptyDocument,idr=emptyIdr}
  
  
relatedLocal = 
  let 
    bg = BoxedGetter "getLocal" getLocal
  in 
    map ((|>) relatedGuide)
    [ expectRetrieved bg
      ( Retrieval 
         "String" 
         "Charvar example"
         setLocalString
         getLocalString
      )
    , expectRetrieved bg
      ( Retrieval 
         "Int" 
         777
         setLocalInt
         getLocalInt
      )
    , expectRetrieved bg
      ( Retrieval 
         "Float" 
         5.7777
         setLocalFloat
         getLocalFloat
      )
    , expectRetrieved bg
      ( Retrieval 
         "Bool" 
         True
         setLocalBool
         getLocalBool
      )  
    ]





    
expectRetrieved : BoxedGetter g c ->  Retrieval g a c -> Guide g c -> Test
expectRetrieved bg r g  =
  let 
    fuzzName = 
      "Regarding field type " ++ r.primitive ++ " and function " ++ bg.functionName
  in 
    fuzzWith {runs=2} (Fuzz.string) fuzzName <|
      \postFix -> 
        let 
          fieldName = "test-only-" ++ postFix 
          nonExisting = bg.function fieldName g  
          retrieved = 
            r.setter fieldName r.value g 
            |> r.getter fieldName 
       in
         Expect.equal
           (nonExisting, retrieved)
           (Nothing,     Just r.value)
       
       

              