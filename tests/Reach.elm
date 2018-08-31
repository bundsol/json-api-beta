module Reach exposing (suite)


import Expect 
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import JsonApi.Base.Guide exposing(Guide)
import JsonApi.Getter as Getter exposing
  ( reachString, reachInt, reachFloat, reachBool
  , reachDefaultString, reachDefaultInt, reachDefaultFloat, reachDefaultBool
  )
  
import JsonApi.Relationship  exposing
  (reach, reachMany)
  
import JsonApi.Setter as Setter exposing
  ( setString, setInt, setFloat, setBool
  )
  

import JsonApi.Relationship as Relationship
import JsonApi.Test.SampleData exposing
  ( newPurchaseGuide
  )
import JsonApi.TopLevel exposing
  ( emptyDocument
  )
import JsonApi exposing 
  ( emptyIdr, setIdr
  )

import List exposing(map)
import Maybe exposing(withDefault, andThen)



type alias Retrieval g a c  = 
  { primitive: String 
  , defaultValue : a
  , value : a 
  , propertyName : String
  , defaultGetter : a -> List String -> String -> Guide g c -> a
  , getter : List String -> String -> Guide g c -> Maybe a
  }


suite :  Test
suite =
  describe "Testing reach- functions"
    [ describe "Fuzz the field name for inserts and retrievals of different types"
       testList
    , describe "Using valid guide"
        [ goodAttributes "Access to attributes"
        , seeEstablishment "I can see the establishment relationship"
        , seeOptionPack "I can see the option-pack relationship"
        , seePredecessor "I can see the option-pack -> bogus -> predecessor  relationship"
        ]
    ]


goodAttributes name = 
  test name <|
    \_ -> 
      Getter.getProperty "creation-date" newPurchaseGuide
      |> Expect.notEqual Nothing
      

seeEstablishment name = 
  test name <|
    \_ -> 
      Relationship.getIdr "establishment" newPurchaseGuide
      |> Expect.notEqual Nothing
      
      
seeOptionPack name = 
  test name <|
    \_ -> 
      Relationship.getIdr "option-pack" newPurchaseGuide
      |> Expect.notEqual Nothing      
      
      
seePredecessor name = 
  test name <|
    \_ -> 
      Relationship.getIdr "option-pack" newPurchaseGuide
      |> Maybe.map (setIdr newPurchaseGuide)
      |> andThen (Relationship.getIdr "bogus")
      |> Maybe.map (setIdr newPurchaseGuide)
      |> andThen (Relationship.getIdr "predecessor")
      |> Expect.notEqual Nothing            
      
      
      
testList =
  map ((|>) newPurchaseGuide)
  [ expectRetrieved 
    ( Retrieval 
       "Int" 
       777
       5
       "rank"
       reachDefaultInt
       reachInt
    )
  , expectRetrieved 
    ( Retrieval 
       "Float" 
       7.777
       8.889
       "quote-amount"
       reachDefaultFloat
       reachFloat
    )
  , expectRetrieved 
    ( Retrieval 
       "String" 
       "Fallback value"
       "Adorable thing"
       "name"
       reachDefaultString
       reachString
    )
  , expectRetrieved 
    ( Retrieval 
       "Bool" 
       False
       True
       "is-valid"
       reachDefaultBool
       reachBool
    )
  ]



    
expectRetrieved :   Retrieval g a c -> Guide g c -> Test
expectRetrieved  r g  =
  test ("Regarding field type " ++ r.primitive)  <|
    \_ -> 
      let 
        fields = ["option-pack", "bogus", "predecessor"]
        wrongPath = ["option-pack", "bogus", "predecesor"]
        exists = 
          reach fields g  /= Nothing
        goodRetrieval =
          r.defaultGetter r.defaultValue fields r.propertyName g
        defaultRetrieval =
          r.defaultGetter r.defaultValue wrongPath r.propertyName g
        retrieved = 
          r.getter fields r.propertyName g
     in
       Expect.equal 
         (r.value,        r.defaultValue,    Just r.value,  True) 
         (goodRetrieval,  defaultRetrieval,  retrieved,     exists)
       
       
