module Misc exposing (..)

import Expect exposing (Expectation)

import Test exposing (..)
import JsonApi.Test.SampleData exposing (..)
import JsonApi exposing
 ( isNew, idOf
 , Primary(..)
 , DocType(..)
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




suite : Test
suite =
  describe "TopLevel Decoding"
  [ test "query if boxed has the boolean value True" <|
    \_ -> 
      tuplicate userGuide
      |> mapFirst (getProperty "is-chosen") 
      |> mapSecond (setBool "is-chosen" True)
      |> mapSecond (isTrue "is-chosen")
      |> Expect.equal (Nothing, True)
      
  , test "query if client side boxed has the boolean value True" <|
    \_ -> 
      tuplicate userGuide
      |> mapFirst (getLocal "is-chosen") 
      |> mapSecond (setLocalBool "is-chosen" True)
      |> mapSecond (isLocalTrue "is-chosen")
      |> Expect.equal (Nothing, True)
      
  , test "verify you got the right new object" <|
        \_ -> 
          reachObject ["establishment"] newPurchaseGuide
          |> Maybe.map .attributes
          |> andThen (Dict.get "name")
          |> Expect.equal (Just (Str "Dollar Bargain"))
      
      
      
  , test "get entire object" <|
    \_ -> 
      getObject userGuide
      |> Maybe.map .attributes
      |> andThen (Dict.get "first-name")
      |> Expect.equal (Just (Str "Sandy"))      
      
  , test "get an actual string as a jsonapi 'id' member"  <|
    \_ -> 
      tuplicate newPurchaseGuide
      |> mapFirst (reachIdOrEmpty ["establishment"])
      |> mapSecond (reachIdOrEmpty ["inexisting"])
      |> Expect.equal ("3", "")

  , test "manipulate subfields" <|
    \_ ->
      tuplicate userGuide
      |> mapFirst (getSubfield "preferences" "font-color") 
      |> mapSecond (setSubfield "preferences" "font-color" (Str "orange"))
      |> mapSecond (getSubfield "preferences" "font-color") 
      |> Expect.equal (Just (Str "blue"), Just (Str "orange"))
      
  , test "set multiple properties at a time"  <|
    \_ -> 
      let 
        getProps g =
          tuplicate g 
          |> mapFirst (getInt "qualification")
          |> mapSecond (getString "status")
        firstWeGot =
          getProps userGuide
        modified = 
          setProperties 
            [ ("qualification", Integer 8)
            , ("status", Str "available")
            ]
            userGuide
        thenWeGot =
          getProps modified
      in 
        Expect.equal 
          (firstWeGot,           thenWeGot)
          ((Nothing, Nothing ),  (Just 8, Just "available"))
          
          
   , test "set multiple client side values at a time"  <|
    \_ -> 
      let 
        getProps g =
          tuplicate g 
          |> mapFirst (getLocalInt "qualification")
          |> mapSecond (getLocalString "status")
        firstWeGot =
          getProps userGuide
        modified = 
          setLocals 
            [ ("qualification", Integer 8)
            , ("status", Str "available")
            ]
            userGuide
        thenWeGot =
          getProps modified
      in 
        Expect.equal 
          (firstWeGot,           thenWeGot)
          ((Nothing, Nothing ),  (Just 8, Just "available"))          
        
  
  , test "decodes valid json api document with list of objects as primary data" <|
    \_ -> 
      case decodedList of 
        Ok (DataDoc (Multiple  idrs), _) -> 
          Expect.greaterThan 0 (List.length idrs) 
        _ -> Expect.fail "TopLevel with list did not conform to spec"

  , test "decodes valid json api document with a single object as primary data" <|
    \_ -> 
      case decodedObject of 
        Ok (DataDoc (Single (Just idr)), _) -> 
          Expect.equal (idOf idr) "2"
        _ -> Expect.fail "TopLevel with single object did not conform to spec"
        

  , test "decodes valid json api document with new object as primary data" <|
    \_ -> 
      case decodedNewObject of 
        Ok (DataDoc (Single (Just idr)), _) -> 
          Expect.true "TopLevel should have had a non persisted resource" (isNew idr) 
        _ -> 
          Expect.fail "TopLevel with 'new' id did not conform to spec"

  , test "should not accept document with empty id if 'meta' doesn't say it is new" <|
    \_ -> 
      decodedWrongObject
      |> Result.map Tuple.first
      |> Expect.err
      
  , test "should be able to store and retrieve local properties" <|
    \_ ->
      setLocal "sample-prop"  (Str "test value") newPurchaseGuide 
      |> getLocal "sample-prop"
      |> Expect.equal (Just  (Str "test value") )
      
  , test "retrieves dictionary as attribute" <|
    \_ ->
      getProperty "options" userGuide 
      |> Maybe.map Boxed.Json.expand
      |> andThen (Dictionary.apply (Dict.get "display"))
      |> andThen (Dictionary.apply (Dict.get "allowed"))
      |> andThen (Boxed.Lst.apply List.head)
      |> Expect.equal (Just (Integer 45))
  ]
  
