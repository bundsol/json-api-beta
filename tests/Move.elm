module Move exposing (suite)


import Expect 

import Test exposing (..)

import JsonApi.Base.Utility exposing
  ( tuplicate
  )

import JsonApi.Getter exposing
  ( localDefaultString
  , localDefaultInt
  , defaultString
  , defaultInt
  , getLocal
  , getProperty
  , getLocalString
  , getInt
  , getString
  )

import JsonApi.Setter exposing
  ( Varname(..)
  , setLocal
  , setProperty
  , swap
  , trade
  , copy
  , move
  )

import JsonApi.Test.SampleData exposing
  ( userGuide
  )

import Boxed exposing (Boxed(..))

import Tuple exposing 
  ( mapSecond
  , mapFirst
  )

suite : Test 
suite =
  describe "Moving values from a variable to another"
  [ interchange "swap local with outbound" 
  , tradePlaces "even the name changed locations"
  , copyValues "copy the value of a variable into another"
  , moveValue "change locality of variable"
  ]


interchange name = 
  test name <|
    \_ ->
      let 
        g =
          setProperty "hobby" (Str "Play guitar") userGuide
          |> setLocal "age" (Integer 30)
        swapped = 
          swap (Local "age") (Outbound "hobby") g
          |> tuplicate
          |> mapFirst (defaultInt 777 "hobby")
          |> mapSecond (localDefaultString "Studying" "age")
      in 
        Expect.equal swapped (30, "Play guitar")

tradePlaces name = 
  test name <|
    \_ ->
      let 
        g =
          setProperty "hobby" (Str "Play guitar") userGuide
          |> setLocal "age" (Integer 30)
        traded = 
          trade (Local "age") (Outbound "hobby") g          
        result = 
          ( getLocalString "hobby" traded
          , getInt "age" traded
          , getProperty "hobby"traded
          , getLocal "age" traded
          )
      in 
        Expect.equal result
          ( Just "Play guitar"
          , Just 30
          , Nothing
          , Nothing
          )

copyValues name =
  test name <|
    \_ ->
      let 
        g =
          setProperty "hobby" (Str "Play guitar") userGuide
          |> setLocal "age" (Integer 30)
        copied = 
          copy (Local "age") (Outbound "hobby") g          
        result = getProperty "hobby"copied
      in 
        Expect.equal result (Just (Integer 30))
        
moveValue name =
  test name <|
    \_ ->
      let 
        g =
          setProperty "hobby" (Str "Play guitar") userGuide
        moved = 
          move (Outbound "hobby") g
        result = 
         ( getString "hobby" moved
         , getLocalString "hobby" moved
         )
      in 
        Expect.equal result (Nothing, Just "Play guitar")
        


        