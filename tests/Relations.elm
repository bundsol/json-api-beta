module Relations exposing (suite)


import Expect 
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import JsonApi.Base.Guide exposing(Guide)


import JsonApi.Base.Utility exposing (tuplicate)
import JsonApi.Getter exposing 
  ( reachDefaultFloat
  , reachFloat
  , reachFilterMap
  , reachReduce
  , getString
  )
  
import JsonApi.Relationship as Relationship exposing
  ( relateSingle
  , relateOneMore
  , getIdr, getIdrs
  , reachMany
  , unrelate
  , isLocal
  , swapData
  , setLocality
  , find
  )

import JsonApi.Test.SampleData exposing
  ( userGuide
  , newPurchaseGuide
  )
  
import JsonApi  exposing (emptyIdr, setIdr)
  
import Maybe exposing (withDefault, andThen)
import Dict 
import List exposing (member)

import Boxed exposing (Boxed(..), asFloat)



suite : Test
suite =
  describe "Establish relationships"
  [ reachManyTest "Now has another relationship taken from among many"
  , oneMoreTest "Process many"
  , unrelateTest "With one less"
  , unrelateSingle "Gone the only one"
  , relationshipIsMany "Relate Single ineffective"
  , relationshipIsSingle "Relate One More ineffective"
  , swap "Swap two relations' data"
  ]


singlePick o =
  case Dict.get "price" o.attributes of 
    Just (Double 0.99) -> Just o.idr
    _ -> Nothing

favStockKey = "fav-stock"

g = userGuide 

reachManyTest name =
  test name <|
    \_ -> 
      let
        firstWeGot = 
          getIdr favStockKey g 
        listSize = 
          reachMany ["stocks"] g
          |> List.length
        relatedIdr = 
          reachFilterMap singlePick ["stocks"] g  
          |> List.head
          |> withDefault JsonApi.emptyIdr
        price = 
          relateSingle favStockKey relatedIdr g
          |> reachDefaultFloat 2.55 [favStockKey] "price" 
      in 
        Expect.equal 
          (firstWeGot, price,  listSize) 
          (Nothing, 0.99,  5)   



goodStocksKey = "good-stocks"



isMember p = 
  member p [Just (Double 1.20), Just (Double 1.30)]

oneOfMany o =
  if Dict.get "price" o.attributes |> isMember then 
      Just o.idr 
  else Nothing

sumBuilder o accum =
  case Dict.get "price" o.attributes of 
    Just (Double price) -> accum + price 
    _ -> accum
    
          
oneMoreTest name =
  test name <|
    \_ -> 
      let
        firstWeGot = 
          Relationship.getIdrs goodStocksKey g 
        listSize = 
          reachMany ["stocks"] g
          |> List.length
        average = 
          case reachFilterMap oneOfMany ["stocks"] g of 
            [s1, s2] -> 
              relateOneMore goodStocksKey s1 g 
              |> relateOneMore goodStocksKey s2 
              |> reachReduce sumBuilder 0.0 [goodStocksKey]
              |> (*) 0.5 
            _ -> 0.0
      in 
        Expect.equal 
          (firstWeGot, average,  listSize) 
          ([], 1.25,  5)     
          
          
unrelateTest name =
  test name <|
    \_ -> 
      let
        (one, newGuide) = 
          case reachFilterMap oneOfMany ["stocks"] g of 
            [s1, s2] -> 
              relateOneMore goodStocksKey s1 g 
              |> relateOneMore goodStocksKey s2 
              |> (,) s1
            _ -> (emptyIdr, g)
        sum = 
          reachReduce sumBuilder 0.0 [goodStocksKey] newGuide
        simplyOne =
          unrelate goodStocksKey one newGuide 
          |> reachReduce sumBuilder 0.0 [goodStocksKey] 
          |> floor
      in 
        Expect.equal 
          (sum,  simplyOne) 
          (2.5,  1)               


unrelateSingle name =
  test name <|
    \_ -> 
      let
        firstWeGot = 
          getIdr favStockKey g 
        relatedIdr = 
          reachFilterMap singlePick ["stocks"] g  
          |> List.head
          |> withDefault JsonApi.emptyIdr
        newGuide =
          relateSingle favStockKey relatedIdr g
        price = 
          reachFloat [favStockKey] "price" newGuide
        noPrice = 
          unrelate favStockKey relatedIdr newGuide
          |> reachFloat [favStockKey] "price" 
      in 
        Expect.equal 
          (firstWeGot, price,     noPrice) 
          (Nothing,    Just 0.99, Nothing)   


relationshipIsMany name =
  test name <|
    \_ ->
      let
        firstWeGot = 
            getIdrs goodStocksKey g 
        ((one, two), newGuide) = 
          case reachFilterMap oneOfMany ["stocks"] g of 
            [s1, s2] -> 
              relateOneMore goodStocksKey s1 g 
              |> (,) (s1,s2)
            _ -> (tuplicate emptyIdr, g)
        idrsQty = 
          getIdrs goodStocksKey newGuide
          |> List.length
        sameQty =
          relateSingle goodStocksKey two newGuide 
          |> getIdrs goodStocksKey 
          |> List.length
      in 
        Expect.equal 
          (firstWeGot,  idrsQty, sameQty) 
          ([],          1,       1)          
          
          
          
          
          
relationshipIsSingle name =
  test name <|
    \_ ->
      let
        firstWeGot = 
          getIdr favStockKey g
          |> Maybe.map JsonApi.typeOf
          |> withDefault ""
        ((one, two), newGuide) = 
          case reachFilterMap oneOfMany ["stocks"] g of 
            [s1, s2] -> 
              relateSingle favStockKey s1 g 
              |> (,) (s1,s2)
            _ -> (tuplicate emptyIdr, g)
        thenWeGot = 
          getIdr favStockKey newGuide
          |> Maybe.map JsonApi.typeOf
          |> withDefault ""
        ineffective =
          relateOneMore favStockKey two newGuide 
          |> relateOneMore favStockKey one 
          |> getIdrs favStockKey 
          |> List.length
      in 
        Expect.equal 
          (firstWeGot,  thenWeGot, ineffective) 
          ("",     "stock",       0)                    
          

swap name = 
  test name <|
    \_ ->
      let
        criterion object = 
          Dict.get "name" object.attributes  ==  Just (Str "Bob's Corner")
        path = ["option-pack", "establishments"]
        g = 
          case find criterion path newPurchaseGuide of 
            Just twinTownIdr -> 
              relateSingle "potential-store" twinTownIdr newPurchaseGuide
              |> setLocality "potential-store" True
            _ -> newPurchaseGuide
        getName field guide =
          getIdr field guide
          |> Maybe.map (setIdr guide)
          |> andThen (getString "name")
          |> withDefault ""
        names = 
          ( getName "establishment" g 
          , getName "potential-store" g
          )
        localities = 
          ( isLocal "establishment" g
          , isLocal "potential-store" g 
          )
        swapped = 
          swapData "establishment" "potential-store" g 
        names2 = 
          ( getName "establishment" swapped
          , getName "potential-store" swapped
          )
        localities2 = 
          ( isLocal "establishment" swapped
          , isLocal "potential-store" swapped
          )
      in 
        Expect.equal
           ( names, localities
           , names2, localities2
           )
           ( ("Dollar Bargain","Bob's Corner"), (False,True)
           , ("Bob's Corner","Dollar Bargain"), (False,True)
           )
        
          
          
          