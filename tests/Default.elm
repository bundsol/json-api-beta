module Default exposing (suite)


import Expect 
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import JsonApi.Base.Guide exposing(Guide)
import JsonApi.Getter as Getter exposing
  ( getProperty, getLocal
  , defaultString, defaultInt, defaultFloat, defaultBool
  , localDefaultString, localDefaultInt, localDefaultFloat, localDefaultBool
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



type alias DefaultRetrieval g a c = 
  { primitive: String 
  , defaultValue: a
  , value : a 
  , setter : String -> a -> Guide g c -> Guide g c
  , getter : a -> String -> Guide g c -> a
  }


type alias BoxedGetter g c = 
  { functionName: String
  , function : (String -> Guide g c -> Maybe c)
  }



suite :  Test
suite =
  describe "Fuzz the field name for inserts and default retrievals of different types"
    (List.concat [primaryPublic, relatedLocal])
  
  
primaryPublic =
  let 
    bg = BoxedGetter "getProperty" getProperty
  in 
    map ((|>) userGuide)
    [ expectRetrieved bg
      ( DefaultRetrieval 
         "Int" 
         5005
         777
         setInt
         defaultInt
      )
    , expectRetrieved bg
      ( DefaultRetrieval 
         "Float" 
         6.006
         5.7777
         setFloat
         defaultFloat
      )
    , expectRetrieved bg
      ( DefaultRetrieval 
         "String" 
         "Fallback value"
         "Charvar example"
         setString
         defaultString
      )
    , expectRetrieved bg
      ( DefaultRetrieval 
         "Bool" 
         False
         True
         setBool
         defaultBool
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
      ( DefaultRetrieval 
         "String" 
         "Fallback value"
         "Charvar example"
         setLocalString
         localDefaultString
      )
    , expectRetrieved bg
      ( DefaultRetrieval 
         "Int" 
         5005
         777
         setLocalInt
         localDefaultInt
      )
    , expectRetrieved bg
      ( DefaultRetrieval 
         "Float" 
         6.006
         5.7777
         setLocalFloat
         localDefaultFloat
      )
    , expectRetrieved bg
      ( DefaultRetrieval 
         "Bool" 
         False
         True
         setLocalBool
         localDefaultBool
      )  
    ]





    
expectRetrieved : BoxedGetter g c ->  DefaultRetrieval g a c -> Guide g c -> Test
expectRetrieved bg r g  =
  let 
    fuzzName = 
      "Regarding field type " ++ r.primitive ++ " and function " ++ bg.functionName
  in 
    fuzzWith {runs=2} Fuzz.string fuzzName <|
      \postFix-> 
        let 
          fieldName = "test-only-" ++ postFix
          nonExisting = bg.function fieldName g  
          fallback = r.getter r.defaultValue fieldName g
          retrieved = 
            r.setter fieldName r.value g 
            |> r.getter r.defaultValue fieldName
          
       in
         Expect.equal (Nothing, r.value, r.defaultValue) (nonExisting, retrieved, fallback)
       
       

              