module JsonApi.Getter exposing
    ( getString, getInt, getFloat, getBool
    , getDictionary, getList, getJson, getProperty, getSubfield, getLocal, getObject, reachObject
    , defaultString, defaultInt, defaultFloat, defaultBool
    , getLocalString, getLocalInt, getLocalFloat, getLocalBool
    , localDefaultString, localDefaultInt, localDefaultFloat, localDefaultBool
    , isNew, isLocalTrue, isNull, isLocalNull, isTrue
    , reachString, reachInt, reachFloat, reachBool
    , reachDefaultString, reachDefaultInt, reachDefaultFloat, reachDefaultBool, reachIdOrEmpty
    , reachFilterMap, reachReduce
    , getLinks
    )

{-| Functions to gain access to resources and their properties


## Primitives

@docs getString, getInt, getFloat, getBool


## Composite values

@docs getDictionary, getList, getJson, getProperty, getSubfield, getLocal, getObject, reachObject


## Default values


### Return either given default value or named attribute.

@docs defaultString, defaultInt, defaultFloat, defaultBool


## Local values


### Get values that are only used at client side. These must have been set using


### any of the `setLocal-` functions.

@docs getLocalString, getLocalInt, getLocalFloat, getLocalBool


## Local default values


### Return given default value or the value of named local property.

@docs localDefaultString, localDefaultInt, localDefaultFloat, localDefaultBool


## Query

@docs isNew, isLocalTrue, isNull, isLocalNull, isTrue


## Reach property


### Follow given relationships, get required field at the last obtained


### resource. All relationships named in the list must be one-to-one.

@docs reachString, reachInt, reachFloat, reachBool


## Reach property or default


### Follow given relationships, get required field at the last obtained


### resource, or return given default value. All relationships named in the list


### must be one-to-one.

@docs reachDefaultString, reachDefaultInt, reachDefaultFloat, reachDefaultBool, reachIdOrEmpty


## Transformation

@docs reachFilterMap, reachReduce


## Links

@docs getLinks

-}

import Boxed exposing (..)
import Dict exposing (Dict)
import Json.Encode exposing (Value)
import JsonApi exposing (Guide)
import JsonApi.Base.Core as Core
    exposing
        ( Identifier
        , Object
        , getId
        , isNew
        , reach
        , reachMany
        )
import JsonApi.Base.Definition as Definition
    exposing
        ( GeneralDictionary
        )
import JsonApi.Base.Guide as Guide exposing (setIdr)
import JsonApi.Base.Link as Link exposing (linkTranslator)
import JsonApi.TopLevel as TopLevel
import List exposing (map)
import Maybe exposing (andThen, withDefault)
import Tuple exposing (mapSecond)




{-|-} 
isNew : Identifier -> Bool
isNew = Core.isNew


{-|-}
getString : String -> Guide g c -> Maybe String 
getString field g =
  getProperty field g 
  |> andThen asString 
  
{-|-}  
getInt : String -> Guide g c -> Maybe Int
getInt field g =
  getProperty field g 
  |> andThen asInt
  
{-|-}  
getFloat : String -> Guide g c -> Maybe Float
getFloat field g =
  getProperty field g 
  |> andThen asFloat


{-|-}  
getBool : String -> Guide g c -> Maybe Bool
getBool field g =
  getProperty field g 
  |> andThen asBool

{-|-}
getProperty : String -> Guide.Guide g a  -> Maybe a
getProperty field g = 
  Core.getAttributes g.idr g.doc
  |> andThen (Dict.get field)

  
{-|-}
defaultString : String -> String -> Guide g c -> String 
defaultString fallback field g =
  getString field g 
  |> withDefault fallback
  
{-|-}  
defaultInt : Int -> String -> Guide g c -> Int
defaultInt fallback field g =
  getInt field g 
  |> withDefault fallback  
  
{-|-}  
defaultFloat : Float -> String -> Guide g c -> Float
defaultFloat fallback field g =
  getFloat field g 
  |> withDefault fallback    
  
{-|-}  
defaultBool : Bool -> String -> Guide g c -> Bool
defaultBool fallback field g =
  getBool field g 
  |> withDefault fallback    
  
  
{-|-}  
localDefaultString : String -> String -> Guide g c -> String 
localDefaultString fallback field g =
  getLocalString field g 
  |> withDefault fallback
  
{-|-}  
localDefaultInt : Int -> String -> Guide g c -> Int
localDefaultInt fallback field g =
  getLocalInt field g 
  |> withDefault fallback    
  
{-|-}  
localDefaultFloat : Float -> String -> Guide g c -> Float
localDefaultFloat fallback field g =
  getLocalFloat field g 
  |> withDefault fallback      
  
  
{-|-}  
localDefaultBool : Bool -> String -> Guide g c -> Bool
localDefaultBool fallback field g =
  getLocalBool field g 
  |> withDefault fallback      
  
  
{-|-}  
getLocalString : String -> Guide g c -> Maybe String 
getLocalString field g =
  getLocal field g 
  |> andThen asString   
  
  
{-|-}  
getLocalBool : String -> Guide g c -> Maybe Bool
getLocalBool field g =
  getLocal field g 
  |> andThen asBool
  
  
{-|-}  
getLocalInt : String -> Guide g c -> Maybe Int
getLocalInt field g =
  getLocal field g 
  |> andThen asInt  
  
{-|-}  
getLocalFloat : String -> Guide g c -> Maybe Float
getLocalFloat field g =
  getLocal field g 
  |> andThen asFloat
  
  
{-|-}  
isTrue : String -> Guide g c -> Bool
isTrue field g =
  getProperty field g == Just(Boolean True)
  
  
{-|-}  
isLocalTrue : String -> Guide g c -> Bool
isLocalTrue field g =
  getLocal field g == Just(Boolean True)  
  
{-|-}  
isNull : String -> Guide g c -> Bool
isNull field g =
  getProperty field g == Just Null  
  
{-|-}  
isLocalNull : String -> Guide g c -> Bool
isLocalNull field g =
  getLocal field g == Just Null    
  
  
{-| Get an attribute as a List of `Boxed`.
-}  
getList : String -> Guide g c -> List (Boxed c)
getList field g =

  case getProperty field g of 
    Just (Lst list) -> list 
    
    _ -> []
  



{-| Get an attribute as a Dict of `Boxed`.
-}  
getDictionary : String -> Guide g c  -> Dict String (Boxed c)
getDictionary property g =

  case getProperty property g of
    Just (Dictionary dict) -> dict 
    
    _ -> Dict.empty



{-| Get an attribute as a `Value`.
-}  
getJson : String -> Guide g c  -> Maybe Value
getJson property g =

  case getProperty property g of
    Just (Json value) -> Just value
    
    _ -> Nothing




{-| Get a member of a boxed Dictionary attribute.
-}  
getSubfield : String -> String -> Guide g c  -> Maybe (Boxed c)
getSubfield property subfield g =
  getDictionary property g 
  |> Dict.get subfield


{-|-}
getObject : Guide.Guide g a -> Maybe (Core.Object a)
getObject  g  = 
  Core.getObject g.idr g.doc  

--private
reachGuide  :  List String -> Guide g a  -> Maybe (Guide g a )
reachGuide  fields  g =
  Core.reach g.idr fields g.doc
  |> Maybe.map (setIdr g)


{-|-}
reachString :  List String -> String-> Guide g a  -> Maybe String
reachString  fields property  g =
  reachGuide fields g
  |> andThen (getString property)
  
  
  
{-|-}  
reachInt :  List String -> String-> Guide g a  -> Maybe Int
reachInt fields property  g =
  reachGuide fields g
  |> andThen (getInt property)
  
  
  
{-|-}  
reachFloat :   List String -> String-> Guide g a  -> Maybe Float
reachFloat  fields property  g =
  reachGuide fields g
  |> andThen (getFloat property)
  
  
{-|-}
reachBool :   List String -> String-> Guide g a  -> Maybe Bool
reachBool  fields property  g =
  reachGuide fields g
  |> andThen (getBool property)
  


{-|-}
reachDefaultString :  String -> List String -> String-> Guide g a  -> String
reachDefaultString defaultVal fields property  g =
  reachGuide fields g
  |> andThen (getString property)
  |> withDefault defaultVal 
  
  
{-|-}  
reachDefaultInt :  Int -> List String -> String-> Guide g a  -> Int
reachDefaultInt defaultVal fields property  g =
  reachGuide fields g
  |> andThen (getInt property)
  |> withDefault defaultVal   
  
  
{-|-}  
reachDefaultFloat :  Float -> List String -> String-> Guide g a  -> Float
reachDefaultFloat defaultVal fields property  g =
  reachGuide fields g
  |> andThen (getFloat property)
  |> withDefault defaultVal     
  
{-|-}
reachDefaultBool :  Bool -> List String -> String-> Guide g a  -> Bool
reachDefaultBool defaultVal fields property  g =
  reachGuide fields g
  |> andThen (getBool property)
  |> withDefault defaultVal     



--private  
getAttributes : Guide g c  -> Maybe (GeneralDictionary (Boxed c))
getAttributes g =
  Core.getAttributes g.idr g.doc   


{-| Reach resource. All relationships named in the list must be one-to-one.
-}    
reachObject :   List String -> Guide.Guide g a  -> Maybe (Core.Object a)
reachObject fields  g =
  Core.reach g.idr fields g.doc
  |> Maybe.map (setIdr g)
  |> andThen getObject

  

  
{-|-}
reachFilterMap : (Object (Boxed c) -> Maybe a)  -> List String -> Guide g c  -> List a
reachFilterMap function fields g =
  let 
  
    ids = 
      Core.reachMany g.idr fields g.doc 
      
    criterion idr =
    
      if isNew idr then Nothing 
      
      else
        Core.getObject idr g.doc 
        |> andThen function 
  in 
    List.filterMap criterion ids
    
    
{-|-}    
reachReduce : (Object (Boxed c) -> b -> b) -> b -> List String -> Guide g c  -> b
reachReduce function accum fields g =
  let 
  
    ids = 
      Core.reachMany g.idr fields g.doc 
      
    builder idr b =
      case (isNew idr, Core.getObject idr g.doc) of 
        (True, _) -> b
        
        (_, Nothing) -> b 
        
        (_, Just o) -> function o b
  in 
    List.foldl builder accum ids    
      
    
  
  

  
{-| Reach resource and obtain the json api `id` value, or an empty string.
-}  
reachIdOrEmpty  : List String -> Guide g a  -> String
reachIdOrEmpty  fields g =
  Core.reach g.idr fields g.doc
  |> andThen getId 
  |> withDefault ""




{-| Get a property that is only used at client side.
-}
getLocal : String -> Guide.Guide g a -> Maybe a
getLocal field g = 
  Core.getLocalAttributes g.idr g.doc 
  |> andThen (Dict.get field)





{-| Get links associated with a `resource object`.
-}
getLinks : Guide g c -> List (String, JsonApi.Link c)
getLinks g = 
  Core.getObject g.idr g.doc
  |> Maybe.map .links
  |> withDefault []
  |> map  linkTranslator
  

