module JsonApi.Setter exposing
    ( Varname(..)
    , setString, setInt, setFloat, setBool
    , setProperty, setSubfield, setLocal, setProperties, setLocals
    , setLocalString, setLocalInt, setLocalFloat, setLocalBool
    , copy, move, promote, remove, swap, trade
    )

{-| Functions to set values of resources attributes


## Types

@docs Varname


## Primitives

@docs setString, setInt, setFloat, setBool


## Composite values

@docs setProperty, setSubfield, setLocal, setProperties, setLocals


## Local values


### Set values that are only used at client side.

@docs setLocalString, setLocalInt, setLocalFloat, setLocalBool


## Moving data

@docs copy, move, promote, remove, swap, trade

-}

import Boxed exposing (..)
import Boxed.Dictionary
import Dict
import JsonApi exposing (Guide)
import JsonApi.Base.Core as Core
    exposing
        ( modifyAttributes
        , modifyLocal
        )
import JsonApi.Base.Definition
    exposing
        ( GeneralPairList
        )
import JsonApi.Base.Guide as Guide
    exposing
        ( setDoc
        , updateDoc
        )
import JsonApi.Base.Utility
    exposing
        ( dropSecond
        )
import JsonApi.Getter
    exposing
        ( getLocal
        , getProperty
        )
import JsonApi.TopLevel as TopLevel





{-|-}
setString : String -> String -> Guide g c -> Guide g c
setString field value g =
  setProperty field (Str value) g 
  

{-|-}  
setInt : String -> Int -> Guide g c -> Guide g c
setInt field value g =
  setProperty field (Integer value) g 
  

{-|-}  
setFloat : String -> Float -> Guide g c -> Guide g c
setFloat field value g =
  setProperty field (Double value) g   
  

{-|-}  
setBool : String -> Bool -> Guide g c -> Guide g c
setBool field value g =
  setProperty field (Boolean value) g     
  
  
{-|-}  
setLocalString  : String -> String -> Guide g c -> Guide g c
setLocalString field value g =
  setLocal field (Str value) g   


{-|-}
setLocalBool  : String -> Bool -> Guide g c -> Guide g c
setLocalBool field value g =
  setLocal field (Boolean value) g
  
  
{-|-}  
setLocalFloat  : String -> Float -> Guide g c -> Guide g c
setLocalFloat field value g =
  setLocal field (Double value) g   
  

{-|-}  
setLocalInt  : String -> Int -> Guide g c -> Guide g c
setLocalInt field value g =
  setLocal field (Integer value) g   
  
  
{-| Set value of a member of a boxed `Dict`. It will insert new member if its
key does not exist in the dictionary.
-}
setSubfield   :  String -> String -> (Boxed c) -> Guide g c  -> Guide g c
setSubfield  property subfield value g =
  let 
    boxer =
      Boxed.Dictionary.apply (Dict.insert subfield value)
      >> Boxed.Dictionary
    modifier attributes =
      Dict.update property (Maybe.map boxer) attributes
  in 
    modifyAttributes g.idr modifier g.doc
    |> setDoc g



{-| Inserts a `Boxed` into a resource's attributes
-}
setProperty  : String -> a-> Guide.Guide g a  -> Guide.Guide g a 
setProperty  property value  g =  
  Dict.insert property value  
  |> modifyAttributes g.idr
  |> (|>) g.doc
  |>  setDoc g
  
  

{-|-}
setLocal  : String -> a-> Guide.Guide g a  -> Guide.Guide g a 
setLocal  property value  g =  
  Dict.insert property value  
  |> modifyLocal g.idr
  |> (|>) g.doc
  |>  setDoc g

  

{-|-}  
setProperties  : GeneralPairList a -> Guide.Guide g a  -> Guide.Guide g a   
setProperties properties g =
  Dict.fromList properties 
  |> Dict.union
  |> modifyAttributes g.idr 
  |> (|>) g.doc
  |> setDoc g
  
  
  
{-|-}  
setLocals  : GeneralPairList a -> Guide.Guide g a  -> Guide.Guide g a   
setLocals properties g =
  Dict.fromList properties 
  |> Dict.union
  |> modifyLocal g.idr 
  |> (|>) g.doc
  |> setDoc g  


{-| Type to tell variable name and its locality
-}  
type Varname = Local String | Outbound String 


{-| Swap the values of two variables. Both variables must exist.
-}  
swap : Varname -> Varname -> Guide.Guide g a -> Guide.Guide g a 
swap vn1 vn2 g = 
  let 
  
    interSwap local outbound = 
      case (getLocal local g, getProperty outbound g) of 
      
        (Just localValue, Just outboundValue) -> 
          setLocal local outboundValue g 
          |> setProperty outbound localValue
          
        _ -> g
        
    intraSwap field1 field2 getter setter = 
      case (getter field1 g, getter field2 g) of 
      
        (Just value1, Just value2) -> 
          setter field1 value2 g 
          |> setter field2 value1
          
        _ -> g
  in 
    case (vn1, vn2) of 
      (Local local, Outbound outbound) -> interSwap local outbound
      
      (Outbound outbound, Local local) -> interSwap local outbound
      
      (Local field1, Local field2) -> intraSwap field1 field2 getLocal setLocal
      
      (Outbound field1, Outbound field2) -> 
        intraSwap field1 field2 getProperty setProperty
      


{-| Have two variables trade places. Both variables must exist.
-}
trade : Varname -> Varname -> Guide.Guide g a -> Guide.Guide g a 
trade vn1 vn2 g = 
  let 
  
    interTrade local outbound =
      Core.trade g.idr local outbound g.doc
      |> setDoc g
      
  in 
    case (vn1, vn2) of 
      (Local local, Outbound outbound) -> interTrade local outbound
      
      (Outbound outbound, Local local) -> interTrade local outbound
      
      _ -> swap vn1 vn2 g

        
  
{-| Copy contents from variable into another. Target does not need to exist.
-}
copy : Varname -> Varname -> Guide.Guide g a -> Guide.Guide g a 
copy vn1 vn2 g = 
  let 
  
    copier source target getter setter =
      case getter source g of 
      
        Just something ->
          setter target something g 
          
        _ -> g 
  in 
    case (vn1, vn2) of 
      (Local local, Outbound outbound) -> 
        copier local outbound getLocal setProperty
        
      (Outbound outbound, Local local) -> 
        copier outbound local getProperty setLocal
        
      (Local field1, Local field2) -> 
        copier field1 field2 getLocal setLocal
        
      (Outbound field1, Outbound field2) -> 
        copier field1 field2 getProperty setProperty
        
        
        
{-| Change locality of a variable, and erase it from its original place. It will
overwrite if a variable with same name already exists at destination. 
-}
move : Varname -> Guide.Guide g a -> Guide.Guide g a 
move varname g = 
  let 
  
    mover field getter setter =
      case getter field g of 
      
        Just something ->
          remove varname g 
          |> setter field something
          
        _ -> g 
  in           
    case varname of 
      Local local -> 
        mover local getLocal setProperty
        
      Outbound outbound -> 
        mover outbound getProperty setLocal
      
  
  
  
{-| Duplicate a client side variable into the outbound namespace. It keeps its
presence at the local namespace as well. It will overwrite if an outbound
variable with same name already exists. 
-}
promote : String -> Guide.Guide g a -> Guide.Guide g a 
promote field g = 
  case getLocal field g of 
    Just something -> 
      setProperty field something g
      
    _ -> g
    
    
    
{-| Delete presence of variable. 
-}    
remove : Varname -> Guide.Guide g a -> Guide.Guide g a 
remove varname g =

  case varname of 
    Local local -> 
      modifyLocal g.idr (Dict.remove local) g.doc
      |> setDoc g 
      
    Outbound outbound ->
      modifyAttributes g.idr (Dict.remove outbound) g.doc
      |> setDoc g 
