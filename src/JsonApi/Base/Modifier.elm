module JsonApi.Base.Modifier exposing
    ( clearDocMeta
    , filterDocMeta
    , insertTagIfType
    , modifyAttributes
    , modifyLocal
    , modifyRelationships
    , renameRelationship
    , setData
    , setDocMeta
    , setMethod
    , setRelationLocality
    , swapData
    , trade
    , unrelate
    )

import Dict
import JsonApi.Base.Accessor
    exposing
        ( claimersOf
        , isNew
        )
import JsonApi.Base.Definition
    exposing
        ( ComplementDictionaryModifier
        , ComplementModifier
        , Data(..)
        , Document
        , GeneralDictionaryModifier
        , IDKey
        , RelationshipModifier
        , RelationshipsModifier
        , WithData
        )
import JsonApi.Base.Utility
    exposing
        ( dropSecond
        , findSpot
        )
import List
    exposing
        ( append
        , filter
        , head
        , isEmpty
        , map
        , member
        , partition
        )
import Set
import Tuple exposing (first, mapFirst, second)





modifyIncluded : ComplementDictionaryModifier  a -> Document a  -> Document a
modifyIncluded complementDictionaryModifier  doc =
  {doc | included = complementDictionaryModifier doc.included }
   


modifyComplement : IDKey -> ComplementModifier a  -> Document a  -> Document a
modifyComplement idKey complementModifier doc  =
    Dict.update idKey (Maybe.map complementModifier) --updates entry in an object complement dictionary     
    |> modifyIncluded 
    |> (|>) doc
    



modifyRelationships : IDKey -> RelationshipsModifier a -> Document  a -> Document a
modifyRelationships idKey  relationshipsModifier doc  =
  ( \c -> {c | relationships = relationshipsModifier c.relationships} )  
  |> modifyComplement idKey
  |> (|>) doc
    



modifyRelationship : IDKey -> String -> RelationshipModifier a -> Document a  -> Document a
modifyRelationship idKey field relationshipModifier doc =  
  Dict.update field (Maybe.map relationshipModifier)  --updates entry in an relationship dictionary
  |> modifyRelationships  idKey 
  |> (|>) doc
  




modifyAttributes : IDKey ->  GeneralDictionaryModifier a -> Document a -> Document a
modifyAttributes idKey  generalDictionaryModifier  doc = 
  (\c -> {c | attributes = generalDictionaryModifier c.attributes} )  
  |> modifyComplement idKey
  |> (|>) doc


  
  
  
modifyLocal : IDKey ->  GeneralDictionaryModifier a -> Document a -> Document a
modifyLocal idKey  generalDictionaryModifier  doc = 
  (\c -> {c | local = generalDictionaryModifier c.local} )  
  |> modifyComplement idKey
  |> (|>) doc  




insertTagIfType : String -> IDKey -> a -> List Int -> List Int
insertTagIfType  type_ (t, _, tag) _ accum =
  case (t == type_, tag > 0) of 
    (True, True) -> tag::accum
    _ -> accum



remove : IDKey -> Document a -> Document a 
remove idKey  doc  =    
  if isEmpty (claimersOf idKey doc) then 
    modifyIncluded (Dict.remove idKey) doc
  else doc
    


unrelate : IDKey ->  String ->IDKey -> Document a -> Document a 
unrelate parent  field child  doc =
  let    
    relationshipModifier r =       
      case r.data of 
        Ids keySet -> 
          keySet
          |> Set.filter ( (/=) child )
          |> Ids       
          |> setData r
        Id (Just idKey) -> 
          if child == idKey then
            setData r (Id Nothing)
          else r          
        _ -> r
    newDoc = 
      modifyRelationship         
        parent         
        field         
        relationshipModifier         
        doc      
  in 
    case isNew child of
      True -> remove child newDoc
      _ -> newDoc      


setDocMeta : String -> a -> Document a -> Document a
setDocMeta key value doc =
  { doc 
  | meta = 
      filter ((/=) key << first) doc.meta
      |> (::) (key, value)
  }




setMethod : {a | method_: Maybe String}  -> Maybe String -> {a | method_: Maybe String}
setMethod record method = 
  {record | method_ = method} 
  
  
  
setData : WithData w  -> Data -> WithData w 
setData  record data =
  {record | data = data} 



filterDocMeta : (String -> a -> Bool) -> Document a -> Document a 
filterDocMeta test doc =
  let metaTest (a,b) = test a b
  in {doc | meta = filter metaTest doc.meta}
  
  
clearDocMeta : Document a -> Document a 
clearDocMeta  doc = {doc | meta = []}



renameRelationship : IDKey -> String -> String -> Document a -> Document a
renameRelationship idKey field newName doc =
  Dict.toList 
  >> partition ((==) field << first)
  >> mapFirst (map (mapFirst (always newName)))
  >> ( \(a,b) -> append a b )
  >> Dict.fromList
  |> modifyRelationships idKey 
  |> (|>) doc


swapData : IDKey -> String -> String -> Document a -> Document a
swapData idKey one two doc =
  let 
    groups rels = 
      Dict.toList rels
      |> partition ((|>) [one,two] << member << first)
    relationshipsModifier rels = 
      case groups rels of 
        ([(name1, r1), (name2, r2)], rest) ->
          let 
            swapped =
              (name1, {r1 | data=r2.data} )::rest
              |> (::) (name2, {r2 | data=r1.data} )
          in 
            case (r1.data, r2.data) of 
              (Id _, Id _) -> Dict.fromList swapped
              (Ids _, Ids _) -> Dict.fromList swapped
              _ -> rels
        _ -> rels
  in 
    modifyRelationships idKey relationshipsModifier doc  
  
  
setRelationLocality : IDKey -> String -> Bool -> Document a -> Document a
setRelationLocality idKey field isLocal doc =
  (\r -> { r | isLocal = isLocal})
  |> modifyRelationship idKey field
  |> (|>) doc  
  
  
  
trade : IDKey -> String -> String -> Document a -> Document a
trade idKey local outbound doc =
  let 
    getPair field dictionary = 
      Dict.partition (dropSecond ((==) field)) dictionary
      |> mapFirst (head << Dict.toList) 
    modifier c = 
      case (getPair local c.local, getPair outbound c.attributes) of 
        ( (Just(localKey, localValue), localRest)        ,
          (Just(outboundKey, outboundValue), outboundRest)   )           
          -> 
            { c 
            | attributes = Dict.insert localKey localValue outboundRest
            , local = Dict.insert outboundKey outboundValue localRest
            }
        _ -> c 
  in 
    modifyComplement idKey modifier doc 
      
  
  
  


