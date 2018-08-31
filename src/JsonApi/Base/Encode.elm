module JsonApi.Base.Encode exposing (documentEncoder)

import Dict
import Json.Encode as Encode
    exposing
        ( Value
        , bool
        , int
        , list
        , null
        , object
        , string
        )
import JsonApi.Base.Accessor
    exposing
        ( getComplement
        , isNew
        , nonLocalRelationshipsIds
        , withDataToKeyList
        )
import JsonApi.Base.Definition
    exposing
        ( Complement
        , Data(..)
        , Document
        , Encoder
        , Entry
        , GeneralDictionary
        , GeneralPairList
        , IDKey
        , Relationship
        )
import JsonApi.Base.Utility
    exposing
        ( dropSecond
        , pair
        )
import List exposing (append, concat, filter, foldl, isEmpty, map, singleton)
import Maybe exposing (andThen, withDefault)
import Set exposing (Set)
import Tuple exposing
  ( mapSecond
  , second
  
  )




putDataIds :  {a | data:  Data} -> List IDKey -> List IDKey
putDataIds  item accum =
  case  item.data of 
    Id (Just idKey) -> 
      idKey::accum 
    Ids keySet ->           
      Set.toList keySet
      |> append accum
    _-> accum
  



idsFromRelationships :    Complement a  -> List IDKey
idsFromRelationships  obj =
  obj.relationships
  |> Dict.values
  |> foldl putDataIds  [] 
    
    
    
    

primaryRelationshipIds :  Document a  ->  List IDKey
primaryRelationshipIds  doc =
  case  doc.data of 
    Id maybeId  -> 
      maybeId
      |> andThen ((|>) doc << getComplement) 
      |> Maybe.map idsFromRelationships 
      |> withDefault []
    Ids list ->
      let 
        build item accum =
          case getComplement item doc of 
            Just obj -> 
              idsFromRelationships  obj 
              |> (|>) accum << (::)
            _ -> accum
      in 
        Set.foldl build [] list
        |> List.concat
    _ -> []
  
    


encodePair : Encoder a -> (String, a) -> (String, Value)
encodePair encoder (key, val) = (key, encoder val)




generalPairListEncoder : Encoder a ->  GeneralPairList a -> Value
generalPairListEncoder encoder pairs = 
  map (encodePair encoder)  pairs 
  |> object
  


generalDictionaryEncoder : Encoder a -> GeneralDictionary a -> Value 
generalDictionaryEncoder encoder  dict =
  Dict.toList dict 
  |> (generalPairListEncoder encoder)
  


encodeRelationship : (String, Relationship a) -> (String , Value)
encodeRelationship  (fieldName, {data}) =    
  ( case data of 
    Ids keySet ->
      Set.toList keySet
      |> map (object << (makeLinkage Included))
      |> list 
    Id (Just idKey) ->          
      makeLinkage Included idKey
      |> object
    _ -> null
  ) 
  |> pair "data"
  |> singleton
  |> object
  |> pair fieldName


type DataLocation = Primary | Included


makeLinkage : DataLocation -> IDKey -> List (String , Value)
makeLinkage location ((type_, id, tag) as idKey) =  
  let
    idInfo =
      case (location, isNew idKey) of
        (Primary, True) -> 
          []
          
        (Included, True)  -> 
          [ ( "meta", object 
              [ ("new-resource-tag", string (toString tag))
              ]
            )
          ]   
          
        (_, False) -> 
          [("id", string id)
          ]
  in
    ("type", string type_)::idInfo
    
    
  





makeObject : Encoder a -> DataLocation -> Entry a  -> Value 
makeObject encoder location (idKey, obj) =
  let    
    attributes = 
      Dict.toList obj.attributes      
      |> map (\(k,v) -> (k, encoder v))
    relationships =
      Dict.toList obj.relationships
      |> filter (second >> .isLocal >> not)
      |> map encodeRelationship
  in
    append 
      (makeLinkage location idKey)    
      [ ("attributes", object attributes)
      , ("relationships", object relationships)
      ]
    |> object
    


  
documentEncoder : Encoder a ->    Document a  -> Value 
documentEncoder encoder  doc =
  let 
    repeat keyList visitedSet =
      if isEmpty keyList then 
        visitedSet
      else 
        let 
          newKeyList =
            map (nonLocalRelationshipsIds doc) keyList
            |> concat
            |> filter ( ((|>) visitedSet << Set.member)  >> not )
          newVisitedSet = 
            Set.union visitedSet (Set.fromList newKeyList)
        in 
          repeat newKeyList newVisitedSet
    primaryIds = 
      withDataToKeyList doc
    primarySet =
      Set.fromList primaryIds
    includedIdSet = 
      repeat primaryIds primarySet
      |> Set.filter ( ((|>) primarySet << Set.member)  >> not )
    primaryDataObjects = 
      Dict.filter (\ k v -> Set.member k primarySet ) doc.included
      |> Dict.toList
    includedObjects =
      Dict.filter (\ k v -> Set.member k includedIdSet ) doc.included
      |> Dict.toList
    primaryDataJson = 
      ( case (doc.data, primaryDataObjects) of 
        (Ids _,_) ->
          map (makeObject encoder Primary) primaryDataObjects
          |> list
        (Id (Just _), [dataObject]) ->          
          makeObject encoder Primary dataObject
        _ -> null
      ) |> pair "data"
  in 
    Encode.object 
    [ ("meta", object (map (mapSecond encoder) doc.meta))
    , primaryDataJson
    , ("included", list (map  (makeObject encoder Included) includedObjects)  )
    ]



