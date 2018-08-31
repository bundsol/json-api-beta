module JsonApi.Base.Accessor exposing
    ( claimersOf
    , getAttributes
    , getComplement
    , getDocMeta
    , getLinks
    , getLocal
    , getObject
    , getRelationLocality
    , getRelationshipLinks
    , getRelationshipMeta
    , getResource
    , idKeyFromData
    , idKeysFromData
    , isInData
    , isNew
    , nonLocalRelationshipsIds
    , reach
    , reachMany
    , track
    , withDataToKeyList
    )

import Dict
import JsonApi.Base.Definition as Definition
    exposing
        ( Complement
        , Data(..)
        , Document
        , GeneralDictionary
        , GeneralPairList
        , IDKey
        , Object
        , Relationship
        , Relationships
        , Resource
        , WithData
        )
import JsonApi.Base.Utility exposing (find)
import List exposing
  ( any
  , concat
  , filter
  , filterMap
  , foldl
  , map
  , singleton
  )
import Maybe exposing (andThen, withDefault)
import Set
import Tuple exposing (first, second)




getComplement : IDKey -> Document a -> Maybe (Complement a)
getComplement  idKey doc =    
  Dict.get idKey doc.included
   

getObject : IDKey -> Document a -> Maybe (Object a)
getObject idKey doc =
  Dict.get idKey doc.included 
  |> Maybe.map (toObject idKey)


getAttributes : IDKey -> Document a -> Maybe (GeneralDictionary a)
getAttributes idKey doc = 
  getComplement idKey doc
  |> Maybe.map .attributes
  
  
getLocal : IDKey -> Document a -> Maybe (GeneralDictionary a)
getLocal idKey doc = 
  getComplement idKey doc
  |> Maybe.map .local
  
  
  

getRelationships : IDKey -> Document a -> Maybe (Relationships a)
getRelationships idKey doc = 
  getComplement idKey doc
  |> Maybe.map .relationships





getRelationship :  IDKey -> String -> Document a  -> Maybe (Relationship a)
getRelationship  idKey field doc =
  getRelationships  idKey doc 
  |> andThen (Dict.get field) 
  


toObject : IDKey ->  Complement a -> Object a 
toObject ((type_, id, num) as idKey) complement =
  { attributes = complement.attributes
  , type_ = type_
  , id = id
  , meta = complement.meta
  , links = complement.links
  , idr = idKey
  }




getResource : IDKey ->  Document a -> Maybe (Resource a)
getResource idKey doc = 
  getComplement idKey doc
  |> Maybe.map (Definition.fuse idKey) 
  
  
  
idKeyFromData : WithData a -> Maybe IDKey
idKeyFromData {data} = 
  case data of 
    Id (Just idKey) -> Just idKey
    _ -> Nothing


idKeysFromData :  WithData a -> List IDKey
idKeysFromData  {data} =
   case data of 
    Ids keySet -> Set.toList keySet
    _ -> []
    


isNew : IDKey -> Bool 
isNew (_,_,newTag) = newTag > 0    



isInData : IDKey -> WithData a -> Bool
isInData id {data} =
  case data of 
  
    Ids keySet -> 
      Set.member id keySet
      
    Id (Just idKey) -> 
      idKey == id
    _ -> False



claimersOf : IDKey -> (Document a) -> List IDKey
claimersOf idKey doc =
  let 
    criterion _ =  
      .relationships
      >> Dict.values
      >> any (isInData idKey)
  in 
    doc.included
    |> Dict.filter criterion 
    |> Dict.keys



getDocMeta : String -> (Document a) -> Maybe a 
getDocMeta key  doc =
  find ((==) key << first) doc.meta
  |> Maybe.map second 




withDataToKeyList : WithData w -> List IDKey
withDataToKeyList {data} =
  case data of 
    Id (Just idKey) -> [idKey]
    Ids keySet -> Set.toList keySet
    _ -> []          




     
nonLocalRelationshipsIds :  Document a -> IDKey -> List IDKey
nonLocalRelationshipsIds doc idKey  =
  getRelationships idKey doc
    |> Maybe.map (Dict.values)
    |> Maybe.map (filter (.isLocal >> not))
    |> withDefault []
    |> map withDataToKeyList
    |> concat






reach : IDKey -> List String ->  Document  a -> Maybe IDKey
reach  idKey fields doc  =
  let 
  
    advance h key =
      getRelationship key h doc 
      |> andThen idKeyFromData
      
    repeat pair =
      case pair of 
      
        (_,Nothing)  ->  Nothing
        
        ([], _)     ->   Nothing
        
        ([last], Just key) ->   advance last key
          
        (h::t, Just key) ->    repeat (t, advance h key)
  in 
    repeat (fields, Just idKey)
    
    
    
    
    
reachMany : IDKey -> List String ->  Document  a -> List IDKey
reachMany idKey fields doc =
  let 
  
    advance h key =
      getRelationship key h doc 
      |> andThen idKeyFromData
      
    repeat pair =
    
      case pair of 
        (_,Nothing)  ->  []
        
        ([], _)     ->   []
        
        ([penul,last] , Just key) ->
        
          case getRelationship key  penul doc of 
            Nothing -> []
            
            Just rel -> 
            
              case (idKeyFromData rel, idKeysFromData rel) of 
                (Nothing, []) ->  []
                
                (Just one, _) ->  repeat ([last], Just one)
                  
                (_, many) ->      filterMap (advance last) many
                  
        ([last], Just key) -> 
          getRelationship key last doc
          |> Maybe.map idKeysFromData
          |> withDefault []
          
        (h::t, Just key) ->
          repeat (t, advance h key)
  in 
    repeat (fields, Just idKey)
    
    
    
track : IDKey -> List String -> IDKey -> Document  a -> Maybe IDKey
track idKey fields subject doc =
  let 
  
    advance h key =
      getRelationship key h doc 
      |> andThen idKeyFromData
      
    repeat pair =
      case pair of 
        (_,Nothing)  ->  Nothing
        
        ([], _)     ->   Nothing
        
        ([penul,last] , Just key) ->
        
          case getRelationship key  penul doc of 
            Nothing -> Nothing
            
            Just rel -> 
            
              case (idKeyFromData rel, idKeysFromData rel) of 
                (Nothing, []) -> Nothing
                
                (Just one, _) -> repeat ([last], Just one)
                  
                (_, many) -> 
                  filterMap (advance last) many
                  |> find ((==) subject)
                  
        ([last], Just key) -> 
        
          ( case getRelationship key last doc 
                 |> Maybe.map .data            of 
                 
            Just(Ids ids) -> Set.toList ids
            
            Just(Id (Just id)) -> singleton id
            
            _ -> []
            
          )|> find ((==) subject) 
          
        (h::t, Just key) ->
          repeat (t, advance h key)
  in 
    repeat (fields, Just idKey)    



    
    
    
    
getLinks : IDKey -> Document a -> List (String , Definition.Link a)
getLinks idKey doc = 
  getComplement idKey doc 
  |> Maybe.map .links
  |> withDefault []
  
  
  
getRelationshipLinks : IDKey -> String -> Document a -> List (String , Definition.Link a)
getRelationshipLinks idKey field doc =
  getRelationship idKey field doc 
  |> Maybe.map .links
  |> withDefault []
  
  
getRelationshipMeta : IDKey -> String -> Document a -> GeneralPairList a
getRelationshipMeta idKey field doc =
  getRelationship idKey field doc 
  |> Maybe.map .meta
  |> withDefault []  
  
  
getRelationLocality : IDKey -> String -> Document a  -> Bool
getRelationLocality idKey field doc =
  getRelationship idKey field  doc  
  |> Maybe.map .isLocal
  |> withDefault False