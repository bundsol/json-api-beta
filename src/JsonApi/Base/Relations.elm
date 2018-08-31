module JsonApi.Base.Relations exposing
    ( createOne
    , incorporate
    , relate
    )

import Dict
import JsonApi.Base.Accessor as Accessor
    exposing
        ( getResource
        , idKeyFromData
        , isNew
        )
import JsonApi.Base.Definition
    exposing
        ( Data(..)
        , Document
        , IDKey
        , Relationship
        , Relationships
        , Resource
        , buildKey
        , newResource
        , toEntry
        , toKey
        )
import JsonApi.Base.Modifier as Modifier
    exposing
        ( insertTagIfType
        , modifyRelationships
        , setData
        )
import JsonApi.Base.Utility exposing (findSpot,pair)
import List exposing (filter, foldl, map, singleton)
import Maybe exposing (andThen, withDefault)
import Set
import String exposing (trim)





-- -------------    Helpers      -------------------


createIncludedId : String -> Document a -> IDKey
createIncludedId type_  doc   =
  Dict.foldl (insertTagIfType type_) [] doc.included      
  |> findSpot
  |> buildKey type_ ""




getPrimaryResource :  Document a -> Maybe (Resource a)
getPrimaryResource doc =
  idKeyFromData doc 
  |> andThen ((|>) doc << getResource)   
  
  
  
getPrimaryResources : Document a -> List (Resource a)
getPrimaryResources  doc=
  case doc.data of 
    Ids keySet ->
      let 
        build idKey accum =
          getResource  idKey doc 
          |> Maybe.map ((|>) accum << (::))
          |> withDefault accum
      in 
        Set.foldl  build [] keySet
    _ -> []





-- -------------    Entity Definitions      -------------------


type Entity a = Complete (Resource a) | Identity IDKey





keyFromEnt : Entity a  -> IDKey
keyFromEnt entity =
  case entity of 
    Identity idKey -> idKey
    Complete ro -> toKey  ro






-- -------------    Relationship Definitions      -------------------

type Aggregation a = Multiple (List (Entity a)) | Single  (Maybe (Entity a) )


buildRelationship :  Aggregation a  -> Relationship a
buildRelationship aggregation =
  { links = []
  ,  meta = []
  , data = 
      case aggregation of 
        Multiple entities ->  
          entities 
          |> map keyFromEnt
          |> Ids << Set.fromList
        Single  possibleEntity ->
          possibleEntity
          |> Id <<  Maybe.map  keyFromEnt
  , isLocal = False
  }    



addToRelationship :  Aggregation a  -> Relationship a -> Relationship a
addToRelationship   aggregation r = 
  ( case (aggregation, r.data) of 
    (Multiple _, Id _ ) ->  r.data
    (Multiple newEntities, Ids locationIds) ->       
      let 
        notExists item = not ( Set.member (keyFromEnt item) locationIds )
      in 
        filter notExists newEntities
        |> map  keyFromEnt
        |> Set.fromList
        |> Set.union locationIds
        |> Ids 
    (Single  Nothing, _) -> 
      Id Nothing
    (Single  _, Ids _) -> r.data
    (Single  (Just newValue), Id _) -> 
      keyFromEnt newValue
      |> Id << Just 
    _ -> r.data     
  ) |> setData r 



insertIntoRelationships : String ->  Aggregation a  -> Relationships a -> Relationships a
insertIntoRelationships field aggregation  rs =  
  Dict.get field rs
  |> Maybe.map (addToRelationship aggregation)
  |> withDefault (buildRelationship aggregation)
  |> \r -> Dict.insert field r rs
    
 

toSureResourceList : Aggregation a -> List (Resource a )
toSureResourceList   aggregation =  
  let 
    build item accum =
      case item of 
        Complete object -> object::accum
        Identity idKey -> 
          if isNew idKey then 
            (newResource idKey)::accum
          else 
            accum
  in 
    ( case aggregation of 
      Multiple entities -> entities
      Single  possibleEntity ->
        possibleEntity
        |> Maybe.map singleton 
        |> withDefault []
    ) |> foldl build []
      



-- If the relationship is to a new id, then a new object
-- will be created automatically
addRelated : IDKey ->  String ->  Aggregation a  -> Document a ->  Document a
addRelated idKey field aggregation  doc  =
  let    
    newIncluded = 
      toSureResourceList aggregation       
      |> map toEntry
      |> Dict.fromList
      |> Dict.union  doc.included
  in
    modifyRelationships
      idKey
      (insertIntoRelationships field aggregation)
      {doc | included =  newIncluded}
    




  
incorporate  : IDKey -> String ->  Document a -> Document a -> Document a
incorporate idKey field source doc  =
  let 
    performCopy aggregation =
      addRelated idKey field aggregation doc 
  in    
    case (getPrimaryResource source, getPrimaryResources source) of
      (Just resource, _) -> 
        Just (Complete resource)
        |> Single
        |> performCopy  
      (Nothing, []) -> 
        doc
      (Nothing, resources) -> 
        map Complete  resources
        |> Multiple
        |> performCopy




relate : IDKey -> String ->  Bool -> IDKey -> Document a  -> Document a 
relate idKey field isSingle source doc =
  ( if isSingle then 
      Single (Just (Identity source))
    else 
      Multiple  [Identity source]
  ) |> addRelated idKey field 
    |> (|>) doc 
    

  
  
createOne : IDKey -> String ->   Bool -> String -> Document a  -> (IDKey, Document a)
createOne idKey field isSingle type_ doc =
  if String.isEmpty(trim type_) then (buildKey "" "" 0, doc)
  else 
    let 
      newIdKey =  createIncludedId type_ doc
    in 
      relate idKey field isSingle newIdKey doc
      |> pair newIdKey
  




