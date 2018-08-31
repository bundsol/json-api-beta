module JsonApi.Base.Core exposing
    ( Identifier
    , Object
    , TopLevel
    , clearDocMeta
    , createOne
    , docDecoder
    , docEncoder
    , emptyDocument
    , emptyId
    , equal
    , filterDocMeta
    , getAllDocMeta
    , getAttributes
    , getDocLinks
    , getDocMeta
    , getErrors
    , getId
    , getLocalAttributes
    , getObject
    , getRelationLocality
    , getRelationshipLinks
    , getRelationshipMeta
    , getType
    , idOf
    , incorporate
    , isNew
    , modifyAttributes
    , modifyLocal
    , reach
    , reachMany
    , relate
    , renameRelationship
    , setDocMeta
    , setRelationLocality
    , swapData
    , track
    , trade
    , trail
    , typeOf
    , unrelate
    , updateMethod
    )



import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import JsonApi.Base.Accessor as Accessor
    exposing
        ( idKeyFromData
        , idKeysFromData
        )
import JsonApi.Base.Decode exposing (topLevelDecoder)
import JsonApi.Base.Definition as Definition
    exposing
        ( Complement
        , DocTypeTaggers
        , Document
        , Encoder
        , GeneralDictionary
        , GeneralDictionaryModifier
        , IDKey
        , Relationship
        , Relationships
        , WithData
        , buildKey
        )
import JsonApi.Base.Encode exposing (documentEncoder)
import JsonApi.Base.Modifier as Modifier
    exposing
        ( insertTagIfType
        , modifyRelationships
        , setData
        )
import JsonApi.Base.Relations as Relations
import JsonApi.Base.Utility exposing (find)
import List
    exposing
        ( any
        , append
        , concat
        , filter
        , filterMap
        , foldl
        , head
        , isEmpty
        , length
        , map
        , singleton
        , sort
        , take
        )
import Maybe exposing (andThen, withDefault)
import Set exposing (Set)
import Tuple
    exposing
        ( mapFirst
        , mapSecond
        )







-- -------------    Identifier definitions      -------------------


type Identifier =  Identifier IDKey | EmptyId | Trail (List String) String String


trail : List String -> String -> String ->  Identifier
trail = Trail
 

track : Identifier -> Identifier -> TopLevel a -> Maybe Identifier
track idr subject (TopLevel doc) =
  case subject of 
    Trail fields type_ id -> 
      Accessor.track (getKey idr) fields (buildKey type_ id 0) doc
      |> Maybe.map Identifier
    a -> Just a
  

emptyId : Identifier
emptyId = EmptyId


equal : Identifier -> Identifier -> Bool
equal a b = (getKey a) == (getKey b)



typeOf : Identifier -> String 
typeOf idr =
  let (t,_,_) = getKey idr
  in t


getType : Identifier -> Maybe String
getType idr = 
  case typeOf idr of
    "" -> Nothing
    something -> Just something

    

isNew : Identifier -> Bool
isNew idr = Accessor.isNew (getKey idr)  




getId : Identifier -> Maybe String
getId a =
  case (not (isNew a), a) of 
    (True, Identifier (_,id,_)) -> Just id
    _ -> Nothing


idOf : Identifier ->  String
idOf a =
  case a of 
    Identifier (_,id,_) -> id
    _ -> ""



getKey : Identifier -> IDKey 
getKey idr =
  case idr of 
    Identifier idKey -> 
      idKey
    Trail _ type_ id -> 
      buildKey "" "" 0
    EmptyId -> 
      buildKey "" "" 0






-- -------------    Direct document definitions      -------------------


type TopLevel a = TopLevel (Document a)


emptyDocument = TopLevel Definition.emptyDocument

  
updateMethod (TopLevel doc) method = 
  Modifier.setMethod doc method
  |> TopLevel



setDocMeta : String -> a -> TopLevel a -> TopLevel a
setDocMeta key value (TopLevel doc) = TopLevel (  Modifier.setDocMeta key value doc )



filterDocMeta : (String -> a -> Bool) ->  TopLevel a -> TopLevel a
filterDocMeta test (TopLevel doc) = Modifier.filterDocMeta test doc |> TopLevel


getDocMeta : String ->  TopLevel a -> Maybe a
getDocMeta key  (TopLevel doc) =   Accessor.getDocMeta key  doc 



clearDocMeta : TopLevel a -> TopLevel a
clearDocMeta  (TopLevel doc) = Modifier.clearDocMeta doc |> TopLevel







-- -------------    Direct resource definitions      -------------------


type alias Object a = Definition.GeneralObject a {idr: Identifier}


turnObject : Definition.Object a -> Object a
turnObject o = 
  { type_ = o.type_
  , id = o.id
  , attributes = o.attributes
  , meta = o.meta
  , links = o.links
  , idr = Identifier o.idr
  }



getAttributes : Identifier -> TopLevel a -> Maybe (GeneralDictionary a)
getAttributes  idr (TopLevel doc) = Accessor.getAttributes (getKey idr) doc


getLocalAttributes : Identifier -> TopLevel a -> Maybe (GeneralDictionary a)
getLocalAttributes  idr (TopLevel doc) = Accessor.getLocal (getKey idr) doc


getObject : Identifier -> TopLevel a -> Maybe (Object a)
getObject  idr (TopLevel doc) = 
  Accessor.getObject (getKey idr) doc 
  |> Maybe.map turnObject




modifyAttributes : Identifier -> GeneralDictionaryModifier a -> TopLevel a -> TopLevel a
modifyAttributes idr generalDictionaryModifier ((TopLevel doc) as topLevel) =
  Modifier.modifyAttributes (getKey idr) generalDictionaryModifier doc
  |> TopLevel
  
  
modifyLocal : Identifier -> GeneralDictionaryModifier a -> TopLevel a -> TopLevel a
modifyLocal idr generalDictionaryModifier ((TopLevel doc) as topLevel) =
  Modifier.modifyLocal (getKey idr) generalDictionaryModifier doc
  |> TopLevel  


trade : Identifier -> String -> String -> TopLevel a -> TopLevel a 
trade idr local outbound (TopLevel doc) = 
  Modifier.trade (getKey idr) local outbound doc |> TopLevel


  

-- -------------    Data functions      -------------------



idFromData :  WithData a -> Maybe Identifier
idFromData  withData = Maybe.map Identifier (idKeyFromData withData)



idsFromData :  WithData a -> List Identifier
idsFromData  withData = map Identifier  (idKeysFromData withData)






-- -------------    Reach functions      -------------------

reach : Identifier -> List String ->  TopLevel  a -> Maybe Identifier
reach  idr fields (TopLevel doc ) =
  Accessor.reach (getKey idr) fields doc 
  |> Maybe.map Identifier



reachMany : Identifier -> List String ->  TopLevel  a -> List Identifier
reachMany  idr fields ((TopLevel doc ) as topLevel ) =
  Accessor.reachMany (getKey idr) fields doc 
  |> map Identifier







--    -----------------      DECODERS AND ENCODERS  ----------------------




docDecoder : Decoder a -> DocTypeTaggers t Identifier -> Decoder (t, TopLevel a)
docDecoder decoder  taggers =
    topLevelDecoder decoder Identifier taggers
    |> Decode.map 
      (\(docType, doc) -> (docType, TopLevel doc))




docEncoder : Encoder a -> TopLevel a -> Value
docEncoder encoder (TopLevel doc) = 
    documentEncoder encoder doc
    




-- -------------    Relationship functions      -------------------


relate : Identifier -> String ->   Bool -> Identifier -> TopLevel a -> TopLevel a
relate idr field isSingle source ((TopLevel doc) as topLevel) =
  case track idr source topLevel of 
    Just good -> 
      Relations.relate (getKey idr) field isSingle (getKey good) doc 
      |> TopLevel
    _ -> topLevel


createOne : Identifier -> String ->   Bool -> String -> TopLevel a  -> (Identifier,  TopLevel a)
createOne idr field isSingle type_ (TopLevel doc) =
  Relations.createOne (getKey idr) field isSingle type_ doc 
  |> mapFirst (Identifier)
  |> mapSecond (TopLevel)
  


incorporate  : Identifier -> String ->  TopLevel a -> TopLevel a -> TopLevel a
incorporate idr field (TopLevel source) (TopLevel doc)  =
  Relations.incorporate (getKey idr) field source doc 
  |> TopLevel


unrelate : Identifier ->  String ->  Identifier -> TopLevel a ->  TopLevel a
unrelate idr field child ((TopLevel doc) as topLevel) =
  case track idr child topLevel of 
    Just good -> 
      Modifier.unrelate (getKey idr) field (getKey child) doc 
      |> TopLevel
    _ -> topLevel
  


setRelationLocality : Identifier -> String -> Bool -> TopLevel a  -> TopLevel a
setRelationLocality idr field isLocal (TopLevel doc) =
  Modifier.setRelationLocality (getKey idr) field isLocal doc
  |> TopLevel
  
  
getRelationLocality : Identifier -> String -> TopLevel a  -> Bool
getRelationLocality idr field  (TopLevel doc) =
  Accessor.getRelationLocality (getKey idr) field  doc


renameRelationship  : Identifier -> String -> String -> TopLevel a  -> TopLevel a
renameRelationship idr field newName (TopLevel doc) =
  Modifier.renameRelationship (getKey idr) field newName doc
  |> TopLevel
  
  
swapData  : Identifier -> String -> String -> TopLevel a  -> TopLevel a
swapData idr one two (TopLevel doc) =
  Modifier.swapData (getKey idr) one two doc
  |> TopLevel  
  
  
  

getErrors : TopLevel a -> List (Definition.Error a)
getErrors (TopLevel doc) = doc.errors



getDocLinks : TopLevel a -> List (String, Definition.Link a)
getDocLinks (TopLevel doc) = doc.links


getAllDocMeta : TopLevel a -> Definition.GeneralPairList a
getAllDocMeta (TopLevel doc) = doc.meta


getRelationshipLinks : Identifier -> String -> TopLevel a -> List (String, Definition.Link a)
getRelationshipLinks idr field (TopLevel doc) = 
  Accessor.getRelationshipLinks  (getKey idr) field doc
  
  
getRelationshipMeta : Identifier -> String -> TopLevel a -> Definition.GeneralPairList a
getRelationshipMeta idr field (TopLevel doc) = 
  Accessor.getRelationshipMeta  (getKey idr) field doc  

