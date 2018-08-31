module JsonApi.Base.Decode exposing
    ( generalDictionaryDecoder
    , topLevelDecoder
    )


import Json.Decode as Decode
    exposing
        ( Decoder
        , Value
        , at
        , bool
        , decodeString
        , decodeValue
        , dict
        , fail
        , field
        , float
        , int
        , keyValuePairs
        , list
        , map2
        , map3
        , map4
        , map6
        , map7
        , map8
        , maybe
        , null
        , oneOf
        , string
        , succeed
        , value
        
        )
import Json.Encode as Encode exposing (encode)
import JsonApi.Base.Accessor
    exposing
        ( isInData
        , isNew
        )
import JsonApi.Base.Definition
    exposing
        ( Complement
        , ComplementDictionary
        , Data(..)
        , DocTypeTaggers
        , Document
        , Entries
        , Entry
        , Error
        , GeneralDictionary
        , GeneralPairList
        , Href
        , IDKey
        , IdTagger
        , JsonApiVersion
        , Link(..)
        , Relationship
        , StringPairList
        , buildKey
        )
import JsonApi.Base.Utility exposing (pair)        
import List
    exposing
        ( all
        , any
        , append
        , concat
        , filter
        , filterMap
        , foldl
        , indexedMap
        , isEmpty
        , map
        , member
        , singleton
        )
import Dict exposing (Dict)        
import Maybe exposing (withDefault)
import Set exposing (Set)
import String exposing
  ( trim
  , join
  
  )
import Tuple exposing
  ( first
  , mapSecond
  )


generalPairListDecoder : Decoder a ->   Decoder (GeneralPairList a)
generalPairListDecoder primitiveDecoder = keyValuePairs primitiveDecoder


strPairListDecoder : Decoder StringPairList
strPairListDecoder = keyValuePairs string
  


generalDictionaryDecoder : Decoder a ->   Decoder (GeneralDictionary a)
generalDictionaryDecoder primitiveDecoder = dict primitiveDecoder




ensureCorrect :  Decoder a -> Maybe Value -> Decoder (Maybe a)
ensureCorrect decoder mv =
  case mv of 
    Nothing -> succeed Nothing
    Just v -> 
      case decodeValue decoder v of 
        Ok a ->  succeed (Just a)
        Err err -> fail (toString err )



validateIfPresent : String ->  Decoder a -> Decoder (Maybe a)
validateIfPresent fieldName decoder =
  (maybe (field fieldName value))
  |> Decode.andThen (ensureCorrect decoder)





{-| Function that falls back to  a given default value in case a particular field is not
present. If the field is present, it makes sure that it is decoded properly. If not, it fails.    
-}
optionalWithDefault : a -> String -> Decoder a -> Decoder a
optionalWithDefault  defaultvalue fieldName decoder =
  validateIfPresent fieldName decoder
  |> Decode.map (withDefault defaultvalue)

  



optionalMetaDecoder : Decoder a ->  Decoder (GeneralPairList a)
optionalMetaDecoder decoder =
  optionalWithDefault [] "meta" (generalPairListDecoder decoder)


  
  
  
hrefDecoder : Decoder a ->  Decoder (Link a)
hrefDecoder decoder =
  Decode.map HrefLink (
    Decode.map2 Href
    (field "href" string)
    (optionalMetaDecoder decoder)
  )
  


urlLinkDecoder :  Decoder (Link a)
urlLinkDecoder  = 
  Decode.map UrlLink string
  


linkDecoder : Decoder a ->  Decoder (Link a)
linkDecoder decoder  =  
  oneOf [urlLinkDecoder , hrefDecoder decoder]




type EmptyManagement = AllowEmpties | NoEmptiesPlease

optionallyAllowLinks : Decoder a ->   List String -> EmptyManagement -> Decoder (List (String, Link a))  
optionallyAllowLinks decoder fieldNames emptyMgmt = 
  let     
    isAllowed item =
      member (first item) fieldNames
    frisk maybeLinks=
      case maybeLinks of 
        Nothing -> succeed []
        Just links -> 
          isAllGood links
    isAllGood links =
      let 
        pass = succeed links
        noPass = fail ("Only " ++ (join "," fieldNames) ++ " are allowed")
      in 
        case (emptyMgmt, isEmpty links, all isAllowed links) of 
          (AllowEmpties, True, _) -> pass
          (NoEmptiesPlease, True, _) -> noPass          
          (_, _, False) -> noPass
          _ -> pass
  in 
    validateIfPresent "links" (keyValuePairs (linkDecoder decoder))    
    |> Decode.andThen frisk



absent : a -> String -> String -> Decoder a
absent defaultvalue field msg = 
  optionalWithDefault defaultvalue field (fail msg)



nullOrAbsent : String -> Decoder String
nullOrAbsent fieldName =
  let 
    msg = "In this case, no " ++ fieldName ++ " would have been ok"    
  in
    oneOf
    [ field fieldName (null "")
    , absent "" fieldName msg 
    ]  




nonEmpty : String -> Decoder String 
nonEmpty fieldName =
  field fieldName string
  |> Decode.andThen 
  ( \s -> case trim s of 
    "" -> fail (fieldName ++ " must not be empty")
    _ -> succeed s 
  )


newPrimary : LinkageDecodingPurpose -> Decoder IDKey
newPrimary linkagePurpose = 
  let 
    primaryId =
      oneOf 
      [ nullOrAbsent "id"
      , field "id" string 
        |> Decode.andThen 
        ( \s -> case trim s of 
          "" -> succeed ""
          "new" -> succeed "" 
          _ -> fail "Id must be empty or absent"
        )
      ]            
  in
    if  linkagePurpose /= ForIncludedResource then          
      map3 buildKey
        (nonEmpty "type")
        primaryId
        (succeed 1)
    else
      persistedIdKey

  
persistedIdKey :  Decoder IDKey  
persistedIdKey =
  map3 buildKey
    (nonEmpty "type")
    (nonEmpty "id") 
    (succeed 0)
    
        

{-| It also accepts a single top level resource that has not been saved yet.
It does so if it sees the entry {is-new: true} in the top level 'meta' field.
It will only accept it as new if id is missing, empty, null or has
the value "new".
If 'meta' says that is new but the other conditions for 'id' are
not met, then it will proceed to decode the resource as a
regular persisted one.
-}
idKeyDecoder : NewResourceTags ->  LinkageDecodingPurpose -> Decoder IDKey
idKeyDecoder tags linkagePurpose = 
  let   
    isCataloged (tag, (type_, _)) =  -- It doesn't care about root object status
      case (linkagePurpose, Dict.get (type_, tag) tags) of 
        (ForBuildingTags, _) -> 
          succeed (type_, "", 2) -- so it doesn't match primary data
        (_, Just intTag) -> 
          succeed (type_, "", intTag)
        _ -> fail (tag ++ " is not a new resource tag")
    newResourceId = 
      map2 pair
        (field "meta" (nonEmpty "new-resource-tag"))
        ( map2 pair
            (nonEmpty "type")
            (absent Nothing "id" "Id must not be present")
        )
      |> Decode.andThen isCataloged
  in 
    oneOf 
      [ newResourceId
      , newPrimary linkagePurpose
      , persistedIdKey
      ]

 



-- TODO: Make case insensitive
checkForReservedNames : GeneralDictionary a -> Decoder (GeneralDictionary a)
checkForReservedNames dict =
  case any ((|>) dict << Dict.member) ["id","type"] of 
    True -> 
      fail "No attribute can be named 'id' or 'type'"
    False -> 
      succeed dict 

    


optionalAttributes : Decoder a ->   Decoder (GeneralDictionary a)
optionalAttributes decoder =   
  optionalWithDefault Dict.empty "attributes" (generalDictionaryDecoder decoder)
  |> Decode.andThen checkForReservedNames  
  




keySetDataDecoder  : NewResourceTags -> Decoder Data
keySetDataDecoder tags = 
  list (idKeyDecoder tags ForIncludedResource)  
  |> Decode.map (Ids << Set.fromList)





singleKeyDataDecoder : NewResourceTags -> Decoder Data
singleKeyDataDecoder tags =  
  oneOf 
  [ idKeyDecoder tags ForIncludedResource
    |> Decode.map (Id << Just)
    , null (Id Nothing)
  ]
  



optionalRelationData : NewResourceTags ->    Decoder Data
optionalRelationData  tags  =     
  let 
    dataDecoder =  
      oneOf 
      [ keySetDataDecoder tags
      , singleKeyDataDecoder tags
      ]    
  in 
    optionalWithDefault NotPresent "data" dataDecoder
    



detectOne : List String -> Decoder Bool
detectOne fieldNames = 
  let
    callAgain tup =
      case  tup  of 
        ([], Nothing) -> succeed False 
        (h::t, Nothing) ->           
          maybe (field h value)
          |> Decode.map (pair t)
          |> Decode.andThen callAgain
        (_, Just _) -> succeed True
  in 
    callAgain  (fieldNames, Nothing)
    
  



relationshipDecoder : NewResourceTags -> Decoder a ->  Decoder (Relationship a)
relationshipDecoder tags decoder  =  
  detectOne ["data", "links", "meta"]
  |> Decode.andThen
  (\atLeastOne -> 
    case atLeastOne of 
      False -> 
        fail "A relationship must have at least one of ['data', 'links' or 'meta']"
      True -> 
        Decode.map4 Relationship 
          (optionalRelationData tags)
          (optionallyAllowLinks decoder ["self", "related"] NoEmptiesPlease)
          (optionalMetaDecoder decoder)
          (succeed False) --TODO: Give backed ability to set this to True
  )
    



    
{-| Extracts only the resource members additional
  to 'type' and 'id'
-}
objectComplementDecoder : NewResourceTags -> Decoder a ->  Decoder (Complement a)
objectComplementDecoder  tags decoder =
  map6 Complement
    (optionalAttributes decoder)
    (succeed Dict.empty)
    (optionalMetaDecoder decoder)
    (optionallyAllowLinks decoder ["self", "first", "last", "prev", "next"]  AllowEmpties)
    (optionalWithDefault Dict.empty "relationships" (dict (relationshipDecoder tags decoder)))
    (succeed False)
    



{-| Presents a resource as a key-value pair.
-}
entryDecoder : NewResourceTags ->   Decoder a -> LinkageDecodingPurpose -> Decoder (Entry a)
entryDecoder  tags decoder linkagePurpose   =  
  Decode.map2  pair
    (idKeyDecoder tags linkagePurpose)
    (objectComplementDecoder tags  decoder) 





optionalJsonapiDecoder : Decoder a ->  Decoder (JsonApiVersion a)
optionalJsonapiDecoder decoder =
  let     
    jsonapiObject =
      map2 JsonApiVersion 
        (optionalWithDefault "1.0" "version" string)
        (optionalMetaDecoder decoder)
  in 
    optionalWithDefault (JsonApiVersion "1.0" []) "jsonapi" jsonapiObject   


type LinkageDecodingPurpose = ForNewResource | ForIncludedResource | ForBuildingTags




{-| 
  Gathers the contents of the 'data' field.
  (This implementation bundles both 'primary' and 'included' resources
  together in an 'included' field).   
-}
primaryDataEntriesDecoder : Decoder a -> (LinkageDecodingPurpose, NewResourceTags ) -> Decoder (Data, Entries a, NewResourceTags)
primaryDataEntriesDecoder decoder (linkagePurpose,tags ) = 
  let 
    many entries =
      ( map first entries        
        |> Ids << Set.fromList
      , entries
      , tags
      ) |> succeed
    one entry =
     let 
        key = first entry
      in 
        ( Id (Just key)
        , singleton entry
        , tags
        ) |> succeed 
  in
    field "data" 
    ( oneOf
      [ list (entryDecoder tags decoder ForIncludedResource)  -- Only Single Object can be new
        |> Decode.andThen many
      , entryDecoder tags decoder linkagePurpose
        |> Decode.andThen one
      , null  (Id Nothing, [], tags)
      ]
    )
    



{-|
  Main decoder. Determines whether document complies with the
  JSON API v1.0 specification, plus the ability to accept
  a not persisted single primary resource
-}
topLevelDecoder : Decoder a -> IdTagger id ->  DocTypeTaggers t id -> Decoder (t, Document a)
topLevelDecoder decoder idTagger taggers =
  let 
    translator b =
     if b then ForBuildingTags else ForIncludedResource
     |> Debug.log "TRANSLATED"
  in 
    oneOf 
    [ maybe (Decode.map translator (at ["meta", "is-new"] bool))
      |> Decode.map (withDefault ForIncludedResource)
      |> Decode.andThen figureOutNewTags
      |> Decode.andThen (primaryDataEntriesDecoder decoder)
      |> Decode.andThen  (documentDecoder decoder)
      |> Decode.andThen  (tagDocument idTagger taggers)-- (pair taggers.data)
    , (errorsDecoder decoder)
      |> Decode.map (pair taggers.errors)
    , (metaDocDecoder decoder)
      |> Decode.map (pair taggers.meta)
    ]
    

tagDocument : IdTagger id ->  DocTypeTaggers t id -> Document a -> Decoder (t, Document a )
tagDocument idTagger taggers doc =
  case doc.data of 
    Id id -> 
      Maybe.map idTagger id
      |> taggers.data.single 
      |> (|>) doc << pair
      |> succeed
    Ids idSet -> 
      Set.toList idSet 
      |> map idTagger
      |> taggers.data.multiple
      |> (|>) doc << pair
      |> succeed 
    _ -> fail "no 'data' element present"
  


type alias NewResourceTags = Dict (String,String) Int



type NewIdEntry = NotNew | NewIdEntry String String


newIdEntryDecoder : Decoder NewIdEntry
newIdEntryDecoder = 
  oneOf 
  [ absent NotNew "id" "It is not supposed to have an 'id'"
    |> Decode.andThen  
        ( always 
          ( map2 NewIdEntry
              (nonEmpty "type" )
              (field "meta" (nonEmpty "new-resource-tag") )
          )
        )
  , succeed NotNew
  ]


withDataNewEntries : Decoder (List NewIdEntry)
withDataNewEntries =
  oneOf
  [ field "data" (list newIdEntryDecoder)
  , field "data" newIdEntryDecoder
    |> Decode.map singleton
  ] 
  

relsNewIdsDecoder :  Decoder (List NewIdEntry)
relsNewIdsDecoder =
  let 
    decoder = 
      oneOf
      [ dict withDataNewEntries
      , succeed Dict.empty
      ]
  in 
    optionalWithDefault Dict.empty "relationships" decoder
    |> Decode.map      (concat << Dict.values)
   


      

figureOutNewTags : LinkageDecodingPurpose ->  Decoder (LinkageDecodingPurpose, NewResourceTags)
figureOutNewTags linkagePurpose =
  let 
    build item accum =
      case item of
        NewIdEntry type_ tag ->
          Dict.insert
            type_ 
            ( case Dict.get type_ accum of 
               Nothing ->
                  Set.singleton tag
               Just set ->
                  Set.insert tag set 
            )
            accum
        _ ->
          accum
    groups  entries =
      foldl build Dict.empty entries  
    buildTags key item accum =
      Set.toList item
      |> map (pair key) 
      |> indexedMap (\a b -> (b,a))
      |> map (mapSecond ((+) 1))
      |> Dict.fromList
      |> Dict.union accum
    scan (purpose, primEntries) primaryRels includedRels =
      concat [primEntries, primaryRels, includedRels]
      |> groups 
      |> Dict.foldl buildTags Dict.empty
      |> pair purpose
    primaryAsEntries =
      oneOf 
      [ map2 
         (\(type_, _, _) mtag -> 
           case mtag of 
            Just tag -> [NewIdEntry type_ tag]
            _ -> [NewIdEntry type_ ""]
        )
        (field "data" (newPrimary linkagePurpose) )
        (maybe (at ["data", "meta"] (nonEmpty "new-resource-tag")) )
      , withDataNewEntries
      , succeed []
      ]
      |> Decode.map 
      ( \entries -> 
         case entries of 
            [NewIdEntry _ _] -> (ForNewResource, entries)
            _ -> (linkagePurpose,entries)
      )
    primaryRelsDecoder = 
      oneOf 
      [ field "data" (list relsNewIdsDecoder)
        |> Decode.map concat
      , field "data" relsNewIdsDecoder 
      , succeed []
      ]
  in
    map3 scan
      primaryAsEntries
      primaryRelsDecoder
      ( (optionalWithDefault [] "included" (list relsNewIdsDecoder) )
        |> Decode.map concat
      )
      
      



documentDecoder : Decoder a -> (Data, Entries a, NewResourceTags) ->  Decoder (Document a)  
documentDecoder decoder (data, primaryEntries, tags) =  
  let 
    includedDecoder  = 
      optionalWithDefault [] "included" (list (entryDecoder tags decoder ForIncludedResource))
      |> Decode.map (append primaryEntries)
      |> Decode.map (Dict.fromList)
  in  
    map7 Document
      (optionalMetaDecoder decoder)
      (optionalJsonapiDecoder  decoder)
      (succeed data)
      (optionallyAllowLinks 
        decoder 
        ["self", "related", "first", "last", "prev", "next"] 
        AllowEmpties
      )
      includedDecoder
      (preventErrorPresence False)
      (succeed Nothing)
      

  

errorsDecoder : Decoder a ->   Decoder (Document a)
errorsDecoder  decoder = 
  map7 Document
    (optionalMetaDecoder decoder)
    (optionalJsonapiDecoder  decoder)
    (preventDataPresence False)
    (succeed [])
    preventIncludedPresence
    (field "errors" (list (errorDecoder decoder)) )
    (succeed Nothing)
  
  

metaDocDecoder: Decoder a ->   Decoder (Document a)
metaDocDecoder decoder  = 
  map7 Document
    (field "meta" (generalPairListDecoder decoder))
    (optionalJsonapiDecoder decoder)
    (preventDataPresence True)
    (succeed [])    
    preventIncludedPresence
    (preventErrorPresence True)
    (succeed Nothing)




  
failWithMessage isForMetaDoc =
  ( case isForMetaDoc of 
    True -> 
      "At this point both 'data' and 'errors' fields have been unable to be decoded"
    False -> 
        "The members 'data' and 'errors' MUST NOT coexist in the same document"
  ) |> fail  




errorPresenceFail : Bool -> Maybe Value -> Decoder (List (Error a))
errorPresenceFail isForMetaDoc arg = 
  case arg of 
    Nothing -> succeed []
    _ -> failWithMessage isForMetaDoc




preventErrorPresence : Bool -> Decoder (List (Error a))
preventErrorPresence isForMetaDoc =
  maybe (field "errors" value)
  |> Decode.andThen (errorPresenceFail isForMetaDoc) -- TODO : Ensure fail  





dataPresenceFail : Bool -> Maybe Value -> Decoder Data
dataPresenceFail isForMetaDoc  arg  = 
  case arg of 
    Nothing -> 
      succeed NotPresent
    _ -> failWithMessage isForMetaDoc
    
      

preventDataPresence : Bool -> Decoder Data
preventDataPresence isForMetaDoc =  
  maybe (field "data" value)
  |> Decode.andThen (dataPresenceFail isForMetaDoc) -- TODO : Ensure fail  
   




includedPresenceFail :  Maybe Value -> Decoder (ComplementDictionary a)
includedPresenceFail   arg  = 
  case arg of 
    Nothing -> 
      succeed Dict.empty
    _ -> fail "Only documents with 'data' can have 'included' resources"




preventIncludedPresence : Decoder (ComplementDictionary a)
preventIncludedPresence =  
  maybe (field "included" value)
  |> Decode.andThen includedPresenceFail



  
errorSourceDecoder  : Decoder StringPairList  
errorSourceDecoder =
  let     
    two ma mb = 
      filterMap identity [ma,mb]
    readyForFilter fieldName = 
      Decode.map 
        (Maybe.map (pair fieldName)) 
        (validateIfPresent fieldName string)
    reserved =
      map2 two 
        (readyForFilter "pointer")
        (readyForFilter "parameter")        
    isNotReserved (key, _) =
      not (member key ["pointer", "parameter"])
    combine = 
      map2 (++)
        reserved 
        ( strPairListDecoder 
          |> Decode.map (filter isNotReserved)
        )
  in 
    optionalWithDefault [] "source" combine   

    


{-| Decodes error id as either a string or an int, if present
-}

optionalErrorIdDecoder : Decoder String 
optionalErrorIdDecoder = 
  let 
    decoder = 
      oneOf 
      [ string
      , Decode.map toString int
      ]
  in 
    optionalWithDefault "" "id" decoder
  



errorDecoder : Decoder a ->  Decoder (Error a)
errorDecoder decoder  =
  map8 Error    
    optionalErrorIdDecoder
    (optionallyAllowLinks decoder ["about"] AllowEmpties)
    (optionalWithDefault "" "status"  string)
    (optionalWithDefault "" "code"  string )
    (optionalWithDefault "" "title"  string )
    (optionalWithDefault "" "detail"  string )    
    errorSourceDecoder
    (optionalMetaDecoder decoder)
    
reflect : Decoder x -> Decoder x
reflect decoder =
  let 
    log result = succeed (Debug.log "PARTIAL RESULT..." result)
  in 
     Decode.andThen  log decoder
  
