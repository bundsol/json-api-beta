module JsonApi.Base.Definition exposing
    ( Complement
    , ComplementDictionary
    , ComplementDictionaryModifier
    , ComplementModifier
    , Data(..)
    , DocTypeTaggers
    , Document
    , Encoder
    , Entries
    , Entry
    , Error
    , GeneralDictionary
    , GeneralDictionaryModifier
    , GeneralError
    , GeneralObject
    , GeneralPairList
    , Href
    , IDKey
    , IdTagger
    , JsonApiVersion
    , Link(..)
    , Object
    , Relationship
    , RelationshipModifier
    , Relationships
    , RelationshipsModifier
    , Resource
    , StringPairList
    , WithData
    , buildKey
    , emptyDocument
    , fuse
    , newResource
    , toComplement
    , toEntry
    , toKey
    )

import Dict exposing (Dict)
import Json.Encode exposing (Value)
import Set exposing (Set)


type Data
    = Ids (Set IDKey)
    | Id (Maybe IDKey)
    | NotPresent


type alias IdTagger id =
    IDKey -> id


type alias WithData w =
    { w | data : Data }


type alias StringPairList =
    List ( String, String )


type alias IDKey =
    ( String, String, Int )


buildKey : String -> String -> Int -> IDKey
buildKey type_ id spot =
    ( type_, id, spot )


type alias GeneralDictionary a =
    Dict String a


type alias GeneralPairList a =
    List ( String, a )


type alias Href a =
    { href : String
    , meta : GeneralPairList a
    }


type Link a
    = UrlLink String
    | HrefLink (Href a)


type alias LinkList a =
    List ( String, Link a )


type alias Relationship a =
    { data : Data
    , links : LinkList a
    , meta : GeneralPairList a
    , isLocal : Bool
    }


type alias Relationships a =
    Dict String (Relationship a)


type alias Complement a =
    { attributes : GeneralDictionary a
    , local : GeneralDictionary a
    , meta : GeneralPairList a
    , links : List ( String, Link a )
    , relationships : Relationships a
    , deleted : Bool
    }


emptyComplement =
    { attributes = Dict.empty
    , local = Dict.empty
    , relationships = Dict.empty
    , links = []
    , meta = []
    , deleted = False
    }


type alias ComplementDictionary a =
    Dict IDKey (Complement a)


type alias JsonApiVersion a =
    { version : String
    , meta : GeneralPairList a
    }


type alias GeneralError a e =
    { e
        | id : String
        , status : String
        , code : String
        , title : String
        , detail : String
        , source : StringPairList
        , meta : GeneralPairList a
    }


type alias Error a =
    { id : String
    , links : LinkList a
    , status : String
    , code : String
    , title : String
    , detail : String
    , source : StringPairList
    , meta : GeneralPairList a
    }


type alias Document a =
    { meta : GeneralPairList a
    , jsonapi : JsonApiVersion a
    , data : Data
    , links : LinkList a
    , included : ComplementDictionary a
    , errors : List (Error a)
    , method_ : Maybe String
    }


emptyDocument =
    { meta = []
    , jsonapi = JsonApiVersion "1.0" []
    , data = NotPresent
    , links = []
    , included = Dict.empty
    , errors = []
    , method_ = Nothing
    }


type alias GeneralDictionaryModifier a =
    GeneralDictionary a -> GeneralDictionary a


type alias ComplementModifier a =
    Complement a -> Complement a


type alias ComplementDictionaryModifier a =
    ComplementDictionary a -> ComplementDictionary a


type alias RelationshipModifier a =
    Relationship a -> Relationship a


type alias RelationshipsModifier a =
    Relationships a -> Relationships a


type alias Entry a =
    ( IDKey, Complement a )


type alias Entries a =
    List (Entry a)


type alias DocTypeTaggers a id =
    { data :
        { single : Maybe id -> a
        , multiple : List id -> a
        }
    , errors : a
    , meta : a
    }


type alias GeneralObject a o =
    { o
        | type_ : String
        , id : String
        , attributes : GeneralDictionary a
        , meta : GeneralPairList a
        , links : List ( String, Link a )
    }


type alias Object a =
    GeneralObject a
        { idr : IDKey
        }


type alias Res a r =
    { r
        | type_ : String
        , id : String
        , newTag : Int
        , attributes : GeneralDictionary a
        , local : GeneralDictionary a
        , meta : GeneralPairList a
        , links : List ( String, Link a )
        , relationships : Relationships a
    }


type alias Resource a =
    Res a {}


toKey : Res a r -> IDKey
toKey ro =
    buildKey ro.type_ ro.id ro.newTag


newResource : IDKey -> Resource a
newResource idKey =
    fuse idKey emptyComplement


toComplement : Res a r -> Complement a
toComplement object =
    { attributes = object.attributes
    , local = object.attributes
    , relationships = object.relationships
    , links = object.links
    , meta = object.meta
    , deleted = False
    }


toEntry : Res a r -> ( IDKey, Complement a )
toEntry ro =
    ( toKey ro, toComplement ro )


fuse : IDKey -> Complement a -> Resource a
fuse ( type_, id, num ) obj =
    { type_ = type_
    , id = id
    , attributes = obj.attributes
    , local = obj.local
    , relationships = obj.relationships
    , links = obj.links
    , meta = obj.meta
    , newTag = num
    }


type alias Encoder a =
    a -> Value
