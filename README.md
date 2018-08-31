# Json Api Plus

Json Api implementation for Elm 0.18, with non-standard additions.


## Background 

For those who leverage Ecto's `changeset` nice features that allows you to
delete, update and create resources in a single POST request to your Elixir
powered web server, the constraint in the Json Api v1.0 specification that only
provides for a single resource to be sent from client side to the back end in a
POST request might render it wanting.

That was the motivation behind this package, that makes use of the `meta` object 
in resources and linkages to send uniquely identifiable multiple `new resources` 
to the server.


## Back end support

So far, a server side decoder is provided for the Elixir language, found at
[ja_nested_params](https://github.com/bundsol/ja_nested_params).


## Broader functionality

The functions provided revolve around a single `top level object` (to borrow
for the Json Api lingo), that gets passed around to every component of your 
application that might have something to add to it. It mimics the Elm model in
some sense, but at the root you end up with an object that is ready to be sent
back to the server.


## Install

    elm package install bundsol/json-api-beta


## Example 

First, run `elm package install bundsol/boxed`

Save the following code in a file called Demo.elm,

```elm 
module Demo exposing (guide)

import JsonApi exposing
  ( docDecoder
  , emptyIdr
  , Primary(..)
  , DocType(..)
  )
  
import JsonApi.TopLevel exposing 
  ( emptyDocument
  )
  
import Boxed exposing (Boxed(..))
import Json.Encode as E 
import Json.Decode exposing
  ( decodeValue
  )
  
o = E.object
s = E.string

p a b = (a,b)

order =
  o 
    [ p "data" 
        ( o
            [ p "type" ( s "user")
            , p "id" ( s "2")
            , p "attributes"
                ( o
                    [ p "first-name" ( s "Sandy")
                    , p "last-name" ( s "Markinson")
                    ]
                )
            ]
        )
    ]
    
    
guide = 
  case decodeValue docDecoder order of 
    Ok ( DataDoc (Single (Just idr)), topLevel ) ->
        { doc = topLevel, idr = idr }

    _ ->
        { doc = emptyDocument, idr = emptyIdr }
  
  
      
```

Then run `elm repl`

Type, ..

    > import Demo exposing (guide)
    > import JsonApi.Getter exposing (getString, getInt)
    > getString "first-name" guide
    Just "Sandy" : Maybe.Maybe String
    > import JsonApi.Setter exposing (setInt)
    > setInt "age" 35 guide |> getInt "age"
    Just 35 : Maybe.Maybe Int
    > import JsonApi.Relationship exposing(createSingle)
    > employee = createSingle "employee" "secretary" guide
    { doc = TopLevel { meta = [], jsonapi = { version = "1.0", meta = [] }, 
    data = Id (Just ("user","2",0)), links = [], included = Dict.fromList 
    [(("employee","",1),{ attributes = Dict.fromList [], local = Dict.fromList
    [], relationships = Dict.fromList [], links = [], meta = [], deleted = False
    }),(("user","2",0),{ attributes = Dict.fromList [("first-name",Str "Sandy"),
    ("last-name",Str "Markinson")], local = Dict.fromList [], meta = [], links =
    [], relationships = Dict.fromList [("secretary",{ links = [], meta = [], 
    data = Id (Just ("employee","",1)), isLocal = False })], deleted = False })]
    , errors = [], method_ = Nothing }, idr = Identifier ("employee","",1) }
    : JsonApi.Base.Guide.Guide {} (Boxed.Boxed a)
    > setInt "age" 22 employee |> getInt "age"
    > Just 22 : Maybe.Maybe Int
    
    
    