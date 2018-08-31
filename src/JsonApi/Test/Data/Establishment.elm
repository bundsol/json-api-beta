module JsonApi.Test.Data.Establishment exposing (establishmentIds, establishment_objects, establishments, favoriteStore, twinTown)

import JsonApi.Test.Shortcuts exposing (..)


favoriteStore =
    [ p "type" (s "establishment")
    , p "id" (s "3")
    , p "attributes"
        ( o
            [ p "name" ( s "Dollar Bargain")
            , p "location" ( s "Optimist Mall")
            , p "city" ( s "Muskogee")
            ]
        )
    ]


twinTown =
    [ p "type" ( s "establishment")
    , p "id" ( s "4")
    , p "attributes"
        ( o
            [ p "name" ( s "Bob's Corner")
            , p "address" ( s "4156 Aspen Ave")
            , p "city" ( s "Warner")
            , p "state" ( s "OK")
            ]
        )
    ]


establishments =
    [ favoriteStore
    , twinTown
    ]


establishmentIds =
    List.map takeId establishments
        |> List.map o


establishment_objects =
    List.map o establishments
