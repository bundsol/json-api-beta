module JsonApi.Test.Data.Want exposing (turkeyFranksWant, want_ids, want_objects, wants)

import JsonApi.Test.Data.Product exposing (..)
import JsonApi.Test.Shortcuts exposing (..)


turkeyFranksWant =
    [ p "type" ( s "want")
    , p "id" ( s "311")
    , p "attributes"
        ( o
            [ p "priority" ( i 4)
            ]
        )
    , p "relationships"
        ( o
            [ p "product"
                ( o
                    [ linkage turkeyFranks
                    ]
                )
            ]
        )
    ]


wants =
    [ turkeyFranksWant
    ]


want_ids =
    List.map takeId wants
        |> List.map o


want_objects =
    List.map o wants
