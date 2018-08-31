module JsonApi.Test.Data.Brand exposing (brand_ids, brand_objects, brands, horizon, maryO, newComer, rumT, wittyChicken)

import JsonApi.Test.Shortcuts exposing (..)


horizon =
    [ p "type" ( s "brand")
    , p "id" ( s "21")
    , p "attributes"
        ( o
            [ p "name" ( s "Horizon")
            , p "opinion" ( s "Not my first choice")
            ]
        )
    ]


wittyChicken =
    [ p "type" ( s "brand")
    , p "id" ( s "12")
    , p "attributes"
        ( o
            [ p "name" ( s "Witty Chicken")
            , p "opinion" ( s "Another kind of freshness")
            ]
        )
    ]


maryO =
    [ p "type" ( s "brand")
    , p "id" ( s "8")
    , p "attributes"
        ( o
            [ p "name" ( s "Mary-O")
            , p "opinion" ( s "Turkey specialist, not expensive")
            ]
        )
    ]


rumT =
    [ p "type" ( s "brand")
    , p "id" ( s "13")
    , p "attributes"
        ( o
            [ p "name" ( s "Rum-T")
            , p "opinion" ( s "Affordable, yet good")
            ]
        )
    ]


newComer =
    [ p "type" ( s "brand")
    , p "attributes"
        ( o
            [ p "name" ( s "New Comer")
            , p "opinion" ( s "Very Competitive")
            ]
        )
    , p "meta"
        ( o
            [ p "new-resource-tag" ( s "AAAABBBB")
            ]
        )
    ]


brands =
    [ horizon
    , wittyChicken
    , rumT
    , maryO
    ]


brand_ids =
    List.map takeId brands
        |> List.map o


brand_objects =
    List.map o brands
