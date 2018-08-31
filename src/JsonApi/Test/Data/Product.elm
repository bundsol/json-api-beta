module JsonApi.Test.Data.Product exposing (chickenThighs, product_ids, product_objects, products, turkeyFranks)

import JsonApi.Test.Shortcuts exposing (..)


chickenThighs =
    [ p "type" ( s "product")
    , p "id" ( s "11")
    , p "attributes"
        ( o
            [ p "name" ( s "Chicken thighs")
            , p "description" ( s "Just the thighs, not the drumsticks")
            ]
        )
    ]


turkeyFranks =
    [ p "type" ( s "product")
    , p "id" ( s "15")
    , p "attributes"
        ( o
            [ p "name" ( s "Turkey franks")
            , p "description" ( s "Turkey wienier")
            ]
        )
    ]


products =
    [ chickenThighs
    , [ p "type" ( s "product")
      , p "id" ( s "12")
      , p "attributes"
            ( o
                [ p "name" ( s "Chicken drumsticks")
                , p "description" ( s "Just the drumsticks, without the thigh")
                ]
            )
      ]
    , [ p "type" ( s "product")
      , p "id" ( s "13")
      , p "attributes"
            ( o
                [ p "name" ( s "Whole chicken")
                , p "description" ( s "Includes both frier and roaster")
                ]
            )
      ]
    , turkeyFranks
    ]


product_ids =
    List.map takeId products
        |> List.map o


product_objects =
    List.map o products
