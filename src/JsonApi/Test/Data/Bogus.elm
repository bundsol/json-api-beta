module JsonApi.Test.Data.Bogus exposing (bogusOne, bogusTwo)

import JsonApi.Test.Shortcuts exposing (..)


bogusOne =
    [ p "type" ( s "bogus")
    , p "id" ( s "11")
    , p "attributes"
        ( o
            [ p "name" ( s "Adorable thing")
            , p "is-valid" ( b True)
            , p"quote-amount" ( f 8.889)
            , p "rank" ( i 5)
            ]
        )
    ]


bogusTwo =
    [ p "type" ( s "bogus")
    , p "id" ( s "19")
    , p "attributes"
        ( o
            [ p "name" ( s "Lovely thing")
            , p "is-valid" ( b True)
            , p "quote-amount" ( f 7.889)
            , p "rank" ( i 6)
            ]
        )
    , p "relationships"
        ( o
            [ p "predecessor"
                ( o
                    [ linkage bogusOne
                    ]
                )
            ]
        )
    ]
