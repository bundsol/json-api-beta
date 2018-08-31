module JsonApi.Test.Data.User exposing (complex, dictionary, sandyMarkinson)

import JsonApi.Test.Data.Stock exposing (..)
import JsonApi.Test.Shortcuts exposing (..)


sandyMarkinson =
    [ p "type" ( s "user")
    , p "id" ( s "2")
    , p "attributes"
        ( o
            [ p "first-name" ( s "Sandy")
            , p "last-name" ( s "Markinson")
            , p "options" ( o complex)
            , p "preferences" ( o dictionary)
            ]
        )
    , p "relationships"
        ( o
            [ p "stocks"
                ( o
                    [ linkages stocks
                    ]
                )
            ]
        )
    ]


complex =
    [ p "display"
        ( o
            [ p "allowed"   ( l   [i 45 , i 100] )
            , p "background-color" ( s "grey")
            ]
        )
    ]


dictionary =
    [ p "font-color" ( s "blue")
    ]
