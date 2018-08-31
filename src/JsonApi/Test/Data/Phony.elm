module JsonApi.Test.Data.Phony exposing (fetchedJson, newPhonies, phonies, phony1, phony2, phony3, phony4, phony5)

import JsonApi.Test.Shortcuts exposing (..)
import List exposing (map)


phony1 =
    [ p "type" ( s "phony")
    , p "id" ( s "16")
    , p "attributes"
        ( o
            [ p "name" ( s "Awful")
            , p "isValid" ( b True)
            , p "quote-amount" ( f 10.5)
            , p "rank" ( i 2)
            ]
        )
    ]


phony2 =
    [ p "type" ( s "phony")
    , p "id" ( s "23")
    , p "attributes"
        ( o
            [ p "name" ( s "Worse")
            , p "isValid" ( b True)
            , p "quote-amount" ( f 30.5)
            , p "rank" ( i 8)
            ]
        )
    ]


phony3 =
    [ p "type" ( s "phony")
    , p "id" ( s "214")
    , p "attributes"
        ( o
            [ p "name" ( s "Much better")
            , p "isValid" ( b False)
            , p "quote-amount" ( f 66.0)
            , p "rank" ( i 12)
            ]
        )
    ]


phony4 =
    [ p "type" ( s "phony")
    , p "attributes"
        ( o
            [ p "name" ( s "Unnamed")
            , p "isValid" ( b False)
            , p "quote-amount" ( f 55.55)
            , p "rank" ( i 19)
            ]
        )
    , p "meta"
        ( o
            [ p "new-resource-tag" ( s "PPPPPPPPPP+++++")
            ]
        )
    ]


phony5 =
    [ p "type" ( s "phony")
    , p "attributes"
        ( o
            [ p "name" ( s "Nameless")
            , p "isValid" ( b False)
            , p "quote-amount" ( f 111.25)
            , p "rank" ( i 15)
            ]
        )
    , p "meta"
        ( o
            [ p"new-resource-tag" ( s "GGGG77777")
            ]
        )
    ]


newPhonies =
    [ phony4
    , phony5
    ]


phonies =
    [ phony1
    , phony2
    , phony3
    ]


fetchedJson =
    o
        [ p "data" ( l (map o phonies) )
        ]
