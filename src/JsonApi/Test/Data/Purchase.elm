module JsonApi.Test.Data.Purchase exposing (new_purchase, new_purchase_option_pack, wrong_purchase)

import JsonApi.Test.Data.Bogus exposing (..)
import JsonApi.Test.Data.Brand exposing (..)
import JsonApi.Test.Data.Choice exposing (..)
import JsonApi.Test.Data.Establishment exposing (..)
import JsonApi.Test.Data.Phony exposing (..)
import JsonApi.Test.Shortcuts exposing (..)


new_purchase =
    [ p "type" ( s "purchase")
    , p "id" ( s "new")
    , p "attributes"
        ( o
            [ p "creation-date"
                ( o
                    [ p "year" ( i 2018)
                    , p "month" ( i 11)
                    , p "day" ( i 7)
                    ]
                )
            ]
        )
    , p "relationships"
        ( o
            [ p "establishment"
                ( o
                    [ linkage favoriteStore
                    ]
                )
            , p "option-pack"
                ( o
                    [ linkage new_purchase_option_pack
                    ]
                )
            , p "loser-brand"
                ( o
                    [ linkage newComer
                    ]
                )
            ]
        )
    ]


new_purchase_option_pack =
    [ p "type" ( s "option-pack")
    , p "id" ( s "new")
    , p "relationships"
        ( o
            [ p "establishments"
                ( o
                    [ linkages establishments
                    ]
                )
            , p "choices"
                ( o
                    [ linkages (newComerFranks :: choices)
                    ]
                )
            , p "bogus"
                ( o
                    [ linkage bogusTwo
                    ]
                )
            , p "phonies"
                ( o
                    [ linkages newPhonies
                    ]
                )
            ]
        )
    ]


wrong_purchase =
    [ p "type" ( s "shopping-list")
    , p "id" ( s "")
    , p "relationships"
        ( o
            [ p "establishment"
                ( o
                    [ linkage favoriteStore
                    ]
                )
            ]
        )
    ]
