module JsonApi.Test.Data.Stock exposing (horzThighAtFav, maryFranksAtTwin, rumFranksAtTwin, stock_ids, stock_objects, stocks, wittyThighAtFav, wittyThighAtTwin)

import JsonApi.Test.Data.Choice exposing (..)
import JsonApi.Test.Data.Establishment exposing (..)
import JsonApi.Test.Shortcuts exposing (..)


horzThighAtFav =
    [ p "type" ( s "stock")
    , p "id" ( s "1002")
    , p "attributes"
        ( o
            [ p "last_checked" ( s "2018-05-05")
            , p "price" ( f 0.99)
            , p "unit" ( s "pound")
            ]
        )
    , p "relationships"
        ( o
            [ p "establishment"
                ( o
                    [ linkage favoriteStore
                    ]
                )
            , p "choice"
                ( o
                    [ linkage horizonThighs
                    ]
                )
            ]
        )
    ]


wittyThighAtFav =
    [ p "type" ( s "stock")
    , p "id" ( s "1022")
    , p "attributes"
        ( o
            [ p "last_checked" ( s "2018-03-12")
            , p "price" ( f 1.2)
            , p "unit" ( s "pound")
            ]
        )
    , p "relationships"
        ( o
            [ p "establishment"
                ( o
                    [ linkage favoriteStore
                    ]
                )
            , p "choice"
                ( o
                    [ linkage wittyThighs
                    ]
                )
            ]
        )
    ]


wittyThighAtTwin =
    [ p "type" ( s "stock")
    , p "id" ( s "1033")
    , p "attributes"
        ( o
            [ p "last_checked" ( s "2018-03-19")
            , p "price" ( f 1.3)
            , p "unit" ( s "pound")
            ]
        )
    , p "relationships"
        ( o
            [ p "establishment"
                ( o
                    [ linkage twinTown
                    ]
                )
            , p "choice"
                ( o
                    [ linkage wittyThighs
                    ]
                )
            ]
        )
    ]


rumFranksAtTwin =
    [ p "type" ( s "stock")
    , p "id" ( s "1034")
    , p "attributes"
        ( o
            [ p "last_checked" ( s "2018-01-19")
            , p "price" ( f 2.3)
            , p "unit" ( s "ounce")
            , p "net_weight" ( f 12.0)
            , p "pack_qty" ( i 12)
            ]
        )
    , p "relationships"
        ( o
            [ p "establishment"
                ( o
                    [ linkage twinTown
                    ]
                )
            , p "choice"
                ( o
                    [ linkage rumFranks
                    ]
                )
            ]
        )
    ]


maryFranksAtTwin =
    [ p "type" ( s "stock")
    , p "id" ( s "1103")
    , p "attributes"
        ( o
            [ p "last_checked" ( s "2017-11-29")
            , p "price" ( f 2.9)
            , p "unit" ( s "ounce")
            , p "net_weight" ( f 11.0)
            , p "pack_qty" ( i 12)
            ]
        )
    , p "relationships"
        ( o
            [ p "establishment"
                ( o
                    [ linkage twinTown
                    ]
                )
            , p "choice"
                ( o
                    [ linkage maryFranks
                    ]
                )
            ]
        )
    ]


stocks =
    [ horzThighAtFav
    , wittyThighAtTwin
    , wittyThighAtFav
    , rumFranksAtTwin
    , maryFranksAtTwin
    ]


stock_ids =
    List.map takeId stocks
        |> List.map o


stock_objects =
    List.map o stocks
