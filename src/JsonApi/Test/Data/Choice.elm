module JsonApi.Test.Data.Choice exposing (choice_ids, choice_objects, choices, horizonThighs, maryFranks, newComerFranks, rumFranks, wittyThighs)

import JsonApi.Test.Data.Brand exposing (..)
import JsonApi.Test.Data.Product exposing (..)
import JsonApi.Test.Shortcuts exposing (..)


horizonThighs =
    [ p "type" ( s "choice")
    , p "id" ( s "55")
    , p "attributes"
        ( o
            [ p "quality" ( i 1)
            ]
        )
    , p "relationships"
        ( o
            [ p "product"
                ( o
                    [ linkage chickenThighs
                    ]
                )
            , p "brand"
                ( o
                    [ linkage horizon
                    ]
                )
            ]
        )
    ]


wittyThighs =
    [ p "type" ( s "choice")
    , p "id" ( s "56")
    , p "attributes"
        ( o
            [ p "quality" ( i 4)
            ]
        )
    , p "relationships"
        ( o
            [ p "product"
                ( o
                    [ linkage chickenThighs
                    ]
                )
            , p "brand"
                ( o
                    [ linkage wittyChicken
                    ]
                )
            ]
        )
    ]


maryFranks =
    [ p "type" ( s "choice")
    , p "id" ( s "57")
    , p "attributes"
        ( o
            [ p "quality" ( i 3)
            ]
        )
    , p "relationships"
        ( o
            [ p "product"
                ( o
                    [ linkage turkeyFranks
                    ]
                )
            , p "brand"
                ( o
                    [ linkage maryO
                    ]
                )
            ]
        )
    ]


rumFranks =
    [ p "type" ( s "choice")
    , p "id" ( s "58")
    , p "attributes"
        ( o
            [ p "quality" ( i 3)
            ]
        )
    , p "relationships"
        ( o
            [ p "product"
                ( o
                    [ linkage turkeyFranks
                    ]
                )
            , p "brand"
                ( o
                    [ linkage rumT
                    ]
                )
            ]
        )
    ]


newComerFranks =
    [ p "type" ( s "choice")
    , p "attributes"
        ( o
            [ p "quality" ( i 3)
            ]
        )
    , p "relationships"
        ( o
            [ p "product"
                ( o
                    [ linkage turkeyFranks
                    ]
                )
            , p "brand"
                ( o
                    [ linkage newComer
                    ]
                )
            ]
        )
    , p "meta"
        ( o
            [ p "new-resource-tag" ( s "NCFRANKSXXXYYYYY")
            ]
        )
    ]


choices =
    [ horizonThighs
    , wittyThighs
    , maryFranks
    , rumFranks
    ]


choice_ids =
    List.map takeId choices
        |> List.map o


choice_objects =
    List.map o choices
