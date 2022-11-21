module Style exposing (global)

import Css
import Css.Global


printBlack =
    Css.rgb 25 25 25


global : List Css.Global.Snippet
global =
    [ Css.Global.html
        [ Css.pct 100 |> Css.height ]
    , Css.Global.body
        [ Css.pct 100 |> Css.height
        , Css.overflow Css.hidden
        , Css.touchAction Css.none
        ]
    , Css.Global.class "noselect"
        [ Css.property "user-select" "none"
        , Css.property "-moz-user-select" "none"
        , Css.property "-webkit-user-select" "none"
        , Css.property "-ms-user-select" "none"
        ]
    , Css.Global.selector "::selection"
        [ Css.backgroundColor (Css.rgb 196 195 217)
        ]
    , Css.Global.class "params-container"
        [ Css.rgb 250 250 250 |> Css.backgroundColor
        , Css.position Css.fixed
        , Css.border3 (Css.px 3) Css.solid printBlack
        , Css.borderRadius (Css.px 5)
        , Css.padding2 (Css.px 20) (Css.px 20)
        , Css.margin2 (Css.px 10) (Css.px 10)
        , Css.left (Css.px 0)
        , Css.top (Css.px 0)
        , Css.zIndex (Css.int 2000)
        ]
    , Css.Global.selector "h3"
        [ Css.px 6 |> Css.marginTop
        , Css.px 10 |> Css.marginBottom
        ]
    ]
