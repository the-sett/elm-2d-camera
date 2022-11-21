module Params exposing (view)

import Camera2d exposing (Camera2d)
import Geometry exposing (Scene, Screen)
import Html as H exposing (Html)
import Html.Attributes as HA
import Json.Encode as Encode
import Pixels exposing (Pixels)
import Point2d
import Quantity exposing (Unitless)


type alias Params a =
    { a
        | camera : Camera2d Unitless Pixels Scene
    }


view : Params a -> Html msg
view model =
    let
        zoomPct =
            Quantity.at (Camera2d.zoom model.camera) (Quantity.float 1.0)
                |> Pixels.toFloat
                |> (*) 100.0
                |> round
                |> String.fromInt

        cameraOrigin =
            Camera2d.origin model.camera
                |> Point2d.toRecord Quantity.toFloat

        focus =
            "{ x = "
                ++ (cameraOrigin.x |> round |> String.fromInt)
                ++ ", y = "
                ++ (cameraOrigin.y |> round |> String.fromInt)
                ++ " }"
    in
    H.div
        [ HA.classList
            [ ( "noselect", True )
            , ( "params-container", True )
            ]
        , Encode.string "x" |> HA.property "data-gb-id"
        , Encode.string "none" |> HA.property "data-gb-action"
        , HA.id "params"
        ]
        [ H.h3 [] [ H.text "Camera" ]
        , H.div
            []
            [ H.div []
                [ H.text "Zoom "
                , zoomPct ++ "\u{2009}%" |> H.text
                ]
            ]
        , H.div
            []
            [ H.div []
                [ H.text "Origin "
                , focus |> H.text
                ]
            ]
        , H.br [] []
        , H.div [] [ H.text "Double click the target to zoom it." ]
        ]
