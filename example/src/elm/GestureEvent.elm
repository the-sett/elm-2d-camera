module GestureEvent exposing
    ( GestureAction(..)
    , GestureEvent(..)
    , gestureDecoder
    )

import BoundingBox2d
import Geometry exposing (BScreen, PScreen)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as DE
import Pixels
import Point2d
import Pointer exposing (EventKind(..))



-- Gesture events describing user pointer actions on the gameboard.


type GestureEvent
    = Root (Maybe PScreen)
    | ItemWithId String GestureAction (Maybe BScreen) (Maybe PScreen)


type GestureAction
    = ActionSelect
    | ActionMoveCamera



-- Codecs


gestureDecoder : String -> EventKind -> Decoder GestureEvent
gestureDecoder rootId _ =
    keyDecoder
        |> Decode.andThen
            (\ctrlKey ->
                Decode.succeed
                    (\maybePos targetFn -> targetFn maybePos)
                    |> DE.andMap (Decode.maybe positionDecoder)
                    |> DE.andMap
                        (Decode.field "target"
                            (clickableDecoder rootId ctrlKey)
                        )
            )


positionDecoder : Decoder PScreen
positionDecoder =
    Decode.map2 Point2d.pixels
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


keyDecoder : Decoder Bool
keyDecoder =
    Decode.map2
        (\ctrl meta -> ctrl || meta)
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "metaKey" Decode.bool)


boundingClientRectDecoder : Decoder BScreen
boundingClientRectDecoder =
    Decode.succeed
        (\x y w h ->
            BoundingBox2d.fromExtrema
                { minX = Pixels.float x
                , minY = Pixels.float y
                , maxX = (x + w) |> Pixels.float
                , maxY = (y + h) |> Pixels.float
                }
        )
        |> DE.andMap (Decode.field "x" Decode.float)
        |> DE.andMap (Decode.field "y" Decode.float)
        |> DE.andMap (Decode.field "width" Decode.float)
        |> DE.andMap (Decode.field "height" Decode.float)


clickableDecoder : String -> Bool -> Decoder (Maybe PScreen -> GestureEvent)
clickableDecoder rootId ctrlKey =
    Decode.oneOf
        [ actionDecoder ctrlKey
        , rootDecoder rootId
        , Decode.lazy
            (\_ ->
                clickableDecoder rootId ctrlKey
                    |> Decode.field "parentElement"
            )
        , Decode.succeed Root
        ]


actionDecoder : Bool -> Decoder (Maybe PScreen -> GestureEvent)
actionDecoder ctrlKey =
    Decode.succeed (\id action bbox -> ItemWithId id action bbox)
        |> DE.andMap (Decode.field "data-drawing-id" Decode.string)
        |> DE.andMap
            (Decode.maybe
                (Decode.field "data-drawing-action" Decode.string
                    |> Decode.andThen gestureActionDecoder
                )
                |> Decode.andThen
                    (\maybePrimAction ->
                        case ( ctrlKey, maybePrimAction ) of
                            ( False, Just primAction ) ->
                                primAction |> Decode.succeed

                            _ ->
                                Decode.fail "No action"
                    )
            )
        |> DE.andMap
            (Decode.field "elmBoundingClientRect" boundingClientRectDecoder
                |> Decode.maybe
            )


gestureActionDecoder : String -> Decoder GestureAction
gestureActionDecoder val =
    case val of
        "select" ->
            Decode.succeed ActionSelect

        "moveCamera" ->
            Decode.succeed ActionMoveCamera

        _ ->
            Decode.fail "Unkown Gesture Action"


rootDecoder : String -> Decoder (Maybe PScreen -> GestureEvent)
rootDecoder rootId =
    Decode.succeed Root
        |> DE.when (Decode.field "id" Decode.string) ((==) rootId)
