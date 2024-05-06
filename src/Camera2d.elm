module Camera2d exposing
    ( Camera2d
    , ZoomSpace
    , zoomedAt, fromZoomSpace
    , origin, zoom
    , setOrigin, setZoom, setZoomAtScreenPoint
    , translateBy, translateByScreenVector
    , pointToScene, pointToScreen
    , vectorToScene, vectorToScreen
    , svgViewBox, svgViewBoxWithFocus
    , interpolateFrom, toZoomSpace
    )

{-| A Camera2d maps a 2d scene to a screen.

The camera is defined by an origin point within the scene which is at the
center of the camera, and a zoom ratio mapping scene units onto screen units.

The screen is defined by a bounding box defined in screen space.

The camera and screen together form a mapping from scene units and coordinates
onto screen units and coordinates. Functions are provided to map points in both
directions of this transformation. For example, going from screen to scene lets
you map pointer events into the scene.

Functions are provided for moving the cameras origin point in scene or screen
space, as well as for adjusting the zoom ratio.

A Camera2d is similar to an `ianmackenzie/elm-geometry` `Frame2d` but with the
addition of the zoom ratio.

@docs Camera2d
@docs ZoomSpace


# Constructors

@docs zoomedAt, fromZoomSpace


# Properties

@docs origin, zoom


# Transformations

@docs setOrigin, setZoom, setZoomAtScreenPoint
@docs translateBy, translateByScreenVector


# Mappings between scene and screen space

@docs pointToScene, pointToScreen
@docs vectorToScene, vectorToScreen


# SVG helpers.

@docs svgViewBox, svgViewBoxWithFocus


# Animation

@docs interpolateFrom, toZoomSpace

-}

import BoundingBox2d exposing (BoundingBox2d)
import Frame2d exposing (Frame2d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, Rate)
import Rectangle2d exposing (Rectangle2d)
import TypedSvg.Attributes
import TypedSvg.Core
import Vector2d exposing (Vector2d)


{-| A Camera onto a 2d scene, centered on a particular origin point in the scene
and with a zoom ratio.
-}
type Camera2d units screenUnits coordinates
    = Camera2d
        { zoomLevel : Quantity Float (Rate screenUnits units)
        , sceneFrame : Frame2d units coordinates { defines : coordinates }
        }


{-| ZoomSpace is a mapping of the camera coordinates from (X, Y, Zoom) to
(X, Y, 1 / Zoom). Linear motion of the camera in ZoomSpace maps to linear motion
of the camera in the 3d space above the drawing.

The camera can be thought of as looking down from above onto a drawing on the 2d
XY-plane at Z = 0. The camera has a height above this plane and always keeps the
plane in perfect focus. As the cameras height changes, so does its degree of zoom.

ZoomSpace is linear with respect to this 3d space above the drawing. Linear
motion of the camera in ZoomSpace will produce linear motion of the camera
as imagined this way, and is useful for getting predictable animations of the
camera position and zoom level.

-}
type ZoomSpace screenUnits coordinates
    = ZoomSpace


{-| Creates a camera centered at the origin point with the given zoom ratio.
-}
zoomedAt :
    Point2d units coordinates
    -> Quantity Float (Rate screenUnits units)
    -> Camera2d units screenUnits coordinates
zoomedAt originPoint zoomLevel =
    { zoomLevel = zoomLevel
    , sceneFrame = Frame2d.atPoint originPoint
    }
        |> Camera2d


{-| Maps [ZoomSpace](#ZoomSpace) to camera coords.
-}
fromZoomSpace : Point3d units (ZoomSpace screenUnits coordinates) -> Camera2d units screenUnits coordinates
fromZoomSpace zoomSpacePoint =
    let
        ( x, y, z ) =
            zoomSpacePoint
                |> Point3d.coordinates

        zoomLevel =
            Quantity.at_ (Quantity.unwrap z |> Quantity.unsafe) (Quantity.unsafe 1.0)
    in
    zoomedAt (Point2d.xy x y) zoomLevel


{-| Maps the camera coords to ZoomSpace.

This is useful in conjunction with [fromZoomSpace](#fromZoomSpace) when animating the
camera. See [ZoomSpace](#ZoomSpace) for an explanation of the geometry of ZoomSpace
and how it simplifies the camera motion.

-}
toZoomSpace : Camera2d units screenUnits coordinates -> Point3d units (ZoomSpace screenUnits coordinates)
toZoomSpace (Camera2d { sceneFrame, zoomLevel }) =
    let
        ( x, y ) =
            Frame2d.originPoint sceneFrame
                |> Point2d.coordinates

        z =
            Quantity.at_ (Quantity.unwrap zoomLevel |> Quantity.unsafe) (Quantity.unsafe 1.0)
    in
    Point3d.xyz x y z


{-| Interpolate between one camera position and another in [ZoomSpace](#ZoomSpace).
-}
interpolateFrom :
    Camera2d units screenUnits coordinates
    -> Camera2d units screenUnits coordinates
    -> Float
    -> Camera2d units screenUnits coordinates
interpolateFrom start end progress =
    let
        startZs =
            toZoomSpace start

        endZs =
            toZoomSpace end
    in
    Point3d.interpolateFrom startZs endZs progress
        |> fromZoomSpace


{-| Gets the camera origin point in scene coordinates.
-}
origin : Camera2d units screenUnits coordinates -> Point2d units coordinates
origin (Camera2d { sceneFrame }) =
    Frame2d.originPoint sceneFrame


{-| Gets the cameras current zoom level.
-}
zoom : Camera2d units screenUnits coordinates -> Quantity Float (Rate screenUnits units)
zoom (Camera2d { zoomLevel }) =
    zoomLevel


{-| Shifts the camera origin point to a new location in scene coordinates.
-}
setOrigin :
    Point2d units coordinates
    -> Camera2d units screenUnits coordinates
    -> Camera2d units screenUnits coordinates
setOrigin originPoint (Camera2d camera) =
    { camera | sceneFrame = Frame2d.atPoint originPoint }
        |> Camera2d


{-| Shifts the camera by a vector in screne coordinates.
-}
translateBy :
    Vector2d units coordinates
    -> Camera2d units screenUnits coordinates
    -> Camera2d units screenUnits coordinates
translateBy vec (Camera2d camera) =
    let
        ( x, y ) =
            Vector2d.components vec

        sceneVec =
            Vector2d.xyIn camera.sceneFrame
                x
                y

        sceneFrame =
            Frame2d.translateBy sceneVec camera.sceneFrame
    in
    { camera | sceneFrame = sceneFrame }
        |> Camera2d


{-| Shifts the camera origin point by a vector in screen coordinates.

This can be convenient when working with pointers on the screen.

-}
translateByScreenVector :
    Vector2d screenUnits screenCoordinates
    -> Camera2d units screenUnits coordinates
    -> Camera2d units screenUnits coordinates
translateByScreenVector vec (Camera2d camera) =
    let
        ( x, y ) =
            Vector2d.components vec

        sceneVec =
            Vector2d.xyIn camera.sceneFrame
                (Quantity.at_ camera.zoomLevel x)
                (Quantity.at_ camera.zoomLevel y)

        sceneFrame =
            Frame2d.translateBy sceneVec camera.sceneFrame
    in
    { camera | sceneFrame = sceneFrame }
        |> Camera2d


{-| Adjusts the camera zoom level.

The origin point will not be shifted by this operation. The effect of this on
the screen mapping will be that the zoom is centered on the middle of the
screen, and the origin point remains at the center of the screen.

-}
setZoom :
    Quantity Float (Rate screenUnits units)
    -> Camera2d units screenUnits coordinates
    -> Camera2d units screenUnits coordinates
setZoom zoomLevel (Camera2d camera) =
    { camera | zoomLevel = zoomLevel }
        |> Camera2d


{-| Adjusts the camera zoom around a point on the screen. The point within the
scene corresponding to the screen point, will be at the same screen point after
the zoom adjustment.

This is convenient when adjusting the zoom around a pointer position, allowing
a user to zoom in on a particular area under the pointer.

Note that if the screen point does not align with the origin point, then the
origin point will be shifted by this operation.

-}
setZoomAtScreenPoint :
    Quantity Float (Rate screenUnits units)
    -> Point2d screenUnits screenCoordinates
    -> BoundingBox2d screenUnits screenCoordinates
    -> Camera2d units screenUnits coordinates
    -> Camera2d units screenUnits coordinates
setZoomAtScreenPoint zoomLevel screenPoint frame (Camera2d camera) =
    let
        zoomedCamera =
            { camera | zoomLevel = zoomLevel }
                |> Camera2d

        beforeZoomPoint =
            pointToScene (Camera2d camera) frame screenPoint

        afterZoomPoint =
            pointToScene zoomedCamera frame screenPoint

        vector =
            -- A vector that shifts the screen point back to its original location
            Vector2d.from afterZoomPoint beforeZoomPoint
    in
    zoomedCamera
        |> translateBy vector


{-| Maps a point in scene space to a point in screen space.

This can be useful when overlaying something in screen space onto a drawing in scene
space, for example a menu or other user interface construction that is not part of
the drawing itself.

-}
pointToScreen :
    Camera2d sceneUnits screenUnits sceneCoordinates
    -> BoundingBox2d screenUnits screenCoordinates
    -> Point2d sceneUnits sceneCoordinates
    -> Point2d screenUnits screenCoordinates
pointToScreen (Camera2d { sceneFrame, zoomLevel }) frame point =
    let
        screenFrame : Frame2d screenUnits screenCoordinates defines
        screenFrame =
            BoundingBox2d.centerPoint frame
                |> Frame2d.atPoint

        ( transX, transY ) =
            Point2d.coordinatesIn sceneFrame point
    in
    Point2d.xyIn screenFrame
        (Quantity.at zoomLevel transX)
        (Quantity.at zoomLevel transY)


{-| Maps a point in screen space to a point in scene space.

This can be useful when mapping pointer events onto a drawing, since the pointer
events will be described by their screen coordinates.

-}
pointToScene :
    Camera2d sceneUnits screenUnits sceneCoordinates
    -> BoundingBox2d screenUnits screenCoordinates
    -> Point2d screenUnits screenCoordinates
    -> Point2d sceneUnits sceneCoordinates
pointToScene (Camera2d { sceneFrame, zoomLevel }) frame point =
    let
        screenFrame : Frame2d screenUnits screenCoordinates defines
        screenFrame =
            BoundingBox2d.centerPoint frame
                |> Frame2d.atPoint

        ( transX, transY ) =
            Point2d.coordinatesIn screenFrame point
    in
    Point2d.xyIn sceneFrame
        (Quantity.at_ zoomLevel transX)
        (Quantity.at_ zoomLevel transY)


{-| Maps a vector in scene space to a vector in screen space.

This can be useful when overlaying something in screen space onto a drawing in scene
space, for example a menu or other user interface construction that is not part of
the drawing itself.

-}
vectorToScreen :
    Camera2d sceneUnits screenUnits sceneCoordinates
    -> BoundingBox2d screenUnits screenCoordinates
    -> Vector2d sceneUnits sceneCoordinates
    -> Vector2d screenUnits screenCoordinates
vectorToScreen (Camera2d { sceneFrame, zoomLevel }) frame vector =
    let
        screenFrame : Frame2d screenUnits screenCoordinates defines
        screenFrame =
            BoundingBox2d.centerPoint frame
                |> Frame2d.atPoint

        point =
            Point2d.translateBy vector Point2d.origin

        ( transX, transY ) =
            Point2d.coordinatesIn sceneFrame point
    in
    Vector2d.xyIn screenFrame
        (Quantity.at zoomLevel transX)
        (Quantity.at zoomLevel transY)


{-| Maps a vector in screen space to a vector in scene space.

This can be useful when mapping pointer events onto a drawing, since the pointer
events will be described by their screen coordinates.

-}
vectorToScene :
    Camera2d sceneUnits screenUnits sceneCoordinates
    -> BoundingBox2d screenUnits screenCoordinates
    -> Vector2d screenUnits screenCoordinates
    -> Vector2d sceneUnits sceneCoordinates
vectorToScene (Camera2d { sceneFrame, zoomLevel }) frame vector =
    let
        screenFrame : Frame2d screenUnits screenCoordinates defines
        screenFrame =
            BoundingBox2d.centerPoint frame
                |> Frame2d.atPoint

        point =
            Point2d.translateBy vector Point2d.origin

        ( transX, transY ) =
            Point2d.coordinatesIn screenFrame point
    in
    Vector2d.xyIn sceneFrame
        (Quantity.at_ zoomLevel transX)
        (Quantity.at_ zoomLevel transY)


{-| Given a camera and a rectangular frame where a drawing will be rendered on
screen, provides the SVG `viewBox` parameters that will yield a correctly scaled
and translated scene space described by that camera.

The screen coordinate defined frame will have the cameras origin point at its
center, and the frame will be the same size as the whole SVG element. See
[svgViewBoxWithFocus](#svgViewBoxWithFocus) for a version where the camera is
centered in a different frame.

-}
svgViewBox :
    Camera2d sceneUnits screenUnits sceneCoordinates
    -> BoundingBox2d screenUnits screenCoordinates
    -> TypedSvg.Core.Attribute a
svgViewBox ((Camera2d { zoomLevel }) as camera) screen =
    let
        ( w, h ) =
            BoundingBox2d.dimensions screen
                |> Tuple.mapBoth Quantity.unwrap Quantity.unwrap

        { x, y } =
            BoundingBox2d.extrema screen
                |> (\e -> Point2d.xy e.minX e.minY)
                |> pointToScene camera screen
                |> Point2d.unwrap

        scaleFactor =
            zoomLevel |> Quantity.unwrap
    in
    TypedSvg.Attributes.viewBox
        (round x |> toFloat)
        (round y |> toFloat)
        (round (w / scaleFactor) |> toFloat)
        (round (h / scaleFactor) |> toFloat)


{-| Given a camera and a rectangular frame where a drawing will be rendered on
screen, provides the SVG `viewBox` parameters that will yield a correctly scaled
and translated scene space described by that camera.

In this case two screen coordinate defined frames are provided to set up the camera.
The first defines the area where the camera is considered to be focussed and centered
on, but this does not have to occupy the whole of the SVG element. The second defines
the boundary of the actual SVG element. Typically the first frame is wholly inside
the second one, but it does not have to be.

This is useful if you want a camera that does not fill all of an SVG, you might have
areas that show other things or some kind of overlay that you consider to be acting
as a boundary to the camera frame.

If you have an SVG that fills the whole `window`, and want the camera focussed on a
`frame` inside that, this would give you the correct SVG viewBox parameters:

    Camera2d.svgViewBoxWithFocus
        camera
        frame
        window

-}
svgViewBoxWithFocus :
    Camera2d sceneUnits screenUnits sceneCoordinates
    -> BoundingBox2d screenUnits screenCoordinates
    -> BoundingBox2d screenUnits screenCoordinates
    -> TypedSvg.Core.Attribute a
svgViewBoxWithFocus ((Camera2d { zoomLevel }) as camera) frame screen =
    let
        ( w, h ) =
            BoundingBox2d.dimensions screen
                |> Tuple.mapBoth Quantity.unwrap Quantity.unwrap

        -- find the top left of the screen relative to the focus frame
        translateToFocus =
            Vector2d.from
                (BoundingBox2d.centerPoint screen)
                (BoundingBox2d.centerPoint frame)

        { x, y } =
            BoundingBox2d.extrema screen
                |> (\e -> Point2d.xy e.minX e.minY)
                --|> Point2d.translateBy translateToFocus
                |> pointToScene camera frame
                |> Point2d.unwrap

        scaleFactor =
            zoomLevel |> Quantity.unwrap
    in
    TypedSvg.Attributes.viewBox
        (round x |> toFloat)
        (round y |> toFloat)
        (round (w / scaleFactor) |> toFloat)
        (round (h / scaleFactor) |> toFloat)
