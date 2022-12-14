**Contacts for Support**
- @rupertlssmith on https://elmlang.slack.com
- @rupert on https://discourse.elm-lang.org

# elm-2d-camera

`elm-2d-camera` provides a camera for projecting, panning and zooming 2d drawings.

The camera can be thought of as looking down from above onto a drawing on the 2d XY-plane at
Z = 0. The camera has a height above this plane and always keeps the plane in perfect focus. As 
the cameras height changes, so does its degree of zoom.

Alternatively you can think of the camera as being at a fixed z-height, but with a zoom lens.

The camera is specified by an origin point, which is where on the drawing the center of 
the camera is looking, and a zoom level. The zoom level is a ratio describing how many
scene units map onto how many projection units. Typically projection units are `Pixel`s
on the screen. The documentation in the code refers to the projection space as 'screen 
space'.

The basic functions of the camera are to: Map between the scene space and screen
space in orrder to project a drawing onto the screen; to map the screen space onto the scene space to project user touch and mouse gestures onto an interactive drawing.

There are functions to zoom the camera whilst keeping a screen space point in a fixed position. This is typically used for centering zooms around a pinch or a mouse wheel gesture in a specific area of the screen.

There are also functions that can help animate the camera as it pans and changes
zoom level. If the zoom level is changed linearly with the camera position it will
appear to move in a non-straight line. However, if the camera model where it is 
defined by a height above a drawing but with a fixed field-of-view lens is used, 
the camera can be made to move in an apparent straight line.