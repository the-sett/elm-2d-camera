**Contacts for Support**
- @rupertlssmith on https://elmlang.slack.com
- @rupert on https://discourse.elm-lang.org

# elm-2d-camera

`elm-2d-camera` provides a Camera for projecting, panning and zooming 2d drawings.

The camera can be thought of as looking down from above onto a drawing on the 2d xy-plane at z=0. The camera has a height above this plane and always keeps the 
plane in perfect focus. As the cameras height changes, so does its degree of zoom.

Alternatively you can think of the camera as being at a fixed z-height, but with a zoom lens.

The camera is specified by an origin point, which is where on the drawing the center of 
the camera is looking, and a zoom level. The zoom level is a ratio describing how many
scene units map onto how many projection units. Typically projection units are `Pixel`s
on the screen. The documentation in the code refers to the projection space as 'screen 
space'.