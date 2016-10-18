module Board.View
    exposing
        ( BoardView
        , view2d
        , view3d
        , view3dPerspective
        )

{-| Acceleration data structure to view a board.
-}

import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)
import Constants exposing (dims)
import Dict exposing (Dict)
import List.Extra
import Math.Vector2 exposing (Vec2, vec2, add, sub, getX, getY, toTuple)
import String
import Svg
import Svg.Attributes exposing (fill, stroke, strokeWidth, points)
import Tiles exposing (ColorTiles)
import Vec2i exposing (Vec2i)


{-| Acceleration data structure to view a board.
-}
type alias BoardView msg =
    ColorTiles -> Svg.Svg msg


{-| A tuple of vectors of the corners of a quad.
-}
type alias Quad =
    ( Vec2, Vec2, Vec2, Vec2 )


{-| Takes a 2D tile location to a polygon to a quad
-}
type alias QuadMap =
    Dict Vec2i Quad



{- A tranformation in planar coordiantes -}


type alias XForm2D =
    Vec2 -> Vec2


{-| Creates a 2d board viewer.

  view2d width depth pixels

Creates a viewer for a board with size ( width X width X depth ) rendering
into a square box of size ( pixles X pixels ).
-}
view2d : Float -> BoardView msg
view2d pixels =
    let
        ( widthPx, heightPx ) =
            if dims.width2d > dims.height2d then
                ( pixels, pixels * (toFloat dims.height2d) / (toFloat dims.width2d) )
            else
                ( pixels * (toFloat dims.width2d) / (toFloat dims.height2d), pixels )

        boardQuad =
            ( vec2 0.0 heightPx
            , vec2 widthPx heightPx
            , vec2 widthPx 0.0
            , vec2 0.0 0.0
            )
    in
        quadMap ( 0, 0 ) ( dims.width2d, dims.height2d ) boardQuad |> view 0.1


{-| Creates a 3d board viewer.

  view2d width depth pixels floorPercent

Creates a viewer for a board with size ( width X width X depth ) rendering
into a square box of size ( pixles X pixels ) with the floor covering
floorPercent % of the ground.

-}
view3d : Float -> Float -> BoardView msg
view3d pixels floorPercent =
    let
        {-
           out3 +---------+ out2
                | \     / |
               in3 +---+ in2
                |  |   |  |
               in0 +---+ in1
                | /     \ |
           out0 +---------+ out1
        -}
        out0 =
            0.0

        out1 =
            pixels

        innerWidthPx =
            floorPercent * pixels

        in0 =
            (out1 - innerWidthPx) * 0.5

        in1 =
            (out1 + innerWidthPx) * 0.5

        -- image-space outer coordinates
        iOut0 =
            vec2 out0 out0

        iOut1 =
            vec2 out1 out0

        iOut2 =
            vec2 out1 out1

        iOut3 =
            vec2 out0 out1

        -- image-space inner coordinates
        ( iIn0, iIn1, iIn2, iIn3 ) =
            ( vec2 in0 in0
            , vec2 in1 in0
            , vec2 in1 in1
            , vec2 in0 in1
            )

        -- wall transformations
        ( floor, wall1, wall2, wall3, wall4 ) =
            ( ( iIn3, iIn2, iIn1, iIn0 )
            , ( iIn0, iIn1, iOut1, iOut0 )
            , ( iIn1, iIn2, iOut2, iOut1 )
            , ( iIn2, iIn3, iOut3, iOut2 )
            , ( iIn3, iIn0, iOut0, iOut3 )
            )

        -- width and height
        ( w, d ) =
            ( dims.width, dims.depth )
    in
        quadMap ( 0, 0 ) ( w, w ) floor
            |> Dict.union (quadMap ( 0 * w, w ) ( w, d ) wall1)
            |> Dict.union (quadMap ( 1 * w, w ) ( w, d ) wall2)
            |> Dict.union (quadMap ( 2 * w, w ) ( w, d ) wall3)
            |> Dict.union (quadMap ( 3 * w, w ) ( w, d ) wall4)
            |> view 0.5


view3dPerspective : Float -> Float -> BoardView msg
view3dPerspective pixels floorPercent =
    let
        ( w, d, wf, df ) =
            ( dims.width, dims.depth, toFloat dims.width, toFloat dims.depth )

        perspective ( x, y, z ) =
            vec2 (alpha * x / (beta + z)) (alpha * y / (beta + z))

        alpha =
            pixels * beta / wf

        beta =
            floorPercent * df / (1.0 - floorPercent)

        screenXform xy =
            vec2 ( pixels / 2.0 + ( getX xy )) ( pixels / 2.0 - ( getY xy ))

        plane w h minI minJ xform =
            List.Extra.lift2 (,) [0..w - 1] [0..h - 1]
                |> List.map
                    (\( i, j ) ->
                        ( ( i + minI, j + minJ)
                        , ( screenXform <| perspective <| xform ( i, j )
                          , screenXform <| perspective <| xform ( i + 1, j )
                          , screenXform <| perspective <| xform ( i + 1, j + 1 )
                          , screenXform <| perspective <| xform ( i, j + 1 )
                          )
                        )
                    )
                |> Dict.fromList

        floor ( i, j ) =
            (-wf / 2.0 + (toFloat i), -wf / 2.0 + (toFloat j), df)

        wall0 ( i, j ) =
            ((toFloat i) - wf / 2.0, wf / 2.0, df - (toFloat j))

        wall1 ( i, j ) =
            (wf / 2.0, wf / 2.0 - (toFloat i), df - (toFloat j))

        wall2 ( i, j ) =
            (wf / 2.0 - (toFloat i), -wf / 2.0, df - (toFloat j))

        wall3 ( i, j ) =
            (-wf / 2.0, (toFloat i) - wf / 2.0, df - (toFloat j))

    in
        plane w w 0 0 floor
            |> Dict.union ( plane w d ( 0 * w ) w wall0 )
            |> Dict.union ( plane w d ( 1 * w ) w wall1 )
            |> Dict.union ( plane w d ( 2 * w ) w wall2 )
            |> Dict.union ( plane w d ( 3 * w ) w wall3 )
            |> view 0.5

{-| View this Tiles
-}
view : Float -> QuadMap -> BoardView msg
view strokeWidthPx quadMap tiles =
    let
        scale : Int -> Color -> Color
        scale amt color =
            let
                rgb =
                    Color.toRgb color
            in
                Color.rgb
                    -- ( round ( amt * ( toFloat rgb.red )))
                    -- ( round ( amt * ( toFloat rgb.green )))
                    -- ( round ( amt * ( toFloat rgb.blue )))
                    (rgb.red - amt)
                    (rgb.green - amt)
                    (rgb.blue - amt)

        toSVGPoly : ( Vec2i, Color ) -> Maybe (Svg.Svg msg)
        toSVGPoly ( ij, fillColor ) =
            Dict.get ij quadMap
                |> Maybe.map
                    (\quad ->
                        Svg.polygon
                            [ fill (colorToCssRgb fillColor)
                            , stroke (colorToCssRgb (scale 20 fillColor))
                            , strokeWidth "1"
                            , points (toSvgPoints quad)
                            , strokeWidth (toString strokeWidthPx)
                            ]
                            []
                    )
    in
        Tiles.toList tiles
            |> List.filterMap toSVGPoly
            |> Svg.g []


{-| Creates a map of quads.

  quadMap ( i, j ) ( w, h ) quad

Maps the lattice [i.. i+w] X [j.. j+h] evenly into the given quad.``
-}
quadMap : Vec2i -> Vec2i -> Quad -> QuadMap
quadMap ( i, j ) ( w, h ) quad =
    let
        vec2i i j =
            Vec2i.vec2i i j |> Vec2i.toVec2f

        ijs =
            List.Extra.lift2 (,) [i..(i + w - 1)] [j..(j + h - 1)]

        xform =
            skew ( (vec2i i j), (vec2i w h) ) quad

        unitQuad lowerLeftCorner =
            ( xform <| lowerLeftCorner
            , xform <| lowerLeftCorner `add` (vec2i 1 0)
            , xform <| lowerLeftCorner `add` (vec2i 1 1)
            , xform <| lowerLeftCorner `add` (vec2i 0 1)
            )
    in
        ijs
            |> List.map (uncurry vec2i >> unitQuad)
            |> List.map2 (,) ijs
            |> Dict.fromList


{-| Convers a quad into an SVG string
-}
toSvgPoints : Quad -> String
toSvgPoints ( a, b, c, d ) =
    [ a, b, c, d ]
        |> List.map
            (\v ->
                (toString <| getX v) ++ "," ++ (toString <| getY v)
            )
        |> String.join " "


{-|
Creates a skew transform which takes the box [i,i+w]*[j,j+h] to the
quadrilateral with corners (a, b, c, d) as follows:

  (i,j+h)  (i+w,j+h)         D (x3, y3)
  D--------C                / \
  |        | __________\   /   \
  |        |           /  /     C (x2,y2)
  A--------B     (x0,y0) A     /
  (i,j)   (i+w,j)         \   /
                           \ /
                            B (x1,y1)
-}
skew : ( Vec2, Vec2 ) -> Quad -> XForm2D
skew ( ij, wh ) ( a, b, c, d ) xy =
    let
        ( u, v ) =
            Math.Vector2.toTuple <| (xy `sub` ij) `div` wh
    in
        vec2 0.0 0.0
            |> add (Math.Vector2.scale ((1.0 - u) * (1.0 - v)) a)
            |> add (Math.Vector2.scale ((u) * (1.0 - v)) b)
            |> add (Math.Vector2.scale ((u) * (v)) c)
            |> add (Math.Vector2.scale ((1.0 - u) * (v)) d)


{-| Divide two vectors
-}
div : Vec2 -> Vec2 -> Vec2
div v1 v2 =
    let
        ( x1, y1 ) =
            Math.Vector2.toTuple v1

        ( x2, y2 ) =
            Math.Vector2.toTuple v2
    in
        vec2 (x1 / x2) (y1 / y2)


{-| Default outline color
-}
outlineColor : Color
outlineColor =
    Color.rgb 220 220 220
