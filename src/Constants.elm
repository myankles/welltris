module Constants exposing
  ( dims
  , dropPieceDelay, dropPieceDelayRatio, fastDropPieceDelay
  , blockWallDelay, foundLinesDelay, dropWallsDelay, endGameDelay
  , wallColor, blockedWallColor, backgroundColors
  )

import Color exposing ( Color )
import Tiles exposing ( ColorTiles )
import Time exposing ( Time, second )


--- SIZE CONSTANTS ---

{-| The dimensions of the board -}

dims : { width: Int, depth : Int, width2d : Int, height2d : Int }
dims =
  let
    width = 8
    depth = 12
  in
    -- 3D width and depth
    { width = width
    , depth = depth

    -- 2D (unwrapped) width and depth
    , width2d = width * 4
    , height2d = width + depth
    }


--- TIME CONSTANTS ---

{-| Length of time between piece drops (at level 1). -}

dropPieceDelay : Time
dropPieceDelay =
  1.0 * second

{-| How much faster each speed level makes the game. -}

dropPieceDelayRatio : Float
dropPieceDelayRatio =
  0.7


{-| Some time constants. -}

fastDropPieceDelay : Time
fastDropPieceDelay =
  0.001 * second


{-| Wall blocking / unblocking animation duration. -}

blockWallDelay : Time
blockWallDelay =
  1.0 * second


{-| Found lines animation duration. -}

foundLinesDelay : Time
foundLinesDelay =
  0.75 * second


{-| Duration of drop walls animation. -}

dropWallsDelay : Time
dropWallsDelay =
  0.2 * second


{-| Duration of end gmae animation. -}

endGameDelay : Time
endGameDelay =
  2.0 * second


--- COLOR CONSTANTS ---


{-| Color of the walls -}

wallColor : Color
wallColor =
  Color.white


blockedWallColor : Color
blockedWallColor =
  Color.rgb 255 25 25

{-\ Background colors for the board. -}

backgroundColors : ColorTiles
backgroundColors =
  let
    -- colors for each wall and the four floors
    wallAndFloorColors =
      [ ( Color.white, Color.white )
      , ( Color.white, Color.white )
      , ( Color.white, Color.white )
      , ( Color.white, Color.white )
      ]
      {--
      [ ( Color.hsl 0.00 0.8 0.8, Color.hsl 0.00 0.6 0.8 )
      , ( Color.hsl 0.25 0.8 0.8, Color.hsl 0.25 0.6 0.8 )
      , ( Color.hsl 0.50 0.8 0.8, Color.hsl 0.50 0.6 0.8 )
      , ( Color.hsl 0.75 0.8 0.8, Color.hsl 0.75 0.6 0.8 )
      ]
      --}

    -- construct a wall with a floor underneath it
    wallAndFloor ( wallColor, floorColor ) =
      Tiles.box dims.width dims.depth wallColor
        |> Tiles.translate 0 dims.width
        |> Tiles.union ( Tiles.box dims.width dims.width floorColor )

    -- append another wall and floor to an existing one
    appendWallAndFloor wallAndFloorColors tiles =
      tiles
        |> Tiles.translate dims.width 0
        |> Tiles.union ( wallAndFloor wallAndFloorColors )

  in
    -- put all four walls together
    List.foldl appendWallAndFloor Tiles.empty wallAndFloorColors
