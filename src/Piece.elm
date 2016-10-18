module Piece exposing
  ( Model, Seed
  , project
  , intersectsFloor, intersectsWall, intersectsBlockedWall
  , random
  )

import Constants exposing ( dims )
import Board.Topology
import List.Extra
import Random
import Random.Extra
import Tiles exposing ( ColorTiles )
import Util

{-| A piece which can drop. -}

type alias Model =
  { tiles : ColorTiles
  , x : Int
  , y : Int
  , rot : Int
  }


{-| A random seed -}

type alias Seed =
  Int


{-| piece constructor -}

piece : ColorTiles -> Int -> Int -> Int -> Model
piece tiles x y rot =
  { tiles = tiles, x = x, y = y, rot = rot }


{-| "project" a piece onto the board, given its translation rotation, etc. -}

project : Model -> ColorTiles
project piece =
  piece.tiles
  |> Tiles.rotate ( piece.rot % 4 )
  |> Tiles.translate piece.x piece.y
  |> Board.Topology.wrapX
  |> Board.Topology.wrapColorFloor
  |> Board.Topology.unwrapColorFloor


{-| Returns true of the piece intersects the floor -}

intersectsFloor : Model -> Bool
intersectsFloor =
  project >> Tiles.toList >> List.any (\(( _, y ), _ ) -> y < dims.width )


{-| Return true if the piece interscts the wall with given index. -}

intersectsWall : Int -> Model -> Bool
intersectsWall wallIndex =
  project >> Tiles.toList >> List.map fst
    >> List.any ( Board.Topology.onWall wallIndex )


{-| Returns true of a piece intersects a blocked wall. -}

intersectsBlockedWall : List Bool -> Model -> Bool
intersectsBlockedWall wallsAreBlocked piece =
  [0 .. 3]
  |> List.Extra.zip wallsAreBlocked
  |> List.filter fst
  |> List.map ( snd >> intersectsWall )
  |> List.any ((|>) piece)

{-| Generate a random piece. -}

random : Seed -> List Bool -> ( Model, Seed )
random seed wallsAreBlocked =
  let
    generateTiles =
      [ Tiles.bar, Tiles.l1, Tiles.l2
      , Tiles.s1, Tiles.s2, Tiles.t, Tiles.square ]
      |> Random.Extra.sample
      |> Random.map ( Maybe.withDefault Tiles.bar )

    generateRotation =
      Random.int 0 3

    pieceIntersectsBlockedWalls =
      intersectsBlockedWall wallsAreBlocked

    generatePiece : Random.Generator Model
    generatePiece =
      Random.pair generateTiles generateRotation
      |> Random.Extra.flatMap (\( tiles, rotation ) ->
          [0 .. ( dims.width2d - 1 )]
          |> List.map ( \x -> piece tiles x dims.height2d rotation )
          |> List.filter ( not << pieceIntersectsBlockedWalls )
          |> Random.Extra.sample
          |> Random.map ( Maybe.withDefault ( piece tiles -1 -1 rotation )))

  in
    Util.randomStepWithIntSeed generatePiece seed
