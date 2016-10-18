module Tests exposing (..)

import Board
import Board.Mode
import Board.Topology
import Expect
import Set
import Test exposing (..)
import Tiles

-- test pattern for the line searchers
addTestPattern : List ( Int, Int ) -> Set.Set ( Int, Int )
addTestPattern coords =
  -- first test pattern
  [ [(0,0),(6,0),(0,6),(6,6)]

  -- second test pattern
  , [(0,8),(9,9),(18,10),(27,11)]

  -- user specified coordinates
  , coords
  ]
    |> List.foldl List.append []
    |> Set.fromList

all : Test
all =
  describe "Welltris"

    [ test "Wall 0 Drop" <|
      \() ->
        Board.runUntilCollision
          { tiles = Tiles.s1
          , x = 4, y = 18, rot = 0
          }
          Set.empty
        |> Expect.equal
          ( Set.fromList [(4,0),(4,1),(5,1),(5,2)] )

    , test "Wall 1 Drop" <|
      \() ->
        Board.runUntilCollision
          { tiles = Tiles.s1
          , x = 10, y = 18, rot = 1
          }
          Set.empty
        |> Expect.equal
          ( Set.fromList [(0,3),(0,4),(1,4),(1,5)] )

    , test "Wall 2 Drop" <|
      \() ->
        Board.runUntilCollision
          { tiles = Tiles.s1
          , x = 22, y = 18, rot = 2
          }
          Set.empty
        |> Expect.equal
          ( Set.fromList [(0,5),(0,6),(1,6),(1,7)] )

    , test "Wall 3 Drop" <|
      \() ->
        Board.runUntilCollision
          { tiles = Tiles.s1
          , x = 24, y = 18, rot = 3
          }
          Set.empty
        |> Expect.equal
          ( Set.fromList [(6,0),(6,1),(7,1),(7,2)] )

    , test "Corner 0-1 Drop" <|
      \() ->
        Board.runUntilCollision
          { tiles = Tiles.s1
          , x = 7, y = 18, rot = 0
          }
          Set.empty
        |> Expect.equal
          ( Set.fromList [(1,7),(2,7),(7,0),(7,1)] )

    , test "Wall 0 Drop and Collision" <|
      \() ->
        Board.runUntilCollision
          { tiles = Tiles.s1
          , x = 4, y = 18, rot = 0
          }
          ( Set.fromList [(4,0),(4,1),(5,1),(5,2)] )
        |> Expect.equal
          ( Set.fromList[(4,0),(4,1),(4,2),(4,3),(5,1),(5,2),(5,3),(5,4)] )

    , test "Corner 0-1 Drop and Collision" <|
      \() ->
        Board.runUntilCollision
          { tiles = Tiles.s1
          , x = 7, y = 18, rot = 0
          }
          ( Set.fromList [(1,7),(2,7),(7,0),(7,1)] )
        |> Expect.equal
          ( Set.fromList [(1,7),(2,7),(3,7),(4,7),(7,0),(7,1),(7,2),(7,3)] )

    , test "Find 1 Line - Wall 0" <|
      \() ->
        Board.runThroughLineFind
          ( addTestPattern [(0,5),(1,5),(2,5),(3,5),(4,5),(5,5),(6,5),(7,5)] )
        |> Expect.equal
          ( Set.fromList [(0,0),(0,5),(0,7),(6,0),(6,5),(9,9),(18,10),(27,11)] )

    , test "Find 1 line - Wall 1" <|
      \() ->
        Board.runThroughLineFind
          ( addTestPattern [(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7)] )
        |> Expect.equal
          ( Set.fromList [(0,0),(0,6),(0,8),(5,0),(5,6),(9,8),(18,10),(27,11)] )

    , test "Find 1 Line - Wall 2" <|
      \() ->
        Board.runThroughLineFind
          ( addTestPattern [(0,3),(1,3),(2,3),(3,3),(4,3),(5,3),(6,3),(7,3)] )
        |> Expect.equal
          ( Set.fromList [(0,1),(0,6),(0,8),(6,1),(6,6),(9,9),(18,9),(27,11)] )

    , test "Find 1 Line - Wall 3" <|
      \() ->
        Board.runThroughLineFind
          ( addTestPattern [(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7)] )
        |> Expect.equal
          ( Set.fromList [(0,8),(1,0),(1,6),(6,0),(6,6),(9,9),(18,10),(27,10)] )

    , test "Find 2 Lines - Walls 0 and 2" <|
      \() ->
        Board.runThroughLineFind
          ( addTestPattern
            [(0,5),(1,5),(2,5),(3,5),(4,5),(5,5),(6,5),(7,5)
            ,(0,3),(1,3),(2,3),(3,3),(4,3),(5,3),(6,3),(7,3)
            ]
          )
        |> Expect.equal
          ( Set.fromList [(0,1),(0,5),(0,7),(6,1),(6,5),(9,9),(18,9),(27,11)] )

    , test "Find 2 Lines - Two crossing lines" <|
      \() ->
        Board.runThroughLineFind
          ( addTestPattern
            [(0,5),(1,5),(2,5),(3,5),(4,5),(5,5),(6,5),(7,5)
            ,(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7)
            ]
          )
        |> Expect.equal
          ( Set.fromList [(0,7),(1,0),(1,5),(6,0),(6,5),(9,9),(18,10),(27,10)] )

    , test "Find 3 Lines - With one Cross" <|
      \() ->
        Board.runThroughLineFind
          ( addTestPattern
            [(0,5),(1,5),(2,5),(3,5),(4,5),(5,5),(6,5),(7,5)
            ,(0,7),(1,7),(2,7),(3,7),(4,7),(5,7),(6,7),(7,7)
            ,(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7)
            ]
          )
        |> Expect.equal
          ( Set.fromList [(0,6),(1,0),(1,5),(6,0),(6,5),(9,9),(18,10),(27,10)] )

    , test "Color topology test (before update)." <|
      \() ->
        Board.debugTopologyRegressionBoard
          |> Board.tiles
          |> Board.Topology.colorFloorTopologicallyCorrect
          |> Expect.true "Expected the color topology to be correct."

    , test "Color topology test (after update)." <|
      \() ->
        Board.debugTopologyRegressionBoard
          |> Board.update ( Board.Tick 0.0 )
          |> Board.tiles
          |> Board.Topology.colorFloorTopologicallyCorrect
          |> Expect.true "Expected the color topology to be correct."

    , test "Unblocking works." <|
      \() ->
        Board.runUntilCollisionWithBlockedWalls
          { tiles = Tiles.bar
          , x = 19, y = 7, rot = 2
          }
          [ Board.Mode.Unblocked, Board.Mode.BlockedFor 1
          , Board.Mode.Unblocked, Board.Mode.Unblocked ]
          ( Set.fromList
              [ (0,4),(0,5),(4,4),(5,4),(5,5),(5,6),(5,7),(6,6),(11,8)
              , (12,8),(12,9),(13,9) ]
          )
        |> Expect.equal
          ( Set.fromList
              [ (0,4),(0,5),(4,0),(4,1),(4,2),(4,3),(4,4),(5,4),(5,5)
              , (5,6),(5,7),(6,3),(6,4),(6,6),(7,2),(7,3) ]
          )

    , test "Complex Line Finding Example" <|
      \() ->
        Board.runUntilCollision
          { tiles = Tiles.l1
          , x = 16, y = 7, rot = 2
          }
          ( Set.fromList
              [ (0,4),(0,5),(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(2,0)
              , (2,3),(2,4),(2,5),(2,6),(2,7),(3,0),(3,3),(3,4),(3,5),(3,6)
              , (3,7),(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(5,1),(5,2)
              , (5,3),(5,4),(5,5),(5,6),(5,7),(6,2),(6,3),(6,4),(6,6),(7,2)
              , (7,3),(12,8),(12,9),(13,8),(14,8),(19,8),(20,8),(20,9)
              ]
          )
        |> Expect.equal
          ( Set.fromList
              [ (0,4),(0,5),(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(2,0)
              , (2,3),(2,4),(2,5),(2,6),(2,7),(3,0),(3,3),(3,4),(3,5),(3,6)
              , (3,7),(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(5,0),(5,2)
              , (5,3),(5,4),(5,6),(6,0),(6,1),(6,2),(6,3),(7,1),(7,2),(7,3)
              , (12,8),(19,8),(20,8),(20,9)
              ]
          )

    , test "Complex post-line dropping example" <|
      \() ->
        Board.runUntilCollision
          { tiles = Tiles.square
          , x = 6, y = 7, rot = 3
          }
          ( Set.fromList
              [ (0,3),(0,4),(0,5),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,0)
              , (3,3),(3,4),(4,0),(4,1),(4,2),(4,3),(4,4),(5,0),(5,1),(5,2)
              , (6,2),(7,2),(7,3),(7,4),(11,8),(11,9),(12,9),(13,9)
              ]
          )
        |> Expect.equal
          ( Set.fromList
              [ (0,4),(1,4),(2,4),(3,1),(4,1),(4,2),(4,3),(5,1),(5,2),(5,3)
              , (6,3),(7,3),(7,4),(11,8),(12,8),(13,8)
              ]
          )
    ]
