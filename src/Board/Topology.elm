module Board.Topology exposing
  ( wall
  , onWall, onAWall, partitionWall
  , wrapX
  , wrapColorFloor, unwrapColorFloor
  , colorFloorTopologicallyCorrect
  , wrapVecFloor, keepOnlyFloor0
  )

import Math.Vector2 exposing ( vec2 )
import Tiles exposing ( ColorTiles, Vec2Tiles )
import Util exposing ( Monoid, Transform )
import Vec2i
import Constants exposing ( dims )

{-| Returns the tiles on the specified wall. -}

wall : Int -> a -> Tiles.Tiles a
wall wallIndex x =
  Tiles.box dims.width dims.depth x
    |> Tiles.translate ( wallIndex * dims.width ) dims.width


{-| Returns true if this point lies on the wall -}

onWall : Int -> Vec2i.Vec2i -> Bool
onWall wallIndex ( x, y ) =
  x >= dims.width * wallIndex && x < dims.width * ( wallIndex + 1 )
    && y >= dims.width


{-| Returns true if this point lies on one of the blocked walls -}
onAWall : List Bool -> Vec2i.Vec2i -> Bool
onAWall blockedWalls xy =
  blockedWalls |> List.indexedMap (,)
    |> List.any (\( wallIndex, blocked ) -> blocked && onWall wallIndex xy )


{-| Partion these tiles into ( on wall i, not on wall i ). -}

partitionWall : Int -> Tiles.Tiles a -> ( Tiles.Tiles a, Tiles.Tiles a )
partitionWall wallIndex tiles =
  let
    ( wallIj, wallWh ) =
      (( dims.width * wallIndex, dims.width ), ( dims.width, dims.depth ))
  in
    ( Tiles.extractBox wallIj wallWh tiles
    , Tiles.clearBox wallIj wallWh tiles )


{-| Wrap a piece around the x axis. -}

wrapX : ColorTiles -> ColorTiles
wrapX
  =  Tiles.toList
  >> List.map ( \(( x, y ), color ) -> (( x % dims.width2d, y ), color ) )
  >> Tiles.fromList


{-| Rotate floors 1 through 3, and union them into (the cannonical) floor 0. -}

wrapColorFloor : ColorTiles -> ColorTiles
wrapColorFloor =
  wrapFloor identity Tiles.union


{-| Take (the cannonical) floor 0 and rotate into floors 1 through 3. -}

unwrapColorFloor : ColorTiles -> ColorTiles
unwrapColorFloor tiles =
  let
    halfWidth = toFloat ( dims.width - 1 ) / 2.0
    copyRotateToFloor i =
      Tiles.extractBox ( 0, 0 ) ( dims.width, dims.width ) tiles
        |> Tiles.rotateAbout i ( vec2 halfWidth halfWidth )
        |> Tiles.translate ( dims.width * i ) 0
  in
    List.foldl ( Tiles.union << copyRotateToFloor ) tiles [1..3]


{-| Return true if the floors have correct symmetry w.r.t. each other. -}

colorFloorTopologicallyCorrect : ColorTiles -> Bool
colorFloorTopologicallyCorrect tiles =
  tiles
    |> wrapColorFloor
    |> unwrapColorFloor
    |> (==) tiles

{-| Rotate floors 1 through 3, and union them into (the cannonical) floor 0. -}

wrapVecFloor : Vec2Tiles -> Vec2Tiles
wrapVecFloor =
  let
    rot90 ( x, y ) = ( y, negate x )
    maybeAdd = Util.monoidWithDefault ( Vec2i.vec2i 0 0 ) Vec2i.add
  in
    wrapFloor rot90 ( Tiles.map2 maybeAdd )


{-| Rotate floors 1 through 3, and union them into (the cannonical) floor 0. -}

wrapFloor : (a -> a) -> Monoid ( Tiles.Tiles a ) -> Transform ( Tiles.Tiles a )
wrapFloor mapFn combineFn tiles =
  let
    halfWidth = toFloat ( dims.width - 1 ) / 2.0
    rotateFloor i =
      Tiles.extractBox ( i * dims.width, 0 ) ( dims.width, dims.width ) tiles
        |> Tiles.translate (negate ( i * dims.width )) 0
        |> Tiles.rotateAbout ( negate i ) ( vec2 halfWidth halfWidth )
        |> Tiles.map ( Util.applyN i mapFn )
  in
    List.foldl ( combineFn << rotateFloor ) tiles [1..3]


{-| Removes all tiles on floors 1, 2, and 3, keepong only those on fllor 0. -}

keepOnlyFloor0 : Tiles.Tiles a -> Tiles.Tiles a
keepOnlyFloor0 =
  Tiles.clearBox ( dims.width, 0 ) ( 3 * dims.width, dims.width )
