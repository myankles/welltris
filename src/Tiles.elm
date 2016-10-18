module Tiles exposing
  ( Tiles
  , ColorTiles, Vec2Tiles
  , empty, box
  , toList , fromList
  , isEmpty, nonEmpty, size, overlap
  , get, member, coords
  , map, indexMap, map2, filter
  , union, translate, rotate, rotateAbout
  , clearBox, extractBox
  , bar, l1, l2, s1, s2, t, square
  )

import Color exposing ( Color )
import Dict exposing (Dict)
import List.Extra
import Math.Vector2 exposing
  ( Vec2, vec2
  , add, sub, scale
  , getX, getY, toTuple
  )
import Maybe.Extra
import Set
import Tuple2
import Util
import Vec2i exposing ( Vec2i )


{-| A set of Tiles, each with a piece of data associated witth it. -}

type Tiles a =
  Tiles (Dict Vec2i a)


{-| The most common case is to have a set of colored tiles. -}

type alias ColorTiles =
  Tiles Color

{-| Sometimes, we need tiles to express a discrete vector for each point. -}

type alias Vec2Tiles =
  Tiles Vec2i


{-| An empty set of tiles -}

empty : Tiles a
empty =
  Tiles Dict.empty


{-| A rectangular box full of tiles. |-}

box : Int -> Int -> a -> Tiles a
box w h x =
  List.Extra.lift2 (,) [ 0 .. (w - 1) ] [ 0 .. (h - 1) ]
    |> List.map (\ij -> ( ij, x ))
    |> fromList


{-| Convert the Tiles to a list of (( Ine, Int ), a ) -}

toList : Tiles a -> List ((Int, Int), a)
toList (Tiles tiles) =
  Dict.toList tiles


{-| Converts a list of ( Vec2i, a ) to a Tiles. -}

fromList : List ( Vec2i, a ) -> Tiles a
fromList =
  Dict.fromList >> Tiles


{-| Returns true if the Tiles contain no elements -}

isEmpty : Tiles a -> Bool
isEmpty ( Tiles t ) =
  Dict.isEmpty t


{-| Opposite of isEmpty -}

nonEmpty : Tiles a -> Bool
nonEmpty =
  not << isEmpty

{-| Returns the size of thes Tiles -}

size : Tiles a -> Int
size ( Tiles t ) =
  Dict.size t

{-| Returns true iff the two Tiles overlap. -}

overlap : Tiles a -> Tiles a -> Bool
overlap ( Tiles t1 ) ( Tiles t2 ) =
  not ( Dict.isEmpty ( Dict.intersect t1 t2 ))


{-| Gets the value at a specific location -}

get : Vec2i -> Tiles a -> Maybe a
get ij ( Tiles t ) =
  Dict.get ij t


{-| Returns true if this tileset contiains this coordinte. -}

member : Vec2i -> Tiles a -> Bool
member ij tiles =
  ( get ij tiles ) /= Nothing

{-\ Gets the set of coordinates, ignoring values. -}

coords : Tiles a -> Set.Set Vec2i
coords ( Tiles t ) =
  t |> Dict.keys |> Set.fromList

{-| Applies a function to each tile. -}

map : ( a -> b ) -> Tiles a -> Tiles b
map f ( Tiles t ) =
  Dict.map ( always f ) t |> Tiles


{-| Applies a function to each tile (takes into account the coordinate). -}

indexMap : ( Vec2i -> a -> b ) -> Tiles a -> Tiles b
indexMap f ( Tiles t ) =
  Dict.map f t |> Tiles


{-| Applies a function to both tile sets. Each key is applied only once.
    f Nothing Nothing will never be called.
-}

map2 : ( Maybe a -> Maybe b -> Maybe c ) -> Tiles a -> Tiles b -> Tiles c
map2 f ( Tiles tA ) ( Tiles tB ) =
  let
    f1 k a tC = Dict.insert k ( f ( Just a ) Nothing ) tC
    f2 k a b tC = Dict.insert k ( f ( Just a ) ( Just b )) tC
    f3 k b tC = Dict.insert k ( f Nothing ( Just b ) ) tC
  in
    Dict.empty
      |> Dict.merge f1 f2 f3 tA tB
      |> Dict.toList
      |> List.filterMap
        ( \x ->
          case x of
            ( ij, Just c ) -> Just ( ij, c )
            ( ij, Nothing ) -> Nothing
        )
      |> Dict.fromList
      |> Tiles


{-| Keeps only tiles for which the predicate is true. -}

filter : ( Vec2i -> a -> Bool ) -> Tiles a -> Tiles a
filter predicate ( Tiles t ) =
  Tiles ( Dict.filter predicate t )


{-| Union in a another Tiles, preferring the first in case of collisions. -}

union : Tiles a -> Tiles a -> Tiles a
union t1 t2 =
  map2 Maybe.Extra.or t1 t2


{-| Translates a piece by a discrete vector. -}

translate : Int -> Int -> Tiles a -> Tiles a
translate delta_x delta_y =
  let
    translate' (( x, y ), color ) = (( x + delta_x, y + delta_y ), color )
  in
    toList >> ( List.map translate' ) >> fromList


{-| Apply n quarter rotations about the piece's center of mass. -}

rotate : Int -> Tiles a -> Tiles a
rotate nTimes tiles =
  let
    -- convert to floating point coordinates
    coords = tiles |> toList |> List.map ( fst >> Vec2i.toVec2f )

    -- compute center of mass
    sum = ( List.foldl Math.Vector2.add ( vec2 0.0 0.0 ) coords )
    centerOfMass = ( 1.0 / ( toFloat ( List.length coords ))) `scale` sum

  in
    rotateAbout nTimes centerOfMass tiles


{-| Apply n quarter rotations about a given pivot.

  rotateAbout nTimes pivot tiles -> tiles'
-}

rotateAbout : Int -> Vec2 -> Tiles a -> Tiles a
rotateAbout nTimes pivot tiles =
  let
    -- split into colors and floating point coordinates
    ( coords, colors ) = tiles |> toList |> List.unzip
    floatCoords = List.map Vec2i.toVec2f coords

    -- rotate about the center of mass
    rotateAboutZero v = vec2 ( negate <| getY v ) ( getX v )
    rotateNTimes = Util.applyN ( nTimes % 4 ) rotateAboutZero
    rotateAboutPivot pt = ( pt `sub`  pivot ) |> rotateNTimes |> ( add pivot )

    -- convert back to integers
    roundToTuple = Tuple2.mapBoth round << toTuple
    rotatedCoords = List.map ( roundToTuple << rotateAboutPivot ) floatCoords
  in
    List.Extra.zip rotatedCoords colors |> fromList


{-| Remove all tiles (x,y) in the box
    i <= x < i + w and j <= y < j+h.
-}

clearBox : Vec2i -> Vec2i -> Tiles a -> Tiles a
clearBox ij wh tiles =
  tiles
    |> toList
    |> List.filter ( not << Vec2i.withinBox ij wh << fst )
    |> fromList


{-| Returns the complement fo clearBox -}

extractBox : Vec2i -> Vec2i -> Tiles a -> Tiles a
extractBox ij wh tiles =
  tiles
    |> toList
    |> List.filter ( Vec2i.withinBox ij wh << fst )
    |> fromList


-- Here are some tetronimos

bar : Tiles Color
bar =
  Tiles <| Dict.fromList
    [ (( 0, -1 ), Color.red )
    , (( 0,  0 ), Color.red )
    , (( 0,  1 ), Color.red )
    , (( 0,  2 ), Color.red )
    ]


l1 : Tiles Color
l1 =
  Tiles <| Dict.fromList
    [ (( -1, 0 ), Color.orange )
    , ((  0, 0 ), Color.orange )
    , ((  1, 0 ), Color.orange )
    , ((  1, 1 ), Color.orange )
    ]


l2 : Tiles Color
l2 =
  Tiles <| Dict.fromList
    [ (( 0, -1 ), Color.green )
    , (( 0,  0 ), Color.green )
    , (( 0,  1 ), Color.green )
    , (( 1,  1 ), Color.green )
    ]


s1 : Tiles Color
s1 =
  Tiles <| Dict.fromList
    [ (( 0, -1 ), Color.blue )
    , (( 0,  0 ), Color.blue )
    , (( 1,  0 ), Color.blue )
    , (( 1,  1 ), Color.blue )
    ]


s2 : Tiles Color
s2 =
  Tiles <| Dict.fromList
    [ (( -1, 0 ), Color.purple )
    , ((  0, 0 ), Color.purple )
    , ((  0, 1 ), Color.purple )
    , ((  1, 1 ), Color.purple )
    ]


t : Tiles Color
t =
  Tiles <| Dict.fromList
    [ (( -1, 0 ), Color.darkBlue )
    , ((  0, 0 ), Color.darkBlue )
    , ((  1, 0 ), Color.darkBlue )
    , ((  0, 1 ), Color.darkBlue )
    ]


square : Tiles Color
square =
  Tiles <| Dict.fromList
    [ (( -1, -1 ), Color.darkGreen )
    , ((  0, -1 ), Color.darkGreen )
    , (( -1,  0 ), Color.darkGreen )
    , ((  0,  0 ), Color.darkGreen )
    ]
