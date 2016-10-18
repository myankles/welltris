module Vec2i exposing
  ( Vec2i, vec2i
  , toVec2f, toVec2i
  , withinBox
  , add, sub
  )

import Math.Vector2

{-| A two dimensional lattice point. -}

type alias Vec2i = ( Int, Int )


{-| Vec2i constructor -}

vec2i : Int -> Int -> Vec2i
vec2i i j =
  ( i, j )


{-| Convert to a float vector -}

toVec2f : Vec2i -> Math.Vector2.Vec2
toVec2f ( i, j ) =
  Math.Vector2.vec2 ( toFloat i ) ( toFloat j )


{-| Convert a floating point vector to an integer vector. -}

toVec2i : Math.Vector2.Vec2 -> Vec2i
toVec2i xy =
  ( round ( Math.Vector2.getX xy )
  , round ( Math.Vector2.getY xy )
  )


{-| withinBox ij wh xy is true if xy lies in the box [i .. i+w) X [j .. i+h). -}

withinBox : Vec2i -> Vec2i -> Vec2i -> Bool
withinBox ( i, j ) ( w, h ) ( x, y ) =
  ( i <= x ) && ( x < ( i + w )) && ( j <= y ) && ( y < ( j + h ))


{-| Vector addition. -}

add : Vec2i -> Vec2i -> Vec2i
add ( x1, y1) ( x2, y2 )=
  ( x1 + x2, y1 + y2 )

{-| Vector subtraction -}

sub : Vec2i -> Vec2i -> Vec2i
sub ( x1, y1) ( x2, y2 )=
  (( x1 - x2 ), ( y1 - y2 ))
