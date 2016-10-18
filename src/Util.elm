module Util exposing
  ( Transform, Monoid
  , applyN, while, loop
  , monoidWithDefault
  , maybeFlatMap
  , randomStepWithIntSeed
  )

import Random exposing ( Generator )

{-| A function with the same domain and codomain. -}

type alias Transform a =
  a -> a


{-| An operation which combines two elements into a third
    , all of the same type.
-}

type alias Monoid a =
  a -> a -> a


{-| apply a function n times -}

applyN : Int -> Transform a -> Transform a
applyN n f x = List.foldl ( always f ) x [ 1..n ]


{-| Repeatedly apply the function until the condition fails. -}

while : ( a -> Bool ) -> ( a -> a ) -> a -> a
while condition func x =
  if condition x then
    while condition func ( func x )
  else
    x

{-| Repatedly apply the function until Nothing is reached. -}

loop : ( a -> Maybe a ) -> a -> a
loop func x =
  case func x of
    Just x' -> loop func x'
    Nothing -> x


{-| Apply a monoid with maybe values so that monoid

      monoidOperator` = monoidWithDefault monoidOperator defaultValue

    has the property that

      moinoidOperator` Nothing Nothing == Nothing

    But in all other circumsrtances, defaultValue is substituted for Nothing.
-}

monoidWithDefault : a -> Monoid a -> Monoid ( Maybe a )
monoidWithDefault defaultValue monoidOperator =
  \maybeX maybeY ->
    if maybeX == Nothing && maybeY == Nothing then
      Nothing

    else
      Just <| monoidOperator
        ( Maybe.withDefault defaultValue maybeX )
        ( Maybe.withDefault defaultValue maybeY )


{-| flatMap for Maybe -}

maybeFlatMap : ( a -> Maybe b ) -> Maybe a -> Maybe b
maybeFlatMap =
  flip ( Maybe.andThen )


{-| Like Random.step but uses Int as the seed type rather than
    the more opaque Random.Seed
-}
randomStepWithIntSeed : Generator a -> Int -> ( a, Int )
randomStepWithIntSeed generator seed =
  let
    seedGenerator =
      Random.int 0 Random.maxInt |> Random.map (\seed -> seed // 3)

  in
    Random.step
      ( Random.pair generator seedGenerator )
      ( Random.initialSeed seed )
    |> fst
