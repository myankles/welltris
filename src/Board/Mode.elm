module Board.Mode exposing
  ( Model(..)
  , WallMode(..)
  , BeginAnimationParams
  , fastDrop
  , beginAnimation, animate
  , getPiece, setPiece
  , addModeColors
  )

{-| The board can be in one of several distinct modes. -}

import Animation
import Color
import Ease
import Piece
import Tiles exposing ( ColorTiles )
import Time exposing ( Time )

{-| These are the possible discrete modes for the board. -}

type Model

  -- Add a new piece to the game
  = SpawnNextPiece

  -- The piece is dropping.
  | DropPiece { piece : Piece.Model, fastDrop : Bool }

  -- Check whether we need to block any walls.
  | BlockWalls { newTiles : ColorTiles }

  -- Unblock walls which have been blocked for too long
  | UnblockWalls { nextWallModes : List WallMode }

  -- Drops unblocked pieces off walls
  | DropWalls1

  -- Find any new lines
  | FindLines

  -- Update the tileset after finding lines (need for animation to work.)
  | SetNewTiles ColorTiles

  -- Drops unblocked pieces off again
  | DropWalls2

  -- No more pieces will be dropped.
  | GameOver

  -- Begin a new animation
  | BeginAnimation BeginAnimationParams

  -- Animate between modes
  | AnimateTo AnimationParams


{-| A wall can be either blocked or unblocked. -}

type WallMode
  = Unblocked
  | BlockedFor Int


{-| Parameters when starting an animation. -}

type alias BeginAnimationParams =
  { nextMode : Model
  , highlightedTiles : ColorTiles
  , duration : Time
  }


{-| Parameters when running an animation. -}

type alias AnimationParams =
  { nextMode : Model
  , highlightedTiles : ColorTiles
  , animation : Animation.Animation
  , currentTime : Time
  }

{-| Set the mode to a fastDrop mode. -}

fastDrop : Model -> Model
fastDrop mode =
  getPiece mode
    |> Maybe.map (\piece -> DropPiece { piece = piece, fastDrop = True } )
    |> Maybe.withDefault mode


{-| Starts an animation at the current time. -}

beginAnimation : Maybe Time -> BeginAnimationParams -> Model
beginAnimation maybeCurrentTime { nextMode, highlightedTiles, duration } =
  case maybeCurrentTime of
    Just currentTime ->
      AnimateTo
        { nextMode = nextMode
        , highlightedTiles = highlightedTiles
        , animation = Animation.animation currentTime
            |> Animation.duration duration
            |> Animation.ease
                ( Ease.inOut ( Ease.retour Ease.linear ) Ease.linear )
        , currentTime = currentTime
        }
    Nothing ->
      nextMode



{-| Transitions to the next mode unless the animatio is in progress. -}

animate : Maybe Time -> AnimationParams -> Model
animate maybeCurrentTime animParams =
  case maybeCurrentTime of
    Just currentTime ->
      if Animation.isDone currentTime animParams.animation then
        animParams.nextMode
      else
        AnimateTo { animParams | currentTime = currentTime }
    Nothing ->
      animParams.nextMode


{-| Extracts the piece from the BoardMode, failing the mode has no piece. -}

getPiece : Model -> Maybe Piece.Model
getPiece mode =
  case mode of
    DropPiece { piece, fastDrop } ->
      if fastDrop then
        -- piece is not editable in fastDrop mode
        Nothing
      else
        Just piece

    BeginAnimation params ->
      getPiece params.nextMode

    AnimateTo params ->
      getPiece params.nextMode

    _ ->
      Nothing


{-| Sets the BoardMode's piece, failing if the piece cannot be set. -}

setPiece : Model -> Piece.Model -> Maybe Model
setPiece mode piece =
  let
    setNextMode animParams mode =
      { animParams | nextMode = mode }

  in
    case mode of
      DropPiece params ->
        if params.fastDrop then
          Nothing
        else
          Just ( DropPiece { piece = piece, fastDrop = False } )


      BeginAnimation params ->
        setPiece params.nextMode piece
          |> Maybe.map ( setNextMode params >> BeginAnimation )

      AnimateTo params ->
        setPiece params.nextMode piece
          |> Maybe.map ( setNextMode params >> AnimateTo )

      _ ->
        Nothing


{-| Updates the board colors based on the Mode. -}

addModeColors : Model -> ColorTiles -> ColorTiles
addModeColors mode tiles =
  case mode of
    SpawnNextPiece ->
      tiles

    DropPiece { piece } ->
      Tiles.union ( Piece.project piece ) tiles

    BlockWalls { newTiles } ->
      Tiles.union newTiles tiles

    UnblockWalls { nextWallModes }->
      tiles

    DropWalls1 ->
      tiles

    FindLines ->
      tiles

    DropWalls2 ->
      tiles

    SetNewTiles _ ->
      tiles

    GameOver ->
      tiles

    BeginAnimation { nextMode, highlightedTiles } ->
        addModeColors nextMode tiles
          |> addHighlights 0.0 highlightedTiles

    AnimateTo { nextMode, highlightedTiles, animation, currentTime } ->
      let
        animVal = Animation.animate currentTime animation
      in
        addModeColors nextMode tiles
          |> addHighlights animVal highlightedTiles


{-| Adds a bunch of highlights according an animation parameter in [0,1]. -}

addHighlights : Float -> ColorTiles -> ColorTiles -> ColorTiles
addHighlights val highlightedTiles tiles =
  let
    toFloatTriplet =
      Color.toRgb >> (\c -> ( toFloat c.red, toFloat c.green, toFloat c.blue ))

    blend color1 color2 =
      let
        (( r1, g1, b1 ), ( r2, g2, b2 )) =
          ( toFloatTriplet color1, toFloatTriplet color2 )
      in
        Color.rgb
         ( round (( 1.0 - val ) * r1 + val * r2 ))
         ( round (( 1.0 - val ) * g1 + val * g2 ))
         ( round (( 1.0 - val ) * b1 + val * b2 ))

    highlight ij color =
      Tiles.get ij highlightedTiles
        |> Maybe.map ( blend color )
        |> Maybe.withDefault color

  in
    Tiles.indexMap highlight tiles
