module Board exposing
  ( Model
  , Msg(..)
  , init
  , update
  , view
  , score, speed
  , runUntilCollision, runUntilCollisionWithBlockedWalls
  , runThroughLineFind, getCoords, compilableString
  , tiles
  , debugTopologyRegressionBoard
  )

{-| The gamboard and anything required to render it. -}

import Board.Mode exposing ( WallMode(..) )
import Board.Topology as Topology
import Board.View exposing ( BoardView )
import Char
import Color exposing ( Color )
import Constants exposing ( dims )
import Dict
import Keyboard
import List.Extra exposing ( lift2 )
import Maybe.Extra
import Piece
import Set
import String.Extra
import Svg
import Tiles exposing ( ColorTiles )
import Time exposing ( Time, second )
import Tuple2
import Util exposing ( Transform, Monoid )
import Vec2i exposing ( Vec2i )

-- I sometimes import these modules for debugging...
{--
import Debug.Extra
import String
--}

-- The complete mode of the board.

type Model = Board
  -- The board is a mode machine driven forward by tick.
  { mode : Board.Mode.Model

  -- The set of tiles on the floor and walls.
  , tiles : ColorTiles

  -- Length 4 list indicating whether walls are blocked.
  , wallModes : List WallMode

  -- The current score
  , score : Int

  -- The curret speed level
  , speed : Int

  -- the source of randomness
  , seed : Piece.Seed
  }


-- A message which can be sent to the board

type Msg
  = Tick Time
  | KeyPress Keyboard.KeyCode
  | IncreaseSpeed
  | EndGame
  | None



{-| Creates an empty Board -}

init : Int -> Model
init seed =
  Board
    { mode = Board.Mode.SpawnNextPiece
    , tiles = Tiles.box dims.width2d 1 Color.white |> Tiles.translate 0 -1
    , wallModes = [ Unblocked, Unblocked, Unblocked, Unblocked ]
    , score = 0
    , speed = 1
    , seed = seed
    }


{-| Updates the game mode -}

update : Msg -> Model -> Model
update msg =
  let
    isAnimating ( Board b ) =
      case b.mode of
        Board.Mode.AnimateTo _ -> True
        Board.Mode.GameOver -> True
        _ -> False

  in
    case msg of
      Tick currentTime ->
        tick ( Just currentTime )
          >> Util.while ( not << isAnimating ) ( tick ( Just currentTime ))

      KeyPress keyCode ->
        decodeKey keyCode

      IncreaseSpeed ->
        (\( Board b ) -> Board { b | speed = b.speed + 1 } )

      EndGame ->
        setMode Board.Mode.GameOver

      None ->
        identity


{- Advances the board. Send in currentTime if applicable. -}

tick : Maybe Time -> Model -> Model
tick maybeCurrentTime ( Board b as board ) =
  let
    updatePiece ( newPiece, newSeed ) =
      Board
        { b
        | mode = Board.Mode.DropPiece { piece = newPiece, fastDrop = False }
        , seed = newSeed
        }
  in
    case b.mode of
      Board.Mode.SpawnNextPiece ->
        updatePiece (Piece.random b.seed ( wallsBlocked board ))

      Board.Mode.DropPiece _ ->
        dropPiece board

      Board.Mode.BlockWalls { newTiles } ->
        blockWalls newTiles board

      Board.Mode.UnblockWalls { nextWallModes } ->
        unblockWalls ( Board { b | wallModes = nextWallModes } )

      Board.Mode.DropWalls1 ->
        dropWalls Board.Mode.FindLines board

      Board.Mode.FindLines ->
        findLines board

      Board.Mode.SetNewTiles newTiles ->
        setMode Board.Mode.DropWalls2 ( Board { b | tiles = newTiles } )

      Board.Mode.DropWalls2 ->
        dropWalls Board.Mode.SpawnNextPiece board

      Board.Mode.GameOver ->
        board

      Board.Mode.BeginAnimation params ->
        setMode ( Board.Mode.beginAnimation maybeCurrentTime params ) board

      Board.Mode.AnimateTo params ->
        setMode ( Board.Mode.animate maybeCurrentTime params ) board


{-| Sets the next mode immediately -}

setMode : Board.Mode.Model -> Model -> Model
setMode nextMode ( Board b ) =
  Board { b | mode = nextMode }


{-| Sets the next mode, animating if any tiles are highlighted. -}

animateIf : Bool -> Board.Mode.BeginAnimationParams -> Model -> Model
animateIf condition animParams =
  setMode <|
    if condition then
      Board.Mode.BeginAnimation animParams
    else
      animParams.nextMode


{-| Updates the model based on a keypress. -}

decodeKey : Keyboard.KeyCode -> Model -> Model
decodeKey keyCode ( Board b as board ) =
  let
    translate : Int -> Int -> Piece.Model -> Piece.Model
    translate counterClockwiseWall clockwiseWall piece =
      if Piece.intersectsWall clockwiseWall piece then
        { piece | x = piece.x + 1 }
      else if Piece.intersectsWall counterClockwiseWall piece then
        { piece | x = piece.x - 1 }
      else
        piece

    ( left, right, up, down ) =
      ( translate 0 2, translate 2 0
      , translate 1 3, translate 3 1
      )

    rotate : Piece.Model -> Piece.Model
    rotate piece =
      { piece | rot = ( piece.rot + 1 ) % 4 }

    drop : Piece.Model -> Piece.Model
    drop =
      identity

    keyMap : Dict.Dict Char ( Piece.Model -> Piece.Model )
    keyMap =
      -- left-hand controls
      [ ( 'A', left ), ( 'D', right ), ( 'W', up ), ( 'X', down )
      , ( 'S', rotate )

      -- right hand controls
      , ( 'J', left ), ( 'L', right ), ( 'I', up ), ( 'M', down ), ( ',', down )
      , ( 'K', rotate )

      ] |> Dict.fromList

  in
    case Char.fromCode keyCode of
      'G' ->
        setMode ( Board.Mode.fastDrop b.mode ) board

      char ->
        Dict.get char keyMap
          |> Maybe.map ( updateUnlessCollision board )
          |> Maybe.withDefault board


{-| Length-4 list indicating which of the four walls are blocked. -}

wallsBlocked : Model -> List Bool
wallsBlocked ( Board b ) =
  List.map ((/=) Unblocked ) b.wallModes


{-| Checks for three collision types

      - whether the old piece exists and intersects the floor
      - whether the new piece intersects a blocked wall
      - whether the new piece intersects existing tiles
-}

findCollisions: Model -> Piece.Model -> Piece.Model -> ( Bool, Bool, Bool )
findCollisions ( Board b as board ) oldPiece newPiece =
    ( Piece.intersectsFloor oldPiece
    , Piece.intersectsBlockedWall ( wallsBlocked board ) newPiece
    , Tiles.overlap b.tiles ( Piece.project newPiece )
    )


{-| Updates the piece as specified, unlesss this causes a collision. -}

updateUnlessCollision : Model -> ( Piece.Model -> Piece.Model ) -> Model
updateUnlessCollision ( Board b as board ) pieceUpdate =
  Board.Mode.getPiece b.mode
    |> Maybe.map ( \piece -> ( piece , pieceUpdate piece ))
    |> Util.maybeFlatMap (\( oldPiece, newPiece ) ->
          case findCollisions board oldPiece newPiece of
            ( False, False, False ) -> Just newPiece
            _ -> Nothing
      )
    |> Util.maybeFlatMap ( Board.Mode.setPiece b.mode )
    |> Maybe.map ( flip setMode board )
    |> Maybe.withDefault board


{-| Update the baord to include the new piece, changing modes
    if the piece collides.
-}

dropPiece : Model -> Model
dropPiece ( Board b as board ) =
  case b.mode of
    Board.Mode.DropPiece params ->
      let
        oldPiece =
          params.piece

        newPiece =
          { oldPiece | y = oldPiece.y - 1 }

        duration =
          if params.fastDrop then
            Constants.fastDropPieceDelay
          else
            Constants.dropPieceDelay *
              Constants.dropPieceDelayRatio ^ toFloat ( b.speed - 1)

        newMode =
          if Tiles.overlap b.tiles ( Piece.project newPiece ) then
            Board.Mode.BlockWalls
              { newTiles = Tiles.union b.tiles ( Piece.project oldPiece ) }

          else
            Board.Mode.BeginAnimation
              { nextMode = Board.Mode.DropPiece { params | piece = newPiece }
              , highlightedTiles = Tiles.empty
              , duration = duration
              }
      in
        setMode newMode board
    _ ->
      board -- Incorrect mode for dropPiece.


{-| Check for walls to block. End the game if all four walls are blocked. -}

blockWalls : ColorTiles -> Model -> Model
blockWalls newTiles ( Board b as board ) =
  let
    nextWallModes =
      Set.diff ( Tiles.coords newTiles ) ( Tiles.coords b.tiles )
        |> Set.foldl
            (\coord wallModes ->
                wallModes |> List.indexedMap
                  (\wallIndex mode ->
                      if Topology.onWall wallIndex coord then
                        BlockedFor 4
                      else
                        mode
                  )
            ) b.wallModes

    newlyBlockedWallTiles =
      nextWallModes
        |> List.indexedMap (\wallIndex wallMode ->
            if wallMode == BlockedFor 4 then
              Topology.wall wallIndex Constants.blockedWallColor
            else
              Tiles.empty
          )
        |> List.foldl Tiles.union Tiles.empty
        |> Tiles.filter (\ ij _ -> not ( Tiles.member ij newTiles ))
  in
    Board { b | tiles = newTiles }
      |> animateIf ( Tiles.nonEmpty newlyBlockedWallTiles )
          { nextMode = Board.Mode.UnblockWalls
              { nextWallModes = nextWallModes }
          , highlightedTiles = newlyBlockedWallTiles
          , duration = Constants.blockWallDelay
          }
      |> checkForGameOver nextWallModes


{-| Decrease blocked wall turn count. -}

unblockWalls : Model -> Model
unblockWalls ( Board b ) =
  let
    nextWallModes =
      b.wallModes |> List.map ( \mode ->
        case mode of
          Unblocked ->
            Unblocked

          BlockedFor 1 ->
            Unblocked

          BlockedFor nDrops ->
            BlockedFor ( nDrops - 1 )
      )

    newlyUnblockedTiles =
      List.Extra.zip3 [0..3] b.wallModes nextWallModes
        |> List.filterMap (\( wallIndex, wallModeBefore, wallModeAfter ) ->
            case ( wallModeBefore, wallModeAfter ) of
              ( BlockedFor _, Unblocked ) ->
                Just ( Topology.wall wallIndex Constants.blockedWallColor )

              _ ->
                Nothing
          )
        |> List.foldl Tiles.union Tiles.empty
        |> Tiles.filter (\ ij _ -> not ( Tiles.member ij b.tiles ))
  in
    Board { b | wallModes = nextWallModes }
      |> animateIf ( Tiles.nonEmpty newlyUnblockedTiles )
          { nextMode = Board.Mode.DropWalls1
          , highlightedTiles = newlyUnblockedTiles
          , duration = Constants.blockWallDelay
          }


{-| Drop unblocked pieces off the walls, then transition to given next mode. -}

dropWalls : Board.Mode.Model -> Model -> Model
dropWalls nextMode ( Board b as board ) =
  let
    nextTiles =
      b.wallModes
        |> List.Extra.findIndices ( (==) Unblocked )
        |> List.foldl (\wallIndex tiles ->
            let
              ( wallTiles, restOfBoard ) =
                Topology.partitionWall wallIndex tiles

              stopConditions =
                [ Tiles.isEmpty
                , Tiles.toList >> List.any
                    (\(( x, y ), color ) -> y < dims.width // 2 )
                , Tiles.overlap restOfBoard
                ]

              drop wallTiles =
                Tiles.translate 0 -1 wallTiles
                  |> Just |> Maybe.Extra.filter
                     ( not << flip List.any stopConditions << (|>))
            in
              Util.loop drop wallTiles
                |> Tiles.union restOfBoard
                |> Topology.wrapColorFloor |> Topology.unwrapColorFloor
          ) b.tiles
  in
    Board { b | tiles = nextTiles }
      |> animateIf (( b.tiles ) /= ( nextTiles ))
          { nextMode = nextMode
          , highlightedTiles = Tiles.empty
          , duration = Constants.dropWallsDelay
          }


{-| Updates the wall modes, ending the game if all walls are blocked. -}

checkForGameOver : List WallMode -> Model -> Model
checkForGameOver nextWallModes ( Board b as board ) =
  if List.any ( (==) Unblocked ) nextWallModes then
    board
  else
    Board { b | wallModes = nextWallModes }
      |> setMode Board.Mode.GameOver


{-| Check for lines in the board, displace tiles, and update the mode. -}

findLines : Model -> Model
findLines ( Board b as board ) =
  let
    maybeAdd =
      Util.monoidWithDefault ( Vec2i.vec2i 0 0 ) Vec2i.add

    findALine ( wallIndex, j ) ( lines, displacement ) =
      let
        line =
          Tiles.extractBox
            ( wallIndex * dims.width, j )
            ( dims.width, 1 ) b.tiles
      in
        if Tiles.size line == dims.width then
          ( Tiles.union lines line
          , displacement
              |> Tiles.extractBox
                ( wallIndex * dims.width, j + 1 )
                ( dims.width, dims.height2d - j - 1)
              |> Tiles.map ( always ( 0, -1) )
              |> Tiles.map2 maybeAdd displacement
          )
        else
          ( lines, displacement )

    maskOutLineDisplacement ( lines, displacement ) =
      ( lines, Tiles.union ( Tiles.map ( always ( 0, 0 )) lines ) displacement )

    wallsAndJs =
      lift2 (,) [ 0 .. 3 ] [ dims.width // 2 .. dims.width - 1 ]

    emptyDisplacement =
      Tiles.map ( always ( 0, 0 )) b.tiles

    startMode =
      ( Tiles.empty, emptyDisplacement )

    ( lines, displacement ) =
      List.foldl findALine startMode wallsAndJs
        |> maskOutLineDisplacement
        |> Tuple2.mapFst ( Topology.wrapColorFloor >> Topology.unwrapColorFloor)
        |> Tuple2.mapSnd ( Topology.wrapVecFloor >> Topology.keepOnlyFloor0 )

    newTiles =
      b.tiles
        |> Tiles.toList
        |> List.filter (\( ij, color ) -> Tiles.get ij lines == Nothing )
        |> Tiles.fromList
        |> displace displacement ( wallsBlocked board )
  in
    board
      |> updateScoreAndSpeed ( Tiles.size lines )
      |> animateIf ( Tiles.nonEmpty lines )
          { nextMode = Board.Mode.SetNewTiles newTiles
          , highlightedTiles = Tiles.map ( always Color.white ) lines
          , duration = Constants.foundLinesDelay
          }


{-| Updates the score of the board, given a number of
    tiles deleted through lines. -}

updateScoreAndSpeed : Int -> Model -> Model
updateScoreAndSpeed nTilesDeleted ( Board b ) =
  let
    newScore =
      b.score + 10 + round (( toFloat ( nTilesDeleted // 4 )) ^ 1.667 )

    newSpeed =
      max b.speed ( 1 + newScore // 500 )

  in
    Board { b | score = newScore, speed = newSpeed }


{-| Shifts tiles according to a displacement field,
    unless the wall is blocked. Assumes the diplacement field is zero on
    all floors but 0.
-}

displace : Tiles.Vec2Tiles -> List Bool -> ColorTiles -> ColorTiles
displace displacement blockedWalls tiles =
  tiles
    |> Topology.keepOnlyFloor0
    |> Tiles.toList
    |> List.map (\( ij, color ) ->
        case ( Topology.onAWall blockedWalls ij, Tiles.get ij displacement ) of
          ( False, Just deltaIj ) -> ( Vec2i.add ij deltaIj, color )
          _ -> ( ij, color )
      )
    |> Tiles.fromList
    |> Topology.wrapColorFloor |> Topology.unwrapColorFloor


-- Renders the board into a group of size width * (computed) heigh in pixels

view : BoardView msg -> Model -> Svg.Svg msg
view boardView ( Board b ) =
  let
    -- make blocked walls red
    blockedWalls =
      b.wallModes
        |> List.indexedMap
            (\wallIndex wallMode ->
              if wallMode == Unblocked then
                Tiles.empty

              else
                Topology.wall wallIndex Constants.blockedWallColor
            )
        |> List.foldl Tiles.union Tiles.empty

      -- add a slight depth dependent darkening
    depthDarken ( x, y ) color =
      let
        depth =
          ( max ( toFloat ( y - dims.width )) 0.0 ) / ( toFloat dims.depth )

        maxDarkening =
          0.075

        depthDarkening =
          1.0 - ( 1.0 - depth ) ^ 1.1 * maxDarkening

        shadowDarkening =
          if x >= dims.width && x < 3 * dims.width then
            0.98
          else
            1.0

        -- { hue, saturation, lightness, alpha } =
        --   Color.toHsl color

        { red, green, blue, alpha } =
          Color.toRgb color

      in
        Color.rgb
          ( round (( toFloat red ) * depthDarkening * shadowDarkening ))
          ( round (( toFloat green ) * depthDarkening * shadowDarkening ))
          ( round (( toFloat blue ) * depthDarkening * shadowDarkening ))
  in
    Constants.backgroundColors
      |> Tiles.union blockedWalls
      |> Tiles.union b.tiles
      |> Board.Mode.addModeColors b.mode
      |> Tiles.indexMap depthDarken
      |> boardView


{-| Returns the score of the board. -}

score : Model -> Int
score ( Board b ) =
  b.score


{-| Returns the speed level of the baord -}

speed : Model -> Int
speed ( Board b ) =
  b.speed

{-| Takes an initial configuration of piece and runs until a collision happens,
    outputting the coordinates.
-}

runUntilCollision : Piece.Model -> Set.Set (Int, Int) -> Set.Set (Int, Int)
runUntilCollision initialPiece initialCoords =
  runUntilCollisionWithBlockedWalls
    initialPiece ( List.repeat 4 Unblocked ) initialCoords


{-| Takes an initial configuration of piece and runs until a collision happens,
    outputting the coordinates (works with blocked walls).
-}

runUntilCollisionWithBlockedWalls : Piece.Model -> List WallMode
  -> Set.Set (Int, Int) -> Set.Set (Int, Int)
runUntilCollisionWithBlockedWalls initialPiece wallModes initialCoords =
  let
    setModeAndWalls ( Board b ) =
      Board
        { b
        | wallModes = wallModes
        , mode =
            Board.Mode.DropPiece { piece = initialPiece, fastDrop = True}
        }

    modeIsntSpawn ( Board b ) =
      ( b.mode /= Board.Mode.SpawnNextPiece )

  in
    init 0
      |> setModeAndWalls
      |> addCoords initialCoords
      |> Util.while modeIsntSpawn ( tick Nothing )
      |> getCoords


{-| Sets up the board and then runs it through the line-finding logic,
    returning the coordinates after all lines were found in succession.
-}

runThroughLineFind : Set.Set ( Int, Int ) -> Set.Set ( Int, Int )
runThroughLineFind initialCoords =
  let
    modeIsntDropWalls2 ( Board b ) =
      b.mode /= Board.Mode.DropWalls2

  in
    init 0
      |> setMode Board.Mode.FindLines
      |> addCoords initialCoords
      |> Util.while modeIsntDropWalls2 ( tick Nothing )
      |> getCoords


{-| Fills in a bunch more tiles on the board. -}

addCoords : Set.Set Vec2i -> Model -> Model
addCoords coords ( Board b ) =
  Board
    { b
    | tiles =
        coords
          |> Set.toList
          |> List.map (\ij -> ( ij, Color.gray ))
          |> Tiles.fromList
          |> Tiles.union b.tiles
          |> Topology.wrapColorFloor
          |> Topology.unwrapColorFloor
    }


{-| Extracts a list of all filled tiles on the board.

    NOTE: This function clears floors [1..3] before returning to reflect and
    old convention we had about how floors were handled.
-}

getCoords : Model -> Set.Set Vec2i
getCoords ( Board b ) =
  b.tiles
    |> Tiles.clearBox ( 0, -1 ) ( dims.width2d, 1 )
    |> Tiles.clearBox ( dims.width, 0 ) ( 3 * dims.width, dims.width )
    |> Tiles.toList
    |> List.map fst
    |> Set.fromList


{-| Returns the board as a string which can be copied-and-pasted
    into source code.
-}

compilableString : Model -> String
compilableString ( Board b as board )=
  toString board
    |> String.Extra.replace ",RGBA " ", Color.rgba "
    |> String.Extra.replace "Tiles (Dict.fromList [" "Tiles.fromList ["
    |> String.Extra.replace ")])" ")]"
    |> String.Extra.replace "mode = " "mode = Board.Mode."


{-| Get the board tiles (for debug purposes). -}

tiles : Model -> ColorTiles
tiles ( Board b ) =
  b.tiles


{-| Used to for the topological symmetry regression test. -}

debugTopologyRegressionBoard : Model
debugTopologyRegressionBoard =
  Board
    { mode = Board.Mode.FindLines
    , wallModes = [ Unblocked, Unblocked, BlockedFor 3, Unblocked ]
    , seed = 18949802
    , score = 0
    , speed = 1
    , tiles =
      [ (0,-1), (1,-1), (2,-1), (3,-1), (4,-1), (4,4), (5,-1), (5,1), (5,2)
      , (5,4), (5,5), (6,-1), (6,1), (6,2), (6,3), (6,4), (6,5), (6,6), (6,7)
      , (7,-1), (7,0), (7,1), (7,2), (7,3), (7,4), (7,5), (7,6), (7,7), (8,-1)
      , (8,6), (8,7), (9,-1), (9,6), (9,7), (10,-1), (10,5), (10,6), (10,7)
      , (11,-1), (11,4), (11,5), (11,6), (11,7), (12,-1), (12,6), (12,7)
      , (13,-1), (13,5), (13,6), (13,7), (13,9), (13,10), (13,11), (13,12)
      , (14,-1), (14,5), (14,6), (14,7), (14,8), (14,9), (14,11), (14,12)
      , (15,-1), (15,7), (16,-1), (16,0), (16,1), (16,2), (16,3), (16,4)
      , (16,5), (16,6), (16,7), (17,-1), (17,0), (17,1), (17,2), (17,3)
      , (17,4), (17,5), (17,6), (18,-1), (18,2), (18,3), (18,5), (18,6)
      , (19,-1), (19,3), (20,-1), (21,-1), (22,-1), (23,-1), (24,-1), (24,0)
      , (25,-1), (25,0), (25,1), (25,2), (26,-1), (26,0), (26,1), (26,2)
      , (27,-1), (27,0), (27,1), (28,-1), (28,0), (28,1), (28,2), (28,3)
      , (29,-1), (29,0), (29,1), (29,2), (30,-1), (30,0), (30,1), (31,-1)
      , (31,0), (31,1)
      ]
      |> List.map ( \ij -> ( ij, Color.black ) )
      |> Tiles.fromList
    }
