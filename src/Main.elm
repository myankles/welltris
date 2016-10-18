port module Main exposing ( main )

import AnimationFrame
import Board
import Board.View
import Char exposing ( fromCode )
import Debug.Extra
import Html exposing (Html)
import Html.App as App
import Html.Attributes
import Html.Events
import Json.Decode
import Keyboard
import Pivot
import Platform.Cmd
import Platform.Sub
import Random
import String
import Svg
import Svg.Attributes exposing (..)
import Time exposing ( Time )
import Bootstrap.Grid as BGrid

-- Used for debugging sometimes
{--

--}

-- Model type

type alias Model =
  { boards : Pivot.Pivot Board.Model
  , paused : Bool
  , view2d : Bool
  , highScore : Int
  }

-- Message Tupe

type Msg

  -- no message
  = None

  -- Start the Game Over
  | StartOver

  -- Create a new game
  | NewGame { seed: Int }

  -- tick the Board
  | Tick Time

  -- turn pause on and off
  | TogglePause

  -- toggle the view code
  | ToggleView

  -- send a keypress to the board
  | SendBoardKeyPress Keyboard.KeyCode

  -- increase the speed of the game
  | IncreaseSpeed

  -- move back through the history
  | HistoryBack

  -- move forward through the history
  | HistoryForward

  -- Javascript gave us the previous high score
  | NewHighScore Int


{-| The initial model. -}

init : (Model, Cmd Msg)
init =
  ( { boards = Pivot.pure ( Board.init 0 )
    , paused = True
    , view2d = False
    , highScore = 0
    }
  , newRandomGame
  )


{-| Subscriptions -}

subscriptions : Model -> Sub Msg
subscriptions _ =
  Platform.Sub.batch
    [ Keyboard.downs decodeKey
    , AnimationFrame.times Tick
    , highScoreSubscription
    ]


{-| Global update function. -}

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let

    updateBoard boardMsg =
      let
        oldBoard =
          Pivot.getC model.boards

        newBoard =
          Board.update boardMsg oldBoard

        boardsUpdate =
          if oldBoard == newBoard then
            identity
          else
            Pivot.addGoR newBoard
      in
        { model | boards = boardsUpdate model.boards }

  in
    updateHighScore <|
      case ( model.paused, msg ) of
        ( _, None ) ->
          model ! []

        ( False, Tick time ) ->
          updateBoard ( Board.Tick time ) ! []

        ( _, TogglePause ) ->
          { model | paused = not model.paused } ! []

        ( _, ToggleView ) ->
          { model | view2d = ( not model.view2d ) } ! []

        ( False, SendBoardKeyPress keyCode ) ->
          updateBoard ( Board.KeyPress keyCode ) ! []

        ( _, HistoryBack ) ->
          { model | boards = Pivot.withRollback Pivot.goL model.boards,
            paused = True } ! []

        ( True, HistoryForward ) ->
          { model | boards = Pivot.withRollback Pivot.goR model.boards } ! []

        ( _, StartOver ) ->
          ( model, newRandomGame )

        ( _, NewGame { seed } ) ->
          { model
          | boards = Pivot.pure ( Board.init seed )
          , paused = False
          } ! []

        ( False, IncreaseSpeed ) ->
          updateBoard Board.IncreaseSpeed ! []

        ( _, NewHighScore highScore ) ->
          { model
          | highScore = highScore
          } ! []

        _ ->
          model ! []


{-| View everything. -}

view : Model -> Html Msg
view model =
  let
    boardView =
      case model.view2d of
        True -> boardView2d
        False -> boardView3d

    showDebugString paused =
      if paused then
        Html.div [ Html.Attributes.style [( "margin-top", "10px" )] ]
          [ Html.strong [] [ Html.text "board : " ]
          , Html.text ( Board.compilableString ( Pivot.getC model.boards ) )
          ]
      else
        Html.text ""

    -- style =
    --   [ Html.Attributes.style
    --     [ ( "margin-left", "10px")
    --     , ( "margin-top", "10px")
    --     ]
    --   ]

    board =
      Pivot.getC model.boards
  in
    BGrid.container
        [ BGrid.row
            [ BGrid.column [ BGrid.ExtraSmall BGrid.Three ] [ ]
            , BGrid.column [ BGrid.ExtraSmall BGrid.Four ]
                [ Svg.svg [ viewBox "0 0 400 400", width "400px" ]
                    [ Board.view boardView ( Pivot.getC model.boards ) ]
                ]
            , BGrid.column [ BGrid.ExtraSmall BGrid.Two ]
            , BGrid.column [ BGrid.ExtraSmall BGrid.Three ] [ ]
            ]
        , BGrid.row
            [ BGrid.column [ BGrid.ExtraSmall BGrid.Three ] [ ]
            , BGrid.column [ BGrid.ExtraSmall BGrid.Two ]
                [ Board.score board |> toString |> (++) "Score: " |> Html.text ]
            , BGrid.column [ BGrid.ExtraSmall BGrid.Two ]
                [ model.highScore |> toString |> (++) "High Score: " |> Html.text ]
            , BGrid.column [ BGrid.ExtraSmall BGrid.Two ]
                [ Board.speed board |> toString |> (++) "Speed: " |> Html.text ]
            , BGrid.column [ BGrid.ExtraSmall BGrid.Three ] [ ]
            ]
        ]
    {--
    Html.div style
    [
      ( Svg.svg [ viewBox "0 0 400 400", width "400px" ]

      )
      , Html.div []
          [ Board.score board |> toString |> (++) "Score: " |> Html.text
          ]
      , Html.div []
          [
          ]
      , Html.div []
          [
          ]
      , Html.div []
          [ Html.button [ Html.Events.onClick IncreaseSpeed ]
              [ Html.text "Increase speed" ]
          , Html.button [ Html.Events.onClick StartOver ]
              [ Html.text "Start over" ]
          , Html.button [ Html.Events.onClick TogglePause ]
              [ Html.text <|
                  case model.paused of
                    False -> "Pause"
                    True -> "Resume"
              ]
          ]
      , showDebugString model.paused
    ]
    --}


-- Main function

main : Program Never
main =
  App.program
  -- TimeTravel.program
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }


-- Decode a key command.

decodeKey: Keyboard.KeyCode -> Msg
decodeKey keyCode =
  case Char.fromCode keyCode of
    'T' -> ToggleView
    'B' -> HistoryBack
    'N' -> HistoryForward
    _ -> SendBoardKeyPress keyCode


-- Create a new random game.

newRandomGame : Cmd Msg
newRandomGame =
  Random.generate (\seed -> NewGame { seed = seed } )
    (Random.int Random.minInt Random.maxInt)


-- port for getting a new high score from Javascript

port getHighScore : (Json.Decode.Value -> msg) -> Sub msg


-- port for setting a new high score

port setNewHighScore : Int -> Cmd msg


-- sets the high score if necesssary

updateHighScore : ( Model, Cmd msg ) -> ( Model, Cmd msg )
updateHighScore ( model, cmd ) =
  let
    currentScore =
      model.boards |> Pivot.getC |> Board.score
  in
    ( model
    , if currentScore > model.highScore then
        Platform.Cmd.batch [ setNewHighScore currentScore, cmd ]
      else
        cmd
    )


-- subscribes and decodes high scores
highScoreSubscription : Sub Msg
highScoreSubscription =
  let
    -- local storage stores strings so we have to decode those
    intDecoder =
      Json.Decode.oneOf
        [ Json.Decode.int
        , Json.Decode.customDecoder Json.Decode.string String.toInt
        ]

    decodeHighScore intValue =
      case Json.Decode.decodeValue intDecoder intValue of
        Ok highScore ->
          NewHighScore highScore

        Err err ->
          None |> Debug.Extra.log "Json error" err
  in
    getHighScore decodeHighScore


-- Size of the baord in pixels.

boardPixels : Float
boardPixels =
  400.0


-- Method for viewing the board in 3d

boardView3d : Board.View.BoardView msg
boardView3d =
  Board.View.view3dPerspective boardPixels 0.4


-- Method for viewing the board in 2d

boardView2d : Board.View.BoardView msg
boardView2d =
  Board.View.view2d boardPixels
