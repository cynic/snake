module Main exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random
import Time
import Json.Decode as D
import Browser.Events
import Browser
-- This is a typical "Snake" game. You have to eat the food in order to get points and avoid the walls and your own tail.
-- The game is over when you hit the wall or your own tail.
-- The game has a score system that increases by 1 every time you eat the food.
-- The game has a speed system that increases by 1 every time you eat the food.
-- The game has a pause system that you can use by pressing the "P" key.
-- The game has a restart system that you can use by pressing the "R" key.
-- The game has a quit system that you can use by pressing the "Q" key.
-- You can move by using the arrow keys on the keyboard.
-- The snake is displayed as a series of orange circles.  Each orange circle is 10px in diameter.
-- The food is displayed as a red circle.  Each red circle is 10px in diameter.
-- The walls are displayed as a black rectangle.
-- The arena of the snake is a 200x100 rectangular grid.

-- Data structures first
type Direction
  = Left
  | Right
  | Up
  | Down

type alias Coordinate =
  (Int, Int)

type alias Snake =
  { direction : Direction
  , body : List Coordinate
  }

type alias Food =
  Coordinate

type GameStatus
  = Paused
  | Running
  | GameOver

type alias Model =
  { snake : Snake
  , food : Food
  , score : Int
  , speed : Int
  , status : GameStatus
  }

type Msg
  = Move
  | ChangeDirection Direction
  | GetRandomCoordinate (Int, Int)
  | EatFood
  | Pause
  | Restart
  | Quit

arenaWidth : Int
arenaWidth =
  50

arenaHeight : Int
arenaHeight =
  20

scalingFactor : Int
scalingFactor =
  15

-- Update function
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Move ->
      case model.status of
        Running ->
          let
            newSnake =
              moveSnake model.snake
          in
            if isGameOver newSnake then
              ( { model | status = GameOver }, Cmd.none )
            else if hasEatenFood newSnake model.food then
              ( { model
                | snake = extendSnakeBody model.snake
                , score = model.score + 1
                , speed = model.speed + 1
                }
              , getRandomCoordinate
              )
            else
              ( { model | snake = newSnake }, Cmd.none )

        _ ->
          ( model, Cmd.none )

    ChangeDirection newDirection ->
      ( { model | snake = { direction = newDirection, body = model.snake.body } }, Cmd.none )

    GetRandomCoordinate (width, height) ->
      ( { model | food = (width, height) }, Cmd.none )

    EatFood ->
      ( { model | score = model.score + 1, speed = model.speed + 1 }, getRandomCoordinate )

    Pause ->
      ( { model | status = if model.status == Paused then Running else Paused }, Cmd.none )

    Restart ->
      init

    Quit ->
      ( { model | status = GameOver }, Cmd.none )

-- Helper functions
nextSnakeBlock : Snake -> Coordinate
nextSnakeBlock snake =
  -- remember, the snake wraps around the arena.
  let
    (x, y) =
      List.head snake.body |> Maybe.withDefault (0, 0)
  in
    case snake.direction of
      Left ->
        if x == 0 then
          (arenaWidth - 1, y)
        else
          (x - 1, y)
      
      Right ->
        if x == arenaWidth - 1 then
          (0, y)
        else
          (x + 1, y)

      Up ->
        if y == 0 then
          (x, arenaHeight - 1)
        else
          (x, y - 1)

      Down ->
        if y == arenaHeight - 1 then
          (x, 0)
        else
          (x, y + 1)

moveSnake : Snake -> Snake
moveSnake snake =
  let
    newHead = nextSnakeBlock snake
  in
  { snake | body = newHead :: List.take (List.length snake.body - 1) snake.body }

extendSnakeBody : Snake -> Snake
extendSnakeBody snake =
  let
    newHead = nextSnakeBlock snake
  in
  { snake | body = newHead :: snake.body }

init : (Model, Cmd Msg)
init =
  ( { snake = { direction = Right, body = [(0, 0)] }
    , food = (arenaWidth // 2, arenaHeight // 2)
    , score = 0
    , speed = 1
    , status = Running
    }
  , getRandomCoordinate
  )

isGameOver : Snake -> Bool
isGameOver snake =
  let
    (x, y) =
      List.head snake.body |> Maybe.withDefault (0, 0)
  in
  List.member (x, y) (List.drop 1 snake.body)

hasEatenFood : Snake -> Food -> Bool
hasEatenFood snake food =
  let
    (x, y) =
      List.head snake.body |> Maybe.withDefault (0, 0)
  in
  (x, y) == food

-- Elm uses a system where you need to call out to JavaScript to do things like generate random numbers.
getRandomCoordinate : Cmd Msg
getRandomCoordinate =
  Random.pair (Random.int 0 (arenaWidth - 1)) (Random.int 0 (arenaHeight - 1))
  |> Random.generate GetRandomCoordinate

statusToString : GameStatus -> String
statusToString status =
  case status of
    Paused -> "Paused"
    Running -> "Running"
    GameOver -> "Game Over"

fst : (a, b) -> a
fst (a, _) = a

snd : (a, b) -> b
snd (_, b) = b

-- View function
view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Snake" ]
    , div [] [ text <| "Score: " ++ String.fromInt model.score ]
    , div [] [ text <| "Speed: " ++ String.fromInt model.speed ]
    , div [] [ text <| "Status: " ++ statusToString model.status ]
    , div [] [ text "Press P to pause, R to restart, Q to quit" ]
    , div
      [ style "width" (String.fromInt (arenaWidth * scalingFactor) ++ "px")
      , style "height" (String.fromInt (arenaHeight * scalingFactor) ++ "px")
      , style "border" "1px solid black"
      , style "position" "absolute"
      , style "left" "0px"
      , style "top" "0px"
      ]
      ([ div
          []
          (List.map
            (\(x, y) ->
              div
                [ style "width" (String.fromInt scalingFactor ++ "px")
                , style "height" (String.fromInt scalingFactor ++ "px")
                , style "background-color" "orange"
                , style "position" "absolute"
                , style "border-radius" "50%"
                , style "left" (String.fromInt (x * scalingFactor) ++ "px")
                , style "top" (String.fromInt (y * scalingFactor) ++ "px")
                ]
                []
            ) model.snake.body
          )
      , div
          [ style "width" (String.fromInt scalingFactor ++ "px")
          , style "height" (String.fromInt scalingFactor ++ "px")
          , style "background-color" "red"
          , style "position" "absolute"
          , style "left" (String.fromInt (fst model.food * scalingFactor) ++ "px")
          , style "top" (String.fromInt (snd model.food * scalingFactor) ++ "px")
          ]
          []
      ])
    ]

keyDecoder : D.Decoder Msg
keyDecoder =
  D.field "key" D.string
    |> D.andThen
        (\key ->
          case key of
            "ArrowLeft" -> D.succeed (ChangeDirection Left)
            "ArrowRight" -> D.succeed (ChangeDirection Right)
            "ArrowUp" -> D.succeed (ChangeDirection Up)
            "ArrowDown" -> D.succeed (ChangeDirection Down)
            "p" -> D.succeed Pause
            "r" -> D.succeed Restart
            "q" -> D.succeed Quit
            _ -> D.fail "Invalid key"
        )

-- Depending on the speed, we want to move the snake.  We also want to subscribe to keypresses.
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every (1000 / toFloat model.speed) (\_ -> Move)
    , Browser.Events.onKeyDown keyDecoder
    ]

-- Main function
main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }