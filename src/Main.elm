module Main exposing (..)
import Time
import Json.Decode as D
import Browser.Events
import Browser
import Data exposing (..)
import Library exposing (..)

-- Update function
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Move ->
      case model.status of
        Running ->
          let
            newSnake = moveSnake model.snake
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
      ( { model
        | snake = { direction = newDirection, body = model.snake.body }
        }
      , Cmd.none
      )

    GetRandomCoordinate (width, height) ->
      ( { model | food = (width, height) }, Cmd.none )

    EatFood ->
      ( { model | score = model.score + 1, speed = model.speed + 1 }, getRandomCoordinate )

    Restart ->
      init

    Quit ->
      ( { model | status = GameOver }, Cmd.none )

-- Helper functions
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

-- View function

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