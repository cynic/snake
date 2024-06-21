module Library exposing (..)
import Data exposing (..)
import Random
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
-- Elm uses a system where you need to call out to JavaScript to do things like generate random numbers.


{-| Asks for a random coordinate within the boundaries of the game arena.

   The first integer represents the x-coordinate, and the second integer represents the y-coordinate.

   This will generate a `GetRandomCoordinate` message, which contains the coordinates

   # Example
   ```
   getRandomCoordinate --> GetRandomCoordinate (x, y)
   ```
-}
getRandomCoordinate : Cmd Msg
getRandomCoordinate =
  Random.pair (Random.int 0 (arenaWidth - 1)) (Random.int 0 (arenaHeight - 1))
  |> Random.generate GetRandomCoordinate
-- END: ed8c6549bwf9
{-| Extracts the first item from a pair of items.

  # Examples
  ```
  fst (1, 2) --> 1
  fst ("hello", "world") --> "hello"
  ```
-}
fst : (a, b) -> a
fst (a, _) = a

{--| Extracts the second item from a pair of items.

  # Examples
  ```
  snd (1, 2) --> 2
  snd ("hello", "world") --> "world"
  ```
-}
snd : (a, b) -> b
snd (_, b) = b

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

{-|
  Calculates the coordinates of the next block for the snake based on its current position and direction.
  The snake wraps around the arena, so if it reaches the edge, it appears on the opposite side.

  # Arguments
  - `snake` : The snake object representing the current state of the snake.

  # Returns
  The coordinates of the next block for the snake.

  # Example
  ```
  let
    snake = Snake { body = [(0, 0)], direction = Right }
    nextBlock = nextSnakeBlock snake
  in
    -- nextBlock will be (1, 0)
  ```

-}
nextSnakeBlock : Snake -> Coordinate
nextSnakeBlock snake =
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

{-|
  This value represents a completely fresh game state, with the snake starting at the top-left corner of the arena, and the food at the center of the arena.
-}
init : (Model, Cmd Msg)
init =
  ( { snake = { direction = Right, body = [(0, 0)] }
    , food = (arenaWidth // 2, arenaHeight // 2)
    , score = 0
    , speed = 1
    , status = Running
    }
  , Cmd.none
  )