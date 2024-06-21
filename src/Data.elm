module Data exposing (..)

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
  = Running
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

statusToString : GameStatus -> String
statusToString status =
  case status of
    Running -> "Running"
    GameOver -> "Game Over"
