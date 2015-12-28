module Utils where

import Date.Format
import Date exposing (..)
import Time exposing (..)

timeToDateString : Time -> String
timeToDateString time =
  toString <| Date.Format.format "%Y-%m-%d" <| Date.fromTime time

dateStringToTime : String -> Time
dateStringToTime dateString =
  let
    converted =
      Date.fromString dateString
  in
    case converted of
      Ok result ->
        toTime result
      Err _ ->
        -1

monthToInt : Date.Month -> Int
monthToInt month =
  case month of
    Date.Jan -> 0
    Date.Feb -> 1
    Date.Mar -> 2
    Date.Apr -> 3
    Date.May -> 4
    Date.Jun -> 5
    Date.Jul -> 6
    Date.Aug -> 7
    Date.Sep -> 8
    Date.Oct -> 9
    Date.Nov -> 10
    Date.Dec -> 11

compareDates : (Int,Int,Int) -> (Int,Int,Int) -> Order
compareDates (yearOne, monthOne, dayOne) (yearTwo, monthTwo, dayTwo) =
  if (yearOne < yearTwo) then
    LT
  else if (yearOne > yearTwo) then
    GT
  else if (monthOne < monthTwo) then
    LT
  else if (monthOne > monthTwo) then
    GT
  else if (dayOne < dayTwo) then
    LT
  else if (dayOne > dayTwo) then
    GT
  else
    EQ

reverseComparison : Order -> Order
reverseComparison comp =
  case comp of
    LT -> GT
    GT -> LT
    EQ -> EQ
