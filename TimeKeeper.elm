module TimeKeeper(Model, Action(..), init, state, update, timeAddress, timeActions, view, loadTime, toItemListAction) where

import Native.CurrentTime
import Html exposing(..)
import Task exposing (Task)
import Time exposing (Time)
import Date exposing (Date)
import Signal
import Date.Format
import ItemList exposing (Action)

-- MODEL

type alias Model =
  { currentTime : Time
  }

loadTime : Time
loadTime =
  Native.CurrentTime.loadTime

init : Model
init =
  { currentTime = loadTime
  }

getCurrentDate : Model -> Date
getCurrentDate model =
  Date.fromTime model.currentTime

state : Signal Model
state =
  Signal.foldp update init timeActions

timeMailBox : Signal.Mailbox Action
timeMailBox =
  Signal.mailbox NoOp

timeAddress : Signal.Address Action
timeAddress =
  timeMailBox.address

timeActions : Signal Action
timeActions =
  timeMailBox.signal

-- UPDATE

type Action
  = AddMillisecond
  | NoOp

update : Action -> Model -> Model
update action model =
  case action of
    AddMillisecond ->
      { model |
          currentTime = model.currentTime + Time.millisecond
      }
    NoOp ->
      model

-- TO ITEMLIST ACTIONS

{--}
toItemListAction : Model -> ItemList.Action
toItemListAction model =
  ItemList.TimeUpdate model.currentTime
  --}

-- VIEW

view : Model -> Html
view model =
  div [] [ text <| toString <| Date.Format.format "%Y-%m-%d" <| getCurrentDate model
         ]
