module Main where

import Html exposing (..)
import ItemList
import Signal
import KeyboardInput
import AddReminder
import Graphics.Element exposing (show)
import Time exposing (Time)
import Utils
import JsonReader
import Effects exposing (Effects, Never)
import Task
import List

-- Name:
-- Student ID:


-- * Add a hotkey to toggle the visibility of 'done' items.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Hide the 'add reminder' functionality and add a hotkey to toggle its
-- * visibility.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Put the current date as the default in the date picker when adding
-- * reminders.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Add a deadline property to reminders and mark all reminders that are past
-- * their deadline.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Add a 'snooze' feature to items, to 'snooze' an item you must provide a
-- * date on which the item has to 'un-snooze'. 'snoozed' items are not visible.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * On startup, read e-mails from a Json document at this url:
-- * http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Periodically check for e-mails from Json (same url).
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Add persistence to your application by using Html local storage so that
-- * newly added reminders are still there after a reload.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Come up with your own extension!
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- Start of program

port jsonRequests : Signal (Task.Task Never ())
port jsonRequests =
  app.jsonTasksSignal

type alias App =
  { itemListState : Signal ItemList.Model
  , jsonTasksSignal : Signal (Task.Task Never ())
  }

app : App
app =
  let
    singleton action = [ action ]

    messages =
        Signal.mailbox []
    address =
        Signal.forwardTo messages.address singleton

    timeSignal =
      Time.every Time.millisecond

    itemListActions =
      Signal.map (\time -> ItemList.TimeUpdate time) timeSignal
    jsonRequestActions =
      Signal.map (\_ -> JsonReader.TimeUpdate) timeSignal

    jsonUpdateStep action (oldModel, _) = JsonReader.update action oldModel
    update actions (model, _) =
        List.foldl jsonUpdateStep (model, Effects.none) actions

    jsonFetchUrl =
      "https://crossorigin.me/https://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json"

    initialJsonReaderModel =
      JsonReader.init jsonFetchUrl Time.minute

    requestActionsToList =
      Signal.map singleton jsonRequestActions
    inputs =
      Signal.merge messages.signal requestActionsToList
    jsonReadState =
      Signal.foldp update (initialJsonReaderModel, Effects.none) inputs

    allJsonReaderModels =
      Signal.map (\x -> fst x) jsonReadState
    jsonItemListFilterFunction jsonReaderModel =
      jsonReaderModel.hasEmails
    filteredJsonReaderModels =
      Signal.filter jsonItemListFilterFunction initialJsonReaderModel allJsonReaderModels
    jsonItemListActions =
      Signal.map (\model -> ItemList.AddAllEmails model.emailList) filteredJsonReaderModels

    theJsonEffectsSignal =
      Signal.map (\x -> snd x) jsonReadState
  in
    { itemListState = Signal.foldp ItemList.update ItemList.init (Signal.mergeMany [jsonItemListActions, ItemList.actions, KeyboardInput.keyboardInput, itemListActions])
    , jsonTasksSignal = Signal.map (Effects.toTask address) theJsonEffectsSignal
    }

{--}
main : Signal Html.Html
main =
  let
    viewFeed =
      Signal.map (ItemList.view ItemList.actionAddress) app.itemListState
  in
    viewFeed
--}

{--
main =
  Signal.map show (Time.every Time.millisecond)
--}

{--
itemListState : Signal ItemList.Model
itemListState =
  Signal.foldp ItemList.update ItemList.init (Signal.mergeMany [ItemList.actions, KeyboardInput.keyboardInput, Signal.map toItemListAction (Time.every Time.millisecond)])
--}

{--
timeKeeperState : Signal TimeKeeper.Model
timeKeeperState =
  Signal.foldp TimeKeeper.update TimeKeeper.init (Signal.map (\x -> TimeKeeper.AddMillisecond) (Time.every Time.millisecond))
--}

toItemListAction : Time -> ItemList.Action
toItemListAction time =
  ItemList.TimeUpdate time

watchSignal : String -> Signal a -> Signal a
watchSignal caption = Signal.map (Debug.watch caption)
--}
