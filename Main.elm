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

-- Name: Thomas Vochten
-- Student ID: r0300128


-- * Add a hotkey to toggle the visibility of 'done' items.
-- Status: Completed
-- Summary: Alt + I toggles the visibility of done items.
--          In addition, there is a button under the Done header
--          that does the same


-- * Hide the 'add reminder' functionality and add a hotkey to toggle its
-- * visibility.
-- Status: Completed
-- Summary: On start-up, the add reminder functionality is hidden.
--          To show it, either press Alt + H or click the
--          "Unhide Add Reminder" button


-- * Put the current date as the default in the date picker when adding
-- * reminders.
-- Status: Completed
-- Summary: On each time update, the current time is fed into the
--          AddReminder module, which converts that into a date


-- * Add a deadline property to reminders and mark all reminders that are past
-- * their deadline.
-- Status: Completed
-- Summary: All reminders that are past their deadline have a red background
--          in the item feed. All statically loaded reminders have their
--          "created" date as deadline. There seems to be some lag on the time
--          signal which may slow the passage of time in the program, however.


-- * Add a 'snooze' feature to items, to 'snooze' an item you must provide a
-- * date on which the item has to 'un-snooze'. 'snoozed' items are not visible.
-- Status: Completed
-- Summary: Items keep track of whether they're snoozed and, if so,
--          when to unsnooze. Based on that, the ItemList can decide whether to
--          show an item. The same remark made for the deadline extension applies
--          here regarding to time signal.


-- * On startup, read e-mails from a Json document at this url:
-- * http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json
-- Status: Completed
-- Summary: The given url is prefixed with https://crossorigin.me.
--          The e-mails there show up in the feed on startup.


-- * Periodically check for e-mails from Json (same url).
-- Status: Completed
-- Summary: Normally, the JsonReader fetches the e-mails every minute.
--          However, there seems to be a bit of lag on the time signal,
--          so it's more like every two to three minutes.


-- * Add persistence to your application by using Html local storage so that
-- * newly added reminders are still there after a reload.
-- Status: Unattempted
-- Summary:


-- * Come up with your own extension!
-- Status: Unattempted
-- Summary:


-- Start of program

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

    jsonUpdateStep action (oldModel, accumulatedEffects) =
        let
            (newModel, additionalEffects) = JsonReader.update action oldModel
        in
            (newModel, Effects.batch [accumulatedEffects, additionalEffects])
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
    {--}
    jsonItemListFilterFunction jsonReaderModel =
      jsonReaderModel.hasEmails
    filteredJsonReaderModels =
      Signal.filter jsonItemListFilterFunction initialJsonReaderModel allJsonReaderModels
    --}
    jsonItemListActions =
      Signal.map (\model -> ItemList.AddAllEmails model.emailList) filteredJsonReaderModels

  in
    { itemListState = Signal.foldp ItemList.update ItemList.init (Signal.mergeMany [jsonItemListActions, ItemList.actions, KeyboardInput.keyboardInput, itemListActions])
    , jsonTasksSignal = Signal.map (Effects.toTask messages.address << snd) jsonReadState
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

port jsonRequests : Signal (Task.Task Never ())
port jsonRequests =
  app.jsonTasksSignal
--}
