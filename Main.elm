module Main where

import Html exposing (..)
import ItemList
import Signal
import KeyboardInput
import AddReminder
import TimeKeeper
import Graphics.Element exposing (show)
import Time
import Utils

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

{--}
main : Signal Html.Html
main =
  let
    viewFeed =
      Signal.map (ItemList.view ItemList.actionAddress) itemListState
  in
    viewFeed
--}

{--

main =
  show (Utils.dateStringToTime "2015-12-28")
  --}

itemListState : Signal ItemList.Model
itemListState =
  Signal.foldp ItemList.update ItemList.init (Signal.mergeMany [ItemList.actions, KeyboardInput.keyboardInput, Signal.map TimeKeeper.toItemListAction timeKeeperState])

timeKeeperState : Signal TimeKeeper.Model
timeKeeperState =
  Signal.foldp TimeKeeper.update TimeKeeper.init (Signal.map (\x -> TimeKeeper.AddMillisecond) (Time.every Time.millisecond))

watchSignal : String -> Signal a -> Signal a
watchSignal caption = Signal.map (Debug.watch caption)
--}
