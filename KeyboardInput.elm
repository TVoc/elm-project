module KeyboardInput where

import ItemList exposing (Action)
import Keyboard
import Char

-- SIGNALS

keyWithAlt : Char -> Signal Bool
keyWithAlt key =
  Signal.map2 (\x y -> x && y) Keyboard.alt (Keyboard.isDown <| Char.toCode <| Char.toUpper key)

keyWithAltRelease : Char -> Signal Bool
keyWithAltRelease key =
  Signal.map2 (\x y -> not x || not y) Keyboard.alt (Keyboard.isDown <| Char.toCode <| Char.toUpper key)

keyToAction : ItemList.Action -> Signal Bool -> Signal ItemList.Action
keyToAction action key =
  let filterTrue =
    Signal.filter (\x -> x) False key
  in
    Signal.map (\x -> action) filterTrue

toNext : Signal ItemList.Action
toNext =
  keyToAction ItemList.NextFocus <| keyWithAlt 'j'

toPrevious : Signal ItemList.Action
toPrevious =
  keyToAction ItemList.PreviousFocus <| keyWithAlt 'k'

toTruncate : Signal ItemList.Action
toTruncate =
  keyToAction ItemList.TruncateCurrent <| keyWithAlt 'o'

toPin : Signal ItemList.Action
toPin =
  keyToAction ItemList.PinCurrent <| keyWithAlt 'p'

toDone : Signal ItemList.Action
toDone =
  keyToAction ItemList.DoneCurrent <| keyWithAlt 'x'

toAltSort : Signal ItemList.Action
toAltSort =
  keyToAction ItemList.AltSort <| keyWithAlt 's'

toMainSort : Signal ItemList.Action
toMainSort =
  keyToAction ItemList.MainSort <| keyWithAltRelease 's'

toHideAddReminder =
  keyToAction ItemList.ToggleHideAddReminder <| keyWithAlt 'h'

keyboardInput : Signal ItemList.Action
keyboardInput =
   Signal.mergeMany [ toNext, toPrevious, toAltSort, toMainSort, toTruncate, toPin,
                     toDone, toHideAddReminder
                    ]

watchSignal : String -> Signal a -> Signal a
watchSignal caption = Signal.map (Debug.watch caption)
