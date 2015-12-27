module ItemList (Model, init, Action(..), update, view, actions, actionAddress, state) where

import Html exposing (..)
import Static
import Item
import Html.Attributes exposing (..)

{--}
-- MAIN
state : Signal Model
state =
  Signal.foldp update init actions

actionMailBox : Signal.Mailbox Action
actionMailBox =
  Signal.mailbox NoOp

actions : Signal Action
actions =
  actionMailBox.signal

actionAddress : Signal.Address Action
actionAddress =
  actionMailBox.address
--}

-- MODEL

type alias Model =
  { items : List (ID, Bool, Item.Model)
  , nextID : Int
  , focusOn : Int
  , altSort : Bool
  }

type alias ID = Int


init : Model
init =
  let
    addReminders =
      List.map AddReminder Static.reminders
    addEmails =
      List.map AddEmail Static.emails
    updates =
      addReminders `List.append` addEmails
  in
    List.foldl update initEmpty updates

initEmpty : Model
initEmpty =
  { items = []
  , nextID = 0
  , focusOn = 0
  , altSort = False
  }

{--
init : Model
init =
  let
    initModel =
      { items = (reminderListBuilder 0 Static.reminders) `List.append` (emailListBuilder (List.length Static.reminders) Static.emails)
      , nextID = List.length Static.reminders + List.length Static.emails
      , focusOn = 0
      , altSort = False
      }
    first = List.head (mainSort initModel).items
    rest = List.tail (mainSort initModel).items
  in
    case first of
      Nothing ->
        initModel
      Just (_, _, itemModel) ->
        case rest of
          Nothing ->
            { initModel |
                items = [(0, True, { itemModel | focus = True })]
            }
          Just theTail ->
            { initModel |
                items = (0, True, {itemModel | focus = True}) :: theTail
            }

reminderListBuilder : Int -> List Static.Reminder -> List(ID, Bool, Item.Model)
reminderListBuilder acc list =
  let
    first = List.head list
    rest = List.tail list
  in
    case first of
      Nothing ->
        []
      Just item ->
        let
          model = Item.initReminder item
        in
          case rest of
            Nothing ->
              [(acc, False, model)]
            Just theTail ->
              (acc, False, model) :: (reminderListBuilder (acc+1) theTail)

emailListBuilder : Int -> List Static.Email -> List(ID, Bool, Item.Model)
emailListBuilder acc list =
  let
    first = List.head list
    rest = List.tail list
  in
    case first of
      Nothing ->
        []
      Just item ->
        let
          model = Item.initEmail item
        in
          case rest of
            Nothing ->
              [(acc, False, model)]
            Just theTail ->
              (acc, False, model) :: (emailListBuilder (acc+1) theTail)
--}


-- UPDATE

type Action
  = AddReminder Static.Reminder
  | AddEmail Static.Email
  | NextFocus
  | PreviousFocus
  | Modify ID Item.Action
  | AltSort
  | MainSort
  {--}
  | FixFocus
  | NoOp
  | TruncateCurrent
  | PinCurrent
  | DoneCurrent
  --}

update : Action -> Model -> Model
update action model =
  case action of
    AddReminder reminder ->
      let
        reminderModel = Item.initReminder reminder
        newModel =
          { items = (model.nextID, False, reminderModel) :: model.items
          , nextID = model.nextID + 1
          , focusOn = model.focusOn
          , altSort = model.altSort
          }
      in
        if newModel.altSort then
          altSort newModel
        else
          mainSort newModel
    AddEmail email ->
      let
        emailModel = Item.initEmail email
        newModel =
          { items = (model.nextID, False, emailModel) :: model.items
          , nextID = model.nextID + 1
          , focusOn = model.focusOn
          , altSort = model.altSort
          }
      in
        if newModel.altSort then
          altSort newModel
        else
          mainSort newModel
    NextFocus ->
      let
        nextFocus = (model.focusOn + 1) % List.length model.items
        newModel =
          { items = model.items
          , nextID = model.nextID
          , focusOn = nextFocus
          , altSort = model.altSort
          }
      in
        fixFocus newModel
    PreviousFocus ->
      let
        nextFocus =
          if (model.focusOn - 1) < 0 then
            if List.length model.items == 0 then
              0
            else
              List.length model.items - 1
          else
            model.focusOn - 1
        newModel =
          { items = model.items
          , nextID = model.nextID
          , focusOn = nextFocus
          , altSort = model.altSort
          }
      in
        fixFocus newModel
    Modify itemID itemAction ->
      let
        updateModel (modelID, bool, itemModel) =
          if modelID == itemID then
            (modelID, bool, Item.update itemAction itemModel)
          else
            (modelID, bool, itemModel)
        newModel =
          { items = List.map updateModel model.items
          , nextID = model.nextID
          , focusOn = model.focusOn
          , altSort = model.altSort
          }
      in
        if newModel.altSort then
          altSort newModel
        else
          mainSort newModel
    AltSort ->
      let
        newModel =
          { model |
              altSort = True
          }
      in
        altSort newModel
    MainSort ->
      let
        newModel =
          { model |
              altSort = False
          }
      in
        mainSort newModel
    {--}
    FixFocus ->
      fixFocus model
    TruncateCurrent ->
      let
        curr = currentItem model
      in
        if curr.truncated then
          itemActionCurrent Item.More model
        else
          itemActionCurrent Item.Less model
    PinCurrent ->
      let
        curr = currentItem model
      in
        if curr.pinned then
          itemActionCurrent Item.Unpin model
        else
          itemActionCurrent Item.Pin model
    DoneCurrent ->
      let
        curr =
          currentItem model
      in
        if curr.done then
          itemActionCurrent Item.Undo model
        else
          itemActionCurrent Item.Do model
    NoOp ->
      model
    --}

mainSort : Model -> Model
mainSort model =
  let
    newModel =
      { model |
          items = List.sortWith mainComparison model.items
      }
  in
    fixFocus newModel

altSort : Model -> Model
altSort model =
  let
    newModel =
      { model |
          items = List.sortWith altComparison model.items
      }
  in
    fixFocus newModel

fixFocus : Model -> Model
fixFocus model =
  let
    newList = fixFocus' 0 model.focusOn model.items
  in
    { model |
        items = newList
    }


fixFocus' acc focusID itemList =
  let
    first = List.head itemList
    rest = List.tail itemList
  in
    case first of
      Nothing ->
        []
      Just (itemID, focus, itemModel) ->
        let
          fixedModel =
            if focusID == acc then
              { itemModel |
                  focus = True
              }
            else
              { itemModel |
                  focus = False
              }
          fixedItem =
            if focusID == itemID then
              (itemID, True, fixedModel)
            else
              (itemID, False, fixedModel)
        in
          case rest of
            Nothing ->
              [fixedItem]
            Just restList ->
              fixedItem :: (fixFocus' (acc + 1) focusID restList)




mainComparison : (Int, Bool, Item.Model) -> (Int, Bool, Item.Model) -> Order
mainComparison (_, _, one) (_, _, two) =
  if (one.done && not two.done) then
    GT
  else if (not one.done && two.done) then
    LT
  else if (one.pinned && not two.pinned) then
    LT
  else if (not one.pinned && two.pinned) then
    GT
  else
    let
      tupleOne = Item.extractDate(one)
      tupleTwo = Item.extractDate(two)
    in
      compareDates tupleOne tupleTwo

altComparison : (Int, Bool, Item.Model) -> (Int, Bool, Item.Model) -> Order
altComparison (_, _, one) (_, _, two) =
  if (one.done && not two.done) then
    GT
  else if (not one.done && two.done) then
    LT
  else
    let
      tupleOne = Item.extractDate(one)
      tupleTwo = Item.extractDate(two)
    in
      reverseComparison (compareDates tupleOne tupleTwo)

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

{--}
currentItem : Model -> Item.Model
currentItem model =
  let
    inList =
      (List.drop (model.focusOn - 1)) <| (List.take model.focusOn model.items)
    theItem =
      List.head inList
  in
    case theItem of
      Nothing ->
        Item.initReminder { body = "", created = "" }
      Just (_, _, item) ->
        item
--}

itemActionCurrent : Item.Action -> Model -> Model
itemActionCurrent action model =
  let
    newItems = itemActionCurrent' 0 model.focusOn action model.items
    newModel =
      { model |
          items = newItems
      }
  in
    if newModel.altSort then
      altSort newModel
    else
      mainSort newModel

itemActionCurrent' : Int -> Int -> Item.Action -> List(ID, Bool, Item.Model) -> List(ID, Bool, Item.Model)
itemActionCurrent' acc focusID action itemList =
  let
    first = List.head itemList
    rest = List.tail itemList
  in
    case first of
      Nothing ->
        []
      Just (itemID, bool, itemModel) ->
        let
          restList =
            case rest of
              Nothing ->
                []
              Just theTail ->
                theTail
        in
          if acc == focusID then
            (itemID, bool, (Item.update action itemModel)) :: restList
          else (itemID, bool, itemModel) :: (itemActionCurrent' (acc + 1) focusID action restList)
-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    todos = takeTodo model
    dones = takeDone model
    todoHtml = List.map (genDiv address) todos
    doneHtml = List.map (genDiv address) dones
  in
    div []
      ( [div [] [Html.h1 [] [Html.text "Todo"]]]
        `List.append`
        todoHtml
        `List.append`
        [div [] [Html.h1 [] [Html.text "Done"]]]
        `List.append`
        doneHtml
      )

genDiv : Signal.Address Action -> (ID, Bool, Item.Model) -> Html
genDiv address (itemID, bool, itemModel) =
  div [] [viewItem address (itemID, bool, itemModel)]

viewItem : Signal.Address Action -> (ID, Bool, Item.Model) -> Html
viewItem address (id, bool, model) =
  Item.view (Signal.forwardTo address (Modify id)) model

takeTodo : Model -> List(ID, Bool, Item.Model)
takeTodo model =
  takeTodo' model.items

takeTodo' items =
  let
    first = List.head items
    rest = List.tail items
  in
    case first of
      Nothing ->
        []
      Just (itemID, bool, itemModel) ->
        if itemModel.done then
          []
        else
          case rest of
            Nothing ->
              [(itemID, bool, itemModel)]
            Just restList ->
              (itemID, bool, itemModel) :: (takeTodo' restList)

takeDone : Model -> List(ID, Bool, Item.Model)
takeDone model =
  takeDone' model.items

takeDone' items =
  let
    first = List.head items
    rest = List.tail items
  in
    case first of
      Nothing ->
        []
      Just (itemID, bool, itemModel) ->
        if itemModel.done then
          case rest of
            Nothing ->
              [(itemID, bool, itemModel)]
            Just restList ->
              (itemID, bool, itemModel) :: (takeDone' restList)
        else
          case rest of
            Nothing ->
              []
            Just restList ->
              takeDone' restList
