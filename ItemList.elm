module ItemList (Model, init, Action(..), update, view, actions, actionAddress, state) where

import Html exposing (..)
import Html.Events exposing (onClick)
import Static
import Item
import Html.Attributes exposing (..)
import AddReminder
import Utils
import Time exposing (Time)

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
  , addReminder : AddReminder.Model
  , altSort : Bool
  , hideDone : Bool
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
  , addReminder = AddReminder.init False
  , altSort = False
  , hideDone = False
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
  | ModifyAddReminder AddReminder.Action
  | ToggleHideAddReminder
  | AltSort
  | MainSort
  {--}
  | FixFocus
  | NoOp
  | TruncateCurrent
  | PinCurrent
  | DoneCurrent
  | ToggleHideDone
  | TimeUpdate Time
  --}

update : Action -> Model -> Model
update action model =
  case action of
    AddReminder reminder ->
      let
        reminderModel = Item.initReminder reminder
        newModel =
          { model |
              items = (model.nextID, False, reminderModel) :: model.items
          ,   nextID = model.nextID + 1
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
          { model |
              items = (model.nextID, False, emailModel) :: model.items
          ,   nextID = model.nextID + 1
          }
      in
        if newModel.altSort then
          altSort newModel
        else
          mainSort newModel
    NextFocus ->
      let
        nextFocus = (model.focusOn + 1) % (focusListIndex model)
        newModel =
          { model |
              focusOn = nextFocus
          }
      in
        fixFocus newModel
    PreviousFocus ->
      let
        nextFocus =
          if (model.focusOn - 1) < 0 then
            if (focusListIndex model) == 0 then
              0
            else
              (focusListIndex model) - 1
          else
            model.focusOn - 1
        newModel =
          { model |
              focusOn = nextFocus
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
          { model |
              items = List.map updateModel model.items
          }
      in
        if newModel.altSort then
          altSort newModel
        else
          mainSort newModel
    ModifyAddReminder addReminderAction ->
      let
        updatedAddReminder = AddReminder.update addReminderAction model.addReminder
        updatedModel =
          case addReminderAction of
            AddReminder.AddReminder reminder hideIt ->
              update (AddReminder reminder) model
            _ ->
              model
      in
        { updatedModel |
            addReminder = updatedAddReminder
        }
    ToggleHideAddReminder ->
      { model |
          addReminder = AddReminder.update AddReminder.ToggleHide model.addReminder
      }
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
        itemActionCurrent Item.ToggleTruncate model
    PinCurrent ->
      let
        curr = currentItem model
      in
        itemActionCurrent Item.TogglePin model
    DoneCurrent ->
      let
        curr =
          currentItem model
      in
        itemActionCurrent Item.ToggleDo model
    ToggleHideDone ->
      let
        notHideDone = not model.hideDone
        newModel =
          { model |
              hideDone = notHideDone
          }
      in
        fixFocus newModel
    TimeUpdate time ->
      let
        theItems = List.map (\(id, bool, itemModel) -> (id, bool, (Item.update (Item.TimeUpdate time) itemModel))) model.items
        newModel =
          { model |
              items = theItems
          ,   addReminder = AddReminder.update (AddReminder.TimeUpdate time) model.addReminder
          }
      in
        if newModel.altSort then
          altSort newModel
        else
          mainSort newModel
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
    maxFocusLength = focusListIndex model
    newModel =
      if model.focusOn >= maxFocusLength then
        let
          newFocusOn =
            if maxFocusLength == 0 then
              0
            else
              maxFocusLength - 1
        in
          { model |
              focusOn = newFocusOn
          }
      else
        model
    newList = fixFocus' 0 newModel.focusOn newModel.items
  in
    { newModel |
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
            if focusID == acc && not itemModel.snooze then
              { itemModel |
                  focus = True
              }
            else
              { itemModel |
                  focus = False
              }
          fixedItem =
            if focusID == itemID && not itemModel.snooze then
              (itemID, True, fixedModel)
            else
              (itemID, False, fixedModel)
          newAcc =
            if not itemModel.snooze then
              acc + 1
            else
              acc
        in
          case rest of
            Nothing ->
              [fixedItem]
            Just restList ->
              fixedItem :: (fixFocus' newAcc focusID restList)

focusListIndex : Model -> Int
focusListIndex model =
  let
    theItems =
      List.map (\(id, bool, itemModel) -> itemModel) model.items
    filterDones =
      if model.hideDone then
        List.filter (\x -> not x.done) theItems
      else
        theItems
    noSnooze =
      filterSnooze filterDones
  in
    List.length noSnooze

filterSnooze : List(Item.Model) -> List(Item.Model)
filterSnooze items =
  List.filter (\x -> not x.snooze) items

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
      Utils.compareDates tupleOne tupleTwo

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
      Utils.reverseComparison (Utils.compareDates tupleOne tupleTwo)

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
        Item.initReminder { body = "", created = "", deadline = "" }
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
          if itemModel.snooze then
            (itemID, bool, itemModel) :: (itemActionCurrent' acc focusID action restList)
          else if acc == focusID then
            (itemID, bool, (Item.update action itemModel)) :: restList
          else (itemID, bool, itemModel) :: (itemActionCurrent' (acc + 1) focusID action restList)
-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    todos = takeTodo model
    dones = takeDone model
    todoHtml = List.map (genDiv address) todos
    doneHtml =
      if model.hideDone then
        []
      else
        List.map (genDiv address) dones
    addReminderView =
      AddReminder.view (Signal.forwardTo address ModifyAddReminder) model.addReminder
  in
    div []
      ( [div [] [Html.h1 [] [Html.text "Todo"]]]
        `List.append`
        todoHtml
        `List.append`
        [div [] [Html.h1 [] [Html.text "Done"], button [ onClick address ToggleHideDone ][ text "Toggle Hide Done Items" ]]]
        `List.append`
        doneHtml
        `List.append`
        [div [] [addReminderView]]
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
          let
            newHead =
              if itemModel.snooze then
                []
              else
                [(itemID, bool, itemModel)]
          in
            case rest of
              Nothing ->
                newHead
              Just restList ->
                newHead `List.append` (takeTodo' restList)

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
          let
            newHead =
              if itemModel.snooze then
                []
              else
                [(itemID, bool, itemModel)]
          in
            case rest of
              Nothing ->
                newHead
              Just restList ->
                newHead `List.append` (takeDone' restList)
        else
          case rest of
            Nothing ->
              []
            Just restList ->
              takeDone' restList
