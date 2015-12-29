module Item (Model, initReminder, initEmail, Action(..), update, view, extractDate, extractTitle) where

import Static
import String
import Html exposing (..)
import Html.Events exposing (on, targetValue)
import Html.Events exposing (onClick)
import Html.Attributes exposing(..)
import Date
import Utils
import Time exposing (Time)

{--}
-- MAIN

{--
main : Signal Html.Html
--main = Html.text "Yep" |> Signal.constant
main = Signal.map (view actionItem.address) state
--}

{--
state : Signal Model
state =
  Signal.foldp update init actions

actionItem : Signal.Mailbox Action
actionItem =
  Signal.mailbox NoOp

actions : Signal Action
actions =
  actionItem.signal

actionAddress : Signal.Address Action
actionAddress =
  actionItem.address
--}

-- MODEL

type ItemType =
  Email
  | Reminder

type alias Model =
  { itemtype : ItemType
  , email : Maybe Static.Email
  , reminder : Maybe Static.Reminder
  , done : Bool
  , pinned : Bool
  , truncated : Bool
  , focus : Bool
  , deadlineExpired : Bool
  , snoozeDate : String
  , snooze : Bool
  }

{--
init : ContainedItem -> Model
--}
initReminder : Static.Reminder -> Model
initReminder reminder =
  { itemtype = Reminder
    , email = Nothing
    , reminder = Just reminder
    , done = False
    , pinned = False
    , truncated = True
    , focus = False
    , deadlineExpired = False
    , snoozeDate = "2015-01-01"
    , snooze = False
  }

initEmail : Static.Email -> Model
initEmail email =
  { itemtype = Email
    , email = Just email
    , reminder = Nothing
    , done = False
    , pinned = False
    , truncated = True
    , focus = False
    , deadlineExpired = False
    , snoozeDate = "2015-01-01"
    , snooze = False
  }

extractReminder : Model -> Static.Reminder
extractReminder model =
  case model.reminder of
    Just reminder
      -> reminder
    Nothing
      -> { body = "", created = "", deadline = "" }

extractEmail : Model -> Static.Email
extractEmail model =
  case model.email of
    Just email
      -> email
    Nothing
      -> { from = "", to = "", title = "", body = "", date = "" }

extractDate : Model -> (Int,Int,Int)
extractDate model =
  case model.itemtype of
    Reminder ->
      let
        reminder = extractReminder model
      in
        extractDateReminder reminder
    Email ->
      let
        email = extractEmail model
      in
        extractDateEmail email

extractDateReminder : Static.Reminder -> (Int,Int,Int)
extractDateReminder reminder =
  let
    dateRes = Date.fromString reminder.created
  in
    case dateRes of
      Ok theDate ->
        (Date.year theDate, Date.month theDate |> Utils.monthToInt, Date.day theDate)
      Err _ ->
        (0, 0, 0)

extractDateEmail : Static.Email -> (Int,Int,Int)
extractDateEmail email =
  let
    dateRes = Date.fromString email.date
  in
    case dateRes of
      Ok theDate ->
        (Date.year theDate, Date.month theDate |> Utils.monthToInt, Date.day theDate)
      Err _ ->
        (0, 0, 0)

extractTitle : Model -> String
extractTitle model =
  case model.itemtype of
    Reminder ->
      let
        reminder = extractReminder model
      in
        extractTitleReminder reminder
    Email ->
      let
        email = extractEmail model
      in
        extractTitleEmail email

extractTitleReminder : Static.Reminder -> String
extractTitleReminder reminder =
  reminder.body

extractTitleEmail : Static.Email -> String
extractTitleEmail email =
  email.title

-- UPDATE

type Action
  = ToggleDo
  | TogglePin
  | ToggleTruncate
  | TimeUpdate Time
  | ChangeSnooze String
  | DoSnooze
  {--}
  | NoOp
  --}

update : Action -> Model -> Model
update action model =
  case action of
    ToggleDo ->
      if model.done then
        { model |
            done = False
        }
      else
        { model |
            done = True
        }
    TogglePin ->
      if model.pinned then
        { model |
            pinned = False
        }
      else
        { model |
            pinned = True
        }
    ToggleTruncate ->
      if model.truncated then
        { model |
            truncated = False
        }
      else
        { model |
            truncated = True
        }
    TimeUpdate time ->
      case model.itemtype of
        Reminder ->
          let
            theReminder = extractReminder model
            snoozeBool =
              if not model.snooze then
                False
              else if time >= (Utils.dateStringToTime model.snoozeDate) then
                False
              else
                True
          in
            if time >= (Utils.dateStringToTime theReminder.deadline) then
              { model |
                  deadlineExpired = True
              ,   snooze = snoozeBool
              }
            else
              { model |
                  deadlineExpired = False
              ,   snooze = snoozeBool
              }
        Email ->
          let
            snoozeBool =
              if not model.snooze then
                False
              else if time >= (Utils.dateStringToTime model.snoozeDate) then
                False
              else
                True
          in
            { model |
                snooze = snoozeBool
            }
    ChangeSnooze date ->
      { model |
          snoozeDate = date
      }
    DoSnooze ->
      { model |
          snooze = True
      }
    {--}
    NoOp ->
      model
    --}


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  case model.itemtype of
    Email ->
      viewEmail address model
    Reminder ->
      viewReminder address model

viewEmail : Signal.Address Action -> Model -> Html
viewEmail address model =
  case model.itemtype of
    Email ->
      let
        email = extractEmail model
        theStyle = genBorderStyle model
      in
        if String.length email.body > 200 then
          div theStyle
            [ div [] [ text (email.title `String.append` " | " `String.append` email.from `String.append` " says:" ) ]
              , br [] []
              , div [] [ text (showEmailBodyTrunc model)]
              , moreLessButton address model
              , doUndoButton address model
              , pinUnpinButton address model
              , br [] []
              , div [] [ text ("\r\n\r\ndate: " `String.append` email.date), fieldInput "date" address ChangeSnooze model.snoozeDate, snoozeButton address model ]
              , br [] []
            ]
        else
          div theStyle
            [ div [] [ text (email.title `String.append` " | " `String.append` email.from `String.append` " says:" )  ]
              , br [] []
              , div [] [ text (email.body)]
              , doUndoButton address model
              , pinUnpinButton address model
              , br [] []
              , div [] [ text ("\r\n\r\ndate: " `String.append` email.date), fieldInput "date" address ChangeSnooze model.snoozeDate, snoozeButton address model ]
              , br [] []
            ]
    Reminder ->
      div [] []

viewReminder : Signal.Address Action -> Model -> Html
viewReminder address model =
  case model.itemtype of
    Reminder ->
      let
        reminder = extractReminder model
        theStyle =
          genStyleExpired model
          `List.append`
          genBorderStyle model
          `List.append`
          genStyleFocusExpired model
      in
        div theStyle
          [ div [] [ text (reminder.body `String.append` "\r\n\r\n")  ]
            , doUndoButton address model
            , pinUnpinButton address model
            , div [] [ text ("\r\n\r\ndate: " `String.append` reminder.created) ]
            , div [] [ text ("\r\n\r\ndeadline: " `String.append` reminder.deadline), fieldInput "date" address ChangeSnooze model.snoozeDate, snoozeButton address model ]
            , br [] []
          ]
    Email ->
      div [] []

genBorderStyle : Model -> List(Html.Attribute)
genBorderStyle model =
  if model.focus then
    [borderStyle]
  else
    []

genStyleExpired : Model -> List(Html.Attribute)
genStyleExpired model =
  if model.deadlineExpired then
    [styleExpired]
  else
    []

genStyleFocusExpired : Model -> List(Html.Attribute)
genStyleFocusExpired model =
  if model.deadlineExpired && model.focus then
    [styleExpiredFocus]
  else
    []

borderStyle : Attribute
borderStyle =
  style
    [ ("border-left", "thick double rgb(201, 31, 31)")
    ]

styleExpired : Attribute
styleExpired =
  style
    [ ("background-color", "rgba(201, 31, 31, 0.2)")
    ]

styleExpiredFocus : Attribute
styleExpiredFocus =
  style
    [ ("border-left", "thick double rgb(201, 31, 31)")
    , ("background-color", "rgba(201, 31, 31, 0.5)")
    ]

moreLessButton : Signal.Address Action -> Model -> Html
moreLessButton address model =
  if model.truncated then
    button [ onClick address ToggleTruncate ] [ text "More" ]
  else
    button [ onClick address ToggleTruncate ] [ text "Less" ]

doUndoButton : Signal.Address Action -> Model -> Html
doUndoButton address model =
  if model.done then
    button [ onClick address ToggleDo ] [ text "Undo" ]
  else
    button [ onClick address ToggleDo ] [ text "Do" ]

pinUnpinButton : Signal.Address Action -> Model -> Html
pinUnpinButton address model =
  if model.pinned then
    button [ onClick address TogglePin ] [ text "Unpin" ]
  else
    button [ onClick address TogglePin ] [ text "Pin" ]

fieldInput : String -> Signal.Address Action -> (String -> Action) -> String -> Html
fieldInput fieldType address toAction content =
  div []
    [ div [] []
    , input
        [ type' fieldType
        , value content
        , on "input" targetValue (\string -> Signal.message address (toAction string))
        ]
        []
    ]

snoozeButton : Signal.Address Action -> Model -> Html
snoozeButton address model =
  button [ onClick address DoSnooze] [ text "Snooze" ]

showEmailBodyTrunc : Model -> String
showEmailBodyTrunc model =
  case model.itemtype of
    Email ->
      let
        email = extractEmail model
      in
        if model.truncated then
          truncate email.body
        else
          email.body
    Reminder ->
      ""

truncate : String -> String
truncate text =
  (String.slice 0 200 text) `String.append` "..."
