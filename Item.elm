module Item (Model, initReminder, initEmail, Action(..), update, view, extractDate, extractTitle) where

import Static
import String
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing(..)
import Date

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
  }

extractReminder : Model -> Static.Reminder
extractReminder model =
  case model.reminder of
    Just reminder
      -> reminder
    Nothing
      -> { body = "", created = "" }

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
        (Date.year theDate, Date.month theDate |> monthToInt, Date.day theDate)
      Err _ ->
        (0, 0, 0)

extractDateEmail : Static.Email -> (Int,Int,Int)
extractDateEmail email =
  let
    dateRes = Date.fromString email.date
  in
    case dateRes of
      Ok theDate ->
        (Date.year theDate, Date.month theDate |> monthToInt, Date.day theDate)
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

-- UPDATE

type Action
  = Do
  | Undo
  | Pin
  | Unpin
  | More
  | Less
  {--}
  | NoOp
  --}

update : Action -> Model -> Model
update action model =
  case action of
    Do ->
      { model |
          done = True
      }
    Undo ->
      { model |
          done = False
      }
    Pin ->
      { model |
          pinned = True
      }
    Unpin ->
      { model |
          pinned = False
      }
    More ->
      { model |
          truncated = False
      }
    Less ->
      { model |
          truncated = True
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
              , div [] [ text ("\r\n\r\ndate: " `String.append` email.date) ]
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
              , div [] [ text ("\r\n\r\ndate: " `String.append` email.date) ]
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
        theStyle = genBorderStyle model
      in
        div theStyle
          [ div [] [ text (reminder.body `String.append` "\r\n\r\n")  ]
            , doUndoButton address model
            , pinUnpinButton address model
            , div [] [ text ("\r\n\r\ndate: " `String.append` reminder.created) ]
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

borderStyle : Attribute
borderStyle =
  style
    [ ("border-left", "thick double rgb(201, 31, 31)")
    ]

moreLessButton : Signal.Address Action -> Model -> Html
moreLessButton address model =
  if model.truncated then
    button [ onClick address More ] [ text "More" ]
  else
    button [ onClick address Less ] [ text "Less" ]

doUndoButton : Signal.Address Action -> Model -> Html
doUndoButton address model =
  if model.done then
    button [ onClick address Undo ] [ text "Undo" ]
  else
    button [ onClick address Do ] [ text "Do" ]

pinUnpinButton : Signal.Address Action -> Model -> Html
pinUnpinButton address model =
  if model.pinned then
    button [ onClick address Unpin ] [ text "Unpin" ]
  else
    button [ onClick address Pin ] [ text "Pin" ]

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
