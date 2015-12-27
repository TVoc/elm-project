module AddReminder(Model, Action(..), init, view, reminderActions, reminderAddress, state, update) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Html.Events exposing (onClick)

-- MODEL

type alias Model =
  { body : String
  , created : String
  }

init : Model
init =
  { body = ""
  , created = "2015-01-01"
  }

state : Signal Model
state =
  Signal.foldp update init reminderActions

reminderMailBox : Signal.Mailbox Action
reminderMailBox =
  Signal.mailbox NoOp

reminderAddress : Signal.Address Action
reminderAddress =
  reminderMailBox.address

reminderActions : Signal Action
reminderActions =
  reminderMailBox.signal

-- UPDATE

type Action
  = ChangeBody String
  | ChangeCreated String
  | AddReminder Model
  | NoOp

update : Action -> Model -> Model
update action model =
  case action of
    ChangeBody newBody ->
      { model |
          body = newBody
      }
    ChangeCreated newCreated ->
      { model |
          created = newCreated
      }
    AddReminder theModel ->
      init
    NoOp ->
      model

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ div [] [ fieldInput "text" address ChangeBody model.body
             , fieldInput "date" address ChangeCreated model.created
             , submitButton address model
             ]

    ]

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

submitButton : Signal.Address Action -> Model -> Html
submitButton address model =
  button [ onClick address (AddReminder model)] [ text "Add" ]
