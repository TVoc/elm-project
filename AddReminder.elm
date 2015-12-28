module AddReminder(Model, Action(..), init, view, update) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Html.Events exposing (onClick)

-- MODEL

type alias Model =
  { body : String
  , created : String
  , hide : Bool
  }

type alias Output =
  { body : String
  , created : String
  }

init : Bool -> Model
init hideIt =
  { body = ""
  , created = "2015-01-01"
  , hide = hideIt
  }

{--
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
--}

-- UPDATE

type Action
  = ChangeBody String
  | ChangeCreated String
  | AddReminder Output Bool
  | ToggleHide
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
    AddReminder theOutput hideIt ->
      init hideIt
    ToggleHide ->
      let
        notHide = not model.hide
      in
        { model |
            hide = notHide
        }
    NoOp ->
      model

takeOutput : Model -> Output
takeOutput model =
  { body = model.body
  , created = model.created
  }

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  if model.hide then
    div []
      [ div [] [ hideButton address model
               ]
      ]
  else
    div []
      [ div [] [ fieldInput "text" address ChangeBody model.body
               , fieldInput "date" address ChangeCreated model.created
               , submitButton address model
               , hideButton address model
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
  button [ onClick address (AddReminder (takeOutput model) model.hide)] [ text "Add" ]

hideButton : Signal.Address Action -> Model -> Html
hideButton address model =
  if model.hide then
    button [ onClick address ToggleHide] [ text "Unhide Add Reminder" ]
  else
    button [ onClick address ToggleHide] [ text "Hide" ]
