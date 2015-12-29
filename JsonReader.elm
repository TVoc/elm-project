module JsonReader where

import Json.Decode as Json exposing ((:=))
import Static exposing(Email)
import Signal
import Effects exposing (Effects, Never)
import Http
import Task
import Time exposing (Time)

-- MODEL

type alias Model =
  { requestFrom : String
  , emailList : EmailList
  , hasEmails : Bool
  , counter : Time
  , maxCounter : Float
  }

type alias EmailList =
  List Static.Email

type alias MaybeEmailList =
  Maybe EmailList

init : String -> Time -> Model
init request theCounter =
  { requestFrom = request
  , emailList = []
  , hasEmails = False
  , counter = theCounter
  , maxCounter = theCounter
  }

taskMailbox : Signal.Mailbox (Effects Action)
taskMailbox =
  Signal.mailbox Effects.none

-- UPDATE

type Action
  = TimeUpdate
  | Input MaybeEmailList

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    TimeUpdate ->
      let
        newModel =
          if (model.counter + 1) >= model.maxCounter then
            { model |
                counter = 0
            ,   hasEmails = False
            }
          else
            { model |
                counter = model.counter + 1
            ,   hasEmails = False
            }
      in
        if (model.counter + 1) >= model.maxCounter then
          (newModel, getJson newModel.requestFrom)
        else
          (newModel, Effects.none)
    Input maybeList ->
      let
        newModel =
          { model |
              emailList = Maybe.withDefault model.emailList maybeList
          ,   hasEmails = True
          }
      in
        (newModel, Effects.none)

-- DECODING

{--
getEmailsFromString : String -> List(Static.Email)
getEmailsFromString string =
  case Json.decodeString emailListDecoder string of
    Ok list ->
      list
    Err _ ->
      []
--}

emailListDecoder : Json.Decoder EmailList
emailListDecoder =
  Json.list emailDecoder

emailDecoder : Json.Decoder Static.Email
emailDecoder =
  Json.map Email
    ("from" := Json.string)
    `apply`
    ("to" := Json.string)
    `apply`
    ("title" := Json.string)
    `apply`
    ("date" := Json.string)
    `apply`
    ("body" := Json.string)

apply : Json.Decoder (a -> b) -> Json.Decoder a -> Json.Decoder b
apply func value =
    Json.object2 (<|) func value

-- EFFECTS
getJson : String -> Effects Action
getJson requestUrl =
  Http.get emailListDecoder requestUrl
   |> Task.toMaybe
   |> Task.map Input
   |> Effects.task
