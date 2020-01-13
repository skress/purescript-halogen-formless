module Example.BootstrapTypeahead.Form where

import Prelude
import DOM.HTML.Indexed (HTMLinput)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array (concat)
import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Class.Console (logShow)
import Example.App.UI.Element as UI
import Example.App.Validation as V
import Example.BootstrapTypeahead.Typeahead as TA
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record (delete)
import Select as Select

-- Form spec
-- equivalent to { name :: String, color :: V.Email, ... }
type User
  = { | UserFormRow F.OutputType }

newtype UserForm r f
  = UserForm (r (UserFormRow f))

derive instance newtypeUserForm' :: Newtype (UserForm r f) _

type UserFormRow f
  = ( name :: f V.FieldError String String
    , color :: f V.FieldError (Maybe String) String
    , planet :: f V.FieldError (Array String) (Array String)
    )

-- Form component types
data Action
  = HandleTypeahead TypeaheadSlotId (TA.Message Maybe String)
  | HandleTypeaheadMulti TypeaheadMultiSlotId (TA.Message Array String)
  | Reset

-- Form child component types
type ChildSlots
  = ( typeahead :: TA.Slot Maybe String TypeaheadSlotId
    , typeaheadMulti :: TA.Slot Array String TypeaheadMultiSlotId
    )

data TypeaheadSlotId
  = Color

data TypeaheadMultiSlotId
  = Planet

derive instance eqTypeaheadSlotId :: Eq TypeaheadSlotId

derive instance ordTypeaheadSlotId :: Ord TypeaheadSlotId

derive instance eqTypeaheadMultiSlotId :: Eq TypeaheadMultiSlotId

derive instance ordTypeaheadMultiSlotId :: Ord TypeaheadMultiSlotId

-- Component spec
component :: F.Component UserForm (Const Void) ChildSlots Unit User Aff
component =
  F.component (const defaultInput)
    $ F.defaultSpec
        { render = render
        , handleAction = handleAction
        , handleEvent = handleEvent
        }
  where
  defaultInput :: F.Input' UserForm Aff
  defaultInput =
    { validators:
      UserForm
        { name: V.minLength 7
        , color: V.exists
        , planet: V.nonEmptyArray
        }
    , initialInputs: Nothing
    }

  handleEvent = case _ of
    F.Submitted outputs -> H.raise (F.unwrapOutputFields outputs)
    F.Changed formState -> logShow $ delete (SProxy :: _ "form") formState

  prx = F.mkSProxies (F.FormProxy :: _ UserForm)

  handleAction = case _ of
    HandleTypeahead slot (TA.SelectionsChanged new) -> case slot of
      Color -> eval $ F.setValidate prx.color new
    HandleTypeaheadMulti slot (TA.SelectionsChanged new) -> case slot of
      Planet -> do
        eval $ F.setValidate prx.planet new
    Reset -> do
      items <- H.query TA._typeahead Color $ H.request TA.getAvailableItems
      logShow $ fromMaybe [] items
      _ <- H.queryAll TA._typeahead TA.clear
      eval F.resetAll
    where
    -- you will usually want to define this pre-applied function if you
    -- are recursively evaluating Formless actions.
    eval act = F.handleAction handleAction handleEvent act

  render :: F.PublicState UserForm () -> F.ComponentHTML UserForm Action ChildSlots Aff
  render st =
    HH.form_
      [ name
      , color
      , planet
      , UI.p_
          """
          You can only attempt to submit this form if it is valid and not already being submitted. You can only attempt to reset the form if it has changed from its initial state.
          """
      , HH.br_
      , UI.grouped_
          [ UI.buttonPrimary
              [ if st.submitting || st.validity /= F.Valid then
                  HP.disabled true
                else
                  HE.onClick \_ -> Just F.submit
              ]
              [ HH.text "Submit" ]
          , UI.button
              [ if not st.dirty then
                  HP.disabled true
                else
                  HE.onClick \_ -> Just $ F.injAction Reset
              ]
              [ HH.text "Reset" ]
          ]
      ]
    where
    name =
      st
        # UI.formlessField input'
            { label: "Name"
            , help: "Write your name"
            , placeholder: "Dale"
            , sym: prx.name
            }

    color =
      field'
        { label: "Color"
        , help: F.getResult prx.color st.form # UI.resultToHelp "Choose a color"
        }
        [ singleTypeahead Color
            { selectionMode: TA.Single
            , placeholder: "Choose a color ..."
            , items:
              [ "Absolute Zero"
              , "Acid green"
              , "Aero"
              , "Aero blue"
              , "African violet"
              , "Barn red"
              , "Cadmium green"
              , "Dark cyan"
              ]
            }
        ]

    planet =
      field'
        { label: "Exoplanet"
        , help:
          F.getResult prx.planet st.form
            # UI.resultToHelp
                "Select your exoplanet"
        }
        [ multiTypeahead Planet
            { selectionMode: TA.Multi
            , placeholder: "Your exoplanet ..."
            , items:
              [ "109 Piscium b"
              , "16 Cygni Bb"
              , "47 Ursae Majoris b"
              , "Gliese 86 b"
              , "11 Comae Berenices b"
              , "14 Herculis b"
              , "4 Ursae Majoris b"
              , "Beta Pictoris b"
              ]
            }
        ]

    singleTypeahead slot input = HH.slot TA._typeahead slot (Select.component TA.input TA.single) input handler
      where
      handler = Just <<< F.injAction <<< HandleTypeahead slot

    multiTypeahead slot input = HH.slot TA._typeaheadMulti slot selectComponent input handler
      where
      selectComponent = Select.component TA.input TA.multi

      handler = Just <<< F.injAction <<< HandleTypeaheadMulti slot

field' :: forall i p. { label :: String, help :: Either String String } -> UI.Plain i p
field' config contents =
  HH.div
    [ UI.class_ "form-group" ]
    ( concat
        [ [ HH.label_
              [ HH.text config.label ]
          ]
        , contents
        , [ case config.help of
              Left str -> helpError_ str
              Right str -> help_ str
          ]
        ]
    )
  where
  help_ str = HH.p [ UI.class_ "text-muted" ] [ HH.text str ]

  helpError_ str = HH.p [ UI.class_ "invalid-feedback" ] [ HH.text str ]

input' :: forall i p. UI.FieldConfig' -> Array (HH.IProp HTMLinput p) -> HH.HTML i p
input' config props =
  field'
    { label: config.label, help: config.help }
    [ HH.input
        $ [ HP.type_ InputText
          , either (const $ UI.class_ "form-control is-invalid") (const $ UI.class_ "form-control") config.help
          , HP.placeholder config.placeholder
          ]
        <> props
    ]
