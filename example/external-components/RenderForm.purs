module Example.ExternalComponents.RenderForm where

import Prelude

import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Example.ExternalComponents.Spec (Form, _email, _name, _language, _whiskey)
import Example.ExternalComponents.Types (FCQ, FCS, Query(..), Slot(..))
import Formless as Formless
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Input as Input
import Ocelot.Components.Typeahead as TA
import Ocelot.Components.Typeahead.Input as TA.Input
import Record as Record

-- | Our render function has access to anything in Formless' State type, plus
-- | anything additional in your own state type.
formless
  :: Formless.State Form
  -> Formless.HTML Query FCQ FCS Form Aff
formless state =
  HH.div_
    [ renderName state
    , renderEmail state
    , renderWhiskey state
    , renderLanguage state
    , Button.buttonPrimary
      [ HE.onClick $ HE.input_ Formless.Submit ]
      [ HH.text "Submit" ]
    ]

----------
-- Helpers

-- | A helper function to render a form text input
renderName :: Formless.State Form -> Formless.HTML Query FCQ FCS Form Aff
renderName state =
  HH.div_
    [ FormField.field_
        { label: "Name"
        , helpText: Just "Write your name."
        , error: join $ map (either Just (const Nothing)) field.result
        , inputId: "name"
        }
        [ Input.input
          [ HP.placeholder "Dale"
          , HP.value field.input
          , Formless.onBlurWith _name
          , Formless.onValueInputWith _name
          ]
        ]
    ]
  where
    field = unwrap $ Record.get _name $ unwrap state.form

renderEmail :: Formless.State Form -> Formless.HTML Query FCQ FCS Form Aff
renderEmail state =
  HH.div_
    [ FormField.field_
        { label: "Email"
        , helpText: Just "Select an email address"
        , error: join $ map (either Just (const Nothing)) field.result
        , inputId: "email"
        }
        [ HH.slot
            EmailTypeahead
            TA.component
            ( TA.Input.defSingle
              [ HP.placeholder "Search email addresses..." ]
              [ "not@anemail.org"
              , "snail@utopia.snailutopia"
              , "blue@jordans@blordans.pordens"
              , "yea_that_won't_work@email.com"
              , "standard@email.com"
              ]
              TA.Input.renderItemString
            )
            ( HE.input (Formless.Raise <<< H.action <<< HandleTypeahead EmailTypeahead) )
        ]
    ]
  where
    field = unwrap $ Record.get _email $ unwrap state.form

renderWhiskey :: Formless.State Form -> Formless.HTML Query FCQ FCS Form Aff
renderWhiskey state =
  HH.div_
    [ FormField.field_
        { label: "Whiskey"
        , helpText: Just "Select a favorite whiskey"
        , error: join $ map (either Just (const Nothing)) field.result
        , inputId: "whiskey"
        }
        [ HH.slot
            WhiskeyTypeahead
            TA.component
            ( TA.Input.defSingle
              [ HP.placeholder "Search whiskies..." ]
              [ "Lagavulin 16"
              , "Kilchoman Blue Label"
              , "Laphroaig"
              , "Ardbeg"
              ]
              TA.Input.renderItemString
            )
            ( HE.input (Formless.Raise <<< H.action <<< HandleTypeahead WhiskeyTypeahead) )
        ]
    ]
  where
    field = unwrap $ Record.get _whiskey $ unwrap state.form

renderLanguage :: Formless.State Form -> Formless.HTML Query FCQ FCS Form Aff
renderLanguage state =
  HH.div_
    [ FormField.field_
        { label: "Language"
        , helpText: Just "Select a favorite language"
        , error: join $ map (either Just (const Nothing)) field.result
        , inputId: "language"
        }
        [ HH.slot
            LanguageTypeahead
            TA.component
            ( TA.Input.defSingle
              [ HP.placeholder "Search lanugages..." ]
              [ "Rust"
              , "Python"
              , "Haskell"
              , "PureScript"
              , "PHP"
              , "JavaScript"
              , "C"
              , "C++"
              , "C#"
              , "C--"
              , "Ruby"
              , "APL"
              ]
              TA.Input.renderItemString
            )
            ( HE.input (Formless.Raise <<< H.action <<< HandleTypeahead LanguageTypeahead) )
        ]
    ]
  where
    field = unwrap $ Record.get _language $ unwrap state.form
