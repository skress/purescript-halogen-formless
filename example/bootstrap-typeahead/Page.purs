module Example.BootstrapTypeahead.Page where

import Prelude
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console (logShow)
import Example.App.UI.Element as UI
import Example.BootstrapTypeahead.Form (ChildSlots, User, UserForm)
import Example.BootstrapTypeahead.Form as Form
import Formless as F
import Halogen as H
import Halogen.Aff (selectElement)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.DOM.Node as Node
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLLinkElement as LinkElement

data Action
  = Initialize
  | HandleFormless User

type ChildSlot
  = ( formless :: F.Slot UserForm (Const Void) ChildSlots User Unit )

component :: H.Component HH.HTML (Const Void) Unit Void Aff
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
    }
  where
  handleAction = case _ of
    Initialize -> do
      maybeElem <- H.liftAff $ selectElement $ QuerySelector "#bulmacss"
      H.liftEffect
        $ for_ maybeElem \e -> do
            for_ (LinkElement.fromHTMLElement e) \le -> do
              LinkElement.setDisabled true le
              let
                cssNode = LinkElement.toNode le
              maybeParentNode <- Node.parentNode cssNode
              for_ maybeParentNode \pn -> do
                Node.removeChild cssNode pn
    HandleFormless user -> logShow (user :: User)

  render st =
    HH.div_
      [ HH.link
          [ HP.href "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css"
          , HP.rel "stylesheet"
          ]
      , HH.link
          [ HP.href "/dist/dropdown.css"
          , HP.rel "stylesheet"
          ]
      , HH.div
          [ UI.class_ "container" ]
          [ HH.div
              [ UI.class_ "row" ]
              [ HH.div
                  [ UI.class_ "col-12" ]
                  [ UI.h1_ [ HH.text "Formless" ]
                  , UI.h2_ [ HH.text "A form implementing a single- and multi-dropdown with typeahead using Bootstrap CSS styles." ]
                  , HH.slot F._formless unit Form.component unit (Just <<< HandleFormless)
                  ]
              ]
          ]
      ]
