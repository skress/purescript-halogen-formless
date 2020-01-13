module Example.BootstrapTypeahead.Dropdown where

import Prelude
import DOM.HTML.Indexed (HTMLspan)
import Data.Array (mapWithIndex, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Example.App.UI.Element (class_)
import Example.App.Validation (class ToText, toText)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Select as Select
import Select.Setters as Setters

type State item
  = ( selected :: Maybe item
    , available :: Array item
    , items :: Array item
    , placeholder :: String
    )

type Input item
  = { items :: Array item
    , placeholder :: String
    }

input :: forall item. Input item -> Select.Input (State item)
input { items, placeholder } =
  { inputType: Select.Toggle
  , search: Nothing
  , debounceTime: Nothing
  , getItemCount: length <<< _.items
  , selected: Nothing
  , available: items
  , items
  , placeholder
  }

toggle ::
  forall item act ps m r.
  ToText item =>
  Array (HH.IProp HTMLspan (Select.Action act)) ->
  { placeholder :: String, selected :: Maybe item | r } ->
  H.ComponentHTML (Select.Action act) ps m
toggle removeProps st =
  HH.div
    (Setters.setToggleProps [ class_ "form-control no-transition py-0" ])
    [ HH.div
        [ class_ "d-flex justify-content-between pt-1" ]
        [ HH.div
            [ class_ "d-flex pt-1 w-100" ]
            [ HH.text $ fromMaybe st.placeholder (toText <$> st.selected) ]
        , HH.div
            [ class_ "d-flex align-items-center" ]
            [ HH.span
                (append removeProps [ class_ "dropdown-indicator-removeall" ])
                [ HH.text "×" ]
            , HH.span
                [ class_ "dropdown-indicator-separator" ]
                [ HH.text "" ]
            , HH.i
                [ class_ "dropdown-indicator-toggle dropdown-toggle" ]
                [ HH.text "" ]
            ]
        ]
    ]

-- | Takes the state and a function that - given an item and an index - returns
-- | the css classes for that item in the dropdown list, returns the html code
-- | for the dropdown menu.
menu ::
  forall f item st act ps m.
  ToText item =>
  Eq item =>
  Select.State ( available :: Array item, selected :: f item | st ) ->
  (item -> Int -> Array (HP.IProp HTMLspan (Select.Action act))) ->
  H.ComponentHTML (Select.Action act) ps m
menu st f =
  if st.visibility == Select.Off then
    HH.text ""
  else
    HH.div
      [ class_ "dropdown" ]
      [ HH.div
          (Setters.setContainerProps [ class_ "dropdown-menu show w-100" ])
          ( mapWithIndex
              ( \ix item ->
                  HH.span
                    (f item ix)
                    [ HH.text (toText item) ]
              )
              st.available
          )
      ]
