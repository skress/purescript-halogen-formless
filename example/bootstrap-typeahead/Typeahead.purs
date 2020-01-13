module Example.BootstrapTypeahead.Typeahead where

import Prelude
import CSS as CSS
import DOM.HTML.Indexed (HTMLspan)
import Data.Array (difference, filter, head, length, reverse, (!!), (:))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isJust)
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Example.App.UI.Element (class_)
import Example.App.Validation (class ToText, toText)
import Example.BootstrapTypeahead.Dropdown as Dropdown
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select as Select
import Select.Setters as Setters
import Web.Event.Event (Event, preventDefault)
import Web.Event.Event as WE
import Web.UIEvent.FocusEvent as FE
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

type Slot f item
  = H.Slot (Select.Query (Query item) ()) (Message f item)

_typeahead = SProxy :: SProxy "typeahead"

_typeaheadSingle = SProxy :: SProxy "typeaheadSingle"

_typeaheadMulti = SProxy :: SProxy "typeaheadMulti"

data SelectionMode
  = Single
  | Multi

data Query item a
  = GetAvailableItems (Array item -> a)
  | Clear a

getAvailableItems :: forall item a. (Array item -> a) -> Select.Query (Query item) () a
getAvailableItems = Select.Query <<< GetAvailableItems

clear :: forall item. Select.Query (Query item) () Unit
clear = Select.Query (Clear unit)

data TAAction item
  = Remove Event item
  | RemoveAll Event
  | StopPropagation Event
  | ShowAndFocus
  | Key (Maybe item) KE.KeyboardEvent
  | SelectAndFocus Select.Target (Maybe ME.MouseEvent)

stopPropagation :: forall item. Event -> Select.Action (TAAction item)
stopPropagation event = Select.Action $ StopPropagation event

type State f item
  = ( selectionMode :: SelectionMode
    , items :: Array item
    , available :: Array item
    , selected :: f item
    , placeholder :: String
    )

type Input item
  = { selectionMode :: SelectionMode
    , items :: Array item
    , placeholder :: String
    }

input :: forall f item. Monoid (f item) => Input item -> Select.Input (State f item)
input { selectionMode, items, placeholder } =
  { selectionMode: selectionMode
  , inputType: Select.Text
  , search: Nothing
  , debounceTime: Nothing
  , getItemCount: length <<< _.available
  , selected: mempty
  , available: items
  , items
  , placeholder
  }

data Message f item
  = SelectionsChanged (f item)

removeItemProps :: forall item. item -> Array (HH.IProp HTMLspan (Select.Action (TAAction item)))
removeItemProps item =
  [ HE.onClick \evt -> Just $ Select.Action $ Remove (ME.toEvent evt) item
  , HE.onFocus \evt -> Just $ stopPropagation (FE.toEvent evt)
  , HE.onMouseDown \evt -> Just $ stopPropagation (ME.toEvent evt)
  , HE.onKeyDown \evt -> Just $ stopPropagation (KE.toEvent evt)
  ]

removeAllItemProps :: forall item. Array (HH.IProp HTMLspan (Select.Action (TAAction item)))
removeAllItemProps =
  [ HE.onClick \evt -> Just $ Select.Action $ RemoveAll (ME.toEvent evt)
  , HE.onFocus \evt -> Just $ stopPropagation (FE.toEvent evt)
  , HE.onMouseDown \evt -> Just $ stopPropagation (ME.toEvent evt)
  , HE.onKeyDown \evt -> Just $ stopPropagation (KE.toEvent evt)
  ]

----------
-- Premade
single ::
  ∀ item i m.
  MonadAff m =>
  ToText item =>
  Eq item =>
  Semigroup item =>
  Select.Spec (State Maybe item) (Query item) (TAAction item) () i (Message Maybe item) m
single = spec' (\i av -> const (av !! i)) identity (const $ const Nothing) filter' render
  where
  filter' items _ = items

  render st = case st.selected of
    Just item ->
      HH.div_
        [ Dropdown.toggle (removeItemProps item) st
        , menuSingle st
        ]
    Nothing ->
      HH.div_
        [ HH.input
            ( Setters.setInputProps
                [ HP.placeholder st.placeholder
                , HP.value st.search
                , class_ "form-control no-transition py-0"
                ]
            )
        , menuSingle st
        ]

-- Adapted version from Select.Setters for the multi select typeahead:
-- The input field is a child of the surrounding select element. When the select element
-- gets the focus (e.g. via click or via keyboard) we need to set the focus to the input
-- element such that the browser displays the caret and the user can start typing
setToggleProps' ::
  forall props item.
  Array (HP.IProp (Setters.ToggleProps props) (Select.Action (TAAction item))) ->
  Array (HP.IProp (Setters.ToggleProps props) (Select.Action (TAAction item)))
setToggleProps' =
  append
    [ HE.onFocus \_ -> Just $ Select.Action ShowAndFocus
    , HE.onMouseDown $ Just <<< Select.ToggleClick
    ]

-- Adapted version from Select.Setters for the multi select typeahead:
-- The input field gets the RefLabel such that the Selection.Focus action focusses on the
-- input element and not on the toggle element. Additionally some event handlers are
-- also configured for the input element instead of the toggle element.
setInputProps' ::
  forall props item.
  Array (HP.IProp (Setters.InputProps props) (Select.Action (TAAction item))) ->
  Array (HP.IProp (Setters.InputProps props) (Select.Action (TAAction item)))
setInputProps' =
  append
    [ HE.onKeyDown $ Just <<< Select.Action <<< Key Nothing
    , HE.onValueInput $ Just <<< Select.Search
    , HE.onBlur \_ -> Just $ Select.SetVisibility Select.Off
    , HP.ref (H.RefLabel "select-input")
    ]

-- Adapted version from Select.Setters for the multi select typeahead:
-- On mouse down event selects the given element and sets the focus back to
-- the input element such that the user can continue selecting other elements.
setItemProps' ::
  forall props item.
  Int ->
  Array (HP.IProp (Setters.ItemProps props) (Select.Action (TAAction item))) ->
  Array (HP.IProp (Setters.ItemProps props) (Select.Action (TAAction item)))
setItemProps' index =
  append
    [ HE.onMouseDown \ev -> Just $ Select.Action $ SelectAndFocus (Select.Index index) (Just ev)
    , HE.onMouseOver \_ -> Just $ Select.Highlight (Select.Index index)
    ]

multi ::
  ∀ item i m.
  MonadAff m =>
  ToText item =>
  Eq item =>
  Select.Spec (State Array item) (Query item) (TAAction item) () i (Message Array item) m
multi = spec' selectByIndex head (filter <<< (/=)) difference render
  where
  selectByIndex ix available selected = case available !! ix of
    Nothing -> selected
    Just item -> item : selected

  render st =
    HH.div_
      [ HH.div
          (setToggleProps' [ class_ $ "ta-multi form-control d-flex justify-content-between no-transition py-0 px-2" <> if st.visibility == Select.Off then "" else " form-control-focus" ])
          [ HH.div
              [ class_ "d-flex justify-content-start" ]
              [ HK.div -- need to use keyed version here otherwise the RefLabel on the input element gets lost when deleting selected items
                  [ class_ "d-flex justify-content-start flex-wrap" ]
                  ( append
                      ( reverse st.selected
                          <#> \i ->
                              Tuple ("selected-" <> toText i)
                                $ HH.button
                                    [ class_ "btn btn-sm btn-info text-nowrap my-1 mr-1 py-0"
                                    , HE.onKeyDown $ Just <<< Select.Action <<< Key (Just i)
                                    ]
                                    [ HH.text $ toText i
                                    , HH.span
                                        (append (removeItemProps i) [ class_ "dropdown-indicator-remove" ])
                                        [ HH.text "×" ]
                                    ]
                      )
                      [ Tuple "input-element"
                          $ HH.input
                              ( setInputProps'
                                  [ class_ "ta-multi-input"
                                  , HP.value st.search
                                  , if (length st.selected > 0) then
                                      HC.style (CSS.width $ CSS.rem (toNumber $ 1 + String.length st.search))
                                    else
                                      HP.placeholder st.placeholder
                                  ]
                              )
                      ]
                  )
              ]
          , HH.div
              [ class_ "d-flex align-items-center" ]
              [ HH.div
                  [ class_ "" ]
                  [ HH.span
                      (append removeAllItemProps [ class_ "dropdown-indicator-removeall" ])
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
      , menuMulti st
      ]

menuSingle ::
  forall item st ps m.
  ToText item =>
  Eq item =>
  Select.State ( available :: Array item, selected :: Maybe item | st ) ->
  H.ComponentHTML (Select.Action (TAAction item)) ps m
menuSingle st =
  Dropdown.menu st
    $ \item ix ->
        Setters.setItemProps ix
          $ case st.selected, st.highlightedIndex of
              Just sItem, _
                | sItem == item -> [ class_ "dropdown-item active" ]
              _, Just i
                | i == ix -> [ class_ "dropdown-item dropdown-item-hover" ]
              _, _ -> [ class_ "dropdown-item" ]

menuMulti ::
  forall item st ps m.
  ToText item =>
  Eq item =>
  Select.State ( available :: Array item, selected :: Array item | st ) ->
  H.ComponentHTML (Select.Action (TAAction item)) ps m
menuMulti st =
  Dropdown.menu st
    $ \item ix ->
        setItemProps' ix
          $ case Just ix == st.highlightedIndex of
              true -> [ class_ "dropdown-item dropdown-item-hover" ]
              _ -> [ class_ "dropdown-item" ]

----------
-- Base component
spec' ::
  ∀ item f i m.
  MonadAff m =>
  Functor f =>
  Monoid (f item) =>
  ToText item =>
  Eq item =>
  (Int -> Array item -> f item -> f item) ->
  ((f item) -> Maybe item) ->
  (item -> f item -> f item) ->
  (Array item -> f item -> Array item) ->
  ( Select.State (State f item) ->
    H.ComponentHTML (Select.Action (TAAction item)) () m
  ) ->
  Select.Spec (State f item) (Query item) (TAAction item) () i (Message f item) m
spec' select' selectLast' remove' filter' render' =
  Select.defaultSpec
    { render = render'
    , handleEvent = handleEvent
    , handleQuery = handleQuery
    , handleAction = handleAction
    }
  where
  handleEvent = case _ of
    Select.Searched string -> do
      st <- H.get
      let
        items = filter (String.contains (String.Pattern string) <<< toText) st.items
      H.modify_ _ { available = filter' items st.selected }
    Select.Selected ix -> do
      st <- H.get
      let
        selected' = select' ix st.available st.selected
      H.modify_
        _
          { selected = selected'
          , available = filter' st.items selected'
          , visibility = Select.Off
          , search = ""
          }
      -- for a multi select typeahead we want the input element to keep it's focus after a Selected event
      case st.selectionMode of
        Multi -> void $ H.fork $ Select.handleAction handleAction handleEvent $ Select.Focus true
        Single -> pure unit
      H.raise $ SelectionsChanged selected'
    _ -> pure unit

  handleQuery :: forall a. Query item a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    Clear a -> do
      st <- H.modify \st -> st { selected = mempty :: f item, available = st.items, search = "" }
      H.raise (SelectionsChanged st.selected)
      pure (Just a)
    GetAvailableItems f -> do
      st <- H.get
      pure $ Just $ f st.available

  handleAction = case _ of
    Remove evt item -> do
      st <- H.get
      let
        selected' = remove' item st.selected
      H.liftEffect $ WE.stopPropagation evt
      H.liftEffect $ preventDefault evt
      H.modify_
        _
          { selected = selected'
          , available = filter' st.items selected'
          }
      H.raise (SelectionsChanged selected')
    RemoveAll evt -> do
      st <- H.modify \st -> st { selected = mempty :: f item, available = st.items, search = "" }
      H.liftEffect $ WE.stopPropagation evt
      H.liftEffect $ preventDefault evt
      H.raise (SelectionsChanged st.selected)
    StopPropagation evt -> do
      H.liftEffect $ WE.stopPropagation evt
      H.liftEffect $ preventDefault evt
      pure unit
    ShowAndFocus -> do
      handle (Select.SetVisibility Select.On)
      handle (Select.Focus true)
    Key mbItem evt -> do
      st <- H.get
      let
        mbItem' = case isJust mbItem, String.length st.search of
          true, _ -> mbItem
          false, 0 -> selectLast' st.selected -- only choose the last selected item if no search was entered
          _, _ -> Nothing
      case KE.key evt, mbItem' of
        "Backspace", Just item -> do
          void $ H.fork $ handle (Select.Action $ Remove (KE.toEvent evt) item)
          handle (Select.Focus true)
        _, _ -> handle (Select.Key evt)
    SelectAndFocus target mbEv -> do
      handle (Select.Select target mbEv)
      handle (Select.Focus true)
    where
    handle act = Select.handleAction handleAction handleEvent act
