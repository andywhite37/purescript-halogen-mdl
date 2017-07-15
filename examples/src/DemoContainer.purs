module DemoContainer where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))

--import DOM.Classy.Event (toEvent)
--import DOM.Event.Event (preventDefault)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Halogen.MDL as MDL
--import Halogen.MDL.Badge as Badge
--import Halogen.MDL.Button as Button
import Halogen.MDL.Layout as Layout
import Halogen.MDL.Navigation as Navigation

import Route (Route(..))
import Route as Route
import DemoButtons as DemoButtons

type State =
  { currentRoute :: Route
  }

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a
  | UpdateRoute Route a
  | OnDemoButtonsMessage DemoButtons.Message a

data Input = Initialize State

type Message = Void

data Slot = PageContentSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

init :: State -> Input
init state = Initialize state

demoContainer :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
demoContainer =
  H.lifecycleParentComponent
    { initialState: initialState
    , initializer: initializer
    , finalizer: finalizer
    , receiver: receiver
    , render
    , eval
    }
  where

  layoutRef :: H.RefLabel
  layoutRef = H.RefLabel "mdl-layout-ref"

  initialState :: Input -> State
  initialState (Initialize state) = state

  initializer :: Maybe (Query Unit)
  initializer = Just $ H.action InitializeComponent

  finalizer :: Maybe (Query Unit)
  finalizer = Just $ H.action FinalizeComponent

  receiver :: Input -> Maybe (Query Unit)
  receiver (Initialize state) = Just $ H.action $ UpdateState state

  render :: State -> H.ParentHTML Query DemoButtons.Query Slot (Aff (HA.HalogenEffects eff))
  render state =
    HH.div
      [ HP.classes [ Layout.classes.layout, Layout.classes.jsLayout, Layout.classes.layoutFixedHeader ]
      , HP.ref layoutRef
      ]
      [ HH.header
        [ HP.classes [ Layout.classes.layoutHeader ] ]
        [ HH.div
          [ HP.classes [ Layout.classes.layoutHeaderRow ] ]
          [ HH.span [ HP.classes [ Layout.classes.layoutTitle] ] [ HH.text "Halogen MDL" ]
          , HH.div [ HP.classes [ Layout.classes.layoutSpacer ] ] []
          , HH.nav
            [ HP.classes [ Navigation.classes.navigation, Layout.classes.layoutLargeScreenOnly ] ]
            [ HH.a
              [ HP.href "#", HP.classes [ Navigation.classes.navigationLink ] ]
              [ HH.text "Link" ]
            , HH.a
              [ HP.href "#", HP.classes [ Navigation.classes.navigationLink ] ]
              [ HH.text "Link" ]
            , HH.a
              [ HP.href "#", HP.classes [ Navigation.classes.navigationLink ] ]
              [ HH.text "Link" ]
            , HH.a
              [ HP.href "#", HP.classes [ Navigation.classes.navigationLink ] ]
              [ HH.text "Link" ]
            ]
          ]
        ]
      , HH.div
        [ HP.classes [ Layout.classes.layoutDrawer ] ]
        [ HH.span
          [ HP.classes [ Layout.classes.layoutTitle ] ]
          [ HH.text "Title" ]
        , HH.nav
          [ HP.classes [ Navigation.classes.navigation ] ]
          [ HH.a
            [ HP.href $ Route.href Home , HP.classes [ Navigation.classes.navigationLink ] ]
            [ HH.text $ Route.label Home ]
          , HH.a
            [ HP.href $ Route.href Badges , HP.classes [ Navigation.classes.navigationLink ] ]
            [ HH.text $ Route.label Badges ]
          , HH.a
            [ HP.href $ Route.href Buttons , HP.classes [ Navigation.classes.navigationLink ] ]
            [ HH.text $ Route.label Buttons ]
          ]
        ]
      , HH.div
        [ HP.classes [ Layout.classes.layoutContent ] ]
        [ HH.div
          [ HP.classes [ HH.ClassName "page-content" ] ]
          [ renderContent state ]
        ]
      ]

  --renderContent :: State ->
  renderContent state = case state.currentRoute of
    Home ->
      HH.div_ [ HH.text $ Route.label state.currentRoute ]
    Badges ->
      HH.div_ [ HH.text $ Route.label state.currentRoute ]
    Buttons ->
      HH.slot
        PageContentSlot
        DemoButtons.demoButtons
        (DemoButtons.init { clickCount: 0 })
        (HE.input OnDemoButtonsMessage)

  eval :: Query ~> H.ParentDSL State Query DemoButtons.Query Slot Message (Aff (HA.HalogenEffects eff))
  eval = case _ of
    InitializeComponent next -> do
      MDL.upgradeElementByRef layoutRef
      pure next
    FinalizeComponent next -> do
      pure next
    UpdateState state next -> do
      H.put state
      pure next
    UpdateRoute route next -> do
      H.modify (\state -> state { currentRoute = route })
      pure next
    OnDemoButtonsMessage _ next -> do
      pure next
