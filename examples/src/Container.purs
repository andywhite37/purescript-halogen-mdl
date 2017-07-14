module Container where

import Prelude

import Control.Monad.Aff (Aff)
import DOM.Classy.Event (toEvent)
import DOM.Event.Event (preventDefault)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
--import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

--import Halogen.MDL.Badge as Badge
import Halogen.MDL.Button as Button
import Halogen.MDL.Layout as Layout
import Halogen.MDL.Navigation as Navigation

import Halogen.MDL as MDL

import Route as Route
import Route (Route(..))

type State =
  { currentRoute :: Route
  , clickCount :: Int
  }

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a
  | UpdateRoute Route a
  | OnButtonMessage Button.Message a

data Input = Initialize State

type Message = Void

data Slot
  = ButtonSlot
derive instance eqButtonSlot :: Eq Slot
derive instance ordButtonSlot :: Ord Slot

init :: State -> Input
init state = Initialize state

container :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
container =
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

  render :: State -> H.ParentHTML Query Button.Query Slot (Aff (HA.HalogenEffects eff))
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
          [ HH.text "Your content goes here" ]
        ]
      ]

{-
    HH.div_
      [ HH.h1_
        [ HH.text $ show state.currentRoute ]
      , HH.slot
          ButtonSlot
          Button.button
          (Button.init { type: Button.Raised, color: Button.Colored, text: "Click this", disabled: false, ripple: true })
          (HE.input OnButtonMessage)
      , HH.p_
        [ HH.text $ "Button has been clicked " <> show state.clickCount <> " times." ]
      , HH.div
        [ HP.classes [Badge.classes.badge]
        , HP.attr (H.AttrName "data-badge") (show state.clickCount)
        ]
        [ HH.text "My badge" ]
      ]
      -}

  eval :: Query ~> H.ParentDSL State Query Button.Query Slot Message (Aff (HA.HalogenEffects eff))
  eval = case _ of
    InitializeComponent next -> do
      element <- H.getHTMLElementRef layoutRef
      case element of
        Just element -> do
          H.liftEff $ MDL.upgradeElement element
        Nothing -> pure unit
      pure next

    FinalizeComponent next -> do
      pure next

    UpdateState state next -> do
      -- TODO: check for change?
      H.put state
      pure next

    UpdateRoute route next -> do
      H.modify (\state -> state { currentRoute = route })
      pure next

    OnButtonMessage (Button.Clicked event) next -> do
      H.liftEff $ preventDefault $ toEvent event
      H.modify (\state -> state { clickCount = state.clickCount + 1 })
      pure next
