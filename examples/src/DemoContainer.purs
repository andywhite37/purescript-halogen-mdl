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
--import Halogen.MDL.Grid as Grid
import Halogen.MDL.Layout as Layout
import Halogen.MDL.MegaFooter as MegaFooter
import Halogen.MDL.Navigation as Navigation

import Route (Route(..))
import Route as Route
--import DemoBadges as DemoBadges
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
      [ HP.classes [ Layout.cl.layout, Layout.cl.jsLayout, Layout.cl.layoutFixedHeader ]
      , HP.ref layoutRef
      ]
      [ HH.header
        [ HP.classes [ Layout.cl.layoutHeader ] ]
        [ HH.div
          [ HP.classes [ Layout.cl.layoutHeaderRow ] ]
          [ HH.span [ HP.classes [ Layout.cl.layoutTitle] ] [ HH.text "Halogen MDL" ]
          , HH.div [ HP.classes [ Layout.cl.layoutSpacer ] ] []
          , HH.nav
            [ HP.classes [ Navigation.cl.navigation, Layout.cl.layoutLargeScreenOnly ] ]
            [ HH.a
              [ HP.href "#", HP.classes [ Navigation.cl.navigationLink ] ]
              [ HH.text "Link" ]
            , HH.a
              [ HP.href "#", HP.classes [ Navigation.cl.navigationLink ] ]
              [ HH.text "Link" ]
            , HH.a
              [ HP.href "#", HP.classes [ Navigation.cl.navigationLink ] ]
              [ HH.text "Link" ]
            , HH.a
              [ HP.href "#", HP.classes [ Navigation.cl.navigationLink ] ]
              [ HH.text "Link" ]
            ]
          ]
        ]
      , HH.div
        [ HP.classes [ Layout.cl.layoutDrawer ] ]
        [ HH.span
          [ HP.classes [ Layout.cl.layoutTitle ] ]
          [ HH.text "Title" ]
        , HH.nav
          [ HP.classes [ Navigation.cl.navigation ] ]
          [ HH.a
            [ HP.href $ Route.href Home , HP.classes [ Navigation.cl.navigationLink ] ]
            [ HH.text $ Route.label Home ]
          , HH.a
            [ HP.href $ Route.href Badges , HP.classes [ Navigation.cl.navigationLink ] ]
            [ HH.text $ Route.label Badges ]
          , HH.a
            [ HP.href $ Route.href Buttons , HP.classes [ Navigation.cl.navigationLink ] ]
            [ HH.text $ Route.label Buttons ]
          ]
        ]
      , HH.div
        [ HP.classes [ Layout.cl.layoutContent ] ]
        [ HH.div
          [ HP.classes [ HH.ClassName "page-content" ] ]
          [ renderContent state
          , MegaFooter.bl.megaFooter
              {
                middleSection:
                  { dropDownSections:
                      [ { title: "Drop down section 1"
                        , linkList:
                            { links:
                                [ { href: "#", text: "Link 1" }
                                , { href: "#", text: "Link 2" }
                                , { href: "#", text: "Link 3" }
                                , { href: "#", text: "Link 4" }
                                ]
                            }
                        }
                      , { title: "Drop down section 2"
                        , linkList:
                            { links:
                                [ { href: "#", text: "Link 1" }
                                , { href: "#", text: "Link 2" }
                                , { href: "#", text: "Link 3" }
                                , { href: "#", text: "Link 4" }
                                ]
                            }
                        }
                      , { title: "Drop down section 3"
                        , linkList:
                            { links:
                                [ { href: "#", text: "Link 1" }
                                , { href: "#", text: "Link 2" }
                                , { href: "#", text: "Link 3" }
                                , { href: "#", text: "Link 4" }
                                ]
                            }
                        }
                      , { title: "Drop down section 4"
                        , linkList:
                            { links:
                                [ { href: "#", text: "Link 1" }
                                , { href: "#", text: "Link 2" }
                                , { href: "#", text: "Link 3" }
                                , { href: "#", text: "Link 4" }
                                ]
                            }
                        }
                      ]
                  }
              , bottomSection:
                  { title: "Bottom title"
                  , linkList:
                      { links:
                          [ { href: "#", text: "Link 1" }
                          , { href: "#", text: "Link 2" }
                          , { href: "#", text: "Link 3" }
                          , { href: "#", text: "Link 4" }
                          ]
                      }
                  }
              }
          ]
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
