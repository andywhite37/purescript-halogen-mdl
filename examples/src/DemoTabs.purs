module DemoTabs where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Halogen.MDL as MDL
import Halogen.MDL.Cell as Cell
import Halogen.MDL.Grid as Grid
import Halogen.MDL.Tabs as Tabs
import Halogen.MDL.RippleEffect as RE

data Tab
  = About
  | Members
  | Albums

derive instance eqTab :: Eq Tab

instance showTab :: Show Tab where
show About = "About"
show Members = "Members"
show Albums = "Albums"

type State = { currentTab :: Tab }

isCurrentTab :: State -> Tab -> Boolean
isCurrentTab state tab = state.currentTab == tab

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a
  | UpdateCurrentTab Tab a

data Input = Initialize State

type Message = Void

type DemoTabsHTML = H.ComponentHTML Query
type DemoTabsDSL eff = H.ComponentDSL State Query Message (Aff (HA.HalogenEffects eff))

init :: State -> Input
init state = Initialize state

demoTabs :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
demoTabs =
  H.lifecycleComponent
    { initialState: initialState
    , initializer: initializer
    , finalizer: finalizer
    , receiver: receiver
    , render
    , eval
    }
  where

  initialState :: Input -> State
  initialState (Initialize state) = state

  initializer :: Maybe (Query Unit)
  initializer = Just $ H.action InitializeComponent

  finalizer :: Maybe (Query Unit)
  finalizer = Just $ H.action FinalizeComponent

  receiver :: Input -> Maybe (Query Unit)
  receiver (Initialize state) = Just $ H.action $ UpdateState state

  render :: State -> DemoTabsHTML
  render state =
    Grid.el.grid_
      [ renderTabsHeader
      , renderDemoHeader "Example with ripple effect"
      , renderDemoSection $ renderExampleWithRipple state
      ]

  renderTabsHeader :: ∀ p i. HH.HTML p i
  renderTabsHeader = Cell.el.cell12Col_ [ HH.h1_ [ HH.text "Tabs" ] ]

  renderDemoHeader :: ∀ p i. String -> HH.HTML p i
  renderDemoHeader name = Cell.el.cell12Col_ [ HH.h3_ [ HH.text name ] ]

  renderDemoSection :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
  renderDemoSection body = Cell.el.cell6Col_ body

  renderExampleWithRipple :: State -> Array (DemoTabsHTML)
  renderExampleWithRipple state =
    [ HH.div
        [ HP.classes [ Tabs.cl.tabs, Tabs.cl.jsTabs, RE.cl.jsRippleEffect ] ]
        [ HH.div
            [ HP.class_ Tabs.cl.tabsTabBar ]
            [ HH.a
                [ HP.href "#about-panel"
                --, HP.classes [ Tabs.cl.tabsTab, Tabs.cl.isActive ]
                , HP.classes ([ Tabs.cl.tabsTab ] <> getActiveClass state About)
                , HE.onClick $ HE.input_ (UpdateCurrentTab About)
                ]
                [ HH.text "Beatles" ]
            , HH.a
                [ HP.href "#members-panel"
                --, HP.classes [ Tabs.cl.tabsTab ]
                , HP.classes ([ Tabs.cl.tabsTab ] <> getActiveClass state Members)
                , HE.onClick $ HE.input_ (UpdateCurrentTab Members)
                ]
                [ HH.text "Members" ]
            , HH.a
                [ HP.href "#albums-panel"
                --, HP.classes [ Tabs.cl.tabsTab ]
                , HP.classes ([ Tabs.cl.tabsTab ] <> getActiveClass state Albums)
                , HE.onClick $ HE.input_ (UpdateCurrentTab Albums)
                ]
                [ HH.text "Discography" ]
            ]
        , HH.div
            --[ HP.classes [ Tabs.cl.tabsPanel ]
            [ HP.classes ([ Tabs.cl.tabsPanel] <> getActiveClass state About)
            , HP.id_ "about-panel"
            ]
            [ HH.p_
                [ HH.b_ [ HH.text "The Beatles" ]
                , HH.text " were a four-piece musical group from Liverpool, England. ..."
                ]
            , HH.p_
                [ HH.text "Their songs were among the best-loved music of all time. ..." ]
            ]
        , HH.div
            --[ HP.classes [ Tabs.cl.tabsPanel ]
            [ HP.classes ([ Tabs.cl.tabsPanel] <> getActiveClass state Members)
            , HP.id_ "members-panel"
            ]
            [ HH.p_ [ HH.text "The Beatles' members were:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "John Lennon (1940-1980)" ]
                , HH.li_ [ HH.text "Paul McCartney (1942-)" ]
                , HH.li_ [ HH.text "George Harrison (1943-2001)" ]
                , HH.li_ [ HH.text "Ringo Starr (1940-)" ]
                ]
            ]
        , HH.div
            [ HP.classes [ Tabs.cl.tabsPanel ]
            , HP.id_ "albums-panel"
            ]
            [ HH.p_ [ HH.text "The Beatles' original UK LPs, in order of release:" ]
            , HH.ol_
                [ HH.li_ [ HH.text "Please, Please Me (1963)" ]
                , HH.li_ [ HH.text "With the Beatles (1963)" ]
                , HH.li_ [ HH.text "..." ]
                ]
            ]
        ]
    , HH.em_
        [ HH.text $ "Current tab: " <> show state.currentTab ]
    ]

  getActiveClass :: State -> Tab -> Array HH.ClassName
  getActiveClass state tab = if isCurrentTab state tab then [Tabs.cl.isActive] else []

  eval :: Query ~> DemoTabsDSL eff
  eval = case _ of
    InitializeComponent next -> do
      H.liftEff $ MDL.upgradeElementsByClassName Tabs.cl.jsTabs
      pure next
    FinalizeComponent next -> do
      pure next
    UpdateState state next -> do
      H.put state
      pure next
    UpdateCurrentTab tab next -> do
      H.modify (_ { currentTab = tab })
      pure next
