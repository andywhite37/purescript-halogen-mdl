module DemoContainer where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Array ((..))
import Data.Const (Const)
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.Data.Prism (type (\/))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Halogen.MDL as MDL
import Halogen.MDL.Basic as Basic
import Halogen.MDL.Layout as Layout
import Halogen.MDL.MegaFooter as MegaFooter
import Halogen.MDL.Navigation as Navigation

import Route (Route(..))
import Route as Route
import DemoHome as DemoHome
import DemoBadges as DemoBadges
import DemoButtons as DemoButtons
import DemoCards as DemoCards
import DemoChips as DemoChips
import DemoDialogs as DemoDialogs
import DemoTabs as DemoTabs

type State =
  { currentRoute :: Route
  }

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a
  | UpdateRoute Route a
  | OnNavClick a
  | OnDemoHomeMessage DemoHome.Message a
  | OnDemoBadgesMessage DemoBadges.Message a
  | OnDemoButtonsMessage DemoButtons.Message a
  | OnDemoCardsMessage DemoCards.Message a
  | OnDemoChipsMessage DemoChips.Message a
  | OnDemoDialogsMessage DemoDialogs.Message a
  | OnDemoTabsMessage DemoTabs.Message a

data Input = Initialize State

type Message = Void

type ChildQuery
  =    DemoHome.Query
  <\/> DemoBadges.Query
  <\/> DemoButtons.Query
  <\/> DemoCards.Query
  <\/> DemoChips.Query
  <\/> DemoDialogs.Query
  <\/> DemoTabs.Query
  <\/> Const Void

type ChildSlot
  =  DemoHomeSlot
  \/ DemoBadgesSlot
  \/ DemoButtonsSlot
  \/ DemoCardsSlot
  \/ DemoChipsSlot
  \/ DemoDialogsSlot
  \/ DemoTabsSlot
  \/ Void

-- Slots
data DemoHomeSlot = DemoHomeSlot
derive instance eqDemoHomeSlot :: Eq DemoHomeSlot
derive instance ordDemoHomeSlot :: Ord DemoHomeSlot
cpDemoHome :: CP.ChildPath DemoHome.Query ChildQuery DemoHomeSlot ChildSlot
cpDemoHome = CP.cp1

data DemoBadgesSlot = DemoBadgesSlot
derive instance eqDemoBadgesSlot :: Eq DemoBadgesSlot
derive instance ordDemoBadgesSlot :: Ord DemoBadgesSlot
cpDemoBadges :: CP.ChildPath DemoBadges.Query ChildQuery DemoBadgesSlot ChildSlot
cpDemoBadges = CP.cp2

data DemoButtonsSlot = DemoButtonsSlot
derive instance eqDemoButtonsSlot :: Eq DemoButtonsSlot
derive instance ordDemoButtonsSlot :: Ord DemoButtonsSlot
cpDemoButtons :: CP.ChildPath DemoButtons.Query ChildQuery DemoButtonsSlot ChildSlot
cpDemoButtons = CP.cp3

data DemoCardsSlot = DemoCardsSlot
derive instance eqDemoCardsSlot :: Eq DemoCardsSlot
derive instance ordDemoCardsSlot :: Ord DemoCardsSlot
cpDemoCards :: CP.ChildPath DemoCards.Query ChildQuery DemoCardsSlot ChildSlot
cpDemoCards = CP.cp4

data DemoChipsSlot = DemoChipsSlot
derive instance eqDemoChipsSlot :: Eq DemoChipsSlot
derive instance ordDemoChipsSlot :: Ord DemoChipsSlot
cpDemoChips :: CP.ChildPath DemoChips.Query ChildQuery DemoChipsSlot ChildSlot
cpDemoChips = CP.cp5

data DemoDialogsSlot = DemoDialogsSlot
derive instance eqDemoDialogsSlot :: Eq DemoDialogsSlot
derive instance ordDemoDialogsSlot :: Ord DemoDialogsSlot
cpDemoDialogs :: CP.ChildPath DemoDialogs.Query ChildQuery DemoDialogsSlot ChildSlot
cpDemoDialogs = CP.cp6

data DemoTabsSlot = DemoTabsSlot
derive instance eqDemoTabsSlot :: Eq DemoTabsSlot
derive instance ordDemoTabsSlot :: Ord DemoTabsSlot
cpDemoTabs :: CP.ChildPath DemoTabs.Query ChildQuery DemoTabsSlot ChildSlot
cpDemoTabs = CP.cp7

type DemoContainerHTML eff = H.ParentHTML Query ChildQuery ChildSlot (Aff (HA.HalogenEffects eff))
type DemoContainerDSL eff = H.ParentDSL State Query ChildQuery ChildSlot Message (Aff (HA.HalogenEffects eff))

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

  drawerRef :: H.RefLabel
  drawerRef = H.RefLabel "mdl-layout-drawer"

  initialState :: Input -> State
  initialState (Initialize state) = state

  initializer :: Maybe (Query Unit)
  initializer = Just $ H.action InitializeComponent

  finalizer :: Maybe (Query Unit)
  finalizer = Just $ H.action FinalizeComponent

  receiver :: Input -> Maybe (Query Unit)
  receiver (Initialize state) = Just $ H.action $ UpdateState state

  render :: State -> DemoContainerHTML eff
  render state =
    HH.div
      [ HP.class_ Layout.cl.layoutContainer ]
      [ HH.div
          [ HP.classes [ Layout.cl.layout, Layout.cl.jsLayout, Layout.cl.layoutFixedHeader ]
          , HP.ref layoutRef
          ]
          [ renderLayoutHeader
          , renderLayoutDrawer
          , renderLayoutContent state
          ]
      ]

  renderLayoutHeader :: DemoContainerHTML eff
  renderLayoutHeader =
    HH.header
      [ HP.classes [ Layout.cl.layoutHeader ] ]
      [ HH.div
        [ HP.classes [ Layout.cl.layoutHeaderRow ] ]
        [ HH.span [ HP.classes [ Layout.cl.layoutTitle] ] [ HH.text "Halogen MDL" ]
        , HH.div [ HP.classes [ Layout.cl.layoutSpacer ] ] []
        , HH.nav
          [ HP.classes [ Navigation.cl.navigation, Layout.cl.layoutLargeScreenOnly ] ]
          [ renderLayoutHeaderLink { href: "#", text: "Link 1" }
          , renderLayoutHeaderLink { href: "#", text: "Link 2" }
          , renderLayoutHeaderLink { href: "#", text: "Link 3" }
          , renderLayoutHeaderLink { href: "#", text: "Link 4" }
          ]
        ]
      ]

  renderLayoutHeaderLink :: Basic.BasicLink -> DemoContainerHTML eff
  renderLayoutHeaderLink link =
    HH.a
      [ HP.href link.href, HP.classes [ Navigation.cl.navigationLink ] ]
      [ HH.text link.text ]

  renderLayoutDrawer :: DemoContainerHTML eff
  renderLayoutDrawer =
    HH.div
      [ HP.classes [ Layout.cl.layoutDrawer ]
      , HP.ref drawerRef
      ]
      [ HH.span
        [ HP.classes [ Layout.cl.layoutTitle ] ]
        [ HH.text "Halogen MDL" ]
      , HH.nav
        [ HP.classes [ Navigation.cl.navigation ] ]
        [ renderLayoutDrawerLink Home
        , renderLayoutDrawerLink Badges
        , renderLayoutDrawerLink Buttons
        , renderLayoutDrawerLink Cards
        , renderLayoutDrawerLink Chips
        , renderLayoutDrawerLink Dialogs
        , renderLayoutDrawerLink Tabs
        ]
      ]

  renderLayoutDrawerLink :: Route -> DemoContainerHTML eff
  renderLayoutDrawerLink route =
    HH.a
      [ HP.href $ Route.href route
      , HP.classes [ Navigation.cl.navigationLink ]
      , HE.onClick $ HE.input_ OnNavClick
      ]
      [ HH.text $ Route.label route ]

  renderLayoutContent :: State -> DemoContainerHTML eff
  renderLayoutContent state =
    HH.div
      [ HP.classes [ Layout.cl.layoutContent ] ]
      [ HH.div
        [ HP.classes [ HH.ClassName "page-content" ] ]
        [ renderPageContent state
        , renderMegaFooter
        ]
      ]

  renderPageContent :: State -> DemoContainerHTML eff
  renderPageContent state = case state.currentRoute of
    Home ->
      HH.slot'
        cpDemoHome
        DemoHomeSlot
        DemoHome.demoHome
        (DemoHome.init unit)
        (HE.input OnDemoHomeMessage)
    Badges ->
      HH.slot'
        cpDemoBadges
        DemoBadgesSlot
        DemoBadges.demoBadges
        (DemoBadges.init unit)
        (HE.input OnDemoBadgesMessage)
    Buttons ->
      HH.slot'
        cpDemoButtons
        DemoButtonsSlot
        DemoButtons.demoButtons
        (DemoButtons.init { clickDemo: { clickCount: 0 }, nonComponentDemo: { isLoading: false } })
        (HE.input OnDemoButtonsMessage)
    Cards ->
      HH.slot'
        cpDemoCards
        DemoCardsSlot
        DemoCards.demoCards
        (DemoCards.init unit)
        (HE.input OnDemoCardsMessage)
    Chips ->
      HH.slot'
        cpDemoChips
        DemoChipsSlot
        DemoChips.demoChips
        (DemoChips.init unit)
        (HE.input OnDemoChipsMessage)
    Dialogs ->
      HH.slot'
        cpDemoDialogs
        DemoDialogsSlot
        DemoDialogs.demoDialogs
        (DemoDialogs.init unit)
        (HE.input OnDemoDialogsMessage)
    Tabs ->
      HH.slot'
        cpDemoTabs
        DemoTabsSlot
        DemoTabs.demoTabs
        (DemoTabs.init unit)
        (HE.input OnDemoTabsMessage)

  renderMegaFooter :: DemoContainerHTML eff
  renderMegaFooter =
    MegaFooter.bl.megaFooter
      { middleSection:
          { dropDownSections: dummyDropDownSection <$> (1 .. 4) }
      , bottomSection:
          { title: "Bottom title"
          , linkList: dummyLinkList
          }
      }

  dummyDropDownSection :: Int -> MegaFooter.DropDownSectionBlock
  dummyDropDownSection i =
    { title: "Drop down section " <> show i
    , linkList: dummyLinkList
    }

  dummyLinkList :: MegaFooter.LinkListBlock
  dummyLinkList =
    { links:
        [ { href: "#", text: "Link 1" }
        , { href: "#", text: "Link 2" }
        , { href: "#", text: "Link 3" }
        , { href: "#", text: "Link 4" }
        ]
    }

  eval :: Query ~> DemoContainerDSL eff
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
    OnNavClick next -> do
      maybeDrawer <- H.getHTMLElementRef drawerRef
      case maybeDrawer of
        Just drawer -> H.liftEff $ Layout.toggleDrawer
        Nothing -> pure unit
      pure next
    OnDemoHomeMessage _ next -> do
      pure next
    OnDemoBadgesMessage _ next -> do
      pure next
    OnDemoButtonsMessage _ next -> do
      pure next
    OnDemoCardsMessage _ next -> do
      pure next
    OnDemoChipsMessage _ next -> do
      pure next
    OnDemoDialogsMessage _ next -> do
      pure next
    OnDemoTabsMessage _ next -> do
      pure next
