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
import DemoBadges as DemoBadges
import DemoButtons as DemoButtons

type State =
  { currentRoute :: Route
  }

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a
  | UpdateRoute Route a
  | OnNavClick a
  | OnDemoBadgesMessage DemoBadges.Message a
  | OnDemoButtonsMessage DemoButtons.Message a

data Input = Initialize State

type Message = Void

type ChildQuery = DemoBadges.Query <\/> DemoButtons.Query <\/> Const Void

-- Slots
data DemoBadgesSlot = DemoBadgesSlot
derive instance eqDemoBadgesSlot :: Eq DemoBadgesSlot
derive instance ordDemoBadgesSlot :: Ord DemoBadgesSlot

data DemoButtonsSlot = DemoButtonsSlot
derive instance eqDemoButtonsSlot :: Eq DemoButtonsSlot
derive instance ordDemoButtonsSlot :: Ord DemoButtonsSlot

type ChildSlot = DemoBadgesSlot \/ DemoButtonsSlot \/ Void

cpDemoBadges :: CP.ChildPath DemoBadges.Query ChildQuery DemoBadgesSlot ChildSlot
cpDemoBadges = CP.cp1

cpDemoButtons :: CP.ChildPath DemoButtons.Query ChildQuery DemoButtonsSlot ChildSlot
cpDemoButtons = CP.cp2

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
      [ HP.classes [ Layout.cl.layout, Layout.cl.jsLayout, Layout.cl.layoutFixedHeader ]
      , HP.ref layoutRef
      ]
      [ renderLayoutHeader
      , renderLayoutDrawer
      , renderLayoutContent state
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
      HH.div_ [ HH.text $ Route.label state.currentRoute ]
    Badges ->
      --HH.div_ [ HH.text $ Route.label state.currentRoute ]
      HH.slot'
        cpDemoBadges
        DemoBadgesSlot
        DemoBadges.demoBadges
        (DemoBadges.init {})
        (HE.input OnDemoBadgesMessage)
    Buttons ->
      HH.slot'
        cpDemoButtons
        DemoButtonsSlot
        DemoButtons.demoButtons
        (DemoButtons.init { clickCount: 0 })
        (HE.input OnDemoButtonsMessage)

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
      drawer <- H.getHTMLElementRef drawerRef
      case drawer of
        Just drawer -> H.liftEff $ Layout.hideLayoutDrawer
        Nothing -> pure unit
      pure next
    OnDemoBadgesMessage _ next -> do
      pure next
    OnDemoButtonsMessage _ next -> do
      pure next
