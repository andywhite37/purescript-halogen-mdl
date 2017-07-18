module DemoBadges where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Halogen.MDL.Badge as Badge
import Halogen.MDL.Cell as Cell
import Halogen.MDL.Grid as Grid
import Halogen.MDL.MaterialIcon as MI

type State = Unit

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a

data Input = Initialize State

type Message = Void

type DemoBadgesHTML = H.ComponentHTML Query
type DemoBadgesDSL eff = H.ComponentDSL State Query Message (Aff (HA.HalogenEffects eff))

init :: State -> Input
init state = Initialize state

demoBadges :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
demoBadges =
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

  render :: State -> DemoBadgesHTML
  render state =
    Grid.el.grid_
      [ renderBadgesHeader
      , renderDemoHeader "Badges on icons (overlap)"
      , renderDemoSection
          [ HH.div
              [ HP.classes [ Badge.cl.badge, Badge.cl.badgeOverlap, MI.cl.materialIcons ]
              , HP.attr Badge.attr.dataBadge "1"
              ]
              [ HH.text MI.str.accountBox ]
          ]
      , renderDemoSection
          [ HH.div
              [ HP.classes [ Badge.cl.badge, Badge.cl.badgeOverlap, MI.cl.materialIcons ]
              , HP.attr Badge.attr.dataBadge "♥"
              ]
              [ HH.text MI.str.accountBox ]
          ]
      , renderDemoHeader "Badges on icons (overlap, no background)"
      , renderDemoSection
          [ HH.div
              [ HP.classes [ Badge.cl.badge, Badge.cl.badgeOverlap, Badge.cl.badgeNoBackground, MI.cl.materialIcons ]
              , HP.attr Badge.attr.dataBadge "1"
              ]
              [ HH.text MI.str.accountBox ]
          ]
      , renderDemoSection
          [ HH.div
              [ HP.classes [ Badge.cl.badge, Badge.cl.badgeOverlap, Badge.cl.badgeNoBackground, MI.cl.materialIcons ]
              , HP.attr Badge.attr.dataBadge "♥"
              ]
              [ HH.text MI.str.accountBox ]
          ]
      , renderDemoHeader "Badges on text (no overlap)"
      , renderDemoSection
          [ HH.span
              [ HP.classes [ Badge.cl.badge ]
              , HP.attr Badge.attr.dataBadge "1"
              ]
              [ HH.text "Inbox" ]
          ]
      , renderDemoSection
          [ HH.span
              [ HP.classes [ Badge.cl.badge ]
              , HP.attr Badge.attr.dataBadge "♥"
              ]
              [ HH.text "Mood" ]
          ]
      , renderDemoHeader "Badges on text (no overlap, no background)"
      , renderDemoSection
          [ HH.span
              [ HP.classes [ Badge.cl.badge, Badge.cl.badgeNoBackground ]
              , HP.attr Badge.attr.dataBadge "1"
              ]
              [ HH.text "Inbox" ]
          ]
      , renderDemoSection
          [ HH.span
              [ HP.classes [ Badge.cl.badge, Badge.cl.badgeNoBackground ]
              , HP.attr Badge.attr.dataBadge "♥"
              ]
              [ HH.text "Mood" ]
          ]
      ]

  renderBadgesHeader :: ∀ p i. HH.HTML p i
  renderBadgesHeader = Cell.el.cell12Col_ [ HH.h1_ [ HH.text "Badges" ] ]

  renderDemoHeader :: ∀ p i. String -> HH.HTML p i
  renderDemoHeader name = Cell.el.cell12Col_ [ HH.h3_ [ HH.text name ] ]

  renderDemoSection :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
  renderDemoSection body = Cell.el.cell3Col_ body

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (HA.HalogenEffects eff))
  eval = case _ of
    InitializeComponent next -> pure next
    FinalizeComponent next -> pure next
    UpdateState state next -> do
      H.put state
      pure next
