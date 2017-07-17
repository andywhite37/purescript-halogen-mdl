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
      , renderBadgesDemo1
      ]

  renderBadgesHeader :: DemoBadgesHTML
  renderBadgesHeader = Cell.el.cell12Col_ [ HH.h1_ [ HH.text "Badges" ] ]

  renderBadgesDemo1 :: DemoBadgesHTML
  renderBadgesDemo1 =
    HH.div_
      [ Cell.el.cell12Col_ [ HH.h3_ [ HH.text "Badges on icons (overlap)" ] ]
      , Cell.el.cell6Col_
          [ HH.div
              [ HP.classes [ Badge.cl.badge, Badge.cl.badgeOverlap, MI.cl.materialIcons ]
              , HP.attr Badge.attr.dataBadge "1"
              ]
              [ MI.el.accountBox ]
          ]
      , Cell.el.cell6Col_
          [ HH.div
              [ HP.classes [ Badge.cl.badge, Badge.cl.badgeOverlap, MI.cl.materialIcons ]
              , HP.attr Badge.attr.dataBadge "♥"
              ]
              [ MI.el.accountBox ]
          ]
      , Cell.el.cell12Col_ [ HH.h3_ [ HH.text "Badges on icons (overlap, no background)" ] ]
      , Cell.el.cell6Col_
          [ HH.div
              [ HP.classes [ Badge.cl.badge, Badge.cl.badgeOverlap, Badge.cl.badgeNoBackground, MI.cl.materialIcons ]
              , HP.attr Badge.attr.dataBadge "1"
              ]
              [ MI.el.accountBox ]
          ]
      , Cell.el.cell6Col_
          [ HH.div
              [ HP.classes [ Badge.cl.badge, Badge.cl.badgeOverlap, Badge.cl.badgeNoBackground, MI.cl.materialIcons ]
              , HP.attr Badge.attr.dataBadge "♥"
              ]
              [ MI.el.accountBox ]
          ]
      , Cell.el.cell12Col_ [ HH.h3_ [ HH.text "Badges on text (no overlap)" ] ]
      , Cell.el.cell6Col_
          [ HH.span
              [ HP.classes [ Badge.cl.badge ]
              , HP.attr Badge.attr.dataBadge "1"
              ]
              [ HH.text "Inbox" ]
          ]
      , Cell.el.cell6Col_
          [ HH.span
              [ HP.classes [ Badge.cl.badge ]
              , HP.attr Badge.attr.dataBadge "♥"
              ]
              [ HH.text "Mood" ]
          ]
      , Cell.el.cell12Col_ [ HH.h3_ [ HH.text "Badges on text (no overlap, no background)" ] ]
      , Cell.el.cell6Col_
          [ HH.span
              [ HP.classes [ Badge.cl.badge, Badge.cl.badgeNoBackground ]
              , HP.attr Badge.attr.dataBadge "1"
              ]
              [ HH.text "Inbox" ]
          ]
      , Cell.el.cell6Col_
          [ HH.span
              [ HP.classes [ Badge.cl.badge, Badge.cl.badgeNoBackground ]
              , HP.attr Badge.attr.dataBadge "♥"
              ]
              [ HH.text "Mood" ]
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (HA.HalogenEffects eff))
  eval = case _ of
    InitializeComponent next -> pure next
    FinalizeComponent next -> pure next
    UpdateState state next -> do
      H.put state
      pure next
