module DemoChips where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
--import Unsafe.Partial (unsafePartial)


import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
--import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP

--import Halogen.MDL as MDL
--import Halogen.MDL.Button as Button
import Halogen.MDL.Chip as Chip
import Halogen.MDL.Cell as Cell
import Halogen.MDL.Color as Color
import Halogen.MDL.Grid as Grid
import Halogen.MDL.MaterialIcon as MI
--import Halogen.MDL.RippleEffect as RE
--import Halogen.MDL.Shadow as Shadow

type State = Unit

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a

data Input = Initialize State

type Message = Void

type DemoChipsHTML = H.ComponentHTML Query
type DemoChipsDSL eff = H.ComponentDSL State Query Message (Aff (HA.HalogenEffects eff))

init :: State -> Input
init state = Initialize state

demoChips :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
demoChips =
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

  render :: State -> DemoChipsHTML
  render state =
    Grid.el.grid_
      [ renderChipsHeader
      , renderDemoHeader "Basic chips"
      , renderDemoSection
          [ HH.span
              [ HP.class_ Chip.cl.chip ]
              [ HH.span
                  [ HP.class_ Chip.cl.chipText ]
                  [ HH.text "Basic chip" ]
              ]
          ]
      , renderDemoSection
          [ HH.span
              [ HP.classes [ Chip.cl.chip, Chip.cl.chipDeletable ] ]
              [ HH.span
                  [ HP.class_ Chip.cl.chipText ]
                  [ HH.text "Deletable chip" ]
              , HH.button
                  [ HP.class_ Chip.cl.chipAction
                  , HP.type_ HP.ButtonButton
                  ]
                  [ HH.i
                      [ HP.class_ MI.cl.materialIcons ]
                      [ HH.text MI.str.cancel ]
                  ]
              ]
          ]
      , renderDemoSection
          [ HH.button
              [ HP.class_ Chip.cl.chip
              , HP.type_ HP.ButtonButton
              ]
              [ HH.span
                  [ HP.class_ Chip.cl.chipText ]
                  [ HH.text "Button chip" ]
              ]
          ]
      , renderDemoHeader "Contact chips"
      , renderDemoSection
          [ HH.span
              [ HP.classes [ Chip.cl.chip, Chip.cl.chipContactM ] ]
              [ HH.span
                  [ HP.classes [ Chip.cl.chipContactE, Color.cl.colorTeal, Color.cl.colorTextWhite ] ]
                  [ HH.text "A" ]
              , HH.span
                  [ HP.class_ Chip.cl.chipText ]
                  [ HH.text "Contact Chip" ]
              ]
          ]
      , renderDemoSection
          [ HH.span
              [ HP.classes [ Chip.cl.chip, Chip.cl.chipContactM, Chip.cl.chipDeletable ] ]
              [ HH.img
                  [ HP.classes [ Chip.cl.chipContactE ]
                  , HP.src "https://getmdl.io/templates/dashboard/images/user.jpg"
                  ]
              , HH.span
                  [ HP.class_ Chip.cl.chipText ]
                  [ HH.text "Deletable Contact Chip" ]
              , HH.a
                  [ HP.class_ Chip.cl.chipAction
                  ]
                  [ HH.i
                      [ HP.class_ MI.cl.materialIcons ]
                      [ HH.text MI.str.cancel ]
                  ]
              ]
          ]
      ]

  renderChipsHeader :: ∀ p i. HH.HTML p i
  renderChipsHeader = Cell.el.cell12Col_ [ HH.h1_ [ HH.text "Chips" ] ]

  renderDemoHeader :: ∀ p i. String -> HH.HTML p i
  renderDemoHeader name = Cell.el.cell12Col_ [ HH.h3_ [ HH.text name ] ]

  renderDemoSection :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
  renderDemoSection body = Cell.el.cell3Col_ body

  eval :: Query ~> DemoChipsDSL eff
  eval = case _ of
    InitializeComponent next -> do
      pure next
    FinalizeComponent next -> do
      pure next
    UpdateState state next -> do
      H.put state
      pure next
