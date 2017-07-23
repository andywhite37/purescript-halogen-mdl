module DemoSpinners where

import Prelude

import Control.Monad.Aff (Aff, delay)
--import Control.Monad.Eff.Timer (setInterval)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))

import CSS as C
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as HC

import Halogen.MDL as MDL
import Halogen.MDL.Cell as Cell
import Halogen.MDL.Grid as Grid
import Halogen.MDL.Spinner as Spinner

type State = Unit

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a

data Input
  = Initialize State

type Message = Void

type DemoSpinnersHTML = H.ComponentHTML Query
type DemoSpinnersDSL eff = H.ComponentDSL State Query Message (Aff (HA.HalogenEffects eff))

init :: State -> Input
init = Initialize

demoSpinners :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
demoSpinners = H.lifecycleComponent
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

  render :: State -> DemoSpinnersHTML
  render state =
    Grid.el.grid_
      [ Cell.el.cell12Col_
          [ HH.h3_ [ HH.text "Spinners" ] ]
      , Cell.el.cell12Col_
          [ HH.div
              [ HP.classes [ Spinner.cl.spinner, Spinner.cl.jsSpinner, Spinner.cl.isActive ] ]
              []
          ]
      , Cell.el.cell12Col_
          [ HH.div
              [ HP.classes [ Spinner.cl.spinner, Spinner.cl.jsSpinner, Spinner.cl.spinnerSingleColor, Spinner.cl.isActive ] ]
              []
          ]
      ]

  eval :: Query ~> DemoSpinnersDSL eff
  eval = case _ of
    InitializeComponent next -> do
      H.liftEff $ MDL.upgradeElementsByClassName Spinner.cl.jsSpinner
      pure next
    FinalizeComponent next -> do
      pure next
    UpdateState state next -> do
      H.put state
      pure next
