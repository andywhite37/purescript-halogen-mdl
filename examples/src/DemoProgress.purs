module DemoProgress where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))

import CSS as C
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as HC

import Halogen.MDL as MDL
import Halogen.MDL.Cell as Cell
import Halogen.MDL.Grid as Grid
import Halogen.MDL.Progress as Progress

type State = { progress :: Int }

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a

data Input
  = Initialize State

type Message = Void

type DemoProgressHTML = H.ComponentHTML Query
type DemoProgressDSL eff = H.ComponentDSL State Query Message (Aff (HA.HalogenEffects eff))

init :: State -> Input
init = Initialize

demoProgress :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
demoProgress = H.lifecycleComponent
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

  render :: State -> DemoProgressHTML
  render state =
    Grid.el.grid_
      [ Cell.el.cell12Col_
          [ HH.h3_ [ HH.text "Determinate progress bar" ] ]
      , Cell.el.cell12Col_
          [ HH.div
              [ HC.style $ C.width $ C.px 250.0
              , HP.classes [ Progress.cl.progress, Progress.cl.jsProgress ]
              ]
              []
          ]
      ]

  eval :: Query ~> DemoProgressDSL eff
  eval = case _ of
    InitializeComponent next -> do
      H.liftEff $ MDL.upgradeElementsByClassName Progress.cl.jsProgress
      pure next
    FinalizeComponent next -> do
      pure next
    UpdateState state next -> do
      H.put state
      pure next
