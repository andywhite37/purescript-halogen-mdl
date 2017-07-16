module DemoHome where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH

import Halogen.MDL.Cell as Cell
import Halogen.MDL.Grid as Grid

type State = Unit
data Query a
  = UpdateState State a
data Input = Initialize State
type Message = Void

type DemoHomeHTML = H.ComponentHTML Query
type DemoHomeDSL eff = H.ComponentDSL State Query Message (Aff (HA.HalogenEffects eff))

init :: State -> Input
init _ = Initialize unit

demoHome :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
demoHome = H.component
  { initialState: initialState
  , receiver
  , render
  , eval
  }
  where

  initialState :: Input -> State
  initialState _ = unit

  receiver :: Input -> Maybe (Query Unit)
  receiver _ = Nothing

  render :: State -> DemoHomeHTML
  render state =
    Grid.el.grid_
      [ renderDemoHomeHeader
      ]

  renderDemoHomeHeader :: DemoHomeHTML
  renderDemoHomeHeader =
    Cell.el.cell12Col_ [ HH.h1_ [ HH.text "Halogen MDL" ] ]


  eval :: Query ~> DemoHomeDSL eff
  eval = case _ of
    UpdateState state next ->
      pure next
