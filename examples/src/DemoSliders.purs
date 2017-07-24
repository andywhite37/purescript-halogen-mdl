module DemoSliders where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Halogen.MDL.Cell as Cell
import Halogen.MDL.Grid as Grid
import Halogen.MDL.Slider as Slider

type State =
  { slider1 :: Number
  , slider2 :: Number
  , slider3 :: Number
  }

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a
  | OnSlider1Message Slider.Message a
  | OnSlider2Message Slider.Message a
  | OnSlider3Message Slider.Message a

data Input
  = Initialize State

type Message = Void

data ChildSlot
  = Slider1Slot
  | Slider2Slot
  | Slider3Slot
derive instance eqChildSlot :: Eq ChildSlot
derive instance ordChildSlot :: Ord ChildSlot

type DemoSlidersHTML eff = H.ParentHTML Query Slider.Query ChildSlot (Aff (HA.HalogenEffects eff))
type DemoSlidersDSL eff = H.ParentDSL State Query Slider.Query ChildSlot Message (Aff (HA.HalogenEffects eff))

init :: State -> Input
init = Initialize

demoSliders :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
demoSliders = H.lifecycleParentComponent
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

  render :: State -> DemoSlidersHTML eff
  render state =
    Grid.el.grid_
      [ Cell.el.cell12Col_
          [ HH.h3_ [ HH.text "Sliders" ] ]
      , Cell.el.cell4Col_
          [ HH.slot
              Slider1Slot
              Slider.slider
              (Slider.init { min: 0.0, max: 100.0, value: state.slider1, step: 1.0, disabled: false })
              (HE.input OnSlider1Message)
          ]
      , Cell.el.cell8Col_
          [ HH.p_ [ HH.text $ "Slider value: " <> show state.slider1 ] ]
      , Cell.el.cell4Col_
          [ HH.slot
              Slider2Slot
              Slider.slider
              (Slider.init { min: 0.0, max: 100.0, value: state.slider2, step: 1.0, disabled: false })
              (HE.input OnSlider2Message)
          ]
      , Cell.el.cell8Col_
          [ HH.p_ [ HH.text $ "Slider value: " <> show state.slider2 ] ]
      , Cell.el.cell4Col_
          -- disabled but tracks slider 2 value
          [ HH.slot
              Slider3Slot
              Slider.slider
              (Slider.init { min: 0.0, max: 100.0, value: state.slider3, step: 1.0, disabled: true })
              (HE.input OnSlider3Message)
          ]
      , Cell.el.cell8Col_
          [ HH.p_ [ HH.text $ "Slider value: " <> show state.slider3 ] ]
      ]

  eval :: Query ~> DemoSlidersDSL eff
  eval = case _ of
    InitializeComponent next -> do
      -- Don't upgrade because we're using our component that fakes the structure
      --H.liftEff $ MDL.upgradeElementsByClassName Slider.cl.jsSlider
      pure next
    FinalizeComponent next -> do
      pure next
    UpdateState state next -> do
      H.put state
      pure next
    OnSlider1Message (Slider.ValueChanged value) next -> do
      H.modify (_ { slider1 = value })
      pure next
    OnSlider2Message (Slider.ValueChanged value) next -> do
      H.modify (_ { slider2 = value })
      -- Set disabled slider 3 value to follow slider 2 value
      _ <- H.query Slider3Slot $ H.action (Slider.SetValue value)
      pure next
    OnSlider3Message (Slider.ValueChanged value) next -> do
      H.modify (_ { slider3 = value })
      pure next
