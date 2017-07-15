module DemoButtons where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
--import Halogen.HTML.Properties as HP

import Halogen.MDL.Button as Button

type State =
  { clickCount :: Int
  }

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a
  | OnButtonMessage Button.Message a

data Input = Initialize State

type Message = Void

data Slot = ButtonSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

init :: State -> Input
init state = Initialize state

demoButtons :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
demoButtons =
  H.lifecycleParentComponent
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

  render :: State -> H.ParentHTML Query Button.Query Slot (Aff (HA.HalogenEffects eff))
  render state =
    HH.div_
      [ HH.h1_ [ HH.text $ "Buttons" ]
      , HH.slot
          ButtonSlot
          Button.button
          (Button.init { type: Button.Raised, color: Button.Colored, text: "Click this", disabled: false, ripple: true })
          (HE.input OnButtonMessage)
      , HH.p_ [ HH.text $ "Button has been clicked " <> show state.clickCount <> " times." ]
      ]

  eval :: Query ~> H.ParentDSL State Query Button.Query Slot Message (Aff (HA.HalogenEffects eff))
  eval = case _ of
    InitializeComponent next -> pure next
    FinalizeComponent next -> pure next
    UpdateState state next -> do
      H.put state
      pure next
    OnButtonMessage (Button.Clicked _) next -> do
      state <- H.get
      H.modify (\state -> state { clickCount = state.clickCount + 1 })
      pure next
