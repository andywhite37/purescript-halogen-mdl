module DemoBadges where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
--import Halogen.HTML.Events as HE
--import Halogen.HTML.Properties as HP

--import Halogen.MDL.Badge as Badge

type State =
  {
  }

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a

data Input = Initialize State

type Message = Void

{-
data Slot = ButtonSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot
-}

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

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_ [ HH.text $ "Badges !!!" ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (HA.HalogenEffects eff))
  eval = case _ of
    InitializeComponent next -> pure next
    FinalizeComponent next -> pure next
    UpdateState state next -> do
      H.put state
      pure next
