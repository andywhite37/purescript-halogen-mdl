module Container where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))

import DOM.Classy.Event (toEvent)
import DOM.Event.Event (preventDefault)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Halogen.MDL.Badge as Badge
import Halogen.MDL.Button as Button

import Route (Route)

type State =
  { currentRoute :: Route
  , clickCount :: Int
  }

data Query a
  = UpdateState State a
  | UpdateRoute Route a
  | OnButtonMessage Button.Message a

data Input = Initialize State

type Message = Void

data Slot
  = ButtonSlot
derive instance eqButtonSlot :: Eq Slot
derive instance ordButtonSlot :: Ord Slot

init :: State -> Input
init state = Initialize state

container :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
container =
  H.parentComponent
    { initialState: initialState
    , receiver: receiver
    , render
    , eval
    }
  where

  initialState :: Input -> State
  initialState (Initialize state) = state

  receiver :: Input -> Maybe (Query Unit)
  receiver (Initialize state) = Just $ H.action $ UpdateState state

  render :: State -> H.ParentHTML Query Button.Query Slot (Aff (HA.HalogenEffects eff))
  render state =
    HH.div_
      [ HH.h1_
        [ HH.text $ show state.currentRoute ]
      , HH.slot
          ButtonSlot
          Button.button
          (Button.init { type: Button.Raised, color: Button.Colored, text: "Click this", disabled: false, ripple: true })
          (HE.input OnButtonMessage)
      , HH.p_
        [ HH.text $ "Button has been clicked " <> show state.clickCount <> " times." ]
      , HH.div
        [ HP.classes [Badge.classes.badge]
        , HP.attr (H.AttrName "data-badge") (show state.clickCount)
        ]
        [ HH.text "My badge" ]
      ]

  eval :: Query ~> H.ParentDSL State Query Button.Query Slot Message (Aff (HA.HalogenEffects eff))
  eval = case _ of
    UpdateState state next -> do
      -- TODO: check for change?
      H.put state
      pure next

    UpdateRoute route next -> do
      H.modify (\state -> state { currentRoute = route })
      pure next

    OnButtonMessage (Button.Clicked event) next -> do
      H.liftEff $ preventDefault $ toEvent event
      H.modify (\state -> state { clickCount = state.clickCount + 1 })
      pure next
