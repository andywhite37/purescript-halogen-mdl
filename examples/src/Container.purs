module Container where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(Nothing))

import DOM.Classy.Event (toEvent)
import DOM.Event.Event (preventDefault)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.MDL.Button as Button

type State =
  { clickCount :: Int
  }

data Query a
  = HandleButton Button.Message a

type Input = Unit

type Message = Void

data Slot = ButtonSlot
derive instance eqButtonSlot :: Eq Slot
derive instance ordButtonSlot :: Ord Slot

container :: H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects ()))
container =
  H.parentComponent
    { initialState: initialState
    , receiver: receiver
    , render
    , eval
    }
  where

  initialState :: Input -> State
  initialState _ =
    { clickCount: 0
    }

  receiver :: Input -> Maybe (Query Unit)
  receiver = const Nothing

  render :: State -> H.ParentHTML Query Button.Query Slot (Aff (HA.HalogenEffects ()))
  render state =
    HH.div_
      [ HH.slot ButtonSlot Button.button (Button.props { ref: "button", text: "Click this", disabled: false }) (HE.input HandleButton)
      , HH.p_
        [ HH.text $ "Button has been clicked " <> show state.clickCount <> " times." ]
      ]

  eval :: Query ~> H.ParentDSL State Query Button.Query Slot Message (Aff (HA.HalogenEffects ()))
  eval = case _ of
    HandleButton (Button.Clicked event) next -> do
      H.liftEff $ preventDefault $ toEvent event
      H.modify (\st -> st { clickCount = st.clickCount + 1 })
      pure next
