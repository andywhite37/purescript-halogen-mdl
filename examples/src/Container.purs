module Container where

import Prelude

import Data.Maybe (Maybe(Nothing))
import Halogen as H
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

container :: ∀ m. H.Component HH.HTML Query Input Message m
container =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { clickCount: 0
    }

  render :: State -> H.ParentHTML Query Button.Query Slot m
  render state =
    HH.div_
      [ HH.slot ButtonSlot Button.button (Button.Initialize { text: "Click this", disabled: false }) (HE.input HandleButton)
      , HH.p_
        [ HH.text $ "Button has been clicked " <> show state.clickCount <> " times." ]
      ]

  eval :: Query ~> H.ParentDSL State Query Button.Query Slot Message m
  eval = case _ of
    HandleButton Button.Clicked next -> do
      H.modify (\st -> st { clickCount = st.clickCount + 1 })
      pure next
