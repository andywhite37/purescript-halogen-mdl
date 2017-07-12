module Halogen.MDL.Button where

import Prelude

import Data.Maybe (Maybe(Nothing))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { text :: String
  , disabled :: Boolean
  }

data Query a = OnClick a

data Input = Initialize State

data Message = Clicked

button :: ∀ m. H.Component HH.HTML Query Input Message m
button =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    --, initialize: ?init
    --, finalizer: ?final
    }
  where

  initialState :: State
  initialState =
    { text: "Click me"
    , disabled: false
    }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.button
      [ HP.classes [HH.ClassName "mdl-button", HH.ClassName "mdl-button--raised"]
      , HE.onClick (HE.input_ OnClick)
      ]
      [ HH.text state.text ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    OnClick next -> do
      H.raise Clicked
      pure next
