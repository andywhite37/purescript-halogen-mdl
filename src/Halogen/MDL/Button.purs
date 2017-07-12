module Halogen.MDL.Button where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))

import DOM.Event.Types (MouseEvent)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Halogen.MDL as MDL
import Halogen.MDL.RippleEffect as RippleEffect

type Props =
  { ref :: String -- unique ref for the button (used for getting the element for the javascript MDL upgradeElement call)
  , text :: String -- text of the button - TODO: want to allow a child component as the content of this button component
  , disabled :: Boolean -- whether the button is disabled
  }

newtype State = State Props
derive instance eqState :: Eq State
derive instance ordState :: Ord State

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a
  | OnClick MouseEvent a

-- TODO: not sure if this is the right approach for Inputs
data Input = Initialize State

data Message = Clicked MouseEvent

-- Creates a Button.Input from the raw Button.Props
props :: Props -> Input
props props = Initialize $ State props

-- MDL classes for buttons
classes ::
  { button :: HH.ClassName
  , buttonRaised :: HH.ClassName
  , jsButton :: HH.ClassName
  }
classes =
  { button: HH.ClassName "mdl-button"
  , buttonRaised: HH.ClassName "mdl-button--raised"
  , jsButton: HH.ClassName "mdl-js-button"
  }

-- MDL button component
button :: H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects ()))
button =
  -- TODO: make this a lifecycleParentComponent so the content of the button can be provided as another component?
  H.lifecycleComponent
    { initialState: initialState
    , initializer: initializer
    , finalizer: finalizer
    , receiver: receiver
    , render
    , eval
    }
  where

  -- Map Input to the initial State
  initialState :: Input -> State
  initialState = case _ of
    Initialize state -> state

  -- Get Query to initialize the component
  initializer :: Maybe (Query Unit)
  initializer = Just $ H.action InitializeComponent

  -- Get Query to finalize the component
  finalizer :: Maybe (Query Unit)
  finalizer = Just $ H.action FinalizeComponent

  -- Map Inputs to Queries
  receiver :: Input -> Maybe (Query Unit)
  receiver = case _ of
    Initialize state -> Just $ H.action $ UpdateState state

  -- Render the button
  render :: State -> H.ComponentHTML Query
  render (State state) =
    HH.button
      -- TODO: allow additional properties to be added to the button (e.g. extra classes, etc.)
      [ HP.ref $ H.RefLabel state.ref
      , HP.classes
        -- TODO: build classes based on TBD state
        [ classes.button
        , classes.buttonRaised
        , classes.jsButton
        , RippleEffect.classes.jsRippleEffect
        ]
      , HE.onClick (HE.input OnClick)
      ]
      -- TODO: I want this to be a HH.slot so the content/children of the button can be a component
      [ HH.text state.text ]

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (HA.HalogenEffects ()))
  eval = case _ of
    -- Initialize the button (i.e. MDL upgradeElement to get javascript effects, like ripple)
    InitializeComponent next -> do
      State state <- H.get
      -- TODO: is there a way to get the component's HTMLElement without using a ref or DOM query?
      element <- H.getHTMLElementRef (H.RefLabel state.ref)
      case element of
        Just element -> do
          H.liftEff $ MDL.upgradeElement element
        Nothing -> pure unit
      pure next

    -- Destroy the button
    FinalizeComponent next -> do
      pure next

    -- Update the button state
    UpdateState state next -> do
      oldState <- H.get
      when (state /= oldState) $ H.put state
      pure next

    -- Handle button click
    OnClick event next -> do
      H.raise $ Clicked event
      pure next
