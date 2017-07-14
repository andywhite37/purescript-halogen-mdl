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

-- MDL classes for buttons

classes ::
  { button :: HH.ClassName
  , buttonRaised :: HH.ClassName
  , buttonFab :: HH.ClassName
  , buttonMiniFab :: HH.ClassName
  , buttonIcon :: HH.ClassName
  , buttonColored :: HH.ClassName
  , buttonPrimary :: HH.ClassName
  , buttonAccent :: HH.ClassName
  , jsButton :: HH.ClassName
  }
classes =
  { button        : HH.ClassName "mdl-button"
  , buttonRaised  : HH.ClassName "mdl-button--raised"
  , buttonFab     : HH.ClassName "mdl-button--fab"
  , buttonMiniFab : HH.ClassName "mdl-button--mini-fab"
  , buttonIcon    : HH.ClassName "mdl-button--icon"
  , buttonColored : HH.ClassName "mdl-button--colored"
  , buttonPrimary : HH.ClassName "mdl-button--primary"
  , buttonAccent  : HH.ClassName "mdl-button--accent"
  , jsButton      : HH.ClassName "mdl-js-button"
  }

-- MDL Button component supporting types

data ButtonType
  = Flat    -- <none>
  | Raised  -- mdl-button--raised
  | Fab     -- mdl-button--fab
  | MiniFab -- mdl-button--mini-fab
  | Icon    -- mdl-button--icon
derive instance eqButtonType :: Eq ButtonType
derive instance ordButtonType :: Ord ButtonType

data ButtonColor
  = Plain   -- <none>
  | Colored -- mdl-button--colored
  | Primary -- mdl-button--primary
  | Accent  -- mdl-button--accent
derive instance eqButtonColor :: Eq ButtonColor
derive instance ordButtonColor :: Ord ButtonColor

type Props =
  { type :: ButtonType
  , color :: ButtonColor
  , disabled :: Boolean -- whether the button is disabled
  , ripple :: Boolean
  , text :: String -- text of the button - TODO: want to allow a child component as the content of this button component
  }

newtype State = State Props
derive instance eqState :: Eq State
derive instance ordState :: Ord State

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a
  | OnClick MouseEvent a

data Input = Initialize State

data Message = Clicked MouseEvent

-- Helper to create initial Input for a Button
init :: Props -> Input
init props = Initialize $ State props

-- MDL button component
button :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
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

  buttonRef :: H.RefLabel
  buttonRef = H.RefLabel "mdl-button-ref"

  -- Map Input to the initial State
  initialState :: Input -> State
  initialState (Initialize state) = state

  -- Get Query to initialize the component
  initializer :: Maybe (Query Unit)
  initializer = Just $ H.action InitializeComponent

  -- Get Query to finalize the component
  finalizer :: Maybe (Query Unit)
  finalizer = Just $ H.action FinalizeComponent

  -- Map Inputs to Queries
  receiver :: Input -> Maybe (Query Unit)
  receiver (Initialize state) = Just $ H.action $ UpdateState state

  -- Render the button
  render :: State -> H.ComponentHTML Query
  render (State props) =
    HH.button
      -- TODO: allow additional properties to be added to the button (e.g. extra classes, etc.)
      [ HP.ref buttonRef
      , HP.classes $ getClasses props
      , HP.disabled props.disabled
      , HE.onClick $ HE.input OnClick
      ]
      -- TODO: I want this to be a HH.slot so the content/children of the button can be a component
      [ HH.text props.text ]
      --[ HH.slot ContentSlot

  getClasses :: Props -> Array HH.ClassName
  getClasses props =
    [ classes.button
    , classes.jsButton
    ] -- required classes
      <> case props.type of
        Flat -> []
        Raised -> [classes.buttonRaised]
        Fab -> [classes.buttonFab]
        MiniFab -> [classes.buttonMiniFab]
        Icon -> [classes.buttonIcon]
      <> case props.color of
        Plain -> []
        Colored -> [classes.buttonColored]
        Primary -> [classes.buttonPrimary]
        Accent -> [classes.buttonAccent]
      <> if props.ripple
         then [RippleEffect.classes.jsRippleEffect]
         else []

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (HA.HalogenEffects eff))
  eval = case _ of
    -- Initialize the button (i.e. MDL upgradeElement to get javascript effects, like ripple)
    InitializeComponent next -> do
      --State props <- H.get
      -- TODO: is there a way to get the component's HTMLElement without using a ref or DOM query?
      element <- H.getHTMLElementRef buttonRef
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
