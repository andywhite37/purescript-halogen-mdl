module Halogen.MDL.Slider where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Number (fromString)

import DOM (DOM)
import DOM.HTML.Types (HTMLElement())
import CSS as C

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- TODO: using the MDL upgradeElement on this is difficult
-- because upgradeElement changes the structure of the HTML.
-- For now, in this slider component, I'm faking the structure of the HTML
-- with none of the MaterialSlider JS API wired up.
-- This means we're not using the MaterialSlider.change function at all,
-- but we'll still FFI and expose it

foreign import change :: ∀ eff. HTMLElement -> Number -> Eff (dom :: DOM | eff) Unit

changeByRef :: forall e s f g p o m. MonadEff (dom :: DOM | e) m => H.RefLabel -> Number -> H.HalogenM s f g p o m Unit
changeByRef ref value = do
  maybeElement <- H.getHTMLElementRef ref
  case maybeElement of
    Just element -> H.liftEff $ change element value
    Nothing -> pure unit

cl ::
  { slider :: HH.ClassName
  , jsSlider :: HH.ClassName
  , sliderContainer :: HH.ClassName
  , sliderBackgroundFlex :: HH.ClassName
  , sliderBackgroundLower :: HH.ClassName
  , sliderBackgroundUpper :: HH.ClassName
  , isUpgraded :: HH.ClassName
  }
cl =
  { slider : HH.ClassName "mdl-slider"
  , jsSlider : HH.ClassName "mdl-js-slider"
  , sliderContainer : HH.ClassName "mdl-slider__container"
  , sliderBackgroundFlex : HH.ClassName "mdl-slider__background-flex"
  , sliderBackgroundLower : HH.ClassName "mdl-slider__background-lower"
  , sliderBackgroundUpper : HH.ClassName "mdl-slider__background-upper"
  , isUpgraded : HH.ClassName "is-upgraded"
  }

attr ::
  { dataUpgraded :: HH.AttrName
  }
attr =
  { dataUpgraded : HH.AttrName "data-upgraded"
  }

type State =
  { min :: Number
  , max :: Number
  , value :: Number
  , step :: Number
  , disabled :: Boolean
  }

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a
  | OnValueInput String a
  | SetValue Number a
  | GetValue (Number -> a)

data Input
  = Initialize State

data Message
  = ValueChanged Number

type SliderHTML = H.ComponentHTML Query
type SliderDSL eff = H.ComponentDSL State Query Message (Aff (HA.HalogenEffects eff))

init :: State -> Input
init = Initialize

slider :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
slider = H.lifecycleComponent
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
  receiver (Initialize state) = Nothing -- receiver ignores Initialize, otherwise the control keeps getting reset

  sliderRef :: H.RefLabel
  sliderRef = H.RefLabel "slider-ref"

  render :: State -> SliderHTML
  render state =
    HH.div
      [ HP.class_ cl.sliderContainer ]
      [ HH.input
          [ HP.classes [ cl.slider, cl.jsSlider, cl.isUpgraded ]
          , HP.type_ HP.InputRange
          , HP.min state.min
          , HP.max state.max
          , HP.step $ HP.Step state.step
          , HP.value $ show state.value
          , HP.tabIndex 0
          , HP.attr attr.dataUpgraded ",MaterialSlider"
          , HP.disabled state.disabled
          , HE.onValueInput $ HE.input OnValueInput
          , HP.ref sliderRef
          ]
      , HH.div
          [ HP.class_ cl.sliderBackgroundFlex ]
          [ HH.div
              [ HP.class_ cl.sliderBackgroundLower
              , HC.style do
                  C.flexShrink 1000
                  C.flexGrow $ getBackgroundLowerFlexGrow state
                  C.flexBasis $ C.pct 0.0
              ]
              []
          , HH.div
              [ HP.class_ cl.sliderBackgroundUpper
              , HC.style do
                  C.flexShrink 1000
                  C.flexGrow $ getBackgroundUpperFlexGrow state
                  C.flexBasis $ C.pct 0.0
              ]
              []
          ]
      ]

  getLowerFraction :: State -> Number
  getLowerFraction state =
    (state.value - state.min) / (state.max - state.min)

  getUpperFraction :: State -> Number
  getUpperFraction state =
    1.0 - (getLowerFraction state)

  getBackgroundLowerFlexGrow :: State -> Int
  getBackgroundLowerFlexGrow state =
    -- TODO: flex-grow is typed as Int... need a better way to set this
    round (1000.0 * getLowerFraction state)

  getBackgroundUpperFlexGrow :: State -> Int
  getBackgroundUpperFlexGrow state =
    round (1000.0 * getUpperFraction state)

  eval :: Query ~> SliderDSL eff
  eval = case _ of
    InitializeComponent next -> do
      pure next
    FinalizeComponent next -> do
      pure next
    UpdateState state next -> do
      state <- H.get
      let oldValue = state.value
      H.put state
      if oldValue /= state.value
        then H.raise $ ValueChanged state.value
        else pure unit
      pure next
    OnValueInput value next -> do
      state <- H.get
      let oldValue = state.value
      case fromString value of
        Just value | oldValue /= value -> do
          H.modify (_ { value = value })
          H.raise $ ValueChanged value
        _ -> pure unit
      pure next
    SetValue value next -> do
      state <- H.get
      let oldValue = state.value
      if oldValue /= value
        then do
          H.modify (_ { value = value })
          H.raise $ ValueChanged value
        else
          pure unit
      pure next
    GetValue reply -> do
      state <- H.get
      pure $ reply state.value
