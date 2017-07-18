module DemoButtons where

import Prelude

import Control.Monad.Aff (Aff, delay)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))

import CSS as C

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Halogen.MDL as MDL
import Halogen.MDL.Button as Button
import Halogen.MDL.Cell as Cell
import Halogen.MDL.Grid as Grid
import Halogen.MDL.RippleEffect as RippleEffect
import Halogen.MDL.Spinner as Spinner

type State =
  { clickDemo :: { clickCount :: Int }
  , nonComponentDemo :: { isLoading :: Boolean }
  }

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a
  | OnClickDemoButtonMessage Button.Message a
  | OnIgnoredButtonMessage Button.Message a
  | OnNonComponentButtonClick a

data Input = Initialize State

type Message = Void

data ChildSlot
  = ClickDemoSlot
  | ColoredFabSlot
  | ColoredFabRippleSlot
  | ColoredFabDisabledSlot
  | PlainFabSlot
  | PlainFabRippleSlot
  | PlainFabDisabledSlot
  | ColoredRaisedSlot
  | ColoredRaisedRippleSlot
  | ColoredRaisedDisabledSlot
  | ColoredAccentRaisedSlot
  | ColoredAccentRaisedRippleSlot
  | ColoredAccentRaisedDisabledSlot
  | PlainRaisedSlot
  | PlainRaisedRippleSlot
  | PlainRaisedDisabledSlot
  | ColoredFlatSlot
  | ColoredFlatRippleSlot
  | ColoredFlatDisabledSlot
  | ColoredAccentFlatSlot
  | ColoredAccentFlatRippleSlot
  | ColoredAccentFlatDisabledSlot
  | PlainFlatSlot
  | PlainFlatRippleSlot
  | PlainFlatDisabledSlot
  | ColoredIconSlot
  | ColoredIconRippleSlot
  | ColoredIconDisabledSlot
  | ColoredMiniFabSlot
  | ColoredMiniFabRippleSlot
  | ColoredMiniFabDisabledSlot
derive instance eqChildSlot :: Eq ChildSlot
derive instance ordChildSlot :: Ord ChildSlot

type DemoButtonsHTML eff = H.ParentHTML Query Button.Query ChildSlot (Aff (HA.HalogenEffects eff))
type DemoButtonsDSL eff = H.ParentDSL State Query Button.Query ChildSlot Message (Aff (HA.HalogenEffects eff))

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

  render :: State -> DemoButtonsHTML eff
  render state =
    HH.div
      [ HP.class_ Grid.cl.grid ]
      [ renderMainHeader

      , renderDemoHeader "Click demo"
      , renderDemoSection
          [ HH.slot
              ClickDemoSlot
              Button.button
              (Button.init { type: Button.Raised, color: Button.Colored, content: Button.Text "Click this", disabled: false, ripple: true })
              (HE.input OnClickDemoButtonMessage)
          , HH.p_ [ HH.text $ "Button has been clicked " <> show state.clickDemo.clickCount <> " times." ]
          ]

      , renderDemoHeader "Colored fab buttons"
      , renderDemoSection
          [ HH.slot
              ColoredFabSlot
              Button.button
              (Button.init { type: Button.Fab, color: Button.Colored, content: Button.IconText "add", disabled: false, ripple: false })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Normal" ]
          ]
      , renderDemoSection
          [ HH.slot
              ColoredFabRippleSlot
              Button.button
              (Button.init { type: Button.Fab, color: Button.Colored, content: Button.IconText "add", disabled: false, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Ripple" ]
          ]
      , renderDemoSection
          [ HH.slot
              ColoredFabDisabledSlot
              Button.button
              (Button.init { type: Button.Fab, color: Button.Colored, content: Button.IconText "add", disabled: true, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Disabled" ]
          ]

      , renderDemoHeader "Plain fab buttons"
      , renderDemoSection
          [ HH.slot
              PlainFabSlot
              Button.button
              (Button.init { type: Button.Fab, color: Button.Plain, content: Button.IconText "add", disabled: false, ripple: false })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Normal" ]
          ]
      , renderDemoSection
          [ HH.slot
              PlainFabRippleSlot
              Button.button
              (Button.init { type: Button.Fab, color: Button.Plain, content: Button.IconText "add", disabled: false, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Ripple" ]
          ]
      , renderDemoSection
          [ HH.slot
              PlainFabDisabledSlot
              Button.button
              (Button.init { type: Button.Fab, color: Button.Plain, content: Button.IconText "add", disabled: true, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Disabled" ]
          ]

      , renderDemoHeader "Colored raised buttons"
      , renderDemoSection
          [ HH.slot
              ColoredRaisedSlot
              Button.button
              (Button.init { type: Button.Raised, color: Button.Colored, content: Button.Text "Click me", disabled: false, ripple: false })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Normal" ]
          ]
      , renderDemoSection
          [ HH.slot
              ColoredRaisedRippleSlot
              Button.button
              (Button.init { type: Button.Raised, color: Button.Colored, content: Button.Text "Click me", disabled: false, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Ripple" ]
          ]
      , renderDemoSection
          [ HH.slot
              ColoredRaisedDisabledSlot
              Button.button
              (Button.init { type: Button.Raised, color: Button.Colored, content: Button.Text "Click me", disabled: true, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Disabled" ]
          ]

      , renderDemoHeader "Colored accent raised buttons"
      , renderDemoSection
          [ HH.slot
              ColoredAccentRaisedSlot
              Button.button
              (Button.init { type: Button.Raised, color: Button.Accent, content: Button.Text "Click me", disabled: false, ripple: false })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Normal" ]
          ]
      , renderDemoSection
          [ HH.slot
              ColoredAccentRaisedRippleSlot
              Button.button
              (Button.init { type: Button.Raised, color: Button.Accent, content: Button.Text "Click me", disabled: false, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Ripple" ]
          ]
      , renderDemoSection
          [ HH.slot
              ColoredAccentRaisedDisabledSlot
              Button.button
              (Button.init { type: Button.Raised, color: Button.Accent, content: Button.Text "Click me", disabled: true, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Disabled" ]
          ]

      , renderDemoHeader "Plain raised buttons"
      , renderDemoSection
          [ HH.slot
              PlainRaisedSlot
              Button.button
              (Button.init { type: Button.Raised, color: Button.Plain, content: Button.Text "Click me", disabled: false, ripple: false })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Normal" ]
          ]
      , renderDemoSection
          [ HH.slot
              PlainRaisedRippleSlot
              Button.button
              (Button.init { type: Button.Raised, color: Button.Plain, content: Button.Text "Click me", disabled: false, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Ripple" ]
          ]
      , renderDemoSection
          [ HH.slot
              PlainRaisedDisabledSlot
              Button.button
              (Button.init { type: Button.Raised, color: Button.Plain, content: Button.Text "Click me", disabled: true, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Disabled" ]
          ]

      , renderDemoHeader "Colored flat buttons"
      , renderDemoSection
          [ HH.slot
              ColoredFlatSlot
              Button.button
              (Button.init { type: Button.Flat, color: Button.Colored, content: Button.Text "Click me", disabled: false, ripple: false })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Normal" ]
          ]
      , renderDemoSection
          [ HH.slot
              ColoredFlatRippleSlot
              Button.button
              (Button.init { type: Button.Flat, color: Button.Colored, content: Button.Text "Click me", disabled: false, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Ripple" ]
          ]
      , renderDemoSection
          [ HH.slot
              ColoredFlatDisabledSlot
              Button.button
              (Button.init { type: Button.Flat, color: Button.Colored, content: Button.Text "Click me", disabled: true, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Disabled" ]
          ]

      , renderDemoHeader "Colored accent flat buttons"
      , renderDemoSection
          [ HH.slot
              ColoredAccentFlatSlot
              Button.button
              (Button.init { type: Button.Flat, color: Button.Accent, content: Button.Text "Click me", disabled: false, ripple: false })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Normal" ]
          ]
      , renderDemoSection
          [ HH.slot
              ColoredAccentFlatRippleSlot
              Button.button
              (Button.init { type: Button.Flat, color: Button.Accent, content: Button.Text "Click me", disabled: false, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Ripple" ]
          ]
      , renderDemoSection
          [ HH.slot
              ColoredAccentFlatDisabledSlot
              Button.button
              (Button.init { type: Button.Flat, color: Button.Accent, content: Button.Text "Click me", disabled: true, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Disabled" ]
          ]

      , renderDemoHeader "Plain flat buttons"
      , renderDemoSection
          [ HH.slot
              PlainFlatSlot
              Button.button
              (Button.init { type: Button.Flat, color: Button.Plain, content: Button.Text "Click me", disabled: false, ripple: false })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Normal" ]
          ]
      , renderDemoSection
          [ HH.slot
              PlainFlatRippleSlot
              Button.button
              (Button.init { type: Button.Flat, color: Button.Plain, content: Button.Text "Click me", disabled: false, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Ripple" ]
          ]
      , renderDemoSection
          [ HH.slot
              PlainFlatDisabledSlot
              Button.button
              (Button.init { type: Button.Flat, color: Button.Plain, content: Button.Text "Click me", disabled: true, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Disabled" ]
          ]

      , renderDemoHeader "Colored icon buttons"
      , renderDemoSection
          [ HH.slot
              ColoredIconSlot
              Button.button
              (Button.init { type: Button.Icon, color: Button.Colored, content: Button.IconText "mood", disabled: false, ripple: false })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Normal" ]
          ]
      , renderDemoSection
          [ HH.slot
              ColoredIconRippleSlot
              Button.button
              (Button.init { type: Button.Icon, color: Button.Colored, content: Button.IconText "mood", disabled: false, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Ripple" ]
          ]
      , renderDemoSection
          [ HH.slot
              ColoredIconDisabledSlot
              Button.button
              (Button.init { type: Button.Icon, color: Button.Colored, content: Button.IconText "mood", disabled: true, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Disabled" ]
          ]

      , renderDemoHeader "Colored mini-fab buttons"
      , renderDemoSection
          [ HH.slot
              ColoredMiniFabSlot
              Button.button
              (Button.init { type: Button.MiniFab, color: Button.Colored, content: Button.IconText "add", disabled: false, ripple: false })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Normal" ]
          ]
      , renderDemoSection
          [ HH.slot
              ColoredMiniFabRippleSlot
              Button.button
              (Button.init { type: Button.MiniFab, color: Button.Colored, content: Button.IconText "add", disabled: false, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Ripple" ]
          ]
      , renderDemoSection
          [ HH.slot
              ColoredMiniFabDisabledSlot
              Button.button
              (Button.init { type: Button.MiniFab, color: Button.Colored, content: Button.IconText "add", disabled: true, ripple: true })
              (HE.input OnIgnoredButtonMessage)
          , HH.p_ [ HH.text "Disabled" ]
          ]

      -- Demo of a custom button that does not use the Halogen.MDL.Button.button component
      , renderDemoHeader "DIY (non-component) button"
      , renderDemoSection $
          [ HH.button
              [ HP.classes [ Button.cl.button , Button.cl.jsButton , Button.cl.buttonRaised , Button.cl.buttonColored , RippleEffect.cl.jsRippleEffect ]
              , HP.type_ HP.ButtonButton
              , HP.ref $ H.RefLabel "non-component-button"
              , HE.onClick $ HE.input_ OnNonComponentButtonClick
              , HP.disabled state.nonComponentDemo.isLoading
              ]
              if state.nonComponentDemo.isLoading
              then
                [ HH.text "Loading" ]
              else
                [ HH.text "Click for async action" ]
          ]
          <>
          if state.nonComponentDemo.isLoading
          then
            [ HH.div
                [ HC.style do
                    C.display C.inlineBlock
                    C.position C.relative
                    C.left $ C.px 10.0
                    C.top $ C.px 8.0
                ]
                [ Spinner.el.spinner_ ]
            ]
          else []
      ]

  renderMainHeader :: ∀ p i. HH.HTML p i
  renderMainHeader = Cell.el.cell12Col_ [ HH.h1_ [ HH.text $ "Buttons" ] ]

  renderDemoHeader :: ∀ p i. String -> HH.HTML p i
  renderDemoHeader name = Cell.el.cell12Col_ [ HH.h3_ [ HH.text  name ] ]

  renderDemoSection :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
  renderDemoSection body =
    Cell.el.cell4Col_ body

  eval :: Query ~> DemoButtonsDSL eff
  eval = case _ of
    InitializeComponent next -> do
      MDL.upgradeElementByRef (H.RefLabel "non-component-button")
      pure next

    FinalizeComponent next -> pure next

    UpdateState state next -> do
      H.put state
      pure next

    OnClickDemoButtonMessage (Button.Clicked _) next -> do
      state <- H.get
      H.modify (\state -> state { clickDemo { clickCount = state.clickDemo.clickCount + 1 } })
      pure next

    OnIgnoredButtonMessage _ next ->
      pure next

    OnNonComponentButtonClick next -> do
      state <- H.get
      H.modify (\state -> state { nonComponentDemo { isLoading = true } })
      -- Make the spinner spin
      -- Another way to do this would be to always put the spinner in the HTML, activate it in InitializeComponent
      -- and just display: none it until it needs to be shown
      H.liftEff $ MDL.upgradeElementsByClassName Spinner.cl.jsSpinner
      H.liftAff $ delay $ Milliseconds 2000.0
      H.modify (\state -> state { nonComponentDemo { isLoading = false } })
      pure next
