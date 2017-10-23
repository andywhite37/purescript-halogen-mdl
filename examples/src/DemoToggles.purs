module DemoToggles where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))

import CSS as C
import DOM.Event.Types (MouseEvent)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Halogen.MDL as MDL
import Halogen.MDL.Cell as Cell
import Halogen.MDL.Grid as Grid
import Halogen.MDL.Checkbox as Checkbox
import Halogen.MDL.Radio as Radio

type State =
  { checkbox1Checked :: Boolean
  , checkbox2Checked :: Boolean
  , radio1Checked :: Boolean
  , radio2Checked :: Boolean
  , radio3Checked :: Boolean
  }

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a
  | OnCheckbox1Click MouseEvent a
  | OnCheckbox2Click MouseEvent a
  | OnRadio1Click MouseEvent a
  | OnRadio2Click MouseEvent a
  | OnRadio3Click MouseEvent a

data Input
  = Initialize State

type Message = Void

type DemoTogglesHTML = H.ComponentHTML Query
type DemoTogglesDSL eff = H.ComponentDSL State Query Message (Aff (HA.HalogenEffects eff))

init :: Input
init = Initialize
  { checkbox1Checked: false
  , checkbox2Checked: true
  , radio1Checked: false
  , radio2Checked: true
  , radio3Checked: false
  }

demoToggles :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
demoToggles = H.lifecycleComponent
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

  render :: State -> DemoTogglesHTML
  render state =
    Grid.el.grid_
      [ Cell.el.cell12Col_
          [ HH.h3_ [ HH.text "Toggles" ] ]
      , Cell.el.cell12Col_
          [ HH.h4_ [ HH.text "Checkboxes" ]
          , Checkbox.bl.checkbox
              { id: "my-checkbox-1"
              , label: "Checkbox"
              , ripple: true
              , checked: state.checkbox1Checked
              , onClick: HE.input OnCheckbox1Click
              }
          , HH.p_ [ HH.text ("Checkbox is: " <> if state.checkbox1Checked then "checked" else "not checked") ]
          , Checkbox.bl.checkbox
              { id: "my-checkbox-2"
              , label: "Checkbox (no ripple)"
              , ripple: false
              , checked: state.checkbox2Checked
              , onClick: HE.input OnCheckbox2Click
              }
          , HH.p_ [ HH.text ("Checkbox is: " <> if state.checkbox2Checked then "checked" else "not checked") ]
          ]
      , Cell.el.cell12Col_
          [ HH.h4_ [ HH.text "Radios" ]
          , Radio.bl.radioGroup
              { container: HH.div_
              , idPrefix: "my-radio"
              , name: "radio-1"
              , ripple: true
              , buttonCSS: C.marginRight $ C.px 20.0
              , buttons:
                  [ { value: "one"
                    , label: "One"
                    , checked: true
                    , onClick: HE.input OnRadio1Click
                    }
                  , { value: "two"
                    , label: "Two"
                    , checked: false
                    , onClick: HE.input OnRadio2Click
                    }
                  ]
              }
          ]
      ]

  eval :: Query ~> DemoTogglesDSL eff
  eval = case _ of
    InitializeComponent next -> do
      H.liftEff $ MDL.upgradeElementsByClassNames [ Checkbox.cl.jsCheckbox, Radio.cl.jsRadio ]
      pure next
    FinalizeComponent next -> do
      pure next
    UpdateState state next -> do
      H.put state
      pure next
    OnCheckbox1Click event next -> do
      H.modify (\state -> state { checkbox1Checked = not state.checkbox1Checked })
      pure next
    OnCheckbox2Click event next -> do
      H.modify (\state -> state { checkbox2Checked = not state.checkbox2Checked })
      pure next
    OnRadio1Click event next -> do
      H.modify (\state -> state { radio1Checked = not state.radio1Checked })
      pure next
    OnRadio2Click event next -> do
      H.modify (\state -> state { radio2Checked = not state.radio2Checked })
      pure next
    OnRadio3Click event next -> do
      H.modify (\state -> state { radio3Checked = not state.radio3Checked })
      pure next
