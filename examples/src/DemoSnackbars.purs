module DemoSnackbars where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Halogen.MDL as MDL
import Halogen.MDL.Button as Button
import Halogen.MDL.Cell as Cell
import Halogen.MDL.Grid as Grid
import Halogen.MDL.RippleEffect as RE
import Halogen.MDL.Snackbar as Snackbar

type State = { snackbar2ActionCount :: Int }

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a
  | OnButton1Click a
  | OnButton2Click a
  | OnSnackbar2Action a

data Input
  = Initialize State

type Message = Void

type DemoSnackbarsHTML = H.ComponentHTML Query
type DemoSnackbarsDSL eff = H.ComponentDSL State Query Message (Aff (HA.HalogenEffects eff))

init :: State -> Input
init = Initialize

demoSnackbars :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
demoSnackbars = H.lifecycleComponent
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

  snackbar1Ref :: H.RefLabel
  snackbar1Ref = H.RefLabel "snackbar-1-ref"

  snackbar2Ref :: H.RefLabel
  snackbar2Ref = H.RefLabel "snackbar-2-ref"

  render :: State -> DemoSnackbarsHTML
  render state =
    Grid.el.grid_
      [ Cell.el.cell12Col_
          [ HH.h3_ [ HH.text "Snackbars" ] ]
      , Cell.el.cell12Col_
          [ HH.button
              [ HP.classes [ Button.cl.button, Button.cl.jsButton, Button.cl.buttonRaised, RE.cl.jsRippleEffect ]
              , HP.type_ HP.ButtonButton
              , HE.onClick $ HE.input_ OnButton1Click
              ]
              [ HH.text "Show snackbar no action" ]
          , HH.div
              [ HP.classes [ Snackbar.cl.snackbar, Snackbar.cl.jsSnackbar ]
              , HP.ref snackbar1Ref
              ]
              [ HH.div
                  [ HP.class_ Snackbar.cl.snackbarText ]
                  [ HH.text "" ]
              , HH.button
                  [ HP.class_ Snackbar.cl.snackbarAction ]
                  [ HH.text "" ]
              ]
          ]
      , Cell.el.cell12Col_
          [ HH.button
              [ HP.classes [ Button.cl.button, Button.cl.jsButton, Button.cl.buttonRaised, RE.cl.jsRippleEffect ]
              , HP.type_ HP.ButtonButton
              , HE.onClick $ HE.input_ OnButton2Click
              ]
              [ HH.text "Show snackbar with action" ]
          , HH.div_
              [ HH.text $ "Action has been clicked " <> show state.snackbar2ActionCount <> " time(s)" ]
          , HH.div
              [ HP.classes [ Snackbar.cl.snackbar, Snackbar.cl.jsSnackbar ]
              , HP.ref snackbar2Ref
              ]
              [ HH.div
                  [ HP.class_ Snackbar.cl.snackbarText ]
                  [ HH.text "" ]
              , HH.button
                  [ HP.class_ Snackbar.cl.snackbarAction
                  , HE.onClick $ HE.input_ OnSnackbar2Action
                  ]
                  [ HH.text "" ]
              ]
          ]
      ]

  eval :: Query ~> DemoSnackbarsDSL eff
  eval = case _ of
    InitializeComponent next -> do
      H.liftEff $ MDL.upgradeElementsByClassNames [Snackbar.cl.jsSnackbar, Button.cl.jsButton]
      pure next
    FinalizeComponent next -> do
      pure next
    UpdateState state next -> do
      H.put state
      pure next
    OnButton1Click next -> do
      Snackbar.showSnackbarNoActionByRef snackbar1Ref
        { message: "Snackbar 1 (no action)"
        , timeout: 2750
        }
      pure next
    OnButton2Click next -> do
      Snackbar.showSnackbarWithActionByRef snackbar2Ref
        { message: "Snackbar 2 (with action)"
        , timeout: 2750
        -- TODO: not sure how to deal with the action handler via Halogen
        , actionHandler: \_ -> pure unit
        , actionText: "Action"
        }
      pure next
    OnSnackbar2Action next -> do
      --Snackbar.hideSnackbarByRef snackbar2Ref
      H.modify (\state -> state { snackbar2ActionCount = state.snackbar2ActionCount + 1 })
      pure next
