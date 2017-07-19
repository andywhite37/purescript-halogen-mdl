module DemoDialogs where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.MDL.Button as Button
import Halogen.MDL.Cell as Cell
--import Halogen.MDL.Chip as Chip
--import Halogen.MDL.Color as Color
--import Halogen.MDL.Dialog (registerDialog, showDialogByRef)
import Halogen.MDL.Dialog as Dialog
import Halogen.MDL.Grid as Grid
--import Halogen.MDL.MaterialIcon as MI
--import Halogen.MDL.RippleEffect as RE
--import Halogen.MDL.Shadow as Shadow

type State = Unit

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a
  | ShowDialogDemo1 a
  | CloseDialogDemo1 a

data Input = Initialize State

type Message = Void

type DemoDialogsHTML = H.ComponentHTML Query
type DemoDialogsDSL eff = H.ComponentDSL State Query Message (Aff (HA.HalogenEffects eff))

init :: State -> Input
init state = Initialize state

demoDialogs :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
demoDialogs =
  H.lifecycleComponent
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

  render :: State -> DemoDialogsHTML
  render state =
    Grid.el.grid_
      [ renderDialogsHeader
      , renderDemoHeader "Simple dialog"
      , renderDemoSection renderDialogDemo1
      ]

  renderDialogsHeader :: ∀ p i. HH.HTML p i
  renderDialogsHeader = Cell.el.cell12Col_ [ HH.h1_ [ HH.text "Dialogs" ] ]

  renderDemoHeader :: ∀ p i. String -> HH.HTML p i
  renderDemoHeader name = Cell.el.cell12Col_ [ HH.h3_ [ HH.text name ] ]

  renderDemoSection :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
  renderDemoSection body = Cell.el.cell12Col_ body

  dialogDemo1Ref :: H.RefLabel
  dialogDemo1Ref = H.RefLabel "dialog-demo-1"

  renderDialogDemo1 :: ∀ p. Array (DemoDialogsHTML)
  renderDialogDemo1 =
    [ HH.button
        [ HP.id_ "show-dialog"
        , HP.type_ HP.ButtonButton
        , HP.classes [ Button.cl.button, Button.cl.buttonRaised, Button.cl.buttonColored ]
        , HE.onClick $ HE.input_ ShowDialogDemo1
        ]
        [ HH.text "Show dialog" ]
    , HH.dialog
        [ HP.class_ Dialog.cl.dialog
        , HP.ref dialogDemo1Ref
        ]
        [ HH.h4
            [ HP.class_ Dialog.cl.dialogTitle ]
            [ HH.text "Allow data collection?" ]
        , HH.div
            [ HP.class_ Dialog.cl.dialogContent ]
            [ HH.p_
                [ HH.text "Allowing us to collect data will let us get you the information you want faster." ]
            ]
        , HH.div
            [ HP.class_ Dialog.cl.dialogActions ]
            [ HH.button
                [ HP.type_ HP.ButtonButton
                , HP.class_ Button.cl.button
                ]
                [ HH.text "Agree" ]
            , HH.button
                [ HP.type_ HP.ButtonButton
                , HP.class_ Button.cl.button
                , HE.onClick $ HE.input_ CloseDialogDemo1
                ]
                [ HH.text "Disagree" ]
            ]
        ]
    ]

  eval :: Query ~> DemoDialogsDSL eff
  eval = case _ of
    InitializeComponent next -> do
      Dialog.registerDialogByRef dialogDemo1Ref
      pure next
    FinalizeComponent next -> do
      pure next
    UpdateState state next -> do
      H.put state
      pure next
    ShowDialogDemo1 next -> do
      Dialog.showDialogByRef dialogDemo1Ref
      pure next
    CloseDialogDemo1 next -> do
      Dialog.closeByRef dialogDemo1Ref
      pure next
