module DemoMenus where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Halogen.MDL as MDL
import Halogen.MDL.Button as Button
import Halogen.MDL.Cell as Cell
import Halogen.MDL.Grid as Grid
import Halogen.MDL.MaterialIcon as MI
import Halogen.MDL.Menu as Menu
import Halogen.MDL.RippleEffect as RE

type State = Unit

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a

data Input
  = Initialize State

type Message = Void

type DemoMenusHTML = H.ComponentHTML Query
type DemoMenusDSL eff = H.ComponentDSL State Query Message (Aff (HA.HalogenEffects eff))

init :: State -> Input
init = Initialize

demoMenus :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
demoMenus = H.lifecycleComponent
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

  render :: State -> DemoMenusHTML
  render state =
    Grid.el.grid_
      [ Cell.el.cell12Col_
          [ HH.h3_ [ HH.text "Menus" ] ]
      , Cell.el.cell3Col_
          [ HH.button
              [ HP.id_ "demo-menu-lower-left"
              , HP.classes [ Button.cl.button, Button.cl.jsButton, Button.cl.buttonIcon ]
              ]
              [ HH.i [ HP.class_ MI.cl.materialIcons ] [ HH.text MI.str.moreVert ] ]
          , HH.ul
              [ HP.classes [ Menu.cl.menu, Menu.cl.menuBottomLeft, Menu.cl.jsMenu, RE.cl.jsRippleEffect ]
              --, HP.for "demo-menu-lower-left" -- not allowed on ul element!
              --, HP.prop (H.PropName "for") "demo-menu-lower-left"
              , HP.attr MDL.attr.for "demo-menu-lower-left"
              ]
              [ HH.li
                  [ HP.class_ Menu.cl.menuItem ]
                  [ HH.text "Some action" ]
              , HH.li
                  [ HP.classes [ Menu.cl.menuItem, Menu.cl.menuItemFullBleedDivider ] ]
                  [ HH.text "Another action" ]
              , HH.li
                  [ HP.class_ Menu.cl.menuItem
                  --, HP.disabled true
                  --, HP.prop (H.PropName "disabled") true
                  , HP.attr MDL.attr.disabled "true"
                  ]
                  [ HH.text "Some action" ]
              , HH.li
                  [ HP.classes [ Menu.cl.menuItem ] ]
                  [ HH.text "Yet another action" ]
              ]
          ]
      , Cell.el.cell12Col_
          [
          ]
      ]

  eval :: Query ~> DemoMenusDSL eff
  eval = case _ of
    InitializeComponent next -> do
      H.liftEff $ MDL.upgradeElementsByClassNames [ Menu.cl.jsMenu, Button.cl.jsButton ]
      pure next
    FinalizeComponent next -> do
      pure next
    UpdateState state next -> do
      H.put state
      pure next
