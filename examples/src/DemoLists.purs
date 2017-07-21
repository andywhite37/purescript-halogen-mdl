module DemoLists where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Halogen.MDL as MDL
import Halogen.MDL.Cell as Cell
import Halogen.MDL.Grid as Grid
import Halogen.MDL.List as List
import Halogen.MDL.MaterialIcon as MI
import Halogen.MDL.RippleEffect as RE

type State = Unit

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a

data Input = Initialize State

type Message = Void

type DemoListsHTML = H.ComponentHTML Query
type DemoListsDSL eff = H.ComponentDSL State Query Message (Aff (HA.HalogenEffects eff))

init :: State -> Input
init state = Initialize state

demoLists :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
demoLists =
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

  render :: State -> DemoListsHTML
  render state =
    Grid.el.grid_
      [ renderListsHeader
      , renderDemoHeader "List example"
      , renderDemoSection renderListExample
      ]

  renderListsHeader :: ∀ p i. HH.HTML p i
  renderListsHeader = Cell.el.cell12Col_ [ HH.h1_ [ HH.text "Lists" ] ]

  renderDemoHeader :: ∀ p i. String -> HH.HTML p i
  renderDemoHeader name = Cell.el.cell12Col_ [ HH.h3_ [ HH.text name ] ]

  renderDemoSection :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
  renderDemoSection body = Cell.el.cell4Col_ body

  renderListExample :: Array (DemoListsHTML)
  renderListExample =
    [ HH.ul
        [ HP.class_ List.cl.list ]
        [ HH.li
            [ HP.classes [ List.cl.listItem, List.cl.listItemTwoLine ] ]
            [ HH.span
                [ HP.class_ List.cl.listItemPrimaryContent ]
                [ HH.i [ HP.classes [ MI.cl.materialIcons, List.cl.listItemAvatar ] ] [ HH.text MI.str.person ]
                , HH.span_ [ HH.text "Bryan Cranston" ]
                , HH.span [ HP.class_ List.cl.listItemSubTitle ] [ HH.text "62 Episodes" ]
                ]
            , HH.span
                [ HP.class_ List.cl.listItemSecondaryContent ]
                [ HH.span [ HP.class_ List.cl.listItemSecondaryInfo ] [ HH.text "Actor" ]
                , HH.span
                    [ HP.class_ List.cl.listItemSecondaryAction ]
                    [ HH.a
                        [ HP.class_ List.cl.listItemSecondaryAction ]
                        [ HH.i [ HP.class_ MI.cl.materialIcons ] [ HH.text MI.str.star ] ]
                    ]
                ]
            ]
        , HH.li
            [ HP.classes [ List.cl.listItem, List.cl.listItemTwoLine ] ]
            [ HH.span
                [ HP.class_ List.cl.listItemPrimaryContent ]
                [ HH.i [ HP.classes [ MI.cl.materialIcons, List.cl.listItemAvatar ] ] [ HH.text MI.str.person ]
                , HH.span_ [ HH.text "Aaron Paul" ]
                , HH.span [ HP.class_ List.cl.listItemSubTitle ] [ HH.text "62 Episodes" ]
                ]
            , HH.span
                [ HP.class_ List.cl.listItemSecondaryContent ]
                [ HH.span [ HP.class_ List.cl.listItemSecondaryInfo ] [ HH.text "Actor" ]
                , HH.span
                    [ HP.class_ List.cl.listItemSecondaryAction ]
                    [ HH.a
                        [ HP.class_ List.cl.listItemSecondaryAction ]
                        [ HH.i [ HP.class_ MI.cl.materialIcons ] [ HH.text MI.str.star ] ]
                    ]
                ]
            ]
        ]

    ]

  eval :: Query ~> DemoListsDSL eff
  eval = case _ of
    InitializeComponent next -> do
      pure next
    FinalizeComponent next -> do
      pure next
    UpdateState state next -> do
      H.put state
      pure next
