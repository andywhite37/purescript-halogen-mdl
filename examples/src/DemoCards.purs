module DemoCards where

import Prelude

import CSS ((|>))
import CSS as C
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..), fromMaybe)
--import Unsafe.Partial (unsafePartial)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP

import Halogen.MDL as MDL
import Halogen.MDL.Button as Button
import Halogen.MDL.Card as Card
import Halogen.MDL.Cell as Cell
import Halogen.MDL.Grid as Grid
import Halogen.MDL.MaterialIcon as MI
import Halogen.MDL.RippleEffect as RE
import Halogen.MDL.Shadow as Shadow

type State = Unit

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a

data Input = Initialize State

type Message = Void

type DemoCardsHTML = H.ComponentHTML Query
type DemoCardsDSL eff = H.ComponentDSL State Query Message (Aff (HA.HalogenEffects eff))

init :: State -> Input
init state = Initialize state

demoCards :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
demoCards =
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

  render :: State -> DemoCardsHTML
  render state =
    Grid.el.grid_
      [ renderCardsHeader
      , renderDemoHeader "Welcome card"
      , renderDemoSection renderWelcomeCard
      , renderDemoHeader "Square card"
      , renderDemoSection renderSquareCard
      ]

  renderCardsHeader :: ∀ p i. HH.HTML p i
  renderCardsHeader = Cell.el.cell12Col_ [ HH.h1_ [ HH.text "Cards" ] ]

  renderDemoHeader :: ∀ p i. String -> HH.HTML p i
  renderDemoHeader name = Cell.el.cell12Col_ [ HH.h3_ [ HH.text name ] ]

  renderDemoSection :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
  renderDemoSection body = Cell.el.cell12Col_ body

  renderWelcomeCard :: ∀ p i. Array (HH.HTML p i)
  renderWelcomeCard =
    [ HC.stylesheet do
        C.select (C.element ".demo-card-wide.mdl-card") do
          C.width $ C.px 512.0
        C.select (C.element ".demo-card-wide" |> C.element ".mdl-card__title") do
          C.color C.white
          C.height $ C.px 176.0
          C.background $ C.url "https://getmdl.io/assets/demos/welcome_card.jpg"
          C.backgroundSize C.cover
          C.backgroundPosition $ C.placed C.sideCenter C.sideCenter
        C.select (C.element ".demo-card-wide" |> C.element ".mdl-card__menu") do
          C.color C.white
    , HH.div
        [ HP.classes [ HH.ClassName "demo-card-wide" , Card.cl.card, Shadow.cl.shadow2dp ] ]
        [ HH.div
            [ HP.class_ Card.cl.cardTitle ]
            [ HH.h2
                [ HP.class_ Card.cl.cardTitleText ]
                [ HH.text "Welcome" ]
            ]
        , HH.div
            [ HP.class_ Card.cl.cardSupportingText ]
            [ HH.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris sagittis pellentesque lacus eleifend lacinia..." ]
        , HH.div
            [ HP.classes [ Card.cl.cardActions, Card.cl.cardBorder ] ]
            [ HH.a
                [ HP.classes [ Button.cl.button, Button.cl.buttonColored, Button.cl.jsButton, RE.cl.jsRippleEffect ] ]
                [ HH.text "Get started" ]
            ]
        , HH.div
            [ HP.classes [ Card.cl.cardMenu ] ]
            [ HH.div
                [ HP.classes [ Button.cl.button, Button.cl.buttonIcon, Button.cl.jsButton, RE.cl.jsRippleEffect ] ]
                [ HH.i
                    [ HP.class_ MI.cl.materialIcons ]
                    [ HH.text MI.str.share ]
                ]
            ]
        ]
    ]

  renderSquareCard :: ∀ p i. Array (HH.HTML p i)
  renderSquareCard =
    [ HC.stylesheet do
        C.select (C.element ".demo-card-square.mdl-card") do
          C.width $ C.px 320.0
          C.height $ C.px 320.0
        C.select (C.element ".demo-card-square" |> C.element ".mdl-card__title") do
          C.color C.white
          C.height $ C.px 176.0
          C.background $ C.url "https://getmdl.io/assets/demos/dog.png"
          C.backgroundColor $ fromMaybe C.white (C.fromHexString "#46B6AC")
          C.backgroundRepeat C.noRepeat
          C.backgroundPosition $ C.placed C.sideRight C.sideBottom
    , HH.div
        [ HP.classes [ HH.ClassName "demo-card-square" , Card.cl.card, Shadow.cl.shadow2dp ] ]
        [ HH.div
            [ HP.classes [ Card.cl.cardTitle, Card.cl.cardExpand ] ]
            [ HH.h2
                [ HP.class_ Card.cl.cardTitleText ]
                [ HH.text "Update" ]
            ]
        , HH.div
            [ HP.classes [ Card.cl.cardSupportingText ] ]
            [ HH.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris sagittis pellentesque lacus eleifend lacinia..." ]
        , HH.div
            [ HP.classes [ Card.cl.cardActions, Card.cl.cardBorder ] ]
            [ HH.a
                [ HP.classes [ Button.cl.button, Button.cl.buttonColored, Button.cl.jsButton, RE.cl.jsRippleEffect ] ]
                [ HH.text "View updates" ]
            ]
        ]
    ]

  eval :: Query ~> DemoCardsDSL eff
  eval = case _ of
    InitializeComponent next -> do
      H.liftEff $ MDL.upgradeElementsByClassName Button.cl.jsButton
      pure next
    FinalizeComponent next -> pure next
    UpdateState state next -> do
      H.put state
      pure next
