module Router where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (lit)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Route (Route(..), urlSegment)
import DemoContainer as DemoContainer

-- Routing logic

routeSignal :: ∀ eff. H.HalogenIO Query Void (Aff (HA.HalogenEffects eff)) -> Aff (HA.HalogenEffects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff matchRoute
  goToRoute driver old new

matchRoute :: Match Route
matchRoute
  =   badges
  <|> buttons
  <|> cards
  <|> chips
  <|> dialogs
  <|> lists
  <|> tabs
  <|> home
  where
    badges = Badges <$ route (urlSegment Badges)
    buttons = Buttons <$ route (urlSegment Buttons)
    cards = Cards <$ route (urlSegment Cards)
    chips = Chips <$ route (urlSegment Chips)
    dialogs = Dialogs <$ route (urlSegment Dialogs)
    lists = Lists <$ route (urlSegment Lists)
    tabs = Tabs <$ route (urlSegment Tabs)
    home = Home <$ lit ""
    route str = lit "" *> lit str

goToRoute :: ∀ eff
  . H.HalogenIO Query Message (Aff (HA.HalogenEffects eff))
  -> Maybe Route
  -> Route
  -> Aff (HA.HalogenEffects eff) Unit
goToRoute driver _ =
  driver.query <<< H.action <<< GoTo

-- Router component

type State =
  { currentRoute :: Route
  }

data Query a
  = OnDemoContainerMessage DemoContainer.Message a
  | GoTo Route a

type Input = Unit

type Message = Void

data Slot = DemoContainerSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
component = H.parentComponent
  { initialState: initialState
  , receiver: receiver
  , render
  , eval
  }
  where
    initialState :: Input -> State
    initialState _ = { currentRoute: Home }

    receiver :: Input -> Maybe (Query Unit)
    receiver _ = Nothing

    render :: State -> H.ParentHTML Query DemoContainer.Query Slot (Aff (HA.HalogenEffects eff))
    render state =
      HH.div
        [ HP.class_ $ HH.ClassName "root" ]
        [ HH.slot
            DemoContainerSlot
            DemoContainer.demoContainer
            (DemoContainer.init { currentRoute: state.currentRoute })
            (HE.input OnDemoContainerMessage)
        ]

    eval :: Query ~> H.ParentDSL State Query DemoContainer.Query Slot Message (Aff (HA.HalogenEffects eff))
    eval = case _ of
      GoTo route next -> do
        H.modify (_ { currentRoute = route })
        pure next

      OnDemoContainerMessage message next -> do
        pure next
