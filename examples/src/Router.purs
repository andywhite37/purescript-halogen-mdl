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

import Route (Route(..))
import Container as Container

-- Routing logic

routeSignal :: ∀ eff. H.HalogenIO Query Void (Aff (HA.HalogenEffects eff)) -> Aff (HA.HalogenEffects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff matchRoute
  goToRoute driver old new

matchRoute :: Match Route
matchRoute
  =   badges
  <|> buttons
  <|> home
  where
    badges = Badges <$ route "badges"
    buttons = Buttons <$ route "buttons"
    home = Home <$ lit ""
    route str = lit "" *> lit str

goToRoute :: ∀ eff
  . H.HalogenIO Query Message (Aff (HA.HalogenEffects eff))
  -> Maybe Route
  -> Route
  -> Aff (HA.HalogenEffects eff) Unit
goToRoute driver _ =
  driver.query <<< H.action <<< GoTo

-- Halogen router component

type State =
  { currentRoute :: Route
  }

data Query a
  = OnContainerMessage Container.Message a
  | GoTo Route a

type Input = Unit

type Message = Void

data Slot = ContainerSlot
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

    render :: State -> H.ParentHTML Query Container.Query Slot (Aff (HA.HalogenEffects eff))
    render state =
      HH.div
        [ HP.class_ $ HH.ClassName "root" ]
        [ HH.slot
            ContainerSlot
            Container.container
            (Container.init { currentRoute: state.currentRoute, clickCount: 0 })
            (HE.input OnContainerMessage)
        ]

    eval :: Query ~> H.ParentDSL State Query Container.Query Slot Message (Aff (HA.HalogenEffects eff))
    eval = case _ of
      GoTo route next -> do
        H.modify (_ { currentRoute = route })
        pure next

      OnContainerMessage message next -> do
        pure next
