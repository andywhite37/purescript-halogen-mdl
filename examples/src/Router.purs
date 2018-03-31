module Router where
---
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import Data.String as Str
import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Event.EventTypes as ET
import DOM.HTML.Event.HashChangeEvent as HCE
import DOM.HTML.Event.Types (HashChangeEvent, readHashChangeEvent) as DOM
import DOM.HTML.Types (windowToEventTarget) as DOM




---
import Prelude
import Control.Alt ((<|>))


import Routing (match)
import Routing.Match (Match)
import Routing.Match.Class (lit)



import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Route (Route(..), urlSegment)
import DemoContainer as DemoContainer

-- A producer coroutine that emits messages whenever the window emits a
-- `hashchange` event.
hashChangeProducer
  :: forall eff
   . CR.Producer DOM.HashChangeEvent (Aff (avar :: AVAR, dom :: DOM | eff)) Unit
hashChangeProducer = CRA.produce \emit ->
  let
    emitter e =
      case runExcept (DOM.readHashChangeEvent (toForeign e)) of
        Left _ -> pure unit
        Right hce -> emit (Left hce)
  in
    liftEff $
      DOM.window
        >>= DOM.windowToEventTarget
        >>> DOM.addEventListener ET.hashchange (DOM.eventListener emitter) false

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ChangeRoute` queries in when it receives inputs from the
-- producer.
hashChangeConsumer
  :: forall eff
   . (Query ~> Aff (HA.HalogenEffects eff))
  -> CR.Consumer DOM.HashChangeEvent (Aff (HA.HalogenEffects eff)) Unit
hashChangeConsumer query = CR.consumer \event -> do
  let
    hash = Str.drop 1 $ Str.dropWhile (_ /= '#') $ HCE.newURL event
  query $ H.action $ GoTo $ matchRouteFromHash hash
  pure Nothing


matchRouteFromHash :: String -> Route
matchRouteFromHash hash =
  case (match matchRoute hash) of
    Left  _ -> Home
    Right route -> route


matchRoute :: Match Route
matchRoute
  =   badges
  <|> buttons
  <|> cards
  <|> chips
  <|> dialogs
  <|> lists
  <|> menus
  <|> progress
  <|> sliders
  <|> snackbars
  <|> spinners
  <|> tabs
  <|> toggles
  <|> home
  where
    badges = Badges <$ route (urlSegment Badges)
    buttons = Buttons <$ route (urlSegment Buttons)
    cards = Cards <$ route (urlSegment Cards)
    chips = Chips <$ route (urlSegment Chips)
    dialogs = Dialogs <$ route (urlSegment Dialogs)
    lists = Lists <$ route (urlSegment Lists)
    menus = Menus <$ route (urlSegment Menus)
    progress = Progress <$ route (urlSegment Progress)
    sliders = Sliders <$ route (urlSegment Sliders)
    snackbars = Snackbars <$ route (urlSegment Snackbars)
    spinners = Spinners <$ route (urlSegment Spinners)
    tabs = Tabs <$ route (urlSegment Tabs)
    toggles = Toggles <$ route (urlSegment Toggles)
    home = Home <$ lit ""
    route str = lit "" *> lit str


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
