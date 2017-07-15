module Halogen.MDL where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (class MonadEff)
import Data.Maybe (Maybe(..))
import DOM (DOM())
import DOM.HTML.Types (HTMLElement())

import Halogen as H

-- Import the native MDL "upgradeElement" function
foreign import upgradeElement :: ∀ e. HTMLElement -> Eff (dom :: DOM | e) Unit

-- Helper function to call `upgradeElement` on an element found by RefLabel
upgradeElementByRef :: forall e s f g p o m. MonadEff ( dom :: DOM | e) m => H.RefLabel -> H.HalogenM s f g p o m Unit
upgradeElementByRef ref = do
  element <- H.getHTMLElementRef ref
  case element of
    Just element -> H.liftEff $ upgradeElement element
    Nothing -> pure unit

-- upgradeElementsByRef :: Array H.RefLabel -> H.HalogenM s f g p o m Unit
