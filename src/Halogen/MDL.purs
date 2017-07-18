module Halogen.MDL where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (class MonadEff)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import DOM (DOM())
import DOM.HTML.Types (HTMLElement())

import Halogen as H
import Halogen.HTML as HH

-- Upgrade element to add MDL behaviors
foreign import upgradeElement :: ∀ e. HTMLElement -> Eff (dom :: DOM | e) Unit

-- Upgrade elements by CSS selector (via querySelector)
foreign import upgradeElementsBySelector :: ∀ e. String -> Eff (dom :: DOM | e) Unit

-- Upgrade elements by CSS selectors (via querySelector)
foreign import upgradeElementsBySelectors :: ∀ e. Array String -> Eff (dom :: DOM | e) Unit

-- Upgrade elements by class
upgradeElementsByClassName :: ∀ e. HH.ClassName -> Eff (dom :: DOM | e) Unit
upgradeElementsByClassName (HH.ClassName className) = upgradeElementsBySelector $ "." <> className

-- Upgrade elements by classes
upgradeElementsByClassNames :: ∀ e. Array (HH.ClassName) -> Eff (dom :: DOM | e) Unit
upgradeElementsByClassNames classNames = void $ traverse upgradeElementsByClassName classNames

-- Upgrade element by Halogen.RefLabel
upgradeElementByRef :: forall e s f g p o m. MonadEff (dom :: DOM | e) m => H.RefLabel -> H.HalogenM s f g p o m Unit
upgradeElementByRef ref = do
  element <- H.getHTMLElementRef ref
  case element of
    Just element -> H.liftEff $ upgradeElement element
    Nothing -> pure unit

-- Upgrade elements by Halogen.RefLabels
upgradeElementsByRefs :: ∀ e s f g p o m. MonadEff (dom :: DOM | e) m => Array H.RefLabel -> H.HalogenM s f g p o m Unit
upgradeElementsByRefs refs = void $ traverse upgradeElementByRef refs

-- Remove class from an element
-- Hacky solution to removing classes added outside of our rendering control
foreign import removeClass :: ∀ e. HTMLElement -> String -> Eff (dom :: DOM | e) Unit
