module Halogen.MDL where

import Prelude
import Control.Monad.Eff (Eff())
--import Data.Maybe (Maybe(..))
import DOM (DOM())
import DOM.HTML.Types (HTMLElement())

--import Halogen as H

foreign import upgradeElement :: ∀ e. HTMLElement -> Eff (dom :: DOM | e) Unit

{-
upgradeElementByRef :: ∀ e. H.RefLabel -> Eff (dom :: DOM | e) Unit
upgradeElementByRef ref = do
  element <- H.getHTMLElementRef ref
  case element of
    Just element -> do
      H.liftEff $ upgradeElement element
    Nothing -> pure unit
  pure unit
-}
