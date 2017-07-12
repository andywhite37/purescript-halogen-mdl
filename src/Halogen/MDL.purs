module Halogen.MDL where

import Prelude (Unit)
import Control.Monad.Eff (Eff())
import DOM (DOM())
import DOM.HTML.Types (HTMLElement())

foreign import upgradeElement :: ∀ e. HTMLElement -> Eff (dom :: DOM | e) Unit
