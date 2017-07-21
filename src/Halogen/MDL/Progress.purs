module Halogen.MDL.Progress where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.HTML.Types (HTMLElement)

import Halogen as H
import Halogen.HTML as HH

foreign import setProgress :: ∀ eff. HTMLElement -> Int -> Eff (dom :: DOM | eff) Unit
foreign import setBuffer :: ∀ eff. HTMLElement -> Int -> Eff (dom :: DOM | eff) Unit

setProgressByRef :: forall e s f g p o m. MonadEff (dom :: DOM | e) m => H.RefLabel -> Int -> H.HalogenM s f g p o m Unit
setProgressByRef ref value = do
  maybeElement <- H.getHTMLElementRef ref
  case maybeElement of
    Just element -> H.liftEff $ setProgress element value
    Nothing -> pure unit

setBufferByRef :: forall e s f g p o m. MonadEff (dom :: DOM | e) m => H.RefLabel -> Int -> H.HalogenM s f g p o m Unit
setBufferByRef ref value = do
  maybeElement <- H.getHTMLElementRef ref
  case maybeElement of
    Just element -> H.liftEff $ setBuffer element value
    Nothing -> pure unit

cl ::
  { progress :: HH.ClassName
  , jsProgress :: HH.ClassName
  , progressIndeterminate :: HH.ClassName
  }
cl =
  { progress   : HH.ClassName "mdl-progress"
  , jsProgress : HH.ClassName "mdl-js-progress"
  , progressIndeterminate : HH.ClassName "mdl-progress__indeterminate"
  }
