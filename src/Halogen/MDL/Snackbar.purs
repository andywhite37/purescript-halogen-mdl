module Halogen.MDL.Snackbar where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.HTML.Types (HTMLElement)

import Halogen as H
import Halogen.HTML as HH

type SnackbarDataNoAction =
  { message :: String
  , timeout :: Int
  }

type SnackbarDataWithAction =
  { message :: String
  , timeout :: Int
  , actionHandler :: ∀ eff. Unit -> Eff ( dom :: DOM | eff ) Unit
  , actionText :: String
  }

foreign import showSnackbarNoAction :: ∀ eff. HTMLElement -> SnackbarDataNoAction -> Eff ( dom :: DOM | eff) Unit
foreign import showSnackbarWithAction :: ∀ eff. HTMLElement -> SnackbarDataWithAction -> Eff ( dom :: DOM | eff) Unit

showSnackbarNoActionByRef :: forall eff s f g p o m. MonadEff (dom :: DOM | eff) m => H.RefLabel -> SnackbarDataNoAction -> H.HalogenM s f g p o m Unit
showSnackbarNoActionByRef ref data_ = do
  maybeElement <- H.getHTMLElementRef ref
  case maybeElement of
    Just element -> H.liftEff $ showSnackbarNoAction element data_
    Nothing -> pure unit

showSnackbarWithActionByRef :: forall eff s f g p o m. MonadEff (dom :: DOM | eff) m => H.RefLabel -> SnackbarDataWithAction -> H.HalogenM s f g p o m Unit
showSnackbarWithActionByRef ref data_ = do
  maybeElement <- H.getHTMLElementRef ref
  case maybeElement of
    Just element -> H.liftEff $ showSnackbarWithAction element data_
    Nothing -> pure unit

foreign import hideSnackbar :: ∀ eff. HTMLElement -> Eff ( dom :: DOM | eff ) Unit

hideSnackbarByRef :: forall eff s f g p o m. MonadEff (dom :: DOM | eff) m => H.RefLabel -> H.HalogenM s f g p o m Unit
hideSnackbarByRef ref = do
  maybeElement <- H.getHTMLElementRef ref
  case maybeElement of
    Just element -> H.liftEff $ hideSnackbar element
    Nothing -> pure unit

cl ::
  { snackbar :: HH.ClassName
  , jsSnackbar :: HH.ClassName
  , snackbarText :: HH.ClassName
  , snackbarAction :: HH.ClassName
  , snackbarActive :: HH.ClassName
  }

cl =
  { snackbar       : HH.ClassName "mdl-snackbar"
  , jsSnackbar     : HH.ClassName "mdl-js-snackbar"
  , snackbarText   : HH.ClassName "mdl-snackbar__text"
  , snackbarAction : HH.ClassName "mdl-snackbar__action"
  , snackbarActive : HH.ClassName "mdl-snackbar--active"
  }
