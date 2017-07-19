module Halogen.MDL.Dialog where

import Prelude
--import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
--import Data.Const (Const)
import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.HTML.Types (HTMLElement())

import Halogen as H
--import Halogen.Aff as HA
import Halogen.HTML as HH

--import Halogen.MDL as MDL

foreign import registerDialog :: ∀ e. HTMLElement -> Eff (dom :: DOM | e) Unit
foreign import showDialog :: ∀ e. HTMLElement -> Eff (dom :: DOM | e) Unit
foreign import close :: ∀ e. HTMLElement -> Eff (dom :: DOM | e) Unit

registerDialogByRef :: forall e s f g p o m. MonadEff (dom :: DOM | e) m => H.RefLabel -> H.HalogenM s f g p o m Unit
registerDialogByRef ref = do
  element <- H.getHTMLElementRef ref
  case element of
    Just element -> H.liftEff $ registerDialog element
    Nothing -> pure unit

showDialogByRef :: forall e s f g p o m. MonadEff (dom :: DOM | e) m => H.RefLabel -> H.HalogenM s f g p o m Unit
showDialogByRef ref = do
  element <- H.getHTMLElementRef ref
  case element of
    Just element -> H.liftEff $ showDialog element
    Nothing -> pure unit

closeByRef :: forall e s f g p o m. MonadEff (dom :: DOM | e) m => H.RefLabel -> H.HalogenM s f g p o m Unit
closeByRef ref = do
  element <- H.getHTMLElementRef ref
  case element of
    Just element -> H.liftEff $ close element
    Nothing -> pure unit

cl ::
  { dialog :: HH.ClassName
  , dialogTitle :: HH.ClassName
  , dialogContent :: HH.ClassName
  , dialogActions :: HH.ClassName
  , dialogActionsFullWidth :: HH.ClassName
  }
cl =
  { dialog                 : HH.ClassName "mdl-dialog"
  , dialogTitle            : HH.ClassName "mdl-dialog__title"
  , dialogContent          : HH.ClassName "mdl-dialog__content"
  , dialogActions          : HH.ClassName "mdl-dialog__actions"
  , dialogActionsFullWidth : HH.ClassName "mdl-dialog__actions--full-width"
  }

{-
type Props = {}

newtype State = State Props
derive instance eqState :: Eq State
derive instance ordState :: Ord State

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a

data Input = Initialize State

data Message = Void

data ContentSlot = ContentSlot
derive instance eqContentSlot :: Eq ContentSlot
derive instance ordContentSlot :: Ord ContentSlot

type DialogHTML cq eff = H.ParentHTML Query cq ContentSlot (Aff (HA.HalogenEffects eff))
type DialogDSL cq eff = H.ParentDSL State Query cq ContentSlot Message (Aff (HA.HalogenEffects eff))

dialog :: ∀ cq ci cm eff. H.Component HH.HTML cq ci cm (Aff (HA.HalogenEffects eff)) -> H.Component HH.HTML Query Input Message (Aff (HA.HalogenEffects eff))
dialog content  =
  H.lifecycleParentComponent
    { initialState: initialState
    , initializer: initializer
    , finalizer: finalizer
    , receiver: receiver
    , render
    , eval
    }
  where
  dialogRef :: H.RefLabel
  dialogRef = H.RefLabel "mdl-dialog"

  initialState :: Input -> State
  initialState _ = State {}

  initializer :: Maybe (Query Unit)
  initializer = Nothing

  finalizer :: Maybe (Query Unit)
  finalizer = Nothing

  receiver :: Input -> Maybe (Query Unit)
  receiver _ = Nothing

  render :: State -> DialogHTML cq eff
  render state =
    HH.slot
      ContentSlot
    --HH.div_ [ HH.text "dialog" ]

  eval :: Query ~> DialogDSL cq eff
  eval = case _ of
    InitializeComponent next -> do
      maybeElement <- H.getHTMLElementRef dialogRef
      case maybeElement of
        Just element -> H.liftEff $ registerDialog element
        Nothing -> pure unit
      pure next
    FinalizeComponent next -> do
      pure next
    UpdateState state next -> do
      H.put state
      pure next
-}
