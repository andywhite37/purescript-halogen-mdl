module Halogen.MDL.Chip where

import Halogen.HTML as HH

cl ::
  { chip :: HH.ClassName
  , chipContactM :: HH.ClassName
  , chipDeletable :: HH.ClassName
  , chipText :: HH.ClassName
  , chipAction :: HH.ClassName
  , chipContactE :: HH.ClassName
  }
cl =
  { chip                : HH.ClassName "mdl-chip"
  , chipContactM        : HH.ClassName "mdl-chip--contact"
  , chipDeletable       : HH.ClassName "mdl-chip--deletable"
  , chipText            : HH.ClassName "mdl-chip__text"
  , chipAction          : HH.ClassName "mdl-chip__action"
  , chipContactE        : HH.ClassName "mdl-chip__contact"
  }
