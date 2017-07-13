module Halogen.MDL.Grid where

import Halogen.HTML as HH

classes ::
  { grid :: HH.ClassName
  , gridNoSpacing :: HH.ClassName
  }
classes =
  { grid          : HH.ClassName "mdl-grid"
  , gridNoSpacing : HH.ClassName "mdl-grid--no-spacing"
  }
