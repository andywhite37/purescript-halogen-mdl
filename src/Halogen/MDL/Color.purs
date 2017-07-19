module Halogen.MDL.Color where

import Halogen.HTML as HH

-- TODO: add all colors
cl ::
  { colorTeal :: HH.ClassName
  , colorTextWhite :: HH.ClassName
  }
cl =
  { colorTeal      : HH.ClassName "mdl-color--teal"
  , colorTextWhite : HH.ClassName "mdl-color-text--white"
  }
