module Halogen.MDL.Badge where

import Halogen.HTML as HH

cl ::
  { badge :: HH.ClassName
  , badgeNoBackground :: HH.ClassName
  , badgeOverlap :: HH.ClassName
  }
cl =
  { badge             : HH.ClassName "mdl-badge"
  , badgeNoBackground : HH.ClassName "mdl-badge--no-background"
  , badgeOverlap      : HH.ClassName "mdl-badge--overlap"
  }
