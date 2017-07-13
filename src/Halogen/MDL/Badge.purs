module Halogen.MDL.Badge where

import Halogen.HTML as HH

classes ::
  { badge :: HH.ClassName
  , badgeNoBackground :: HH.ClassName
  , badgeOverlap :: HH.ClassName
  }
classes =
  { badge             : HH.ClassName "mdl-badge"
  , badgeNoBackground : HH.ClassName "mdl-badge--no-background"
  , badgeOverlap      : HH.ClassName "mdl-badge--overlap"
  }
