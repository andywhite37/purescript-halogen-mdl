module Halogen.MDL.Badge where

--import Prelude

import Halogen.HTML as HH
--import Halogen.HTML.Properties as HP

--import Halogen.MDL.MaterialIcon as MI

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

attr ::
  { dataBadge :: HH.AttrName
  }
attr =
  { dataBadge : HH.AttrName "data-badge"
  }

{-
type Props =
  { badge :: String
  , overlap :: Boolean
  , noBackground :: Boolean
  , forIcon :: Boolean
  }

el ::
  { badge_ :: ∀ p i. Props -> Array (HH.HTML p i) -> HH.HTML p i
  }
el =
  { badge_: \props children -> HH.div
      [ HP.classes $ getClasses props
      , HP.attr attr.dataBadge props.badge
      ]
      children
  }
  where

  getClasses :: Props -> Array (HH.ClassName)
  getClasses props =
    [ cl.badge ]
    <> (if props.overlap then [cl.badgeOverlap] else [])
    <> (if props.noBackground then [cl.badgeNoBackground] else [])
    <> (if props.forIcon then [MI.cl.materialIcons] else [])
-}
