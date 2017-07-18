module Halogen.MDL.Card where

--import Prelude

import Halogen.HTML as HH

cl ::
  { card :: HH.ClassName
  , cardBorder :: HH.ClassName
  , cardExpand :: HH.ClassName
  , cardTitle :: HH.ClassName
  , cardTitleText :: HH.ClassName
  , cardSubtitleText :: HH.ClassName
  , cardMedia :: HH.ClassName
  , cardSupportingText :: HH.ClassName
  , cardActions :: HH.ClassName
  , cardMenu :: HH.ClassName
  }
cl =
  { card               : HH.ClassName "mdl-card"
  , cardExpand         : HH.ClassName "mdl-card--expand"
  , cardBorder         : HH.ClassName "mdl-card--border"
  , cardTitle          : HH.ClassName "mdl-card__title"
  , cardTitleText      : HH.ClassName "mdl-card__title-text"
  , cardSubtitleText   : HH.ClassName "mdl-card__subtitle-text"
  , cardMedia          : HH.ClassName "mdl-card__media"
  , cardSupportingText : HH.ClassName "mdl-card__supporting-text"
  , cardActions        : HH.ClassName "mdl-card__actions"
  , cardMenu           : HH.ClassName "mdl-card__menu"
  }
