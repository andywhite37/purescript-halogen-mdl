module Halogen.MDL.Menu where

import Halogen.HTML as HH

cl ::
  { menu :: HH.ClassName
  , jsMenu :: HH.ClassName
  , menuItem :: HH.ClassName
  , menuItemFullBleedDivider :: HH.ClassName
  , menuTopLeft :: HH.ClassName
  , menuTopRight :: HH.ClassName
  , menuBottomLeft :: HH.ClassName
  , menuBottomRight :: HH.ClassName
  }
cl =
  { menu                     : HH.ClassName "mdl-menu"
  , jsMenu                   : HH.ClassName "mdl-js-menu"
  , menuItem                 : HH.ClassName "mdl-menu__item"
  , menuItemFullBleedDivider : HH.ClassName "mdl-menu__item--full-bleed-divider"
  , menuTopLeft              : HH.ClassName "mdl-menu--top-left"
  , menuTopRight             : HH.ClassName "mdl-menu--top-right"
  , menuBottomLeft           : HH.ClassName "mdl-menu--bottom-left"
  , menuBottomRight          : HH.ClassName "mdl-menu--bottom-right"
  }
