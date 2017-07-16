module Halogen.MDL.Navigation where

import Halogen.HTML as HH

cl ::
  { navigation :: HH.ClassName
  , navigationLink :: HH.ClassName
  }
cl =
  { navigation     : HH.ClassName "mdl-navigation"
  , navigationLink : HH.ClassName "mdl-navigation__link"
  }
