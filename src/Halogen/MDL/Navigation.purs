module Halogen.MDL.Navigation where

import Halogen.HTML as HH

classes ::
  { navigation :: HH.ClassName
  , navigationLink :: HH.ClassName
  }
classes =
  { navigation     : HH.ClassName "mdl-navigation"
  , navigationLink : HH.ClassName "mdl-navigation__link"
  }
