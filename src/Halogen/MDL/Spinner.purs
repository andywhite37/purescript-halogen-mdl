module Halogen.MDL.Spinner where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

cl ::
  { spinner :: HH.ClassName
  , spinnerSingleColor :: HH.ClassName
  , jsSpinner :: HH.ClassName
  , isActive :: HH.ClassName
  }
cl =
  { spinner            : HH.ClassName "mdl-spinner"
  , spinnerSingleColor : HH.ClassName "mdl-spinner--single-color"
  , jsSpinner          : HH.ClassName "mdl-js-spinner"
  , isActive           : HH.ClassName "is-active"
  }

el ::
  { spinner_ :: ∀ p i. HH.HTML p i
  }
el =
  { spinner_ : HH.div [ HP.classes [ cl.spinner, cl.jsSpinner, cl.isActive ] ] []
  }
