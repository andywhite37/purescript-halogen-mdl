module Halogen.MDL.Grid where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

cl ::
  { grid :: HH.ClassName
  , gridNoSpacing :: HH.ClassName
  }
cl =
  { grid          : HH.ClassName "mdl-grid"
  , gridNoSpacing : HH.ClassName "mdl-grid--no-spacing"
  }

el ::
  { grid_ :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
  }
el =
  { grid_: \children ->
      HH.div [ HP.class_ cl.grid ] children
  }
