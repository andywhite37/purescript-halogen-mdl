module Halogen.MDL.Logo where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

cl ::
  { logo :: HH.ClassName
  }
cl =
  { logo : HH.ClassName "mdl-logo"
  }

el ::
  { logo_ :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
  , logoText_ :: ∀ p i. String -> HH.HTML p i
  }
el =
  { logo_: \children -> HH.div [ HP.class_ cl.logo ] children
  , logoText_: \text -> el.logo_ $ [ HH.text text ]
  }
