module Halogen.MDL.Basic where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type BasicLink = { href :: String, text :: String }

el ::
  { link_ :: ∀ p i. BasicLink -> HH.HTML p i
  }
el =
  { link_ : \v -> HH.a [ HP.href v.href ] [ HH.text v.text ]
  }
