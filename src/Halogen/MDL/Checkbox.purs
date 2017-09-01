module Halogen.MDL.Checkbox where

import Prelude
import Data.Maybe (Maybe)

import DOM.Event.Types (MouseEvent)

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Halogen.MDL.RippleEffect as RE

cl ::
  { checkbox :: HH.ClassName
  , jsCheckbox :: HH.ClassName
  , checkboxInput :: HH.ClassName
  , checkboxLabel :: HH.ClassName
  }
cl =
  { checkbox : HH.ClassName "mdl-checkbox"
  , jsCheckbox : HH.ClassName "mdl-js-checkbox"
  , checkboxInput : HH.ClassName "mdl-checkbox__input"
  , checkboxLabel : HH.ClassName "mdl-checkbox__label"
  }

type CheckboxOptions i =
  { id :: String
  , label :: String
  , ripple :: Boolean
  , checked :: Boolean
  , onClick :: MouseEvent -> Maybe i
  }

bl ::
  { checkbox :: ∀ p i. CheckboxOptions i -> HH.HTML p i
  }
bl =
  { checkbox : \options ->
      HH.label
        [ HP.classes
            ( [ cl.checkbox, cl.jsCheckbox ]
              <> if options.ripple then [ RE.cl.jsRippleEffect ] else [ ]
            )
        , HP.for options.id
        ]
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.id_ options.id
            , HP.class_ cl.checkboxInput
            , HP.checked options.checked
            , HE.onClick options.onClick
            ]
        , HH.span
            [ HP.class_ cl.checkboxLabel ]
            [ HH.text options.label ]
        ]
  }
