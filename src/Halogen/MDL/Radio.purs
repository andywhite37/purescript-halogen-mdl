module Radio where

import Prelude

import Data.Maybe (Maybe)
import DOM.Event.Types (MouseEvent)

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Halogen.MDL.RippleEffect as RE

cl ::
  { radio :: HH.ClassName
  , jsRadio :: HH.ClassName
  , radioButton :: HH.ClassName
  , radioLabel :: HH.ClassName
  }
cl =
  { radio: HH.ClassName "mdl-radio"
  , jsRadio: HH.ClassName "mdl-js-radio"
  , radioButton: HH.ClassName "mdl-radio__button"
  , radioLabel: HH.ClassName "mdl-radio__label"
  }

type RadioButtonOptions i =
  { id :: String
  , name :: String
  , value :: String
  , checked :: Boolean
  , label :: String
  , ripple :: Boolean
  , onClick :: MouseEvent -> Maybe i
  }

bl ::
  { radio :: ∀ p i. RadioButtonOptions i -> HH.HTML p i
  }
bl =
  { radio : \options ->
      HH.label
        [ HP.classes
            ( [ cl.radio, cl.jsRadio ]
              <> if options.ripple then [ RE.cl.jsRippleEffect ] else []
            )
        , HP.for options.id
        ]
        [ HH.input
            [ HP.type_ HP.InputRadio
            , HP.id_ options.id
            , HP.name options.name
            , HP.value options.value
            , HP.class_ cl.radioButton
            , HP.checked options.checked
            , HE.onClick options.onClick
            ]
        , HH.span
            [ HP.class_ cl.radioLabel ]
            [ HH.text options.label ]
        ]
  }

