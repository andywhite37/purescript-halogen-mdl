module Halogen.MDL.Radio where

import Prelude

import Data.Array (mapWithIndex)
import Data.Maybe (Maybe)

import CSS as C
import DOM.Event.Types (MouseEvent)

import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
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
  , css :: C.CSS
  , value :: String
  , checked :: Boolean
  , label :: String
  , ripple :: Boolean
  , onClick :: MouseEvent -> Maybe i
  }

type RadioGroupOptions p i =
  { container :: Array (HH.HTML p i) -> HH.HTML p i
  , idPrefix :: String
  , name :: String
  , buttonCSS :: C.CSS
  , ripple :: Boolean
  , buttons :: Array (RadioGroupButtonOptions i)
  }

type RadioGroupButtonOptions i =
  { value :: String
  , label :: String
  , checked :: Boolean
  , onClick :: MouseEvent -> Maybe i
  }

bl ::
  { radio :: ∀ p i. RadioButtonOptions i -> HH.HTML p i
  , radioGroup :: ∀ p i. RadioGroupOptions p i -> HH.HTML p i
  }
bl =
  { radio : \options ->
      HH.label
        [ HP.classes
            ( [ cl.radio, cl.jsRadio ]
              <> if options.ripple then [ RE.cl.jsRippleEffect ] else []
            )
        , HP.for options.id
        , HC.style options.css
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
  , radioGroup : \groupOptions ->
      let --createButton :: ∀ p i. Int -> RadioGroupOptions p i -> RadioGroupButtonOptions i -> RadioButtonOptions i
        createRadioButton index groupButtonOptions =
          { id : groupOptions.idPrefix <> " " <> (show index)
          , name: groupOptions.name
          , value: groupButtonOptions.value
          , label: groupButtonOptions.label
          , checked: groupButtonOptions.checked
          , onClick: groupButtonOptions.onClick
          , ripple: groupOptions.ripple
          , css: groupOptions.buttonCSS
          }
        --buttonOptions :: Array (RadioButtonOptions i)
        radioButtons = mapWithIndex createRadioButton groupOptions.buttons
        radios = bl.radio <$> radioButtons
      in
        groupOptions.container radios
  }

