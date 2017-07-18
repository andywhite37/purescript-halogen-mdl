module Halogen.MDL.Shadow where

import Halogen.HTML as HH

cl ::
  { shadow2dp :: HH.ClassName
  , shadow3dp :: HH.ClassName
  , shadow4dp :: HH.ClassName
  , shadow6dp :: HH.ClassName
  , shadow8dp :: HH.ClassName
  , shadow16dp :: HH.ClassName
  }
cl =
  { shadow2dp  : HH.ClassName "mdl-shadow--2dp"
  , shadow3dp  : HH.ClassName "mdl-shadow--3dp"
  , shadow4dp  : HH.ClassName "mdl-shadow--4dp"
  , shadow6dp  : HH.ClassName "mdl-shadow--6dp"
  , shadow8dp  : HH.ClassName "mdl-shadow--8dp"
  , shadow16dp : HH.ClassName "mdl-shadow--16dp"
  }
