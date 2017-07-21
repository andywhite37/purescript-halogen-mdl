module Halogen.MDL.List where

import Halogen.HTML as HH

cl ::
  { list :: HH.ClassName
  , listItem :: HH.ClassName
  , listItemSubTitle :: HH.ClassName
  , listItemTwoLine :: HH.ClassName
  , listItemThreeLine :: HH.ClassName
  , listItemPrimaryContent :: HH.ClassName
  , listItemAvatar :: HH.ClassName
  , listItemIcon :: HH.ClassName
  , listItemSecondaryContent :: HH.ClassName
  , listItemSecondaryInfo :: HH.ClassName
  , listItemSecondaryAction :: HH.ClassName
  , listItemTextBody :: HH.ClassName
  }
cl =
  { list                     : HH.ClassName "mdl-list"
  , listItem                 : HH.ClassName "mdl-list__item"
  , listItemSubTitle         : HH.ClassName "mdl-list__item-sub-title"
  , listItemTwoLine          : HH.ClassName "mdl-list__item--two-line"
  , listItemThreeLine        : HH.ClassName "mdl-list__item--three-line"
  , listItemPrimaryContent   : HH.ClassName "mdl-list__item-primary-content"
  , listItemAvatar           : HH.ClassName "mdl-list__item-avatar"
  , listItemIcon             : HH.ClassName "mdl-list__item-icon"
  , listItemSecondaryContent : HH.ClassName "mdl-list__item-secondary-content"
  , listItemSecondaryInfo    : HH.ClassName "mdl-list__item-secondary-info"
  , listItemSecondaryAction  : HH.ClassName "mdl-list__item-secondary-action"
  , listItemTextBody         : HH.ClassName "mdl-list__item-text-body"
  }
