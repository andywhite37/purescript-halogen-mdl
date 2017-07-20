module Halogen.MDL.Tabs where

import Halogen.HTML as HH

cl ::
  { tabs :: HH.ClassName
  , jsTabs :: HH.ClassName
  , tabsTabBar :: HH.ClassName
  , tabsTab :: HH.ClassName
  , isActive :: HH.ClassName
  , tabsPanel :: HH.ClassName
  }
cl =
  { tabs       : HH.ClassName "mdl-tabs"
  , jsTabs     : HH.ClassName "mdl-js-tabs"
  , tabsTabBar : HH.ClassName "mdl-tabs__tab-bar"
  , tabsTab    : HH.ClassName "mdl-tabs__tab"
  , isActive   : HH.ClassName "is-active"
  , tabsPanel  : HH.ClassName "mdl-tabs__panel"
  }
