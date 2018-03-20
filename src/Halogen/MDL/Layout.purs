module Halogen.MDL.Layout where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)

import Halogen.HTML as HH

foreign import toggleDrawer :: ∀ e. Eff (dom :: DOM | e) Unit

cl ::
  { layout :: HH.ClassName
  , layoutFixedTabs :: HH.ClassName
  , layoutLargeScreenOnly :: HH.ClassName
  , layoutSmallScreenOnly :: HH.ClassName
  , layoutNoDrawerButton :: HH.ClassName
  , layoutNoDesktopDrawerButton :: HH.ClassName
  , layoutContainer :: HH.ClassName
  , layoutContent :: HH.ClassName
  , layoutDrawer :: HH.ClassName
  , layoutFixedDrawer :: HH.ClassName
  , layoutFixedHeader :: HH.ClassName
  , layoutHeader :: HH.ClassName
  , layoutHeaderScroll :: HH.ClassName
  , layoutHeaderSeamed :: HH.ClassName
  , layoutHeaderTransparent :: HH.ClassName
  , layoutHeaderWaterfall :: HH.ClassName
  , layoutHeaderWaterfallHideTop :: HH.ClassName
  , layoutHeaderRow :: HH.ClassName
  , layoutTab :: HH.ClassName
  , isActive :: HH.ClassName
  , layoutTabBar :: HH.ClassName
  , layoutTabManualSwitch :: HH.ClassName
  , layoutTabPanel :: HH.ClassName
  , layoutTitle :: HH.ClassName
  , layoutIcon :: HH.ClassName
  , layoutSpacer :: HH.ClassName
  , jsLayout :: HH.ClassName
  }
cl =
  { layout                       : HH.ClassName "mdl-layout"
  , layoutFixedTabs              : HH.ClassName "mdl-layout--fixed-tabs"
  , layoutLargeScreenOnly        : HH.ClassName "mdl-layout--large-screen-only"
  , layoutSmallScreenOnly        : HH.ClassName "mdl-layout--small-screen-only"
  , layoutNoDrawerButton         : HH.ClassName "mdl-layout--no-drawer-button"
  , layoutNoDesktopDrawerButton  : HH.ClassName "mdl-layout--no-desktop-drawer-button"
  , layoutContainer              : HH.ClassName "mdl-layout__container"
  , layoutContent                : HH.ClassName "mdl-layout__content"
  , layoutDrawer                 : HH.ClassName "mdl-layout__drawer"
  , layoutFixedDrawer            : HH.ClassName "mdl-layout--fixed-drawer"
  , layoutFixedHeader            : HH.ClassName "mdl-layout--fixed-header"
  , layoutHeader                 : HH.ClassName "mdl-layout__header"
  , layoutHeaderScroll           : HH.ClassName "mdl-layout__header--scroll"
  , layoutHeaderSeamed           : HH.ClassName "mdl-layout__header--seamed"
  , layoutHeaderTransparent      : HH.ClassName "mdl-layout__header--transparent"
  , layoutHeaderWaterfall        : HH.ClassName "mdl-layout__header--waterfall"
  , layoutHeaderWaterfallHideTop : HH.ClassName "mdl-layout__header--waterfall-hide-top"
  , layoutHeaderRow              : HH.ClassName "mdl-layout__header-row"
  , layoutTab                    : HH.ClassName "mdl-layout__tab"
  , isActive                     : HH.ClassName "is-active"
  , layoutTabBar                 : HH.ClassName "mdl-layout__tab-bar"
  , layoutTabManualSwitch        : HH.ClassName "mdl-layout__tab-manual-switch"
  , layoutTabPanel               : HH.ClassName "mdl-layout__tab-panel"
  , layoutTitle                  : HH.ClassName "mdl-layout__title"
  , layoutIcon                   : HH.ClassName "mdl-layout-icon"
  , layoutSpacer                 : HH.ClassName "mdl-layout-spacer"
  , jsLayout                     : HH.ClassName "mdl-js-layout"
  }
