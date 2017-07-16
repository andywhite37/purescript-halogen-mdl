module Halogen.MDL.MiniFooter where

import Halogen.HTML as HH

cl ::
  { miniFooter :: HH.ClassName
  , miniFooterLeftSection :: HH.ClassName
  , miniFooterRightSection :: HH.ClassName
  , miniFooterLinkList :: HH.ClassName
  , miniFooterSocialBtn :: HH.ClassName
  }
cl =
  { miniFooter                : HH.ClassName "mdl-mini-footer"
  , miniFooterLeftSection     : HH.ClassName "mdl-mini-footer__left-section"
  , miniFooterRightSection    : HH.ClassName "mdl-mini-footer__right-section"
  , miniFooterLinkList        : HH.ClassName "mdl-mini-footer__link-list"
  , miniFooterSocialBtn       : HH.ClassName "mdl-mini-footer__social-btn"
  }
