module Halogen.MDL.MegaFooter where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Halogen.MDL.Basic as Basic
import Halogen.MDL.Logo as Logo

-- Class constants

cl ::
  { megaFooter :: HH.ClassName
  , megaFooterHeading :: HH.ClassName
  , megaFooterHeadingCheckbox :: HH.ClassName
  , megaFooterTopSection :: HH.ClassName
  , megaFooterBottomSection :: HH.ClassName
  , megaFooterLeftSection :: HH.ClassName
  , megaFooterMiddleSection :: HH.ClassName
  , megaFooterRightSection :: HH.ClassName
  , megaFooterDropDownSection :: HH.ClassName
  , megaFooterLinkList :: HH.ClassName
  , megaFooterSocialBtn :: HH.ClassName
  }
cl =
  { megaFooter                : HH.ClassName "mdl-mega-footer"
  , megaFooterHeading         : HH.ClassName "mdl-mega-footer__heading"
  , megaFooterHeadingCheckbox : HH.ClassName "mdl-mega-footer__heading-checkbox"
  , megaFooterTopSection      : HH.ClassName "mdl-mega-footer__top-section"
  , megaFooterBottomSection   : HH.ClassName "mdl-mega-footer__bottom-section"
  , megaFooterLeftSection     : HH.ClassName "mdl-mega-footer__left-section"
  , megaFooterMiddleSection   : HH.ClassName "mdl-mega-footer__middle-section"
  , megaFooterRightSection    : HH.ClassName "mdl-mega-footer__right-section"
  , megaFooterDropDownSection : HH.ClassName "mdl-mega-footer__drop-down-section"
  , megaFooterLinkList        : HH.ClassName "mdl-mega-footer__link-list"
  , megaFooterSocialBtn       : HH.ClassName "mdl-mega-footer__social-btn"
  }

-- Element helper functions

el ::
  { megaFooter_ :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
  , middleSection_ :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
  , bottomSection_ :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
  , dropDownSection_ :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
  , linkList_ :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
  , linkListItem_ :: ∀ p i. HH.HTML p i -> HH.HTML p i
  }
el =
  { megaFooter_: \children -> HH.footer [ HP.class_ cl.megaFooter ] children
  , middleSection_: \children -> HH.div [ HP.class_ cl.megaFooterMiddleSection ] children
  , bottomSection_: \children -> HH.div [ HP.class_ cl.megaFooterBottomSection ] children
  , dropDownSection_: \children -> HH.div [ HP.class_ cl.megaFooterDropDownSection ] children
  , linkList_: \children -> HH.ul [ HP.class_ cl.megaFooterLinkList ] children
  , linkListItem_: \children -> HH.li_ [ children ]
  }

type MegaFooterBlock =
  { middleSection :: MiddleSectionBlock
  , bottomSection :: BottomSectionBlock
  }

type MiddleSectionBlock =
  { dropDownSections :: Array DropDownSectionBlock
  }

type DropDownSectionBlock =
  { title :: String
  , linkList :: LinkListBlock
  }

type BottomSectionBlock =
  { title :: String
  , linkList :: LinkListBlock
  }

type LinkListBlock =
  { links :: Array Basic.BasicLink
  }

-- Helper functions for creating commonly used blocks of HTML

bl ::
  { megaFooter :: ∀ p i. MegaFooterBlock -> HH.HTML p i
  , middleSection :: ∀ p i. MiddleSectionBlock -> HH.HTML p i
  , bottomSection :: ∀ p i. BottomSectionBlock -> HH.HTML p i
  , dropDownSection :: ∀ p i. DropDownSectionBlock -> HH.HTML p i
  , linkList :: ∀ p i. LinkListBlock -> HH.HTML p i
  }
bl =
  { megaFooter: \block ->
      el.megaFooter_
        [ bl.middleSection block.middleSection
        , bl.bottomSection block.bottomSection
        ]

  , middleSection: \block ->
      el.middleSection_
        (bl.dropDownSection <$> block.dropDownSections)

  , bottomSection: \block ->
      el.bottomSection_
        [ Logo.el.logoText_ block.title
        , bl.linkList block.linkList
        ]

  , dropDownSection: \block ->
      el.dropDownSection_
        [ HH.input [ HP.class_ cl.megaFooterHeadingCheckbox, HP.type_ HP.InputCheckbox, HP.checked true ]
        , HH.h1 [ HP.class_ cl.megaFooterHeading ] [ HH.text block.title ]
        , bl.linkList block.linkList
        ]

  , linkList: \block ->
      el.linkList_
        ((el.linkListItem_ <<< Basic.el.link_) <$> block.links)
  }
