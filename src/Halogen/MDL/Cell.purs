module Halogen.MDL.Cell where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

cl ::
  { cell :: HH.ClassName
  , cell1Col :: HH.ClassName
  , cell2Col :: HH.ClassName
  , cell3Col :: HH.ClassName
  , cell4Col :: HH.ClassName
  , cell5Col :: HH.ClassName
  , cell6Col :: HH.ClassName
  , cell7Col :: HH.ClassName
  , cell8Col :: HH.ClassName
  , cell9Col :: HH.ClassName
  , cell10Col :: HH.ClassName
  , cell11Col :: HH.ClassName
  , cell12Col :: HH.ClassName
  , cell1ColDesktop :: HH.ClassName
  , cell2ColDesktop :: HH.ClassName
  , cell3ColDesktop :: HH.ClassName
  , cell4ColDesktop :: HH.ClassName
  , cell5ColDesktop :: HH.ClassName
  , cell6ColDesktop :: HH.ClassName
  , cell7ColDesktop :: HH.ClassName
  , cell8ColDesktop :: HH.ClassName
  , cell9ColDesktop :: HH.ClassName
  , cell10ColDesktop :: HH.ClassName
  , cell11ColDesktop :: HH.ClassName
  , cell12ColDesktop :: HH.ClassName
  , cell1ColTablet :: HH.ClassName
  , cell2ColTablet :: HH.ClassName
  , cell3ColTablet :: HH.ClassName
  , cell4ColTablet :: HH.ClassName
  , cell5ColTablet :: HH.ClassName
  , cell6ColTablet :: HH.ClassName
  , cell7ColTablet :: HH.ClassName
  , cell8ColTablet :: HH.ClassName
  , cell9ColTablet :: HH.ClassName
  , cell10ColTablet :: HH.ClassName
  , cell11ColTablet :: HH.ClassName
  , cell12ColTablet :: HH.ClassName
  , cell1ColPhone :: HH.ClassName
  , cell2ColPhone :: HH.ClassName
  , cell3ColPhone :: HH.ClassName
  , cell4ColPhone :: HH.ClassName
  , cell5ColPhone :: HH.ClassName
  , cell6ColPhone :: HH.ClassName
  , cell7ColPhone :: HH.ClassName
  , cell8ColPhone :: HH.ClassName
  , cell9ColPhone :: HH.ClassName
  , cell10ColPhone :: HH.ClassName
  , cell11ColPhone :: HH.ClassName
  , cell12ColPhone :: HH.ClassName
  , cell1Offset :: HH.ClassName
  , cell2Offset :: HH.ClassName
  , cell3Offset :: HH.ClassName
  , cell4Offset :: HH.ClassName
  , cell5Offset :: HH.ClassName
  , cell6Offset :: HH.ClassName
  , cell7Offset :: HH.ClassName
  , cell8Offset :: HH.ClassName
  , cell9Offset :: HH.ClassName
  , cell10Offset :: HH.ClassName
  , cell11Offset :: HH.ClassName
  , cell12Offset :: HH.ClassName
  , cell1OffsetDesktop :: HH.ClassName
  , cell2OffsetDesktop :: HH.ClassName
  , cell3OffsetDesktop :: HH.ClassName
  , cell4OffsetDesktop :: HH.ClassName
  , cell5OffsetDesktop :: HH.ClassName
  , cell6OffsetDesktop :: HH.ClassName
  , cell7OffsetDesktop :: HH.ClassName
  , cell8OffsetDesktop :: HH.ClassName
  , cell9OffsetDesktop :: HH.ClassName
  , cell10OffsetDesktop :: HH.ClassName
  , cell11OffsetDesktop :: HH.ClassName
  , cell12OffsetDesktop :: HH.ClassName
  , cell1OffsetTablet :: HH.ClassName
  , cell2OffsetTablet :: HH.ClassName
  , cell3OffsetTablet :: HH.ClassName
  , cell4OffsetTablet :: HH.ClassName
  , cell5OffsetTablet :: HH.ClassName
  , cell6OffsetTablet :: HH.ClassName
  , cell7OffsetTablet :: HH.ClassName
  , cell8OffsetTablet :: HH.ClassName
  , cell9OffsetTablet :: HH.ClassName
  , cell10OffsetTablet :: HH.ClassName
  , cell11OffsetTablet :: HH.ClassName
  , cell12OffsetTablet :: HH.ClassName
  , cell1OffsetPhone :: HH.ClassName
  , cell2OffsetPhone :: HH.ClassName
  , cell3OffsetPhone :: HH.ClassName
  , cell4OffsetPhone :: HH.ClassName
  , cell5OffsetPhone :: HH.ClassName
  , cell6OffsetPhone :: HH.ClassName
  , cell7OffsetPhone :: HH.ClassName
  , cell8OffsetPhone :: HH.ClassName
  , cell9OffsetPhone :: HH.ClassName
  , cell10OffsetPhone :: HH.ClassName
  , cell11OffsetPhone :: HH.ClassName
  , cell12OffsetPhone :: HH.ClassName
  , cellOrder1 :: HH.ClassName
  , cellOrder2 :: HH.ClassName
  , cellOrder3 :: HH.ClassName
  , cellOrder4 :: HH.ClassName
  , cellOrder5 :: HH.ClassName
  , cellOrder6 :: HH.ClassName
  , cellOrder7 :: HH.ClassName
  , cellOrder8 :: HH.ClassName
  , cellOrder9 :: HH.ClassName
  , cellOrder10 :: HH.ClassName
  , cellOrder11 :: HH.ClassName
  , cellOrder12 :: HH.ClassName
  , cellOrder1Desktop :: HH.ClassName
  , cellOrder2Desktop :: HH.ClassName
  , cellOrder3Desktop :: HH.ClassName
  , cellOrder4Desktop :: HH.ClassName
  , cellOrder5Desktop :: HH.ClassName
  , cellOrder6Desktop :: HH.ClassName
  , cellOrder7Desktop :: HH.ClassName
  , cellOrder8Desktop :: HH.ClassName
  , cellOrder9Desktop :: HH.ClassName
  , cellOrder10Desktop :: HH.ClassName
  , cellOrder11Desktop :: HH.ClassName
  , cellOrder12Desktop :: HH.ClassName
  , cellOrder1Tablet :: HH.ClassName
  , cellOrder2Tablet :: HH.ClassName
  , cellOrder3Tablet :: HH.ClassName
  , cellOrder4Tablet :: HH.ClassName
  , cellOrder5Tablet :: HH.ClassName
  , cellOrder6Tablet :: HH.ClassName
  , cellOrder7Tablet :: HH.ClassName
  , cellOrder8Tablet :: HH.ClassName
  , cellOrder9Tablet :: HH.ClassName
  , cellOrder10Tablet :: HH.ClassName
  , cellOrder11Tablet :: HH.ClassName
  , cellOrder12Tablet :: HH.ClassName
  , cellOrder1Phone :: HH.ClassName
  , cellOrder2Phone :: HH.ClassName
  , cellOrder3Phone :: HH.ClassName
  , cellOrder4Phone :: HH.ClassName
  , cellOrder5Phone :: HH.ClassName
  , cellOrder6Phone :: HH.ClassName
  , cellOrder7Phone :: HH.ClassName
  , cellOrder8Phone :: HH.ClassName
  , cellOrder9Phone :: HH.ClassName
  , cellOrder10Phone :: HH.ClassName
  , cellOrder11Phone :: HH.ClassName
  , cellOrder12Phone :: HH.ClassName
  , cellHideDesktop :: HH.ClassName
  , cellHideTablet :: HH.ClassName
  , cellHidePhone :: HH.ClassName
  , cellStretch :: HH.ClassName
  , cellTop :: HH.ClassName
  , cellMiddle :: HH.ClassName
  , cellBottom :: HH.ClassName
  }
cl =
  { cell : HH.ClassName "mdl-cell"
  , cell1Col : HH.ClassName "mdl-cell--1-col"
  , cell2Col : HH.ClassName "mdl-cell--2-col"
  , cell3Col : HH.ClassName "mdl-cell--3-col"
  , cell4Col : HH.ClassName "mdl-cell--4-col"
  , cell5Col : HH.ClassName "mdl-cell--5-col"
  , cell6Col : HH.ClassName "mdl-cell--6-col"
  , cell7Col : HH.ClassName "mdl-cell--7-col"
  , cell8Col : HH.ClassName "mdl-cell--8-col"
  , cell9Col : HH.ClassName "mdl-cell--9-col"
  , cell10Col : HH.ClassName "mdl-cell--10-col"
  , cell11Col : HH.ClassName "mdl-cell--11-col"
  , cell12Col : HH.ClassName "mdl-cell--12-col"
  , cell1ColDesktop : HH.ClassName "mdl-cell--1-col-desktop"
  , cell2ColDesktop : HH.ClassName "mdl-cell--2-col-desktop"
  , cell3ColDesktop : HH.ClassName "mdl-cell--3-col-desktop"
  , cell4ColDesktop : HH.ClassName "mdl-cell--4-col-desktop"
  , cell5ColDesktop : HH.ClassName "mdl-cell--5-col-desktop"
  , cell6ColDesktop : HH.ClassName "mdl-cell--6-col-desktop"
  , cell7ColDesktop : HH.ClassName "mdl-cell--7-col-desktop"
  , cell8ColDesktop : HH.ClassName "mdl-cell--8-col-desktop"
  , cell9ColDesktop : HH.ClassName "mdl-cell--9-col-desktop"
  , cell10ColDesktop : HH.ClassName "mdl-cell--10-col-desktop"
  , cell11ColDesktop : HH.ClassName "mdl-cell--11-col-desktop"
  , cell12ColDesktop : HH.ClassName "mdl-cell--12-col-desktop"
  , cell1ColTablet : HH.ClassName "mdl-cell--1-col-tablet"
  , cell2ColTablet : HH.ClassName "mdl-cell--2-col-tablet"
  , cell3ColTablet : HH.ClassName "mdl-cell--3-col-tablet"
  , cell4ColTablet : HH.ClassName "mdl-cell--4-col-tablet"
  , cell5ColTablet : HH.ClassName "mdl-cell--5-col-tablet"
  , cell6ColTablet : HH.ClassName "mdl-cell--6-col-tablet"
  , cell7ColTablet : HH.ClassName "mdl-cell--7-col-tablet"
  , cell8ColTablet : HH.ClassName "mdl-cell--8-col-tablet"
  , cell9ColTablet : HH.ClassName "mdl-cell--9-col-tablet"
  , cell10ColTablet : HH.ClassName "mdl-cell--10-col-tablet"
  , cell11ColTablet : HH.ClassName "mdl-cell--11-col-tablet"
  , cell12ColTablet : HH.ClassName "mdl-cell--12-col-tablet"
  , cell1ColPhone : HH.ClassName "mdl-cell--1-col-phone"
  , cell2ColPhone : HH.ClassName "mdl-cell--2-col-phone"
  , cell3ColPhone : HH.ClassName "mdl-cell--3-col-phone"
  , cell4ColPhone : HH.ClassName "mdl-cell--4-col-phone"
  , cell5ColPhone : HH.ClassName "mdl-cell--5-col-phone"
  , cell6ColPhone : HH.ClassName "mdl-cell--6-col-phone"
  , cell7ColPhone : HH.ClassName "mdl-cell--7-col-phone"
  , cell8ColPhone : HH.ClassName "mdl-cell--8-col-phone"
  , cell9ColPhone : HH.ClassName "mdl-cell--9-col-phone"
  , cell10ColPhone : HH.ClassName "mdl-cell--10-col-phone"
  , cell11ColPhone : HH.ClassName "mdl-cell--11-col-phone"
  , cell12ColPhone : HH.ClassName "mdl-cell--12-col-phone"
  , cell1Offset : HH.ClassName "mdl-cell--1-offset"
  , cell2Offset : HH.ClassName "mdl-cell--2-offset"
  , cell3Offset : HH.ClassName "mdl-cell--3-offset"
  , cell4Offset : HH.ClassName "mdl-cell--4-offset"
  , cell5Offset : HH.ClassName "mdl-cell--5-offset"
  , cell6Offset : HH.ClassName "mdl-cell--6-offset"
  , cell7Offset : HH.ClassName "mdl-cell--7-offset"
  , cell8Offset : HH.ClassName "mdl-cell--8-offset"
  , cell9Offset : HH.ClassName "mdl-cell--9-offset"
  , cell10Offset : HH.ClassName "mdl-cell--10-offset"
  , cell11Offset : HH.ClassName "mdl-cell--11-offset"
  , cell12Offset : HH.ClassName "mdl-cell--12-offset"
  , cell1OffsetDesktop : HH.ClassName "mdl-cell--1-offset-desktop"
  , cell2OffsetDesktop : HH.ClassName "mdl-cell--2-offset-desktop"
  , cell3OffsetDesktop : HH.ClassName "mdl-cell--3-offset-desktop"
  , cell4OffsetDesktop : HH.ClassName "mdl-cell--4-offset-desktop"
  , cell5OffsetDesktop : HH.ClassName "mdl-cell--5-offset-desktop"
  , cell6OffsetDesktop : HH.ClassName "mdl-cell--6-offset-desktop"
  , cell7OffsetDesktop : HH.ClassName "mdl-cell--7-offset-desktop"
  , cell8OffsetDesktop : HH.ClassName "mdl-cell--8-offset-desktop"
  , cell9OffsetDesktop : HH.ClassName "mdl-cell--9-offset-desktop"
  , cell10OffsetDesktop : HH.ClassName "mdl-cell--10-offset-desktop"
  , cell11OffsetDesktop : HH.ClassName "mdl-cell--11-offset-desktop"
  , cell12OffsetDesktop : HH.ClassName "mdl-cell--12-offset-desktop"
  , cell1OffsetTablet : HH.ClassName "mdl-cell--1-offset-tablet"
  , cell2OffsetTablet : HH.ClassName "mdl-cell--2-offset-tablet"
  , cell3OffsetTablet : HH.ClassName "mdl-cell--3-offset-tablet"
  , cell4OffsetTablet : HH.ClassName "mdl-cell--4-offset-tablet"
  , cell5OffsetTablet : HH.ClassName "mdl-cell--5-offset-tablet"
  , cell6OffsetTablet : HH.ClassName "mdl-cell--6-offset-tablet"
  , cell7OffsetTablet : HH.ClassName "mdl-cell--7-offset-tablet"
  , cell8OffsetTablet : HH.ClassName "mdl-cell--8-offset-tablet"
  , cell9OffsetTablet : HH.ClassName "mdl-cell--9-offset-tablet"
  , cell10OffsetTablet : HH.ClassName "mdl-cell--10-offset-tablet"
  , cell11OffsetTablet : HH.ClassName "mdl-cell--11-offset-tablet"
  , cell12OffsetTablet : HH.ClassName "mdl-cell--12-offset-tablet"
  , cell1OffsetPhone : HH.ClassName "mdl-cell--1-offset-phone"
  , cell2OffsetPhone : HH.ClassName "mdl-cell--2-offset-phone"
  , cell3OffsetPhone : HH.ClassName "mdl-cell--3-offset-phone"
  , cell4OffsetPhone : HH.ClassName "mdl-cell--4-offset-phone"
  , cell5OffsetPhone : HH.ClassName "mdl-cell--5-offset-phone"
  , cell6OffsetPhone : HH.ClassName "mdl-cell--6-offset-phone"
  , cell7OffsetPhone : HH.ClassName "mdl-cell--7-offset-phone"
  , cell8OffsetPhone : HH.ClassName "mdl-cell--8-offset-phone"
  , cell9OffsetPhone : HH.ClassName "mdl-cell--9-offset-phone"
  , cell10OffsetPhone : HH.ClassName "mdl-cell--10-offset-phone"
  , cell11OffsetPhone : HH.ClassName "mdl-cell--11-offset-phone"
  , cell12OffsetPhone : HH.ClassName "mdl-cell--12-offset-phone"
  , cellOrder1 : HH.ClassName "mdl-cell--order-1"
  , cellOrder2 : HH.ClassName "mdl-cell--order-2"
  , cellOrder3 : HH.ClassName "mdl-cell--order-3"
  , cellOrder4 : HH.ClassName "mdl-cell--order-4"
  , cellOrder5 : HH.ClassName "mdl-cell--order-5"
  , cellOrder6 : HH.ClassName "mdl-cell--order-6"
  , cellOrder7 : HH.ClassName "mdl-cell--order-7"
  , cellOrder8 : HH.ClassName "mdl-cell--order-8"
  , cellOrder9 : HH.ClassName "mdl-cell--order-9"
  , cellOrder10 : HH.ClassName "mdl-cell--order-10"
  , cellOrder11 : HH.ClassName "mdl-cell--order-11"
  , cellOrder12 : HH.ClassName "mdl-cell--order-12"
  , cellOrder1Desktop : HH.ClassName "mdl-cell--order-1-desktop"
  , cellOrder2Desktop : HH.ClassName "mdl-cell--order-2-desktop"
  , cellOrder3Desktop : HH.ClassName "mdl-cell--order-3-desktop"
  , cellOrder4Desktop : HH.ClassName "mdl-cell--order-4-desktop"
  , cellOrder5Desktop : HH.ClassName "mdl-cell--order-5-desktop"
  , cellOrder6Desktop : HH.ClassName "mdl-cell--order-6-desktop"
  , cellOrder7Desktop : HH.ClassName "mdl-cell--order-7-desktop"
  , cellOrder8Desktop : HH.ClassName "mdl-cell--order-8-desktop"
  , cellOrder9Desktop : HH.ClassName "mdl-cell--order-9-desktop"
  , cellOrder10Desktop : HH.ClassName "mdl-cell--order-10-desktop"
  , cellOrder11Desktop : HH.ClassName "mdl-cell--order-11-desktop"
  , cellOrder12Desktop : HH.ClassName "mdl-cell--order-12-desktop"
  , cellOrder1Tablet : HH.ClassName "mdl-cell--order-1-tablet"
  , cellOrder2Tablet : HH.ClassName "mdl-cell--order-2-tablet"
  , cellOrder3Tablet : HH.ClassName "mdl-cell--order-3-tablet"
  , cellOrder4Tablet : HH.ClassName "mdl-cell--order-4-tablet"
  , cellOrder5Tablet : HH.ClassName "mdl-cell--order-5-tablet"
  , cellOrder6Tablet : HH.ClassName "mdl-cell--order-6-tablet"
  , cellOrder7Tablet : HH.ClassName "mdl-cell--order-7-tablet"
  , cellOrder8Tablet : HH.ClassName "mdl-cell--order-8-tablet"
  , cellOrder9Tablet : HH.ClassName "mdl-cell--order-9-tablet"
  , cellOrder10Tablet : HH.ClassName "mdl-cell--order-10-tablet"
  , cellOrder11Tablet : HH.ClassName "mdl-cell--order-11-tablet"
  , cellOrder12Tablet : HH.ClassName "mdl-cell--order-12-tablet"
  , cellOrder1Phone : HH.ClassName "mdl-cell--order-1-phone"
  , cellOrder2Phone : HH.ClassName "mdl-cell--order-2-phone"
  , cellOrder3Phone : HH.ClassName "mdl-cell--order-3-phone"
  , cellOrder4Phone : HH.ClassName "mdl-cell--order-4-phone"
  , cellOrder5Phone : HH.ClassName "mdl-cell--order-5-phone"
  , cellOrder6Phone : HH.ClassName "mdl-cell--order-6-phone"
  , cellOrder7Phone : HH.ClassName "mdl-cell--order-7-phone"
  , cellOrder8Phone : HH.ClassName "mdl-cell--order-8-phone"
  , cellOrder9Phone : HH.ClassName "mdl-cell--order-9-phone"
  , cellOrder10Phone : HH.ClassName "mdl-cell--order-10-phone"
  , cellOrder11Phone : HH.ClassName "mdl-cell--order-11-phone"
  , cellOrder12Phone : HH.ClassName "mdl-cell--order-12-phone"
  , cellHideDesktop : HH.ClassName "mdl-cell--hide-desktop"
  , cellHideTablet : HH.ClassName "mdl-cell--hide-tablet"
  , cellHidePhone : HH.ClassName "mdl-cell--hide-phone"
  , cellStretch : HH.ClassName "mdl-cell--stretch"
  , cellTop : HH.ClassName "mdl-cell--top"
  , cellMiddle : HH.ClassName "mdl-cell--middle"
  , cellBottom : HH.ClassName "mdl-cell--bottom"
  }

el ::
  { cell4Col_ :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
  , cell12Col_ :: ∀ p i. Array (HH.HTML p i) -> HH.HTML p i
  }
el =
  { cell4Col_: \children -> HH.div [ HP.classes [ cl.cell, cl.cell4Col ] ] children
  , cell12Col_: \children -> HH.div [ HP.classes [ cl.cell, cl.cell12Col ] ] children
  }
