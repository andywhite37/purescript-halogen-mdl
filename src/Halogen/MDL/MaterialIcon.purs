module Halogen.MDL.MaterialIcon where

import Halogen.HTML as HH

cl ::
  { materialIcons :: HH.ClassName
  }
cl =
  { materialIcons: HH.ClassName "material-icons"
  }

el ::
  { _3dRotation :: ∀ p i. HH.HTML p i
  , accessibility :: ∀ p i. HH.HTML p i
  , accessibile :: ∀ p i. HH.HTML p i
  , accountBalance :: ∀ p i. HH.HTML p i
  , accountBalanceWallet :: ∀ p i. HH.HTML p i
  , accountBox :: ∀ p i. HH.HTML p i
  , accountCircle :: ∀ p i. HH.HTML p i
  , addShoppingCart :: ∀ p i. HH.HTML p i
  , alarm :: ∀ p i. HH.HTML p i
  }
el =
  { _3dRotation : HH.text "3d_rotation"
  , accessibility : HH.text "accessibility"
  , accessibile : HH.text "accessibile"
  , accountBalance : HH.text "accoun_balance"
  , accountBalanceWallet : HH.text "accoun_balance_wallet"
  , accountBox : HH.text "account_box"
  , accountCircle : HH.text "account_circle"
  , addShoppingCart : HH.text "add_shopping_cart"
  , alarm : HH.text "alarm"
  }
