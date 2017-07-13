module Route where

import Prelude

data Route
  = Home
  | Badges
  | Buttons

instance showRoute :: Show Route where
show Home = "Home"
show Badges = "Badges"
show Buttons = "Buttons"
