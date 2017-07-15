module Route where

import Prelude

data Route
  = Home
  | Badges
  | Buttons

urlSegment :: Route -> String
urlSegment Home = "home"
urlSegment Badges = "badges"
urlSegment Buttons = "buttons"

href :: Route -> String
href route = "#/" <> urlSegment route

label :: Route -> String
label = show

instance showRoute :: Show Route where
show Home = "Home"
show Badges = "Badges"
show Buttons = "Buttons"
