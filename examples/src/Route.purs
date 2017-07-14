module Route where

import Prelude

data Route
  = Home
  | Badges
  | Buttons

path :: Route -> String
path Home = "home"
path Badges = "badges"
path Buttons = "buttons"

href :: Route -> String
href route = "#/" <> path route

label :: Route -> String
label = show

instance showRoute :: Show Route where
show Home = "Home"
show Badges = "Badges"
show Buttons = "Buttons"
