module Route where

import Prelude

data Route
  = Home
  | Badges
  | Buttons
  | Cards
  | Chips
  | Dialogs
  | Lists
  | Progress
  | Tabs

urlSegment :: Route -> String
urlSegment Home = "home"
urlSegment Badges = "badges"
urlSegment Buttons = "buttons"
urlSegment Cards = "cards"
urlSegment Chips = "chips"
urlSegment Dialogs = "dialogs"
urlSegment Lists = "lists"
urlSegment Progress = "progress"
urlSegment Tabs = "tabs"

href :: Route -> String
href route = "#/" <> urlSegment route

label :: Route -> String
label = show

instance showRoute :: Show Route where
show Home = "Home"
show Badges = "Badges"
show Buttons = "Buttons"
show Cards = "Cards"
show Chips = "Chips"
show Dialogs = "Dialogs"
show Lists = "Lists"
show Progress = "Progress"
show Tabs = "Tabs"
