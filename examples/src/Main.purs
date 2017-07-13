module Main where

import Prelude
import Control.Monad.Aff (forkAff)
import Control.Monad.Eff (Eff)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Router as Router

main :: ∀ eff. Eff (HA.HalogenEffects eff) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  driver <- runUI Router.component unit body
  forkAff $ Router.routeSignal driver
