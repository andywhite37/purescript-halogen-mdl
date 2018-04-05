module Main where

import Prelude
import Control.Monad.Eff (Eff)

import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Control.Coroutine as CR


import Router as Router
import Route as Route

main :: ∀ eff. Eff (HA.HalogenEffects eff) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  driver <- runUI Router.component unit body
  driver.query $ H.action $ Router.GoTo Route.Home
  CR.runProcess (Router.hashChangeProducer CR.$$ Router.hashChangeConsumer driver.query)
