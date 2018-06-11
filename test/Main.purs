module Test.Main where

import Prelude

import Effect (Effect)
import Test.Network.RemoteDataTest as RemoteData
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  RemoteData.tests
