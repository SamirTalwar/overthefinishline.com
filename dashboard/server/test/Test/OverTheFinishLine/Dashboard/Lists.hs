{-# LANGUAGE ScopedTypeVariables #-}

module Test.OverTheFinishLine.Dashboard.Lists where

import Data.Functor.Identity
import Test.Hspec

import OverTheFinishLine.Dashboard.Lists

spec :: SpecWith ()
spec = do
  describe "groupQueryBy" $
    it "groups by a key function and maps the values" $ do
      let list :: [Int] = [1 .. 10]
      let expected = [(0, [20, 40 .. 100]), (1, [10, 30 .. 100])]
      let actual = groupQueryBy (`mod` 2) (* 10) list
      actual `shouldBe` expected

  describe "liftedGroupQueryBy" $
    it "groups by a monadic key function and maps the values monadically" $ do
      let list :: [Int] = [1 .. 10]
      let expected = [(0, [20, 40 .. 100]), (1, [10, 30 .. 100])]
      let actual = runIdentity $ liftedGroupQueryBy (,) (Identity . (`mod` 2)) (Identity . (* 10)) list
      actual `shouldBe` expected
