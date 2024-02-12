module Test.Network.RemoteData (spec) where

import Prelude
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (lmap, rmap)
import Data.Bitraversable (bitraverse)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Network.RemoteData (RemoteData(..), isFailure, isSuccess)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotSatisfy, shouldSatisfy)

spec :: Spec Unit
spec = do
  describe "RemoteData" do
    applicativeSpec
    predicatesSpec
    showSpec
    bifunctorSpec
    foldableSpec
    travserableSpec
    bifoldableSpec
    bitraversableSpec

applicativeSpec :: Spec Unit
applicativeSpec = do
  describe "Applicative" do
    it "apply Failure,Failure"
      $ shouldEqual
          ((*) <$> Failure "Left" <*> Failure "Right")
          (Failure "Left" :: RemoteData String Int)
    it "apply Failure,Success"
      $ shouldEqual
          ((*) <$> Failure "Left" <*> Success 2)
          (Failure "Left")
    it "apply Success,Failure"
      $ shouldEqual
          ((*) <$> Success 5 <*> Failure "Right")
          (Failure "Right")
    it "apply Success,Success"
      $ shouldEqual
          ((*) <$> Success 5 <*> Success 2)
          (Success 10 :: RemoteData String Int)

predicatesSpec :: Spec Unit
predicatesSpec = do
  describe "predicates" do
    it "Success isSuccess"
      $ shouldSatisfy (Success 5 :: RemoteData String Int) isSuccess
    it "Failure not isSuccess"
      $ shouldNotSatisfy (Failure "err" :: RemoteData String Int) isSuccess
    it "Failure isFailure"
      $ shouldSatisfy (Failure "err" :: RemoteData String Int) isFailure
    it "Success not isFailure"
      $ shouldNotSatisfy (Success 5 :: RemoteData String Int) isFailure

showSpec :: Spec Unit
showSpec = do
  describe "Show" do
    it "show NotAsked"
      $ shouldEqual (show (NotAsked :: RemoteData String Int)) "RemoteData.NotAsked"
    it "show Loading"
      $ shouldEqual (show (Loading :: RemoteData String Int)) "RemoteData.Loading"
    it "show Failure"
      $ shouldEqual (show (Failure "Error!" :: RemoteData String Int)) "RemoteData.Failure \"Error!\""
    it "show Success"
      $ shouldEqual (show (Success 5 :: RemoteData String Int)) "RemoteData.Success 5"

bifunctorSpec :: Spec Unit
bifunctorSpec = do
  describe "Bifunctor" do
    it "lmap"
      $ shouldEqual (Failure "SEGFAULT!") (lmap (flip append "!") (Failure "SEGFAULT" :: RemoteData String Int))
    it "rmap"
      $ shouldEqual (Success 10) (rmap ((*) 2) (Success 5 :: RemoteData String Int))

foldableSpec :: Spec Unit
foldableSpec = do
  describe "Foldable" do
    it "fold NotAsked"
      $ shouldEqual "" (foldMap identity NotAsked)
    it "fold Loading"
      $ shouldEqual "" (foldMap identity Loading)
    it "fold Failure"
      $ shouldEqual "" (foldMap identity (Failure "Error"))
    it "fold Success"
      $ shouldEqual "It" (foldMap identity (Success "It"))

travserableSpec :: Spec Unit
travserableSpec = do
  describe "Traversable" do
    let
      aTraversal :: RemoteData String Int -> Maybe (RemoteData String Int)
      aTraversal = traverse pure
    it "traverse NotAsked"
      $ shouldEqual (Just (NotAsked)) (aTraversal NotAsked)
    it "traverse Loading"
      $ shouldEqual (Just (Loading)) (aTraversal Loading)
    it "traverse Failure"
      $ shouldEqual (Just (Failure "error")) (aTraversal (Failure "error"))
    it "traverse Success"
      $ shouldEqual (Just (Success 7)) (aTraversal (Success 7))

bifoldableSpec :: Spec Unit
bifoldableSpec = do
  describe "Bifoldable" do
    let
      aBifold = bifoldMap (pure <<< show) pure
    it "bifold NotAsked"
      $ shouldEqual [] (aBifold NotAsked)
    it "bifold Loading"
      $ shouldEqual [] (aBifold Loading)
    it "bifold Failure"
      $ shouldEqual [ "5" ] (aBifold (Failure 5))
    it "bifold Success"
      $ shouldEqual [ "It" ] (aBifold (Success "It"))

bitraversableSpec :: Spec Unit
bitraversableSpec = do
  describe "Bitraversable" do
    let
      aBitraversal :: RemoteData String Int -> Maybe (RemoteData String String)
      aBitraversal = bitraverse pure (pure <<< show)
    it "bitraverse NotAsked"
      $ shouldEqual (Just (NotAsked)) (aBitraversal NotAsked)
    it "bitraverse Loading"
      $ shouldEqual (Just (Loading)) (aBitraversal Loading)
    it "bitraverse Failure"
      $ shouldEqual (Just (Failure "error")) (aBitraversal (Failure "error"))
    it "bitraverse Success"
      $ shouldEqual (Just (Success "7")) (aBitraversal (Success 7))
