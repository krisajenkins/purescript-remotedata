module Test.Network.RemoteDataTest (tests) where

import Prelude

import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (lmap, rmap)
import Data.Bitraversable (bitraverse)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Network.RemoteData (RemoteData(..), isFailure, isSuccess)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, assertFalse, equal)

tests :: TestSuite
tests = do
  suite "RemoteData" do
    test "Applicative" do
      equal ((*) <$> Success 5 <*> Success 2)
        (Success 10 :: RemoteData String Int)
      equal ((*) <$> Failure "Left" <*> Failure "Right")
        (Failure "Left" :: RemoteData String Int)
    test "isSuccess" do
      assert "Success is Success" $ isSuccess (Success 5)
      assertFalse "Failure is not Success" $ isSuccess (Failure "err")
    test "isFailure" do
      assert "Failure is Failure" $ isFailure (Failure "err")
      assertFalse "Success is not Failure" $ isFailure (Success 5)
    test "Show" do
      equal (show (NotAsked :: RemoteData String Int)) "RemoteData.NotAsked"
      equal (show (Loading :: RemoteData String Int)) "RemoteData.Loading"
      equal (show (Failure "Error!" :: RemoteData String Int)) "RemoteData.Failure \"Error!\""
      equal (show (Success 5 :: RemoteData String Int)) "RemoteData.Success 5"
    test "Bifunctor" do
      equal (Success 10) (rmap ((*) 2) (Success 5 :: RemoteData String Int))
      equal (Failure "SEGFAULT!") (lmap (flip append "!") (Failure "SEGFAULT" :: RemoteData String Int))
    test "Foldable" do
      equal "Test" (foldMap identity (Success "Test"))
      equal "" (foldMap identity (Failure "Error"))
      equal "" (foldMap identity NotAsked)
      equal "" (foldMap identity Loading)
    test "Traversable"
      let aTraversal :: RemoteData String Int -> Maybe (RemoteData String Int)
          aTraversal = traverse pure
      in do equal (Just (Success 7)) (aTraversal (Success 7))
            equal (Just (Failure "error")) (aTraversal (Failure "error"))
            equal (Just (Loading)) (aTraversal Loading)
            equal (Just (NotAsked)) (aTraversal NotAsked)
    test "Bifoldable"
      let aBifold = bifoldMap (pure <<< show) pure
      in do equal ["Test"] (aBifold (Success "Test"))
            equal ["5"] (aBifold (Failure 5))
            equal [] (aBifold Loading)
            equal [] (aBifold NotAsked)
    test "Bitraversable" do
      let aBitraversal :: RemoteData String Int -> Maybe (RemoteData String String)
          aBitraversal = bitraverse pure (pure <<< show)
      equal (Just (Success "7")) (aBitraversal (Success 7))
      equal (Just (Failure "error")) (aBitraversal (Failure "error"))
      equal (Just (Loading)) (aBitraversal Loading)
      equal (Just (NotAsked)) (aBitraversal NotAsked)
