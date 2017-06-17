module Test.Network.RemoteDataTest (tests) where

import Prelude

import Data.Bifunctor (lmap, rmap)
import Network.RemoteData (RemoteData(..), isFailure, isRefreshing, isSuccess)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, assertFalse, equal)

tests :: forall e. TestSuite e
tests = do
  suite "RemoteData" do
    test "Applicative" do
      equal ((*) <$> Success 5 <*> Success 2)
        (Success 10 :: RemoteData String Int)
      equal ((*) <$> Refreshing 5 <*> Refreshing 2)
        (Refreshing 10 :: RemoteData String Int)
      equal ((*) <$> Success 5 <*> Refreshing 2)
        (Success 10 :: RemoteData String Int)
      equal ((*) <$> Refreshing 5 <*> Success 2)
        (Success 10 :: RemoteData String Int)
      equal ((*) <$> Failure "Left" <*> Failure "Right")
        (Failure "Left" :: RemoteData String Int)
    test "isSuccess" do
      assert "Success is Success" $ isSuccess (Success 5)
      assertFalse "Failure is not Success" $ isSuccess (Failure "err")
    test "isRefreshing" do
      assert "Refreshing is Refreshing" $ isRefreshing (Refreshing 5)
      assertFalse "Failure is not Refreshing" $ isRefreshing (Failure "err")
    test "isFailure" do
      assert "Failure is Failure" $ isFailure (Failure "err")
      assertFalse "Success is not Failure" $ isFailure (Success 5)
      assertFalse "Refreshing is not Failure" $ isFailure (Refreshing 5)
    test "show" do
      equal (show (NotAsked :: RemoteData String Int)) "RemoteData.NotAsked"
      equal (show (Loading :: RemoteData String Int)) "RemoteData.Loading"
      equal (show (Failure "Error!" :: RemoteData String Int)) "RemoteData.Failure \"Error!\""
      equal (show (Success 5 :: RemoteData String Int)) "RemoteData.Success 5"
      equal (show (Refreshing 5 :: RemoteData String Int)) "RemoteData.Refreshing 5"
    test "bifunctor" do
      equal (Success 10) (rmap ((*) 2) (Success 5 :: RemoteData String Int))
      equal (Refreshing 10) (rmap ((*) 2) (Refreshing 5 :: RemoteData String Int))
      equal (Failure "SEGFAULT!") (lmap (flip append "!") (Failure "SEGFAULT" :: RemoteData String Int))
