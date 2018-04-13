module Network.RemoteData where

import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Monad (class Monad)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Function (const, id)
import Data.Functor (class Functor)
import Data.Generic (class Generic)
import Data.Lens (Prism', is, prism)
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Show (class Show, show)
import Data.Unit (Unit, unit)

-- | A datatype representing fetched data.
-- |
-- | If you find yourself continually using `Maybe (Either e a)` to
-- | represent data loaded from an external source, or you have a
-- | habit of shuffling errors away to where they can be quietly
-- | ignored, consider using this. It makes it easier to represent the
-- | real state of a remote data fetch and handle it properly.
-- |
-- | For more on the motivation, take a look at the blog post
-- | [How Elm Slays A UI Antipattern](http://blog.jenkster.com/2016/06/how-elm-slays-a-ui-antipattern.html).
-- | This is a port of that original Elm module.
data RemoteData e a
  = NotAsked
  | Loading
  | Failure e
  | Success a
  | Refreshing a

derive instance genericRemoteData :: (Generic e, Generic a) => Generic (RemoteData e a)

derive instance eqRemoteData :: (Eq e, Eq a) => Eq (RemoteData e a)

derive instance functorRemoteData :: Functor (RemoteData e)

instance showRemoteData :: (Show e, Show a) => Show (RemoteData e a) where
  show NotAsked = "RemoteData.NotAsked"
  show Loading = "RemoteData.Loading"
  show (Failure err) = "RemoteData.Failure " <> show err
  show (Success value) = "RemoteData.Success " <> show value
  show (Refreshing value) = "RemoteData.Refreshing " <> show value

-- | Maps functions to the `Failure` and `Success` values.
instance bifunctorRemoteData :: Bifunctor RemoteData where
  bimap _ _ NotAsked = NotAsked
  bimap _ _ Loading = Loading
  bimap f _ (Failure err) = Failure (f err)
  bimap _ g (Success value) = Success (g value)
  bimap _ g (Refreshing value) = Refreshing (g value)

-- | If both values are `Success`, the function is applied.
-- | If both values are `Refreshing`, the function is applied.
-- | If both are `Failure`, the first failure is returned.
instance applyRemoteData :: Apply (RemoteData e) where
  apply (Success f) (Success value) = Success (f value)
  apply (Failure err) _ = Failure err
  apply _ (Failure err) = Failure err
  apply NotAsked _ = NotAsked
  apply _ NotAsked = NotAsked
  apply Loading _ = Loading
  apply _ Loading = Loading
  apply (Refreshing f) (Refreshing value) = Refreshing (f value)
  apply (Refreshing f) (Success value) = Success (f value)
  apply (Success f) (Refreshing value) = Success (f value)

instance bindRemoteData :: Bind (RemoteData e) where
  bind NotAsked _ = NotAsked
  bind Loading _ = Loading
  bind (Failure err) _ = (Failure err)
  bind (Success value) f = f value
  bind (Refreshing value) f = f value

instance applicativeRemoteData :: Applicative (RemoteData e) where
  pure value = Success value

instance monadRemoteData :: Monad (RemoteData e)

instance monadThrowRemoteData :: MonadThrow e (RemoteData e) where
  throwError = Failure

instance monadErrorRemoteData :: MonadError e (RemoteData e) where
  catchError (Failure e) f = f e
  catchError (Success value) _ = Success value
  catchError NotAsked _ = NotAsked
  catchError Loading _ = Loading
  catchError (Refreshing value) _ = Refreshing value

------------------------------------------------------------

-- | Convert a `RemoteData` to a `Maybe`.
toMaybe :: forall e a. RemoteData e a -> Maybe a
toMaybe (Success value) = Just value
toMaybe _ = Nothing

-- | Convert a `Maybe` to `RemoteData`.
fromMaybe :: forall e a. Maybe a -> RemoteData e a
fromMaybe Nothing = NotAsked
fromMaybe (Just value) = Success value

-- | Convert an `Either` to `RemoteData`
fromEither :: forall e a. Either e a -> RemoteData e a
fromEither (Left err) = Failure err
fromEither (Right value) = Success value

-- | Takes a default value, a function, and a `RemoteData` value. If
-- | the data is `Success`, apply the function to the value, otherwise
-- | return the default.
-- |
-- | See also `withDefault`.
maybe :: forall e a b. b -> (a -> b) -> RemoteData e a -> b
maybe default' f (Success value) = f value
maybe default' f _ = default'

-- | If the `RemoteData` has been successfully loaded, return that,
-- | otherwise return a default value.
withDefault :: forall e a. a -> RemoteData e a -> a
withDefault default' = maybe default' id

------------------------------------------------------------
-- Prisms & Lenses (oh my!)

_NotAsked :: forall a e. Prism' (RemoteData e a) Unit
_NotAsked = prism (const NotAsked) unwrap
  where
    unwrap NotAsked = Right unit
    unwrap y = Left y

_Loading :: forall a e. Prism' (RemoteData e a) Unit
_Loading = prism (const Loading) unwrap
  where
    unwrap Loading = Right unit
    unwrap y = Left y

_Failure :: forall a e. Prism' (RemoteData e a) e
_Failure = prism Failure unwrap
  where
    unwrap (Failure x) = Right x
    unwrap y = Left y

_Success :: forall a e. Prism' (RemoteData e a) a
_Success = prism Success unwrap
  where
    unwrap (Success x) = Right x
    unwrap y = Left y

_Refreshing :: forall a e. Prism' (RemoteData e a) a
_Refreshing = prism Refreshing unwrap
  where
    unwrap (Refreshing x) = Right x
    unwrap y = Left y

------------------------------------------------------------

-- | Simple predicate.
isNotAsked :: forall e a. RemoteData e a -> Boolean
isNotAsked = is _NotAsked

-- | Simple predicate.
isLoading :: forall e a. RemoteData e a -> Boolean
isLoading = is _Loading

-- | Simple predicate.
isFailure :: forall e a. RemoteData e a -> Boolean
isFailure = is _Failure

-- | Simple predicate.
isSuccess :: forall e a. RemoteData e a -> Boolean
isSuccess = is _Success

-- | Simple predicate.
isRefreshing :: forall e a. RemoteData e a -> Boolean
isRefreshing = is _Refreshing
