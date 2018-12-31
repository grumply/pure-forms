{-# LANGUAGE GeneralizedNewtypeDeriving, NoDeriveAnyClass, CPP, AllowAmbiguousTypes,
             RankNTypes, ViewPatterns, TypeApplications, ScopedTypeVariables,
             MultiParamTypeClasses, FlexibleContexts #-}
module Pure.Forms
  ( module Pure.Forms
  , (<$|), (=<|), (|>=), (=<||>), (=<||>=)
  ) where

import Pure hiding (get)
import Pure.Data.Txt.Trie as Trie
import Pure.Data.JSON

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.State (StateT,evalStateT)
import Control.Monad.State.Class

import Data.Maybe
import Data.Typeable

type Trie phantom = TxtTrie Value

newtype FormStore = FormStore { unFormStore :: SRef (TxtTrie Value) }
newtype FormM ty m a = FormM { unFormM :: StateT FormStore (PureM (Trie ty) m) a }
  deriving (Functor,Applicative,Monad,Alternative,MonadPlus)

type FormIO ty a = FormM ty IO a

instance Monad m => MonadState FormStore (FormM ty m) where
  {-# INLINE get #-}
  get = FormM get
  {-# INLINE put #-}
  put s = FormM (put s)

instance MonadIO m => MonadIO (FormM ty m) where
  {-# INLINE liftIO #-}
  liftIO = FormM . liftIO

instance MonadTrans (FormM ty) where
  {-# INLINE lift #-}
  lift = FormM . lift . lift

instance MonadFix m => MonadFix (FormM ty m) where
  {-# INLINE mfix #-}
  mfix = FormM . mfix . (unFormM .)

formWith :: forall ty m. (Monad m,Typeable m) => (forall a. m a -> IO a) -> FormM ty m View -> View
formWith g f = runPureWith g mempty $ \(FormStore -> st) -> evalStateT (unFormM f) st

form :: FormIO ty View -> View
form f = runPureWithIO mempty $ \(FormStore -> st) -> evalStateT (unFormM f) st

value :: forall a m. (MonadIO m, MonadState FormStore m, FromJSON a) => Txt -> m (Maybe a)
value k = do
  FormStore ref <- get
  store <- getWith ref
  case Trie.lookup k store of
    Just (fromJSON -> Success a) -> pure (Just a)
    _ -> pure Nothing

store :: forall a m. (MonadIO m, MonadState FormStore m, ToJSON a) => Txt -> a -> m ()
store k v = get >>= \st -> storeWith st k v

valueWith :: forall a m. (MonadIO m, FromJSON a) => FormStore -> Txt -> m (Maybe a)
valueWith (FormStore ref) f = do
  m <- getWith ref
  case Trie.lookup f m of
    Just (fromJSON -> Success a) -> pure (Just a)
    _ -> pure Nothing

storeWith :: forall a m. (MonadIO m, ToJSON a) => FormStore -> Txt -> a -> m ()
storeWith (FormStore ref) k v = modifyWith ref (Trie.insert k (toJSON v))

field :: forall ty a m. (MonadIO m, ToJSON a, FromJSON a)
      => Txt -> a -> ((a -> IO ()) -> FormM ty m View) -> FormM ty m (View,a)
field k def f = get >>= \st -> do
  ma <- value k
  v <- f (storeWith st k)
  pure (v,fromMaybe def ma)
