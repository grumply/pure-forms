{-# LANGUAGE GeneralizedNewtypeDeriving, NoDeriveAnyClass, CPP, AllowAmbiguousTypes,
             RankNTypes, ViewPatterns, TypeApplications, ScopedTypeVariables,
             MultiParamTypeClasses, FlexibleContexts #-}
module Pure.Forms where

import Pure hiding (get)
import Pure.Data.Txt.Trie as Trie
import Pure.Data.JSON

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.State.Class as State
import Control.Monad.Trans
import Control.Monad.Trans.State (StateT,evalStateT)

import Data.Maybe
import Data.Typeable

newtype FormState ty = FormState { unFormState :: (ty,TxtTrie Value) }
newtype FormRef ty = FormRef { unFormRef :: SRef (FormState ty) }
newtype FormM ty m a = FormM { unFormM :: StateT (FormRef ty) (PureM (FormState ty) m) a }
  deriving (Functor,Applicative,Monad,Alternative,MonadPlus)

type FormIO ty a = FormM ty IO a

instance Monad m => MonadState (FormRef ty) (FormM ty m) where
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

formWith :: forall ty m. (Monad m, Typeable ty, Typeable m) => ty -> (forall a. m a -> IO a) -> FormM ty m View -> View
formWith ty g f = runPureWith g (FormState (ty,mempty)) $ \(FormRef -> st) -> evalStateT (unFormM f) st

form :: Typeable ty => ty -> FormIO ty View -> View
form ty f = runPureWithIO (FormState (ty,mempty)) $ \(FormRef -> st) -> evalStateT (unFormM f) st

value :: forall a ty m. (MonadIO m, FromJSON a) => Txt -> FormM ty m (Maybe a)
value k = do
  FormRef ref <- get
  FormState (_,store) <- getWith ref
  case Trie.lookup k store of
    Just (fromJSON -> Success a) -> pure (Just a)
    _ -> pure Nothing

store :: forall a ty m. (MonadIO m, ToJSON a) => Txt -> a -> FormM ty m ()
store k v = get >>= \st -> storeWith st k v

valueWith :: forall a ty m. (MonadIO m, FromJSON a) => FormRef ty -> Txt -> m (Maybe a)
valueWith (FormRef ref) f = do
  FormState (_,store) <- getWith ref
  case Trie.lookup f store of
    Just (fromJSON -> Success a) -> pure (Just a)
    _ -> pure Nothing

storeWith :: forall a ty m. (MonadIO m, ToJSON a) => FormRef ty -> Txt -> a -> m ()
storeWith (FormRef ref) k v = modifyWith ref (FormState . fmap (Trie.insert k (toJSON v)) . unFormState)

field :: forall ty a m. (MonadIO m, ToJSON a, FromJSON a)
      => Txt -> a -> ((a -> IO ()) -> FormM ty m View) -> FormM ty m (View,a)
field k def f = get >>= \st -> do
  ma <- value k
  v <- f (storeWith st k)
  pure (v,fromMaybe def ma)
