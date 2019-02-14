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

data FormState ty = FormState 
  { formStateTy :: ty
  , formStateStore :: !(TxtTrie Value)
  }
newtype FormRef ty = FormRef { unFormRef :: SRef (FormState ty) }
newtype FormM ty a = FormM { unFormM :: StateT (FormRef ty) (PureM (FormState ty)) a }
  deriving (Functor,Applicative,Monad,Alternative,MonadPlus)

instance MonadState (FormRef ty) (FormM ty) where
  {-# INLINE get #-}
  get = FormM get
  {-# INLINE put #-}
  put s = FormM (put s)

instance MonadIO (FormM ty) where
  {-# INLINE liftIO #-}
  liftIO = FormM . liftIO

instance MonadFix (FormM ty) where
  {-# INLINE mfix #-}
  mfix = FormM . mfix . (unFormM .)

form :: Typeable ty => ty -> FormM ty View -> View
form ty f = runPure (FormState ty mempty) $ do
  sref <- askSRef
  evalStateT (unFormM f) (FormRef sref)

value :: forall a ty. FromJSON a => Txt -> FormM ty (Maybe a)
value k = do
  FormRef ref <- get
  FormState _ store <- readSRef ref
  case Trie.lookup k store of
    Just (fromJSON -> Success a) -> pure (Just a)
    _ -> pure Nothing

store :: forall a ty. ToJSON a => Txt -> a -> FormM ty ()
store k v = get >>= \st -> storeWith st k v

valueWith :: forall a ty m. (MonadIO m, FromJSON a) => FormRef ty -> Txt -> m (Maybe a)
valueWith (FormRef ref) f = do
  FormState _ store <- readSRef ref
  case Trie.lookup f store of
    Just (fromJSON -> Success a) -> pure (Just a)
    _ -> pure Nothing

storeWith :: forall a ty m. (MonadIO m, ToJSON a) => FormRef ty -> Txt -> a -> m ()
storeWith (FormRef ref) k v = 
  modifySRef ref $ \(FormState ty st) -> 
    FormState ty (Trie.insert k (toJSON v) st)

field :: forall ty a. (ToJSON a, FromJSON a)
      => Txt -> a -> ((a -> IO ()) -> FormM ty View) -> FormM ty (View,a)
field k def f = get >>= \st -> do
  ma <- value k
  v <- f (storeWith st k)
  pure (v,fromMaybe def ma)
