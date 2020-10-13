{-# LANGUAGE GeneralizedNewtypeDeriving, NoDeriveAnyClass, CPP, AllowAmbiguousTypes,
             RankNTypes, ViewPatterns, TypeApplications, ScopedTypeVariables,
             MultiParamTypeClasses, FlexibleContexts #-}
module Pure.Forms where

import Pure hiding (get)
import Pure.State
import Pure.Data.Txt.Trie as Trie
import Pure.Data.JSON

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.State (StateT,evalStateT)

import Data.Maybe
import Data.Typeable

data FormState ty = FormState 
  { formStateTy :: ty
  , formStateStore :: !(TxtTrie Value)
  }
newtype FormM ty a = FormM { unFormM :: PureM (FormState ty) a }
  deriving (Functor,Applicative,Monad,Alternative,MonadPlus,MonadIO,MonadFix,MonadSRef (FormState ty))

form :: Typeable ty => ty -> FormM ty View -> View
form ty = runPure (FormState ty mempty) . unFormM

value :: forall a ty. FromJSON a => Txt -> FormM ty (Maybe a)
value k = do
  FormState _ store <- get
  case Trie.lookup k store of
    Just (fromJSON -> Success a) -> pure (Just a)
    _ -> pure Nothing

store :: forall a ty. ToJSON a => Txt -> a -> FormM ty ()
store k v = sref >>= \st -> storeWith st k v

valueWith :: forall a ty m. (MonadIO m, FromJSON a) => SRef (FormState ty) -> Txt -> m (Maybe a)
valueWith ref f = do
  FormState _ store <- readSRef ref
  case Trie.lookup f store of
    Just (fromJSON -> Success a) -> pure (Just a)
    _ -> pure Nothing

storeWith :: forall a ty m. (MonadIO m, ToJSON a) => SRef (FormState ty) -> Txt -> a -> m ()
storeWith ref k v = 
  modifySRef ref $ \(FormState ty st) -> 
    FormState ty (Trie.insert k (toJSON v) st)

field :: forall ty a. (ToJSON a, FromJSON a)
      => Txt -> a -> ((a -> IO ()) -> FormM ty View) -> FormM ty (View,a)
field k def f = 
  sref >>= \st -> do
    ma <- value k
    v <- f (storeWith st k)
    pure (v,fromMaybe def ma)
