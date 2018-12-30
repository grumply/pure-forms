{-# LANGUAGE GeneralizedNewtypeDeriving, NoDeriveAnyClass, CPP, AllowAmbiguousTypes,
             RankNTypes, ViewPatterns, TypeApplications, ScopedTypeVariables,
             MultiParamTypeClasses, FlexibleContexts #-}
module Pure.Forms where

import Pure.State
import Pure.Data.Txt.Trie as Trie
import Pure.Data.JSON

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.State (StateT,evalStateT)
import Control.Monad.State.Class

import Data.Typeable

newtype FormStore = FormStore { unFormStore :: Ref (TxtTrie Value) }
newtype FormM m a = FormM { unFormM :: StateT FormStore (PureM (TxtTrie Value) m) a }
  deriving (Functor,Applicative,Monad,Alternative,MonadPlus)

type Form = FormM IO View

instance Monad m => MonadState FormStore (FormM m) where
  {-# INLINE get #-}
  get = FormM get
  {-# INLINE put #-}
  put s = FormM (put s)

instance MonadIO m => MonadIO (FormM m) where
  {-# INLINE liftIO #-}
  liftIO = FormM . liftIO

instance MonadTrans FormM where
  {-# INLINE lift #-}
  lift = FormM . lift . lift

instance MonadFix m => MonadFix (FormM m) where
  {-# INLINE mfix #-}
  mfix = FormM . mfix . (unFormM .)

value :: forall a m. (MonadIO m, MonadState FormStore m, FromJSON a) => Txt -> m (Maybe a)
value k = do
  FormStore ref <- get
  store <- getWith ref
  case Trie.lookup k store of
    Just (fromJSON -> Success a) -> pure (Just a)
    _ -> pure Nothing

store :: forall a m. (MonadIO m, ToJSON a) => FormStore -> Txt -> a -> m ()
store (FormStore ref) k v = modifyWith ref (Trie.insert k (toJSON v))

valueFrom :: forall a m. (MonadIO m, FromJSON a) => FormStore -> Txt -> m (Maybe a)
valueFrom (FormStore ref) f = do
  m <- getWith ref
  case Trie.lookup f m of
    Just (fromJSON -> Success a) -> pure (Just a)
    _ -> pure Nothing

formWith :: (Monad m,Typeable m) => (forall a. m a -> IO a) -> FormM m View -> View
formWith g f = runPureWith g mempty $ \(FormStore -> st) -> evalStateT (unFormM f) st

form :: Form -> View
form f = runPureWithIO mempty $ \(FormStore -> st) -> evalStateT (unFormM f) st

field :: Monad m => (FormStore -> FormM m View) -> FormM m View
field = (=<< get)

-- example usage; more combinators to be added
input :: forall a m. (ToJSON a, FromTxt a, Monad m) => Txt -> FormM m View
input k = field $ \st -> Input =<| OnInput (withInput (store @a st k . fromTxt))
