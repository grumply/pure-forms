{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, NoMonomorphismRestriction, FlexibleContexts, PartialTypeSignatures, ApplicativeDo, ConstraintKinds, RankNTypes, UndecidableInstances, TypeApplications, ExistentialQuantification, PatternSynonyms, ViewPatterns, TypeFamilies, LambdaCase, RecursiveDo, FlexibleInstances, MultiParamTypeClasses #-}
module Pure.Forms where

import Pure hiding (Text,Result,Input,text,modify,Name,Display,display,get,ask,Left,Right)
import qualified Pure

import qualified Data.Map as Map

import Control.Monad.State as St

import Data.Hashable

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Data
import Data.List as List
import Data.Maybe
import Data.Typeable
import Text.Read hiding (lift,get)

import Data.String

import Prelude hiding (log)

import Unsafe.Coerce

-- | A `Task` is a reified reader of an accessor function for the same context.
newtype Task c k v x = Task { run :: forall f. c f => (k -> f v) -> f x }

-- | Apply a task to an accessor and wrap the result in a `Task`.
task :: (forall f. c f => (k -> f v) -> f x) -> Task c k v x
task t = Task (t $)

-- | Apply an accessor to a key and wrap the result in a `Task`.
fetch :: k -> Task c k v v
fetch t = task ($ t)

instance Functor (Task Functor k v) where
    fmap f (Task a) = Task (fmap f . a)

instance Functor (Task Applicative k v) where
    fmap f (Task a) = Task (fmap f . a)

instance Functor (Task Monad k v) where
    fmap f (Task a) = Task (fmap f . a)

instance Functor (Task Alternative k v) where
    fmap f (Task a) = Task (fmap f . a)

instance Functor (Task MonadPlus k v) where
    fmap f (Task a) = Task (fmap f . a)

-- instance Functor (Task MonadFix k v) where
--     fmap f (Task a) = Task (fmap f . a)

instance Functor (Task (MonadReader r) k v) where
    fmap f (Task a) = Task (fmap f . a)

instance Functor (Task (MonadWriter w) k v) where
    fmap f (Task a) = Task (fmap f . a)

instance Functor (Task (MonadState s) k v) where
    fmap f (Task a) = Task (fmap f . a)

instance Functor (Task MonadIO k v) where
    fmap f (Task a) = Task (fmap f . a)

instance Functor (Task Applicative k v) => Applicative (Task Applicative k v) where
    pure a = Task $ \_ -> pure a    
    (<*>) (Task fab) (Task fa) = Task $ \f -> ($) <$> fab f <*> fa f

instance Functor (Task Monad k v) => Applicative (Task Monad k v) where
    pure a = Task (const (pure a))
    (<*>) (Task fab) (Task fa) = Task $ \f -> ($) <$> fab f <*> fa f

instance Functor (Task Alternative k v) => Applicative (Task Alternative k v) where
    pure a = Task (const (pure a))
    (<*>) (Task fab) (Task fa) = Task $ \f -> ($) <$> fab f <*> fa f

-- instance Functor (Task MonadFix k v) => Applicative (Task MonadFix k v) where
--     pure a = Task (const (pure a))
--     (<*>) (Task fab) (Task fa) = Task $ \f -> ($) <$> fab f <*> fa f

instance Functor (Task (MonadReader r) k v) => Applicative (Task (MonadReader r) k v) where
    pure a = Task (const (pure a))
    (<*>) (Task fab) (Task fa) = Task $ \f -> ($) <$> fab f <*> fa f

instance Functor (Task (MonadWriter w) k v) => Applicative (Task (MonadWriter w) k v) where
    pure a = Task (const (pure a))
    (<*>) (Task fab) (Task fa) = Task $ \f -> ($) <$> fab f <*> fa f

instance Functor (Task (MonadState s) k v) => Applicative (Task (MonadState s) k v) where
    pure a = Task (const (pure a))
    (<*>) (Task fab) (Task fa) = Task $ \f -> ($) <$> fab f <*> fa f

instance Functor (Task MonadIO k v) => Applicative (Task MonadIO k v) where
    pure a = Task (const (pure a))
    (<*>) (Task fab) (Task fa) = Task $ \f -> ($) <$> fab f <*> fa f

instance Functor (Task MonadPlus k v) => Applicative (Task MonadPlus k v) where
    pure a = Task (const (pure a))
    (<*>) (Task fab) (Task fa) = Task $ \f -> ($) <$> fab f <*> fa f

instance Applicative (Task Monad k v) => Monad (Task Monad k v) where
    return a = Task (const (return a))
    (>>=) (Task ta) afb = Task $ \f -> ta f >>= flip run f . afb

-- instance Applicative (Task MonadFix k v) => Monad (Task MonadFix k v) where
--     return a = Task (const (return a))
--     (>>=) (Task ta) afb = Task $ \f -> ta f >>= flip run f . afb

instance Applicative (Task (MonadReader r) k v) => Monad (Task (MonadReader r) k v) where
    return a = Task (const (return a))
    (>>=) (Task ta) afb = Task $ \f -> ta f >>= flip run f . afb

instance Applicative (Task (MonadWriter w) k v) => Monad (Task (MonadWriter w) k v) where
    return a = Task (const (return a))
    (>>=) (Task ta) afb = Task $ \f -> ta f >>= flip run f . afb

instance Applicative (Task (MonadState s) k v) => Monad (Task (MonadState s) k v) where
    return a = Task (const (return a))
    (>>=) (Task ta) afb = Task $ \f -> ta f >>= flip run f . afb

instance Applicative (Task MonadIO k v) => Monad (Task MonadIO k v) where
    return a = Task (const (return a))
    (>>=) (Task ta) afb = Task $ \f -> ta f >>= flip run f . afb

instance Applicative (Task MonadPlus k v) => Monad (Task MonadPlus k v) where
    return a = Task (const (return a))
    (>>=) (Task ta) afb = Task $ \f -> ta f >>= flip run f . afb

instance Applicative (Task Alternative k v) => Alternative (Task Alternative k v) where
    empty = Task (const empty)
    (<|>) (Task t1) (Task t2) = Task $ \f -> t1 f <|> t2 f

instance Applicative (Task MonadPlus k v) => Alternative (Task MonadPlus k v) where
    empty = Task (const empty)
    (<|>) (Task t1) (Task t2) = Task $ \f -> t1 f <|> t2 f

instance (Alternative (Task MonadPlus k v), Monad (Task MonadPlus k v)) => MonadPlus (Task MonadPlus k v) where
    mzero = Task (const mzero)
    mplus (Task t1) (Task t2) = Task $ \f -> mplus (t1 f) (t2 f)

-- instance MonadFix (Task MonadFix k v) where
--     mfix at = Task $ \f -> mdo
--         a <- run (at a) f
--         return a

instance (Monad (Task (MonadState i) k v)) => MonadState i (Task (MonadState i) k v) where
    get = Task (const get)
    put i = Task (const (put i))

instance (Monad (Task (MonadReader r) k v)) => MonadReader r (Task (MonadReader r) k v) where
    ask = Task (const ask)
    local f t = Task (local f . run t)

instance (Monad (Task (MonadWriter w) k v), Monoid w) => MonadWriter w (Task (MonadWriter w) k v) where
    writer aw = Task (const (writer aw))
    listen t = Task (listen . run t)
    pass t = Task (pass . run t)

data Key
    = TextKey    Txt ((Txt -> IO ()) -> View)
    | forall a. SelectKey Txt (Maybe a) ((a -> IO ()) -> View)
    | ButtonKey  Txt (IO () -> View)
    | DisplayKey Txt (IO () -> View)
    | forall a. LiftKey (IO a)

instance Hashable Key where
    hashWithSalt s (TextKey t _)    = hashWithSalt s t
    hashWithSalt s (SelectKey t _ _)= hashWithSalt s t
    hashWithSalt s (ButtonKey t _)  = hashWithSalt s t
    hashWithSalt s (DisplayKey t _) = hashWithSalt s t
    hashWithSalt _ (LiftKey _)      = 0

data Value
    = Text Txt
    | forall a. Selected (Maybe a)
    | Clicked Bool
    | forall a. Lifted a
    | Unit

type Form c = Task c Key Value

type Identifier = Txt

text_ :: Functor (Form c) => Identifier -> ((Txt -> IO ()) -> View) -> Form c Txt
text_ nm f = fmap (\(Text t) -> t) (fetch (TextKey nm f))

text :: Functor (Form c) => Identifier -> Form c Txt
text nm = text_ nm (\g -> Pure.Input <| Id nm . OnInput (withInput g))

readMaybe_ :: (Functor (Form c)) => Read a => Identifier -> ((Txt -> IO ()) -> View) -> Form c (Maybe a)
readMaybe_ nm f = fmap (Text.Read.readMaybe . fromTxt) (text_ nm f)

readMaybe :: (Read a, Functor (Form c)) => Identifier -> Form c (Maybe a)
readMaybe nm = fmap (Text.Read.readMaybe . fromTxt) (text nm)

readEither_ :: (Read a, Functor (Form c)) => Identifier -> ((Txt -> IO ()) -> View) -> Form c (Either Txt a)
readEither_ nm f = fmap (either (Left . toTxt) Right . Text.Read.readEither . fromTxt) (text_ nm f)

readEither :: (Read a, Functor (Form c)) => Identifier -> Form c (Either Txt a)
readEither nm = fmap (either (Left . toTxt) Right . Text.Read.readEither . fromTxt) (text nm)

button_ :: Functor (Form c) => Identifier -> (IO () -> View) -> Form c Bool
button_ nm f = fmap (\(Clicked b) -> b) (fetch (ButtonKey nm f))

button :: Functor (Form c) => Identifier -> Txt -> Form c Bool
button nm l = button_ nm (\f -> Button <| Id nm . Type "button" . OnClick (const f) |> [ fromTxt l ])

simpleRadio :: (Data a, Typeable a) => a -> (a -> IO ()) -> Bool -> (View,View)
simpleRadio a f b =
    let ty = toTxt $ tyConName $ typeRepTyCon (typeOf a)
        i = toTxt (show (toConstr a))
    in ( Pure.Input <| Id i . Type "radio" . Pure.Name ty . (if b then Checked "" else id) . OnClick (const (f a))
       , Label <| For i |> [ fromTxt i ]
       )

radio_ :: Functor (Form c) => Identifier -> Maybe a -> ((a -> IO ()) -> View) -> Form c (Maybe a)
radio_ nm ma f = fmap (\(Pure.Forms.Selected ma) -> unsafeCoerce ma) (fetch (SelectKey nm ma f))

radioEnum :: (Enum a, Bounded a, Data a, Typeable a, Functor (Form c)) => Identifier -> Maybe a -> Form c (Maybe a)
radioEnum nm ma = radio_ nm ma $ \f ->
        Div <| Id nm |>
            (uncurry merge $ unzip $ fmap (\a -> simpleRadio a f False) [minBound..maxBound])
    where
        merge (x:xs) (y:ys) = x : y : merge xs ys
        merge [] y = y

simpleCheckbox :: (Data a, Typeable a) => a -> (a -> IO ()) -> Bool -> (View,View)
simpleCheckbox a f b =
    let ty = toTxt $ tyConName $ typeRepTyCon (typeOf a)
        i = toTxt (show (toConstr a))
    in ( Pure.Input <| Id i . Type "checkbox" . Pure.Name ty . (if b then Checked "" else id) . OnClick (const (f a))
       , Label <| For i |> [ fromTxt i ]
       )

checkbox_ :: Functor (Form c) => Identifier -> Maybe a -> ((a -> IO ()) -> View) -> Form c (Maybe a)
checkbox_ nm ma f = fmap (\(Pure.Forms.Selected ma) -> unsafeCoerce ma) (fetch (SelectKey nm ma f))

checkboxes :: (Data a, Typeable a, Applicative (Form c)) => Identifier -> [(a,Bool)] -> Form c [a]
checkboxes nm = fmap catMaybes . sequenceA . fmap option
    where
        option (a,b) =
            radio_ nm (if b then Just a else Nothing) $ \f ->
                let (i,l) = simpleCheckbox a f b
                in Div <| Id nm |> [ l, i ]

checkboxEnum :: (Eq a, Enum a, Bounded a, Data a, Typeable a, Applicative (Form c)) => Identifier -> [a] -> Form c [a]
checkboxEnum nm selected = checkboxes nm (fmap (\x -> (x,x `List.elem` selected)) [minBound .. maxBound])

log :: (Show a, Functor (Form c)) => a -> Form c ()
log = io . print

io :: Functor (Form c) => IO a -> Form c a
io ioa = fmap (\(Lifted a) -> unsafeCoerce a) (fetch (LiftKey ioa))

display :: Functor (Form c) => Txt -> View -> Form c Value
display nm = displayWith nm . const

displayWith :: Functor (Form c) => Txt -> (IO () -> View) -> Form c Value
displayWith nm f = fetch (DisplayKey nm f)

buildForm :: (c (StateT [(Int,View)] IO))
          => (Int -> Value -> View -> IO ()) 
          -> (Int -> Value -> IO () -> IO ())
          -> IO ()
          -> Map.Map Int (View,Value) 
          -> Task c Key Value a
          -> IO [(Int,View)]
buildForm insert update rerun state f = execStateT (run f fetch) []
    where
        fetch k@(hash -> h) = go (Map.lookup h state) h k

        add h i v = do
            st <- St.get
            when (isNothing (Prelude.lookup h st)) (modify (++ [(h,v)]))
            return i

        go (Just (view,value)) h = \case
            DisplayKey nm f -> add h value (f rerun)
            _               -> add h value view
        go Nothing h = \case
            TextKey nm f -> do
                let v = f (\t -> update h (Text t) (return ()))
                    i = Text ""
                lift (insert h i v)
                add h i v
            SelectKey nm ma f -> do
                let v = f (\x -> update h (Pure.Forms.Selected (Just x)) (return ()))
                    i = Pure.Forms.Selected ma
                lift (insert h i v)
                add h i v
            ButtonKey nm f -> do
                let v = f (update h (Clicked True) reset) 
                    i = Clicked False
                    reset = insert h i v
                lift reset
                add h i v
            DisplayKey nm f -> do
                let i = Unit
                    v = f rerun 
                lift (insert h i v)
                add h i v
            LiftKey ioa -> do
                a <- lift ioa
                return (Lifted a)

data FormState = FormState
    { formView :: [(Int,View)]
    , formState :: Map.Map Int (View,Value)
    }

form :: (Typeable a, Typeable c, c (StateT [(Int,View)] IO)) => Txt -> Form c a -> View
form nm f = formWith nm f id

formWith :: (Typeable a, Typeable c, c (StateT [(Int,View)] IO)) => Txt -> Form c a -> (View -> View) -> View
formWith nm f v = flip LibraryComponentIO (nm,f,v) $ \self -> 
    let 
        upd = modify_ self . const

        updM = modifyM_ self . const

        insert h v view = updM $ \fs -> do
            let formState' = Map.insert h (view,v) (formState fs) 
            return (fs { formState = formState' },return ())

        update h v before = updM $ \fs -> do
            let formState' = Map.adjust (\(view,_) -> (view,v)) h (formState fs) 
            return (fs { formState = formState' }, before >> run)

        run = void $ do
            (_,f,_) <- Pure.ask self
            st <- Pure.get self
            vs <- buildForm insert update run (formState st) f
            upd $ \fs -> 
                let formState' = Map.intersection (formState fs) (Map.fromList (fmap (\(i,v) -> (i,(v,Unit))) vs))
                in fs { formView = vs }

        empty = FormState [] mempty

    in
        def
            { construct = return empty
            , executing = run
            , receive = \np os -> do
                run
                return os
            , updated = \o@(nm,_,_) _ -> do
                (nm',_,_) <- Pure.ask self
                when (nm /= nm') $ 
                    updM $ \_ -> 
                        return (empty,run)
            , render = \(_,_,f) (FormState v _) -> f (Keyed Form <||#> v)
            }