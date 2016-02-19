{-# OPTIONS -Wall #-}
{-# LANGUAGE BangPatterns #-}

{-|
Module      : Esge.Core
Description : Most lowlevel edge module.
Copyright   : (c) Simon Goller, 2015
License     : BSD
Maintainer  : neosam@posteo.de

Esge.Core provides the low level data types and functions to the user.
It defines the core Ingame type, what Action, Meta and Storage, methods
to access Ingame e.g. to schedule Actions and run one iteration.

This module does not know anything about Individuals or Rooms and so on.

-}
module Esge.Core (
            -- * Data types
            Meta,
            MetaList,
            Storage (Storage),
            Action,
            Ingame,

            -- * Storage primitive values
            StoreString (StoreString),
            StoreBool (StoreBool),
            StoreInt (StoreInt),
            StoreFloat (StoreFloat),

            -- * Data classes
            Storageable (toStorage, fromStorage),
            Actionable (toAction),

            -- * Functions
            defaultIngame,
            defaultIngameWithStorage,
            storageGet,
            storage,
            allOfType,
            scheduleAction,
            getIngameResponse,
            step,

            -- * Action
            -- ** Action helpers
            action,
            getIngame,
            setIngame,
            replaceIngame,

            -- ** Ingame reader
            storageGetA,
            storageA,
            allOfTypeA,
            scheduleActionA,
            getIngameResponseA,

            -- ** Ingame manipulators
            storageInsertA,
            storageRemoveA,
            insertStoragesA,
            setIngameResponseA
        ) where

import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Control.Monad (liftM, ap)

-- | Key value pair using strings
type Meta = (String, String)
-- | List of of key value pairs mainly used as storage
type MetaList = [Meta]

-- | Used to hold an element of the game like a person, room, action.
-- Add custom elements by using the Storageable class
--
-- @
--  Storage key type metas
-- @
data Storage = Storage String String MetaList
    deriving (Show, Read, Eq)

-- | Add a new kind of storage or element to the game by adding these
-- transformer functions.
class Storageable a where
    -- | Transform to custom type to strage
    toStorage :: a -> Storage
    -- | Try to ransform storage to type
    fromStorage :: Storage -> Maybe a

-- | To convert a String to 'Storage' - to store the key
newtype StoreString = StoreString (String, String)
    deriving (Show, Read)
-- | To convert a Bool to 'Storage' - to store the key
newtype StoreBool = StoreBool (String, Bool)
    deriving (Show, Read)
-- | To convert an Int to 'Storage' - to store the key
newtype StoreInt = StoreInt (String, Int)
    deriving (Show, Read)
-- | To convert a Float to 'Storage' - to store the key
newtype StoreFloat = StoreFloat (String, Float)
    deriving (Show, Read)

-- | Make String storageable
instance Storageable StoreString where
    toStorage (StoreString (key, str)) = Storage key "string" [("val", str)]
    fromStorage = stringFromStorage "string"

-- | Make Bool storageable
instance Storageable StoreBool where
    toStorage (StoreBool (key, a)) = Storage key "bool" [("val", show a)]
    fromStorage a = case primitiveFromStorage "bool" a of
        Just x -> Just $ StoreBool x
        Nothing -> Nothing

-- | Make Int storageable
instance Storageable StoreInt where
    toStorage (StoreInt (key, a)) = Storage key "int" [("val", show a)]
    fromStorage a = case primitiveFromStorage "int" a of
        Just x -> Just $ StoreInt x
        Nothing -> Nothing

-- | Make Float storageable
instance Storageable StoreFloat where
    toStorage (StoreFloat (key, a)) = Storage key "float" [("val", show a)]
    fromStorage a = case primitiveFromStorage "float" a of
        Just x -> Just $ StoreFloat x
        Nothing -> Nothing

-- | Make a Stogage able to convert into itself.
-- | Then a Starge can also be used for Storageable functions
instance Storageable Storage where
    toStorage sto = sto
    fromStorage sto = Just sto


-- | Intended to help to make String storageable
stringFromStorage :: String -> Storage -> Maybe StoreString
stringFromStorage typeStr1 (Storage key typeStr2 metas) =
    if typeStr1 /= typeStr2 then Nothing
    else case lookup "val" metas of
            Just str -> Just $ StoreString (key, str)
            Nothing -> Nothing

-- | Intended to help to make all types storageable which implement
-- the Read class to make primitives like Int and Bool Storageable
primitiveFromStorage :: Read a => String -> Storage -> Maybe (String, a)
primitiveFromStorage typeStr sto = case stringFromStorage typeStr sto of
    Nothing -> Nothing
    Just (StoreString (key, str)) -> case readMaybe str of
        Just a -> Just (key, a)
        Nothing -> Nothing


-- | In ingame modifying function.
--   Can be added as Action in ingame to modify the game in an iteration
newtype Action a = Action (Ingame -> (a, Ingame))

-- | Turn the internal used 'Action' function to an 'Action'
action :: (Ingame -> (a, Ingame)) -> Action a
action fn = Action fn

-- | Get the current ingame state
getIngame :: Action Ingame
getIngame = action $ \ingame -> (ingame, ingame)

-- | Set new 'Ingame' state
setIngame :: Ingame -> Action ()
setIngame ingame = action $ \_ ->
    ((), ingame)

runAction :: Action a -> Ingame -> Ingame
runAction act ingame =
    let Action fn = act
        (_, ingame') = fn ingame
    in ingame'

-- | Transform in 'Ingame' to 'Ingame' function to an Action ()
replaceIngame :: (Ingame -> Ingame) -> Action ()
replaceIngame fn = do
    ingame <- getIngame
    setIngame $ fn ingame

applyActions :: Action a -> (a -> Action b) -> Action b
applyActions (Action act) fn = Action $ \ ingame ->
    let (x, ingame') = act ingame
        Action act2 = fn x
    in act2 ingame'

returnAction :: a -> Action a
returnAction x = Action $ \ ingame -> (x, ingame)

instance Monad Action where
    (>>=) = applyActions
    return = returnAction

instance Functor Action where
    fmap = liftM

instance Applicative Action where
    pure = return
    (<*>) = ap


-- | Typeclass to transform something into an action.
--   Could be a special storage for example which is used as trigger
--   in the game.
class Actionable a where
    toAction :: a -> Action ()


-- | Represents the whole game state.  It contains
--
-- * All items like Rooms, Individuals, Trigger and so on as 'Storage'.
--   This can be extended by other Haskell modules.
-- * List of 'Action's for the next iteration.
-- * Response back to the caller (for output and so on).  There can be
--   unlimited "channels".  Values are always Strings.
data Ingame = Ingame [Storage] -- All items
                         [Action ()]  -- Actions on next step
                         MetaList  -- Engine response

-- | Empfy state, used to initialize
defaultIngame :: Ingame
defaultIngame = Ingame [] [] []

-- | Return default ingame with the given 'Storage's.
--
-- This is mainly used to initialize the 'Ingame' state.
defaultIngameWithStorage :: [Storage] -> Ingame
defaultIngameWithStorage sto = Ingame sto [] []

-- | Replace Storages in Ingame
setStorage :: [Storage] -> Ingame -> Ingame
setStorage sto (Ingame _ a b) = Ingame sto a b

-- | Get all Storages from Ingame
storage :: Ingame -> [Storage]
storage (Ingame sto _ _) = sto

-- | Get all 'Storage's
storageA :: Action [Storage]
storageA = do
    ingame <- getIngame
    return $ storage ingame

-- | Set all Actions in Ingame
setActions :: [Action ()] -> Ingame -> Ingame
setActions xs (Ingame a _ b) = Ingame a xs b

-- | Set responese in Ingame
setResponse :: MetaList -> Ingame -> Ingame
setResponse xs (Ingame a b _) = Ingame a b xs


-- | Set a single ingame response
setIngameResponseA :: String     -- ^ Response channel
                   -> String     -- ^ Response text
                   -> Action ()  -- ^ Modification action
setIngameResponseA key val = replaceIngame $ setIngameResponse_ key val

setIngameResponse_ :: String  -- ^ Response channel
                   -> String  -- ^ Response text
                   -> Ingame  -- ^ Ingame before
                   -> Ingame  -- ^ Modified Ingame
setIngameResponse_ key val ingame@(Ingame _ _ xs) =
    setResponse ((key, val) : xs) ingame

-- | Get a single ingame response
getIngameResponse :: String  -- ^ Response channel
                  -> Ingame  -- ^ Ingame
                  -> String  -- ^ Response value.  "" if not found.
getIngameResponse key (Ingame _ _ xs) = maybe "" id $ lookup key xs

-- | Get a single ingame response
getIngameResponseA :: String             -- ^ Response channel
                   -> Action String      -- ^ String result
getIngameResponseA key = do
    ingame <- getIngame
    return $ getIngameResponse key ingame


-- | Register Action for next iteration
scheduleAction :: Action () -> Ingame -> Ingame
scheduleAction x ingame@(Ingame _ xs _) = setActions (x : xs) ingame

-- | Register 'Action' for next iteration
scheduleActionA :: Action () -> Action ()
scheduleActionA x = replaceIngame $ scheduleAction x

-- | Do one iterations and run all Actions
step :: Ingame -> Ingame
step (Ingame sto actions _) =
    foldr run ingame actions
    where ingame = Ingame sto [] []
          run act ingame' = runAction act ingame'

-- | Get id from storage
storageId :: Storage -> String
storageId (Storage sid _ _) = sid

-- | Add a value to the storage list
insertInMetaList :: String -> Storage -> [Storage] -> [Storage]
insertInMetaList _ x xs = x : xs


storageInsert_ :: Storageable a => a -> Ingame -> Ingame
storageInsert_ a ingame@(Ingame sto _ _) = setStorage sto' ingame
    where sto' = insertInMetaList key storageItem sto
          storageItem = toStorage a
          key = storageId storageItem

-- | Insert a Storageable item to the Ingame Storage
storageInsertA :: Storageable a => a -> Action ()
storageInsertA sto = replaceIngame $ storageInsert_ sto

-- | Get storage with given key from Ingame
storageGet :: String -> Ingame -> Maybe Storage
storageGet key ingame = storageLookup key $ storage ingame

-- | Get storge with given key
storageGetA :: String -> Action (Maybe Storage)
storageGetA k = do 
    ingame <- getIngame
    return $ storageGet k ingame

-- | Got storage with given key from Storage List
storageLookup :: String -> [Storage] -> Maybe Storage
storageLookup key storages = lookup key tupleList
     where tupleList = map storageToTuple storages


-- | Translate a storage to a tuple with its key and the storage.
--   Used for lookup
storageToTuple :: Storage -> (String, Storage)
storageToTuple sto@(Storage key _ _) = (key, sto)

-- | Got all Storages which can be transformed to a type
allOfType :: Storageable a => Ingame -> [a]
allOfType ingame = catMaybes $ map fromStorage $ storage ingame

-- | Got all 'Storage's which can be fransformed to a type
allOfTypeA :: Storageable a => Action [a]
allOfTypeA = do
    ingame <- getIngame
    return $ allOfType ingame


-- | Insert a list of 'Storage's
insertStoragesA :: Storageable a => [a] -> Action ()
insertStoragesA xs = replaceIngame $ insertStorages_ xs

insertStorages_ :: Storageable a => [a] -> Ingame -> Ingame
insertStorages_ xs ingame = foldr insert ingame xs
    where insert = storageInsert_


-- | Remove element from storage
storageRemoveA :: String -> Action ()
storageRemoveA key = replaceIngame $ storageRemove_ key

storageRemove_ :: String         -- ^ Storage key to remove
              -> Ingame         -- ^ State to edit
              -> Ingame         -- ^ Modified state
storageRemove_ key ingame = setStorage storage' ingame
    where storage' = filter removeFn $ storage ingame
          removeFn (Storage storageKey _ _) = storageKey /= key

