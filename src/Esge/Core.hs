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
            storageInsert,
            storageGet,
            storage,
            allOfType,
            scheduleAction,
            setIngameResponse,
            getIngameResponse,
            step


        ) where

import Text.Read (readMaybe)
import Data.Maybe (catMaybes)

-- | Key value pair using strings
type Meta = (String, String)
-- | List of of key value pairs mainly used as storage
type MetaList = [Meta]

-- | Used to hold an element of the game like a person, room, action.
-- Add custom elements by using the Storageable class
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
    toStorage storage = storage
    fromStorage storage = Just storage


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
primitiveFromStorage typeStr storage = case stringFromStorage typeStr storage of
    Nothing -> Nothing
    Just (StoreString (key, str)) -> case readMaybe str of
        Just a -> Just (key, a)
        Nothing -> Nothing


-- | In ingame modifying function.
--   Can be added as Action in ingame to modify the game in an iteration
type Action = Ingame -> Ingame

-- | Typeclass to transform something into an action.
--   Could be a special storage for example which is used as trigger
--   in the game.
class Actionable a where
    toAction :: a -> Action


-- | State object
data Ingame = Ingame [Storage] -- All items
                         [Action]  -- Actions on next step
                         MetaList  -- Engine response

-- | Empfy state, used to initialize
defaultIngame :: Ingame
defaultIngame = Ingame [] [] []

-- | Replace Storages in Ingame
setStorage :: [Storage] -> Ingame -> Ingame
setStorage storage (Ingame _ a b) = Ingame storage a b

-- | Get all Storages from Ingame
storage :: Ingame -> [Storage]
storage (Ingame storage _ _) = storage

-- | Set all Actions in Ingame
setActions :: [Action] -> Ingame -> Ingame
setActions xs (Ingame a _ b) = Ingame a xs b

-- | Set responese in Ingame
setResponse :: MetaList -> Ingame -> Ingame
setResponse xs (Ingame a b _) = Ingame a b xs


-- | Set a single ingame response
setIngameResponse :: String  -- ^ Response channel
                  -> String  -- ^ Response text
                  -> Ingame  -- ^ Ingame before
                  -> Ingame  -- ^ Modified Ingame
setIngameResponse key val ingame@(Ingame _ _ xs) =
    setResponse ((key, val) : xs) ingame

-- | Get a single ingame response
getIngameResponse :: String  -- ^ Response channel
                  -> Ingame  -- ^ Ingame
                  -> String  -- ^ Response value.  "" if not found.
getIngameResponse key (Ingame _ _ xs) = maybe "" id $ lookup key xs

-- | Register Action for next iteration
scheduleAction :: Action -> Ingame -> Ingame
scheduleAction x ingame@(Ingame _ xs _) = setActions (x : xs) ingame

-- | Remove all Responese
pruneResponse :: Ingame -> Ingame
pruneResponse = setResponse []

-- | Do one iterations and run all Actions
step :: Ingame -> Ingame
step (Ingame storage actions metas) =
    foldr run ingame actions
    where ingame = Ingame storage [] []
          run action ingame = action ingame

-- | Get id from storage
storageId :: Storage -> String
storageId (Storage id _ _) = id

-- | Add a value to the storage list
insertInMetaList :: String -> Storage -> [Storage] -> [Storage]
insertInMetaList key x xs = x : xs


-- | Insert a Storageable item to the Ingame Storage
storageInsert :: Storageable a => a -> Ingame -> Ingame
storageInsert a ingame@(Ingame storage _ _) = setStorage storage' ingame
    where storage' = insertInMetaList key storageItem storage
          storageItem = toStorage a
          key = storageId storageItem

-- | Get storage with given key from Ingame
storageGet :: String -> Ingame -> Maybe Storage
storageGet key ingame = storageLookup key $ storage ingame

-- | Got storage with given key from Storage List
storageLookup :: String -> [Storage] -> Maybe Storage
storageLookup key storages = lookup key tupleList
     where tupleList = map storageToTuple storages


-- | Translate a storage to a tuple with its key and the storage.
--   Used for lookup
storageToTuple :: Storage -> (String, Storage)
storageToTuple storage@(Storage key _ _) = (key, storage)

-- | Got all Storages which can be transformed to a type
allOfType :: Storageable a => Ingame -> [a]
allOfType ingame = catMaybes $ map fromStorage $ storage ingame
