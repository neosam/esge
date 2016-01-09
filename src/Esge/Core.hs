{-|
Module      : Esge.Core
Description : Most lowlevel edge module.
Copyright   : (c) Simon Goller, 2015
License     : BSD
Maintainer  : neosam@posteo.de

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Esge.Core (
            Meta,
            MetaList,
            Storage (Storage),
            Action,
            Ingame,

            StoreString (StoreString),
            StoreBool (StoreBool),
            StoreInt (StoreInt),
            StoreFloat (StoreFloat),

            Storageable (toStorage, fromStorage),
            Actionable (toAction),

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

newtype StoreString = StoreString (String, String)
    deriving (Show, Read)
newtype StoreBool = StoreBool (String, Bool)
    deriving (Show, Read)
newtype StoreInt = StoreInt (String, Int)
    deriving (Show, Read)
newtype StoreFloat = StoreFloat (String, Float)
    deriving (Show, Read)

instance Storageable StoreString where
    toStorage (StoreString (key, str)) = Storage key "string" [("val", str)]
    fromStorage = stringFromStorage "string"

instance Storageable StoreBool where
    toStorage (StoreBool (key, a)) = Storage key "bool" [("val", show a)]
    fromStorage a = case primitiveFromStorage "bool" a of
        Just x -> Just $ StoreBool x
        Nothing -> Nothing

instance Storageable StoreInt where
    toStorage (StoreInt (key, a)) = Storage key "int" [("val", show a)]
    fromStorage a = case primitiveFromStorage "int" a of
        Just x -> Just $ StoreInt x
        Nothing -> Nothing

instance Storageable StoreFloat where
    toStorage (StoreFloat (key, a)) = Storage key "float" [("val", show a)]
    fromStorage a = case primitiveFromStorage "float" a of
        Just x -> Just $ StoreFloat x
        Nothing -> Nothing

instance Storageable Storage where
    toStorage storage = storage
    fromStorage storage = Just storage


stringFromStorage :: String -> Storage -> Maybe StoreString
stringFromStorage typeStr1 (Storage key typeStr2 metas) =
    if typeStr1 /= typeStr2 then Nothing
    else case lookup "val" metas of
            Just str -> Just $ StoreString (key, str)
            Nothing -> Nothing

primitiveFromStorage :: Read a => String -> Storage -> Maybe (String, a)
primitiveFromStorage typeStr storage = case stringFromStorage typeStr storage of
    Nothing -> Nothing
    Just (StoreString (key, str)) -> case readMaybe str of
        Just a -> Just (key, a)
        Nothing -> Nothing



type Action = Ingame -> Ingame

class Actionable a where
    toAction :: a -> Action



data Ingame = Ingame [Storage] -- All items
                         [Action]  -- Actions on next step
                         MetaList  -- Engine response


defaultIngame :: Ingame
defaultIngame = Ingame [] [] []

setStorage :: [Storage] -> Ingame -> Ingame
setStorage storage (Ingame _ a b) = Ingame storage a b

storage :: Ingame -> [Storage]
storage (Ingame storage _ _) = storage

setActions :: [Action] -> Ingame -> Ingame
setActions xs (Ingame a _ b) = Ingame a xs b

setResponse :: MetaList -> Ingame -> Ingame
setResponse xs (Ingame a b _) = Ingame a b xs


setIngameResponse :: String -> String -> Ingame -> Ingame
setIngameResponse key val ingame@(Ingame _ _ xs) =
    setResponse ((key, val) : xs) ingame

getIngameResponse :: String -> Ingame -> String
getIngameResponse key (Ingame _ _ xs) = maybe "" id $ lookup key xs

scheduleAction :: Action -> Ingame -> Ingame
scheduleAction x ingame@(Ingame _ xs _) = setActions (x : xs) ingame

pruneResponse :: Ingame -> Ingame
pruneResponse = setResponse []

step :: Ingame -> Ingame
step (Ingame storage actions metas) =
    foldr run ingame actions
    where ingame = Ingame storage [] []
          run action ingame = action ingame

storageId :: Storage -> String
storageId (Storage id _ _) = id

insertInMetaList :: String -> Storage -> [Storage] -> [Storage]
insertInMetaList key x xs = x : xs


storageInsert :: Storageable a => a -> Ingame -> Ingame
storageInsert a ingame@(Ingame storage _ _) = setStorage storage' ingame
    where storage' = insertInMetaList key storageItem storage
          storageItem = toStorage a
          key = storageId storageItem

storageGet :: String -> Ingame -> Maybe Storage
storageGet key ingame = storageLookup key $ storage ingame

storageLookup :: String -> [Storage] -> Maybe Storage
storageLookup key storages = lookup key tupleList
     where tupleList = map storageToTuple storages

storageToTuple :: Storage -> (String, Storage)
storageToTuple storage@(Storage key _ _) = (key, storage)

allOfType :: Storageable a => Ingame -> [a]
allOfType ingame = catMaybes $ map fromStorage $ storage ingame
