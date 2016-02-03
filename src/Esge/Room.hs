{-|
Module      : Esge.Room
Description : Add rooms to the game.
Copyright   : (c) Simon Goller, 2015
License     : BSD
Maintainer  : neosam@posteo.de

Provides the Room type and useful functions for Rooms to the game.

-}

module Esge.Room (
            -- * Type definition
            Room (Room, key, title, desc, individual, exits, items),

            -- * Lookup and null object
            nullRoom,
            allRooms,
            getRoomMaybe,
            getRoom,

            -- * Room accessing functions
            addIndividualId,
            removeIndividualId,
            exitNames
        ) where

import qualified Esge.Core as EC

-- | Main room type
data Room = Room {
    key :: String,                  -- ^ Lookup key
    title :: String,                -- ^ Room name
    desc :: String,                 -- ^ Room description
    individual :: [String],         -- ^ Individual keys placed in room
    exits :: [(String, String)],    -- ^ Room label and destination room
    items :: [(String, String)]     -- ^ Item label and destination item
} deriving (Show, Read, Eq)

-- | Merge the Strings to a single String
mergeStrPair :: String              -- ^ Separator to place between the strings
             -> (String, String)    -- ^ Strings to merge
             -> String              -- ^ Result String
mergeStrPair sep (a, b) = a ++ sep ++ b

-- | Split comma separated String
splitStrPair :: String -> (String, String)
splitStrPair str = let (first, second) = break (== ',') str in
    (first, tail second)

-- | Room can be converted to Storage and back
instance EC.Storageable Room where
    toStorage room = EC.Storage (key room) "room" [
        ("title", title room), ("desc" , desc room),
        ("individual", unwords $ individual room),
        ("exits", unwords $ map (mergeStrPair ",") $ exits room),
        ("items", unwords $ map (mergeStrPair ",") $ items room)]
    fromStorage (EC.Storage key t metas) =
        if t /= "room" then Nothing
        else do
            title <- lookup "title" metas
            desc <- lookup "desc" metas
            individual <- lookup "individual" metas
            exits <- lookup "exits" metas
            items <- lookup "items" metas
            return Room {
                key = key,
                title = title,
                desc = desc,
                individual = words individual,
                exits = map splitStrPair $ words exits,
                items = map splitStrPair $ words items
            }

-- | Default room usually used in case of an error
nullRoom = Room {
    key = "nullRoom",
    title = "Null Room",
    desc = "If you see this room, you have an error",
    individual = [],
    exits = [],
    items = []
}

-- | Get all 'Room's from an 'EC.Ingame' storage
allRooms :: EC.Ingame -> [Room]
allRooms = EC.allOfType

-- | Lookup 'Room' with given key in 'EC.Ingame' or Nothing
getRoomMaybe :: EC.Ingame       -- ^ Ingame to search
                -> String       -- ^ Rooms key
                -> Maybe Room   -- ^ Room if found or Nothing
getRoomMaybe ingame key = do
    storage <- EC.storageGet key ingame
    EC.fromStorage storage


-- | Lookup 'Room' with given key in 'EC.Ingame' or nullRoom
getRoom :: EC.Ingame  -- ^ Ingame to search
        -> String     -- ^ Rooms key
        -> Room       -- ^ Room if found or Nothing
getRoom ingame key = 
    let maybeRoom = getRoomMaybe ingame key in
    case maybeRoom of
        Nothing -> nullRoom
        Just room -> room

-- | Add individual id to Room
addIndividualId :: String -> Room -> Room
addIndividualId key room = room { individual = key : individual room }

-- | Remove individual id from Room
removeIndividualId :: String -> Room -> Room
removeIndividualId key room = room { individual = filter (/= key) $
                                                        individual room }

-- | Get exit names from 'Room'
exitNames :: Room -> [String]
exitNames room = map fst $ exits room
