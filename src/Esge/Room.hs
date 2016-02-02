module Esge.Room (
            Room (Room, key, title, desc, individual, exits, items),
            nullRoom,
            allRooms,
            getRoomMaybe,
            getRoom,

            addIndividualId,
            removeIndividualId,
            exitNames
        ) where

import qualified Esge.Core as EC

data Room = Room {
    key :: String,
    title :: String,
    desc :: String,
    individual :: [String],
    exits :: [(String, String)],
    items :: [(String, String)]
} deriving (Show, Read, Eq)

mergeStrPair :: String -> (String, String) -> String
mergeStrPair sep (a, b) = a ++ sep ++ b

splitStrPair :: String -> (String, String)
splitStrPair str = let (first, second) = break (== ',') str in
    (first, tail second)

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

nullRoom = Room {
    key = "nullRoom",
    title = "",
    desc = "",
    individual = [],
    exits = [],
    items = []
}

allRooms :: EC.Ingame -> [Room]
allRooms = EC.allOfType

getRoomMaybe :: EC.Ingame -> String -> Maybe Room
getRoomMaybe ingame key = do
    storage <- EC.storageGet key ingame
    EC.fromStorage storage


getRoom :: EC.Ingame -> String -> Room
getRoom ingame key = 
    let maybeRoom = getRoomMaybe ingame key in
    case maybeRoom of
        Nothing -> nullRoom
        Just room -> room

addIndividualId :: String -> Room -> Room
addIndividualId key room = room { individual = key : individual room }

removeIndividualId :: String -> Room -> Room
removeIndividualId key room = room { individual = filter (/= key) $
                                                        individual room }

exitNames :: Room -> [String]
exitNames room = map fst $ exits room
