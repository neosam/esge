{-|
Module      : Esge.Base
Description : Highlevel basic esge module.
Copyright   : (c) Simon Goller, 2015
License     : BSD
Maintainer  : neosam@posteo.de

Basic required functions in order create a game.
They are higher level and let you for example move Individuals.

-}

module Esge.Base (
        Error (RoomNotFoundError, ExitNotFoundError),

        individualsInRoomId,
        individualsInRoom,
        roomOfIndividualId,
        roomOfIndividual,
        beam,
        move
    ) where

import qualified Esge.Core as EC
import qualified Esge.Room as ER
import qualified Esge.Individual as EI
import Data.Maybe (catMaybes)

-- | Thrown in case of something goes wrong
data Error = RoomNotFoundError String
           | ExitNotFoundError String
    deriving (Show, Read, Eq)

-- | Return all individuals from the given room id.
--   Result is Nothing if room is not found.
individualsInRoomId :: String -> EC.Ingame -> Maybe [EI.Individual]
individualsInRoomId key ingame = do
    room <- ER.getRoomMaybe ingame key
    let inds = ER.individual room :: [String]
    let storages = catMaybes $
            map (flip EC.storageGet $ ingame) inds :: [EC.Storage]
    let maybeInds = map EC.fromStorage storages :: [Maybe EI.Individual]
    return $ catMaybes maybeInds

-- | Return all individuals from the given room
individualsInRoom :: ER.Room -> EC.Ingame -> Maybe [EI.Individual]
individualsInRoom room ingame = individualsInRoomId (ER.key room) ingame

-- | Room which contains the given Individual id.
--   Returns nullRoom if not found
roomOfIndividualId :: String -> EC.Ingame -> ER.Room
roomOfIndividualId key ingame =
    let rooms = ER.allRooms ingame :: [ER.Room]
        a = filter (\x -> elem key $ ER.individual x) rooms in
    if null a then ER.nullRoom
    else head a

-- | Room which contains the given Individual
roomOfIndividual :: EI.Individual -> EC.Ingame -> ER.Room
roomOfIndividual ind ingame = roomOfIndividualId (EI.key ind) ingame

-- | Place Individual in other room
beam :: EI.Individual -> String -> EC.Ingame -> Either Error EC.Ingame
beam ind roomId ingame = 
    let indKey = EI.key ind
        fromRoom = roomOfIndividual ind ingame in
    case ER.getRoomMaybe ingame roomId of
    Nothing -> Left $ RoomNotFoundError roomId
    Just room ->
        let room' = ER.addIndividualId indKey room
            fromRoom' = ER.removeIndividualId indKey fromRoom
            ingame' = EC.storageInsert fromRoom' $
                        EC.storageInsert room' ingame in
        Right ingame'

-- | Move individual through an exit to another room
move :: EI.Individual -> String -> EC.Ingame -> Either Error EC.Ingame
move ind exitRoom ingame =
    let fromRoom = roomOfIndividual ind ingame in
    if exitRoom `elem` (map fst $ ER.exits fromRoom) then
        beam ind (maybe "" id $ lookup exitRoom $ ER.exits fromRoom) ingame
    else Left $ ExitNotFoundError exitRoom
