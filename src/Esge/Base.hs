module Esge.Base (
        Error (RoomNotFoundError),

        individualsInRoomId,
        individualsInRoom,
        roomOfIndividualId,
        roomOfIndividual,
        beam
    ) where

import qualified Esge.Core as EC
import qualified Esge.Room as ER
import qualified Esge.Individual as EI
import Data.Maybe (catMaybes)

data Error = RoomNotFoundError String
    deriving (Show, Read, Eq)

individualsInRoomId :: String -> EC.Ingame -> Maybe [EI.Individual]
individualsInRoomId key ingame = do
    room <- ER.getRoomMaybe ingame key
    let inds = ER.individual room :: [String]
    let storages = catMaybes $
            map (flip EC.storageGet $ ingame) inds :: [EC.Storage]
    let maybeInds = map EC.fromStorage storages :: [Maybe EI.Individual]
    return $ catMaybes maybeInds

individualsInRoom :: ER.Room -> EC.Ingame -> Maybe [EI.Individual]
individualsInRoom room ingame = individualsInRoomId (ER.key room) ingame

roomOfIndividualId :: String -> EC.Ingame -> ER.Room
roomOfIndividualId key ingame =
    let rooms = ER.allRooms ingame :: [ER.Room]
        a = filter (\x -> elem key $ ER.individual x) rooms in
    if null a then ER.nullRoom
    else head a

roomOfIndividual :: EI.Individual -> EC.Ingame -> ER.Room
roomOfIndividual ind ingame = roomOfIndividualId (EI.key ind) ingame

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

