{-# OPTIONS -Wall #-}
{-|
Module      : Esge.Base
Description : Highlevel basic esge module.
Copyright   : (c) Simon Goller, 2015
License     : BSD
Maintainer  : neosam@posteo.de

Basic required functions in order create a game.
They are higher level and let you for example move Individuals.

Many features are introduced by this module

* Error handling
* Individual and Room interaction
* State
* Player handling
* Provide basic 'EC.Action's

-}

module Esge.Base (
        -- * Type definitions
        Error (RoomNotFoundError, ExitNotFoundError),
        State (stPlayer, stVersion, stName),

        -- * Error functions
        printError,

        -- * Individual/Room interaction
        individualsInRoomId,
        individualsInRoom,
        roomsOfIndividualId,
        roomOfIndividualId,
        roomOfIndividual,
        beam,
        move,

        -- * State functions
        nullState,
        state,

        -- * Player specific functions
        player,
        currRoom,

        -- * Actions
        -- ** Long term actions
        infinityAction,
        condInfinityAction,
        condDropableAction,
        condSingleAction,
        delayedAction,
        -- ** Game state actions
        moveRoomAction,
        showRoomAction,
        showIndAction,
        showStateAction,
        showStorageAction
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
    let rooms = roomsOfIndividualId key ingame in
    if null rooms then ER.nullRoom
                  else head rooms

-- | Return all rooms, where the individual is located.
-- 
-- This should only be one or nothing otherwise there is most likely an issue
-- in the story setup.  Normally, 'roomOfIndividualId' or 'roomOfIndividual'
-- should be used.  The purpose of this function is as helper for the
-- plausability check.
roomsOfIndividualId :: String -> EC.Ingame -> [ER.Room]
roomsOfIndividualId key ingame =
    let rooms = ER.allRooms ingame :: [ER.Room] in
    filter (\x -> elem key $ ER.individual x) rooms


-- | Room which contains the given Individual
roomOfIndividual :: EI.Individual -> EC.Ingame -> ER.Room
roomOfIndividual ind ingame = roomOfIndividualId (EI.key ind) ingame

-- | Place Individual in other room
beam :: EI.Individual -> String -> EC.Action (Either Error ())
beam ind roomId = do
    ingame <- EC.getIngame
    let indKey = EI.key ind
        fromRoom = roomOfIndividual ind ingame
    case ER.getRoomMaybe ingame roomId of
        Nothing -> return $ Left $ RoomNotFoundError roomId
        Just room -> do
            let room' = ER.addIndividualId indKey room
                fromRoom' = ER.removeIndividualId indKey fromRoom
            EC.storageInsertA fromRoom'
            EC.storageInsertA room'
            return $ Right ()

-- | Move individual through an exit to another room
move :: EI.Individual -> String -> EC.Action (Either Error ())
move ind exitRoom = do
    ingame <- EC.getIngame
    let fromRoom = roomOfIndividual ind ingame
    if exitRoom `elem` (map fst $ ER.exits fromRoom)
     then beam ind (maybe "" id $ lookup exitRoom $ ER.exits fromRoom)
     else return $ Left $ ExitNotFoundError exitRoom


-- | Important state values
data State = State {
    stPlayer :: String,
    stVersion :: String,
    stName :: String
} deriving (Eq, Read, Show)

-- | State to use in case of an error
nullState :: State
nullState = State "" "" ""


-- | State can be stored in 'EC.Ingame'
instance EC.Storageable State where
    toStorage st = EC.Storage "state" "esgeState" [
            ("player", stPlayer st),
            ("version", stVersion st),
            ("name", stName st)
        ]
    fromStorage (EC.Storage _ t metas) =
        if t /= "esgeState" then Nothing
        else do
            ply <- lookup "player" metas
            version <- lookup "version" metas
            name <- lookup "name" metas
            return State {
                stPlayer = ply,
                stVersion = version,
                stName = name
            }

-- | Get state from 'EC.Ingame'
state :: EC.Ingame -> State
state ingame = maybe nullState id $ do
    storage <- EC.storageGet "state" ingame
    st <- EC.fromStorage storage
    return st

-- | Get player from 'EC.Ingame'
player :: EC.Ingame -> EI.Individual
player ingame =
    let playerId = stPlayer $ state ingame in
        EI.getIndividualNull ingame playerId

-- | Get room where player is located
currRoom :: EC.Ingame -> ER.Room
currRoom ingame = roomOfIndividual (player ingame) ingame


-- | Print 'Error' to 'EC.Ingame' output response
printError :: Error -> EC.Action ()
printError err = EC.setIngameResponseA "output" (
    case err of
        RoomNotFoundError str -> "Room not found: '" ++ str ++ "'"
        ExitNotFoundError str -> "Exit not found: " ++ str ++ "'"
    )

-- | Move player in 'Room' of behind exit.
moveRoomAction :: String -> EC.Action ()
moveRoomAction exitName = do
    ingame <- EC.getIngame
    res <- move (player ingame) exitName
    case res of
         Left err -> printError err
         Right () -> return ()

-- | Display room
showRoomAction :: EC.Action ()
showRoomAction = do
    ingame <- EC.getIngame
    let room = currRoom ingame
        roomText = ER.title room ++ "\n" ++
                   ER.desc room ++ "\n" ++
                   "Exits: " ++ (unwords $ ER.exitNames room)
    EC.setIngameResponseA "output" roomText

-- | Show individual
showIndAction :: EI.Individual -> EC.Action ()
showIndAction ind = EC.setIngameResponseA "output" indText
    where indText = EI.name ind

-- | Show state for debugging
showStateAction :: EC.Action ()
showStateAction = do
    ingame <- EC.getIngame
    let stateText = show $ state ingame
    EC.setIngameResponseA "output" stateText

-- | Show the whole 'Storage'
showStorageAction :: EC.Action ()
showStorageAction = do
    ingame <- EC.getIngame
    let stateText = show $ EC.storage ingame
    EC.setIngameResponseA "output" stateText



-- | Run given action and re register
infinityAction :: EC.Action () -> EC.Action ()
infinityAction act = do
    let self = infinityAction act
    EC.replaceIngame $ EC.scheduleAction self
    act

-- | Run given action only if condition is true
condInfinityAction :: (EC.Ingame -> Bool) -> EC.Action () -> EC.Action ()
condInfinityAction condFn act = do
    let self = condInfinityAction condFn act
    EC.replaceIngame $ EC.scheduleAction self
    ingame <- EC.getIngame
    if condFn ingame then act
                     else return ()

-- | Run given action as long as condition is true, then remove it
condDropableAction :: (EC.Ingame -> Bool) -> EC.Action () -> EC.Action ()
condDropableAction condFn act = do
    let self = condDropableAction condFn act
    ingame <- EC.getIngame
    if condFn ingame
     then do
        EC.replaceIngame $ EC.scheduleAction self
        act
     else return ()

-- | Do nothing until the condition becomes true, then run 'Action' once
--   and remove.
condSingleAction :: (EC.Ingame -> Bool) -> EC.Action () -> EC.Action ()
condSingleAction condFn act = do
    let self = condSingleAction condFn act
    ingame <- EC.getIngame
    if condFn ingame
     then act
     else EC.replaceIngame $ EC.scheduleAction self

-- | Wait n times and then run the action
delayedAction :: Int -> EC.Action () -> EC.Action ()
delayedAction n act = do
    let self = delayedAction (n - 1) act
    if n <= 0 then act
        else EC.replaceIngame $ EC.scheduleAction self

