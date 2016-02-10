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
    toStorage state = EC.Storage "state" "esgeState" [
            ("player", stPlayer state),
            ("version", stVersion state),
            ("name", stName state)
        ]
    fromStorage (EC.Storage key t metas) =
        if t /= "esgeState" then Nothing
        else do
            player <- lookup "player" metas
            version <- lookup "version" metas
            name <- lookup "name" metas
            return State {
                stPlayer = player,
                stVersion = version,
                stName = name
            }

-- | Get state from 'EC.Ingame'
state :: EC.Ingame -> State
state ingame = maybe nullState id $ do
    storage <- EC.storageGet "state" ingame
    state <- EC.fromStorage storage
    return state

-- | Get player from 'EC.Ingame'
player :: EC.Ingame -> EI.Individual
player ingame =
    let playerId = stPlayer $ state ingame in
        EI.getIndividualNull ingame playerId

-- | Get room where player is located
currRoom :: EC.Ingame -> ER.Room
currRoom ingame = roomOfIndividual (player ingame) ingame


-- | Print 'Error' to 'EC.Ingame' output response
printError :: Error -> EC.Ingame -> EC.Ingame
printError err ingame = EC.setIngameResponse "output" (
    case err of
        RoomNotFoundError str -> "Room not found: '" ++ str ++ "'"
        ExitNotFoundError str -> "Exit not found: " ++ str ++ "'"
    ) ingame

-- | Move player in 'Room' of behind exit.
moveRoomAction :: String -> EC.Action
moveRoomAction exitName ingame =
    case move (player ingame) exitName ingame of
         Left err -> printError err ingame
         Right ingame -> ingame

-- | Display room
showRoomAction :: EC.Action
showRoomAction ingame = EC.setIngameResponse "output" roomText ingame
    where roomText = ER.title room ++ "\n" ++
                     ER.desc room ++ "\n" ++
                     "Exits: " ++ (unwords $ ER.exitNames room)
          room = currRoom ingame

-- | Show individual
showIndAction :: EI.Individual -> EC.Action
showIndAction ind ingame = EC.setIngameResponse "output" indText ingame
    where indText = EI.name ind

-- | Show state for debugging
showStateAction :: EC.Action
showStateAction ingame = EC.setIngameResponse "output" stateText ingame
    where stateText = show $ state ingame

-- | Show the whole 'Storage'
showStorageAction :: EC.Action
showStorageAction ingame = EC.setIngameResponse "output" stateText ingame
    where stateText = show $ EC.storage ingame



-- | Run given action and re register
infinityAction :: EC.Action -> EC.Action
infinityAction act ingame = EC.scheduleAction self ingame'
    where ingame' = act ingame
          self = infinityAction act

-- | Run given action only if condition is true
condInfinityAction :: (EC.Ingame -> Bool) -> EC.Action -> EC.Action
condInfinityAction condFn act ingame = EC.scheduleAction self ingame'
    where ingame' = if condFn ingame then act ingame
                                     else ingame
          self = condInfinityAction condFn act

-- | Run given action as long as condition is true, then remove it
condDropableAction :: (EC.Ingame -> Bool) -> EC.Action -> EC.Action
condDropableAction condFn act ingame = if condFn ingame 
                                        then EC.scheduleAction self ingame'
                                        else ingame
    where ingame' = act ingame
          self = condDropableAction condFn act

-- | Do nothing until the condition becomes true, then run 'Action' once
--   and remove.
condSingleAction :: (EC.Ingame -> Bool) -> EC.Action -> EC.Action
condSingleAction condFn act ingame = if condFn ingame
                                      then act ingame
                                      else EC.scheduleAction self ingame
    where self = condSingleAction condFn act
