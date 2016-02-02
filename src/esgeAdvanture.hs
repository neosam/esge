import qualified Esge.Core as EC
import qualified Esge.Base as EB
import qualified Esge.Terminal as ET
import qualified Esge.Parser as EP
import qualified Esge.Individual as EI
import qualified Esge.Room as ER

quitCommand :: ET.Command
quitCommand _ term = return (ET.modifyIngame ET.setDone term, False)


insertStorages :: EC.Storageable a => [a] -> EC.Ingame -> EC.Ingame
insertStorages xs ingame = foldr insert ingame xs
    where insert = EC.storageInsert


preparedIngame :: IO (Maybe EC.Ingame)
preparedIngame = do
    parseResult <- EP.loadFile "story.esge"
    case parseResult of
        Left err -> do putStr ((show err) ++ "\n")
                       return Nothing
        Right storage ->
                       return $ Just $ insertStorages storage EC.defaultIngame


data State = State {
    stPlayer :: String,
    stVersion :: String,
    stName :: String
} deriving (Eq, Read, Show)

nullState :: State
nullState = State "" "" ""

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

state :: EC.Ingame -> State
state ingame = maybe nullState id $ do
    storage <- EC.storageGet "state" ingame
    state <- EC.fromStorage storage
    return state

player :: EC.Ingame -> EI.Individual
player ingame =
    let playerId = stPlayer $ state ingame in
        EI.getIndividualNull ingame playerId

currRoom :: EC.Ingame -> ER.Room
currRoom ingame = EB.roomOfIndividual (player ingame) ingame

arg :: String -> Int -> String
arg str i = words str !! i

printError :: EB.Error -> EC.Ingame -> EC.Ingame
printError err ingame = EC.setIngameResponse "output" (
    case err of
        EB.RoomNotFoundError str -> "Room not found: '" ++ str ++ "'"
        EB.ExitNotFoundError str -> "Exit not found: " ++ str ++ "'"
    ) ingame

moveRoomAction :: String -> EC.Action
moveRoomAction exitName ingame =
    case EB.move (player ingame) exitName ingame of
         Left err -> printError err ingame
         Right ingame -> ingame

showRoomAction :: EC.Action
showRoomAction ingame = EC.setIngameResponse "output" roomText ingame
    where roomText = ER.title room ++ "\n" ++
                     ER.desc room ++ "\n" ++
                     "Exits: " ++ (unwords $ ER.exitNames room)
          room = currRoom ingame

showIndAction :: EI.Individual -> EC.Action
showIndAction ind ingame = EC.setIngameResponse "output" indText ingame
    where indText = EI.name ind

showStateAction :: EC.Action
showStateAction ingame = EC.setIngameResponse "output" stateText ingame
    where stateText = show $ state ingame

showStorageAction :: EC.Action
showStorageAction ingame = EC.setIngameResponse "output" stateText ingame
    where stateText = show $ EC.storage ingame

moveAction :: String -> EC.Action
moveAction exit ingame = case EB.move pl exit ingame of
        Left err -> printError err ingame
        Right ingame -> ingame
    where pl = player ingame



ingameCmd :: (String -> EC.Ingame -> EC.Ingame) -> ET.Command
ingameCmd mod cmd term = return $ (ET.modifyIngame modifier term, True)
    where modifier = mod cmd

actionCmd :: EC.Action -> ET.Command
actionCmd action = ingameCmd mod
    where mod _ ingame = EC.scheduleAction action ingame

actionIngameCmd :: (String -> EC.Ingame -> EC.Action) -> ET.Command
actionIngameCmd fn cmd term = actionCmd (fn cmd $ ET.ingame term) cmd term


showRoomCmd :: ET.Command
showRoomCmd = actionCmd showRoomAction

showPlayerCmd :: ET.Command
showPlayerCmd = actionIngameCmd showPlayer
    where showPlayer _ ingame = showIndAction $ player ingame

showStateCmd :: ET.Command
showStateCmd = actionCmd showStateAction

showStorageCmd :: ET.Command
showStorageCmd = actionCmd showStorageAction

moveCmd :: ET.Command
moveCmd = actionIngameCmd fn
    where fn cmd ingame = moveAction $ exit cmd
          exit cmd = arg cmd 1

main = do
    maybeIngame <- preparedIngame
    case maybeIngame of
     Nothing -> return ()
     Just ingame -> do
     let term = ET.addCommand "quit" quitCommand $
                ET.addCommand "q" quitCommand $
                ET.addCommand "b" showRoomCmd $
                ET.addCommand "p" showPlayerCmd $
                ET.addCommand "s" showStateCmd $
                ET.addCommand "storage" showStorageCmd $
                ET.addCommand "m" moveCmd $
                ET.setIngame ingame $
                ET.defaultTerminal
     ET.repl term
     return ()
