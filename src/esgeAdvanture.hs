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
        EB.RoomNotFoundError str -> "Room not found: " ++ str
        EB.ExitNotFoundError str -> "Exit not found: " ++ str
    ) ingame

moveRoomAction :: String -> EC.Action
moveRoomAction exitName ingame =
    case EB.move (player ingame) exitName ingame of
         Left err -> printError err ingame
         Right ingame -> ingame

-- Coming next
-- * actions
--   * moveRoom
--   * showRoom
--   *

main = do
    maybeIngame <- preparedIngame
    case maybeIngame of
     Nothing -> return ()
     Just ingame -> do
     let term = ET.addCommand "quit" quitCommand $
                ET.addCommand "q" quitCommand $
                ET.defaultTerminal
     ET.repl term
     return ()
