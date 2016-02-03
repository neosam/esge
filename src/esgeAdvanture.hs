import qualified Esge.Core as EC
import qualified Esge.Base as EB
import qualified Esge.Terminal as ET
import qualified Esge.Parser as EP
import qualified Esge.Individual as EI
import qualified Esge.Room as ER

preparedIngame :: IO (Maybe EC.Ingame)
preparedIngame = do
    parseResult <- EP.loadFile "story.esge"
    case parseResult of
        Left err -> do putStr ((show err) ++ "\n")
                       return Nothing
        Right storage ->
                return $ Just $ EC.insertStorages storage EC.defaultIngame

main = do
    maybeIngame <- preparedIngame
    case maybeIngame of
     Nothing -> return ()
     Just ingame -> do
     let term = ET.addCommand "quit" ET.quitCommand $
                ET.addCommand "q" ET.quitCommand $
                ET.addCommand "b" ET.showRoomCmd $
                ET.addCommand "p" ET.showPlayerCmd $
                ET.addCommand "s" ET.showStateCmd $
                ET.addCommand "storage" ET.showStorageCmd $
                ET.addCommand "m" ET.moveCmd $
                ET.setIngame ingame $
                ET.defaultTerminal
     ET.repl term
     return ()
