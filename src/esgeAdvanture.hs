import qualified Esge.Core as EC
import qualified Esge.Base as EB
import qualified Esge.Terminal as ET
import qualified Esge.Parser as EP
import qualified Esge.Individual as EI
import qualified Esge.Room as ER
import qualified Esge.Run as ERun

preparedIngame :: IO (Maybe EC.Ingame)
preparedIngame = do
    parseResult <- EP.loadFile [] "story.esge"
    case parseResult of
        Left err -> do putStr ((show err) ++ "\n")
                       return Nothing
        Right storage ->
                return $ Just $ EC.insertStorages storage EC.defaultIngame

main = do
    ERun.replRun "story.esge" [
            ERun.defaultRepl
        ]
    return ()
