module Esge.Terminal(
            -- * Data types
            Command,
            Terminal,

            -- * Static values
            defaultTerminal,

            -- * Flow functions
            step,
            repl,

            -- * Accessor functions
            addCommand,
            isDone,
            setDone,
            setIngame,
            ingame,
            findCommand,
            modifyIngame
        ) where

import qualified Esge.Core as EC
import System.IO

type Command = String -> Terminal -> IO (Terminal, Bool)

data Terminal = Terminal {
    ingame :: EC.Ingame,
    commands :: [(String, Command)],
    prompt :: String
}

defaultTerminal :: Terminal
defaultTerminal = Terminal {
    ingame = EC.defaultIngame,
    commands = [],
    prompt = "\n> "
}

isDone :: EC.Ingame -> Bool
isDone ingame = case EC.storageGet "done" ingame of
    Just _ -> True
    Nothing -> False

setIngame :: EC.Ingame -> Terminal -> Terminal
setIngame ingame terminal = terminal { ingame = ingame }

setDone :: EC.Ingame -> EC.Ingame
setDone ingame = EC.storageInsert (EC.StoreBool ("done", True)) ingame

repl :: Terminal -> IO Terminal
repl terminal = if isDone $ ingame terminal
                    then return terminal
                    else do
                        terminal' <- step terminal
                        repl terminal'

step :: Terminal -> IO Terminal
step terminal = do
    putStr (prompt terminal)
    hFlush stdout
    command <- getLine
    (terminal', doStep) <- runCommand command terminal
    if not doStep then return terminal'
        else do
            let terminal'' = terminal' { ingame = EC.step $ ingame terminal' }
            putStr $ EC.getIngameResponse "output" $ ingame terminal''
            return terminal''

findCommand :: String -> [(String, Command)] -> Maybe Command
findCommand cmd cmds = if cmd == "" then Nothing
    else lookup key cmds
        where key = head $ words cmd

runCommand :: String -> Terminal -> IO (Terminal, Bool)
runCommand cmd terminal = case findCommand cmd $ commands terminal of
    Nothing -> do
        putStr "What??"
        return (terminal, False)
    Just command -> command cmd terminal

addCommand :: String -> Command -> Terminal -> Terminal
addCommand key cmd terminal =
    terminal { commands = (key, cmd) : commands terminal }

modifyIngame :: (EC.Ingame -> EC.Ingame) -> Terminal -> Terminal
modifyIngame fn terminal = terminal { ingame = fn $ ingame terminal }
