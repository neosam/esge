{-|
Module      : Esge.Terminal
Description : IO module for textadvanture like user interaction..
Copyright   : (c) Simon Goller, 2015
License     : BSD
Maintainer  : neosam@posteo.de



-}

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
            modifyIngame,

            -- * Command helper
            arg,
            ingameCmd,
            actionCmd,
            actionIngameCmd,

            -- * Commands
            quitCommand,
            showRoomCmd,
            showPlayerCmd,
            showStateCmd,
            showStorageCmd,
            moveCmd
        ) where

import qualified Esge.Core as EC
import qualified Esge.Base as EB
import System.IO

-- | User input action.
--   If second parameter of the result is True, a iteration is performed.
--   For actions like show room, this should be True but for help command,
--   It should be false.
type Command = String                -- ^ Input command
            -> Terminal              -- ^ Terminal input
            -> IO (Terminal, Bool)

-- | Terminal state
data Terminal = Terminal {
    ingame :: EC.Ingame,              -- ^ Holds the Esge Ingame state
    commands :: [(String, Command)],  -- ^ All available commands
    prompt :: String                  -- ^ User prompt to display
}

-- | Initial Terminal
defaultTerminal :: Terminal
defaultTerminal = Terminal {
    ingame = EC.defaultIngame,
    commands = [],
    prompt = "\n> "
}

-- | Check game is quit
isDone :: EC.Ingame -> Bool
isDone ingame = case EC.storageGet "done" ingame of
    Just _ -> True
    Nothing -> False

-- | Replace Ingame in Terminal
setIngame :: EC.Ingame -> Terminal -> Terminal
setIngame ingame terminal = terminal { ingame = ingame }

-- | Set if game is quit
setDone :: EC.Ingame -> EC.Ingame
setDone ingame = EC.storageInsert (EC.StoreBool ("done", True)) ingame

-- | Run default user interaction loop
repl :: Terminal -> IO Terminal
repl terminal = if isDone $ ingame terminal
                    then return terminal
                    else do
                        terminal' <- step terminal
                        repl terminal'

-- | Do one iteration
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

-- | Lookup a command of of the command list
findCommand :: String -> [(String, Command)] -> Maybe Command
findCommand cmd cmds = if cmd == "" then Nothing
    else lookup key cmds
        where key = head $ words cmd

-- | Evaluate command
runCommand :: String -> Terminal -> IO (Terminal, Bool)
runCommand cmd terminal = case findCommand cmd $ commands terminal of
    Nothing -> do
        putStr "What??"
        return (terminal, False)
    Just command -> command cmd terminal

-- | Provide a new user command
addCommand :: String    -- ^ User command which triggers the action
           -> Command   -- ^ Command action
           -> Terminal  -- ^ State type
           -> Terminal  -- ^ New state
addCommand key cmd terminal =
    terminal { commands = (key, cmd) : commands terminal }

-- | Change the ingame state inside the Terminal
modifyIngame :: (EC.Ingame -> EC.Ingame)  -- ^ Ingame modifier function
             -> Terminal                  -- ^ Terminal state
             -> Terminal                  -- ^ Updated terminal state
modifyIngame fn terminal = terminal { ingame = fn $ ingame terminal }



-- | Command which quits the game
quitCommand :: Command
quitCommand _ term = return (modifyIngame setDone term, False)


-- | Get the nth word from String
arg :: String -> Int -> String
arg str i = words str !! i

-- | Generate 'Command' to manipulite Ingame
ingameCmd :: (String -> EC.Ingame -> EC.Ingame) -> Command
ingameCmd mod cmd term = return $ (modifyIngame modifier term, True)
    where modifier = mod cmd


-- | Command which schedules the 'Action' for next iteration.
actionCmd :: EC.Action -> Command
actionCmd action = ingameCmd mod
    where mod _ ingame = EC.scheduleAction action ingame

-- | More advanced 'Action' scheduling 'Command' generator.
--
-- It receives a function, which takes the command and the ingame
-- but no terminal.
actionIngameCmd :: (String -> EC.Ingame -> EC.Action) -> Command
actionIngameCmd fn cmd term = actionCmd (fn cmd $ ingame term) cmd term


-- | Show room which contains the player
showRoomCmd :: Command
showRoomCmd = actionCmd EB.showRoomAction

-- | Show player
showPlayerCmd :: Command
showPlayerCmd = actionIngameCmd showPlayer
    where showPlayer _ ingame = EB.showIndAction $ EB.player ingame

-- | Display state for debugging
showStateCmd :: Command
showStateCmd = actionCmd EB.showStateAction

-- | Show storage for debugging
showStorageCmd :: Command
showStorageCmd = actionCmd EB.showStorageAction

-- | Move player to given exit
moveCmd :: Command
moveCmd = actionIngameCmd fn
    where fn cmd ingame = EB.moveRoomAction $ exit cmd
          exit cmd = arg cmd 1

