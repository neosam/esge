{-|
Module      : Esge.Run
Description : Esge engine initialization module
Copyright   : (c) Simon Goller, 2015
License     : BSD
Maintainer  : neosam@posteo.de



-}

module Esge.Run (
        -- * Ingame loaders
        IngameInit,
        IngameMod (IngameMod),
        ingameLoader,
        ingameRun,

        -- * Repl loaders
        ReplInit,
        ReplInitGen,
        ReplMod (ReplMod),
        replLoader,
        replRun,

        -- ** Initializers
        defaults,
        defaultRepl,

        -- * Plausability checks
        PCheck,
        plausabilityCheck) where
import qualified Esge.Core as EC
import qualified Esge.Base as EB
import qualified Esge.Parser as EP
import qualified Esge.Terminal as ET
import qualified Esge.Individual as EI
import qualified Esge.Room as ER

-- | Plausability check definition.  Left and an error message in used
--   in case of an error and Right on success.
type PCheck = EC.Ingame -> Either String ()

-- | Passed check
pCheckPassed :: Either String ()
pCheckPassed = Right ()

-- | Hold function depending on the game type
data ActionFactory = ActionFactory {
    delayedAction :: (Int, Float) -> EC.Action -> EC.Action,
    shortDelayedAction :: Float -> EC.Action -> EC.Action
}

-- | Hold initializer of the ingame
data IngameInit = IngameInit [PCheck] [EP.BlockParser] [IngameMod]

-- | Ingame initializer modifier used to add initializers
--type IngameLoader = IngameInit -> IngameInit

-- | Run a list of plausability checks.
plausabilityCheck :: [PCheck]           -- ^ List of checks
                    -> EC.Ingame        -- ^ Ingame state to chock
                    -> Either String () -- ^ Error message or success
plausabilityCheck xs ingame = foldr (>>) pCheckPassed xs'
    where xs' = map ($ ingame) xs

-- | Ingame modifier for the loader
newtype IngameMod = IngameMod (EC.Ingame -> EC.Ingame)

-- | Generates a modifier function for the ingame initializers
-- and will add the given attributes to a 'IngameLoader'
ingameLoader :: ([PCheck], [EP.BlockParser], [IngameMod])
             -> IngameInit
ingameLoader (pChecks, parsers, mods) =
                IngameInit pChecks parsers mods

-- | Maps error to a string so it is compatible with plausability checks
loadFile2 :: [EP.BlockParser] -> FilePath -> IO (Either String [EC.Storage])
loadFile2 parsers filename = do
    parseResult <- EP.loadFile parsers filename
    case parseResult of
        Left err -> return $ Left ("Parser error:  " ++ show err)
        Right storage -> return $ Right storage

-- | Empty loader
defaultLoader :: IngameInit
defaultLoader = IngameInit [] [] []

-- | Get function out of  mod
unpackIngameMod :: IngameMod -> (EC.Ingame -> EC.Ingame)
unpackIngameMod (IngameMod fn) = fn

-- | Run 'IngameMods' on 'Ingame'
applyIngameMods :: EC.Ingame -> [IngameMod] -> EC.Ingame
applyIngameMods ingame xs = foldr ($) ingame $ map unpackIngameMod xs

-- | Initializes the ingame state
ingameRun :: FilePath           -- ^ Path to story file
            -> [IngameInit]     -- ^ Initialization for custom parsers,
                                --   plausability checks and ingame modifiers
            -> IO (Either String EC.Ingame)
                                -- ^ Return either an error 'String' on the
                                --   left or the 'EC.Ingame' on the right
ingameRun filename inits  = do
    let mergedInits = mergeInits inits
        IngameInit pChecks parsers mods = mergedInits
    storageEither <- loadFile2 parsers filename
    return $ do
        storages <- storageEither
        let ingame = applyIngameMods (EC.insertStorages storages
                                                        EC.defaultIngame) mods
        checkResults <- plausabilityCheck pChecks ingame
        return ingame

-- | Merge a list of 'IngameInit' into one 'IngameInit'
mergeInits :: [IngameInit] -> IngameInit
mergeInits xs = foldr mergeInit defaultLoader xs

mergeInit :: IngameInit -> IngameInit -> IngameInit
mergeInit (IngameInit pChecks1 parsers1 mods1)
          (IngameInit pChecks2 parsers2 mods2) =
            IngameInit (pChecks1 ++ pChecks2)
                       (parsers1 ++ parsers2)
                       (mods1 ++ mods2)


-- | Terminal modifier
newtype ReplMod = ReplMod (ET.Terminal -> ET.Terminal)

-- | Terminal initializer
data ReplInit = ReplInit [ReplMod] IngameInit

-- | Repl init generator
type ReplInitGen = ActionFactory -> IO ReplInit

-- | Create initializer function
replLoader :: ([ReplMod], IngameInit) -> ReplInit
replLoader (mods, ingameLoader) = ReplInit mods ingameLoader

-- | Run the shit
replRun :: FilePath -> [ReplInitGen] -> IO ET.Terminal
replRun filename gens = do
    loaders <- mapM ($ replActionFactory) gens
    let ingameInits = map ingameFromReplInit loaders
    possibleIngame <- ingameRun filename ingameInits
    case possibleIngame of
        Left err -> do
            putStr err
            return ET.defaultTerminal
        Right ingame -> do
            let mergedReplInits = mergedModsFromReplInit loaders
                terminalIngame = ET.setIngame ingame ET.defaultTerminal
                terminal = foldr applyReplMod terminalIngame mergedReplInits
            ET.repl terminal

-- | Get 'IngameInit's from 'ReplInit'
ingameFromReplInit :: ReplInit -> IngameInit
ingameFromReplInit (ReplInit _ ingameLoader) = ingameLoader

-- | Merge all 'ReplMod's from a list of 'ReplInit' into one List
mergedModsFromReplInit :: [ReplInit] -> [ReplMod]
mergedModsFromReplInit xs = concat mods
     where mods = map modsFromReplInit xs

-- | Get the 'ReplMod's from an 'ReplInit'
modsFromReplInit :: ReplInit -> [ReplMod]
modsFromReplInit (ReplInit mods _) = mods

-- | Apply all 'ReplMods' on an 'ET.Terminal'
applyReplMod :: ReplMod -> ET.Terminal -> ET.Terminal
applyReplMod (ReplMod mod) terminal = mod terminal

replDelayedAction :: (Int, Float) -> EC.Action -> EC.Action
replDelayedAction (ticks, _) act = EB.delayedAction ticks act

replShortDelayAction :: Float -> EC.Action -> EC.Action
replShortDelayAction _ act = act

replActionFactory = ActionFactory {
    delayedAction = replDelayedAction,
    shortDelayedAction = replShortDelayAction
}


--- Plausapility checks
-- | Check for every individual if it is only in placed in one room
checkUniqueIndividualInRoom :: PCheck
checkUniqueIndividualInRoom ingame =
    let individuals = EI.allIndividuals ingame
        results = map (checkIndividualUniquness ingame) individuals
    in foldr (>>) pCheckPassed results

checkIndividualUniquness :: EC.Ingame -> EI.Individual -> Either String ()
checkIndividualUniquness ingame ind =
    let indKey = EI.key ind
        rooms = EB.roomsOfIndividualId indKey ingame
    in if length rooms > 1 
            then Left ("Individual is assigned to multiple rooms: " ++ indKey)
            else pCheckPassed


-- | Check if all exits in the rooms really exist
checkValidExits :: PCheck
checkValidExits ingame =
    let rooms = ER.allRooms ingame
        exits = concat $ map ER.exits rooms
        results = map (checkValidExit ingame) exits
    in foldr (>>) pCheckPassed results

checkValidExit :: EC.Ingame -> (String, String) -> Either String ()
checkValidExit ingame (exitName, roomId) =
    case ER.getRoomMaybe ingame roomId of
        Nothing -> Left ("Room id " ++ roomId ++ " of exit " ++ exitName
                        ++ " not found")
        Just _ -> pCheckPassed

-- | Check if player is set up correctly
checkPlayerSetup :: PCheck
checkPlayerSetup ingame = if EB.player ingame == EI.nullIndividual
                                then Left "Player setup incorrect"
                                else pCheckPassed


-- | Default ingame setup
defaults :: ActionFactory -> IngameInit
defaults fac = ingameLoader (defaultPChecks,
                           defaultParsers,
                           defaultMods)

defaultPChecks = [checkUniqueIndividualInRoom,
                  checkValidExits,
                  checkPlayerSetup]
defaultParsers = []
defaultMods = []

-- | Default repl setup
defaultRepl :: ReplInitGen
defaultRepl fac = do return $ replLoader (defaultReplMods, defaults fac)

defaultReplMods = [defaultCommands]

defaultCommands :: ReplMod
defaultCommands = ReplMod fn
    where fn term = ET.addCommand "quit" ET.quitCommand $
                ET.addCommand "q" ET.quitCommand $
                ET.addCommand "b" ET.showRoomCmd $
                ET.addCommand "p" ET.showPlayerCmd $
                ET.addCommand "s" ET.showStateCmd $
                ET.addCommand "storage" ET.showStorageCmd $
                ET.addCommand "m" ET.moveCmd term
