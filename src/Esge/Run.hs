{-|
Module      : Esge.Run
Description : Esge engine initialization module
Copyright   : (c) Simon Goller, 2015
License     : BSD
Maintainer  : neosam@posteo.de



-}

module Esge.Run (PCheck, plausabilityCheck) where
import Data.Foldable (foldrM)
import qualified Esge.Core as EC

-- | Plausability check definition.  Left and an error message in used
--   in case of an error and Right on success.
type PCheck = EC.Ingame -> Either String ()

-- | Run a list of plausability checks.
plausabilityCheck :: [PCheck]           -- ^ List of checks
                    -> EC.Ingame        -- ^ Ingame state to chock
                    -> Either String () -- ^ Error message or success
plausabilityCheck xs ingame = foldr (>>) (Right ()) xs'
    where xs' = map ($ ingame) xs

