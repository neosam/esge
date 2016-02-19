{-# OPTIONS -Wall #-}
{-|
Module      : Esge.Individual
Description : Add individuals to game
Copyright   : (c) Simon Goller, 2015
License     : BSD
Maintainer  : neosam@posteo.de

Add Individual type and functions to the game.

-}

module Esge.Individual (
            Individual(Individual, key, name, desc, health, mana, items),
            allIndividuals,
            getIndividual,
            getIndividualNull,
            nullIndividual
        ) where

import qualified Esge.Core as EC

-- | Individual type
data Individual = Individual {
    key :: String,
    name :: String,
    desc :: String,
    health :: (Int, Int),
    mana :: (Int, Int),
    items :: [(String, String)]
} deriving (Show, Read, Eq)

-- | Used in case of errors
nullIndividual :: Individual
nullIndividual = Individual "nullIndividual" "" "" (0, 0) (0, 0) []

-- | Merge a pair of Int to a String
mergeIntPair :: String      -- ^ Separator between the numbers
            -> (Int, Int)   -- ^ Ints to merge
            -> String       -- ^ Generated String
mergeIntPair sep (a, b) = show a ++ sep ++ show b

-- | Merge a pair of Strings
mergeStringPair :: String               -- ^ Separator
                -> (String, String)     -- ^ Strings to merge
                -> String               -- ^ Generated String
mergeStringPair sep (a, b) = a ++ sep ++ b

--- | Split comma separated String into two Strings
strToStrPair :: String -> (String, String)
strToStrPair str = let (first, second) = break (== ',') str in
    (first, tail second)

-- | Split comma separaed Ints to Int pair
strToIntPair :: String -> (Int, Int)
strToIntPair str = (read a, read b)
    where (a, b) = strToStrPair str

-- | Individual can be stored in 'EC.Ingame'
instance EC.Storageable Individual where
    toStorage ind = EC.Storage (key ind) "individual" [
            ("name", name ind),
            ("desc", desc ind),
            ("health", mergeIntPair "," $ health ind),
            ("mana", mergeIntPair "," $ mana ind),
            ("items", unwords $ map (mergeStringPair ",") $ items ind)
        ]
    fromStorage (EC.Storage stKey t metas) =
        if t /= "individual" then Nothing
        else do
            stName <- lookup "name" metas
            stDesc <- lookup "desc" metas
            stHealth <- lookup "health" metas
            stMana <- lookup "mana" metas
            stItems <- lookup "items" metas
            return Individual {
                key = stKey,
                name = stName,
                desc = stDesc,
                health = strToIntPair stHealth,
                mana = strToIntPair stMana,
                items = map strToStrPair $ words stItems
            }

-- | Lookup Individual in 'EC.Ingame'
getIndividual :: EC.Ingame          -- ^ Ingame to search
             ->  String             -- ^ Individual key to search
             ->  Maybe Individual   -- ^ Individual if found or Nothing
getIndividual ingame k = do
    storage <- EC.storageGet k ingame
    EC.fromStorage storage

-- | Lookup Individual in 'EC.Ingame'
getIndividualNull :: EC.Ingame      -- ^ Ingame to search
                    -> String       -- ^ Individual key to search
                    -> Individual   -- ^ Individual if found or nullIndividual
getIndividualNull ingame k =
    maybe nullIndividual id $ getIndividual ingame k

-- | Return all individuals
allIndividuals :: EC.Ingame -> [Individual]
allIndividuals = EC.allOfType
