module Esge.Individual (
            Individual(Individual, key, name, desc, health, mana, items),
            getIndividual,
            getIndividualNull,
            nullIndividual
        ) where

import qualified Esge.Core as EC

data Individual = Individual {
    key :: String,
    name :: String,
    desc :: String,
    health :: (Int, Int),
    mana :: (Int, Int),
    items :: [(String, String)]
} deriving (Show, Read, Eq)

nullIndividual :: Individual
nullIndividual = Individual "nullIndividual" "" "" (0, 0) (0, 0) []

mergeIntPair :: String -> (Int, Int) -> String
mergeIntPair sep (a, b) = show a ++ sep ++ show b

mergeStringPair :: String -> (String, String) -> String
mergeStringPair sep (a, b) = a ++ sep ++ b

strToStrPair :: String -> (String, String)
strToStrPair str = let (first, second) = break (== ',') str in
    (first, tail second)

strToIntPair :: String -> (Int, Int)
strToIntPair str = (read a, read b)
    where (a, b) = strToStrPair str

instance EC.Storageable Individual where
    toStorage ind = EC.Storage (key ind) "individual" [
            ("name", name ind),
            ("desc", desc ind),
            ("health", mergeIntPair "," $ health ind),
            ("mana", mergeIntPair "," $ mana ind),
            ("items", unwords $ map (mergeStringPair ",") $ items ind)
        ]
    fromStorage (EC.Storage key t metas) =
        if t /= "individual" then Nothing
        else do
            name <- lookup "name" metas
            desc <- lookup "desc" metas
            health <- lookup "health" metas
            mana <- lookup "mana" metas
            items <- lookup "items" metas
            return Individual {
                key = key,
                name = name,
                desc = desc,
                health = strToIntPair health,
                mana = strToIntPair mana,
                items = map strToStrPair $ words items
            }

getIndividual :: EC.Ingame -> String -> Maybe Individual
getIndividual ingame key = do
    storage <- EC.storageGet key ingame
    EC.fromStorage storage

getIndividualNull :: EC.Ingame -> String -> Individual
getIndividualNull ingame key =
    maybe nullIndividual id $ getIndividual ingame key
