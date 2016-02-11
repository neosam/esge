{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Test.Hspec
import qualified Esge.Core as EC
import qualified Esge.Individual as EI
import qualified Esge.Room as ER
import qualified Esge.Parser as EP
import qualified Esge.Base as EB
import qualified Esge.Run as ERun


-- I import qualified so that it's clear which
-- functions are from the parsec library:
import qualified Text.Parsec as Parsec

-- I am the error message infix operator, used later:
--import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
--import Control.Applicative

-- Get the Identity monad from here:
--import Control.Monad.Identity (Identity)



defaultStorage :: EC.Storage
defaultStorage = EC.Storage "title" "type" []

secondStorage :: EC.Storage
secondStorage = EC.Storage "title" "type2" []

outputAction :: EC.Action
outputAction ingame = EC.setIngameResponse "output" "test" ingame

myIndividual :: EI.Individual
myIndividual = EI.Individual "ind" "Mr. Blub" "This is mister blub"
                             (100, 100) (200, 200) [("some", "thing")]

foobarParser :: EP.BlockParser
foobarParser = do
    _ <- Parsec.string "Foobar"
    EP.dbleol
    return $ EC.Storage "foo" "bar" []


myRoom :: ER.Room
myRoom = ER.Room "room" "A room" "This is a room" ["ind"]
                [("Garage", "garage"), ("Brewery", "brewery")]
                [("Bitcoin", "coin"), ("Beer", "beer")]

main :: IO ()
main = hspec $ do
    describe "The core module" $ do
        context "which provides a storage functionality" $ do
            let ingame = EC.defaultIngame
            let inserted = EC.storageInsert defaultStorage ingame
            let storage1 = EC.storageGet "uiae" inserted
            let storage2 = EC.storageGet "title" inserted
            let inserted2 = EC.storageInsert secondStorage ingame
            it "should return Nothing if nothing was inserted" $ do
                storage1 `shouldBe` Nothing
            it "stould return the right inserted value" $ do
                storage2 `shouldBe`  (Just defaultStorage)
            it "should return the last inserted value" $ do
                EC.storageGet "title" inserted2 `shouldBe` (Just secondStorage)
            it "should be possible to delete a value" $ do
                let removed = EC.storageRemove "title" inserted
                    hopefullyNothing = EC.storageGet "title" removed
                hopefullyNothing `shouldBe` Nothing
        context "which provides a action and response functionality" $ do
            let ingame = EC.defaultIngame
            let inserted = EC.scheduleAction outputAction ingame
            let ran = EC.step inserted
            let output = EC.getIngameResponse "output" ran
            let ran2 = EC.step ran
            let output2 = EC.getIngameResponse "output" ran2
            it "should provide the given response in output" $ do
                output `shouldBe` "test"
            it "should be empty after the next step" $ do
                output2 `shouldBe` ""

    describe "The individual module" $ do
        it "has a key" $ do
            EI.key myIndividual `shouldBe` "ind"
        it "has a name" $ do
            EI.name myIndividual `shouldBe` "Mr. Blub"
        it "has a description" $ do
            EI.desc myIndividual `shouldBe` "This is mister blub"
        it "has a health" $ do
            EI.health myIndividual `shouldBe` (100, 100)
        it "has a mana" $ do
            EI.mana myIndividual `shouldBe` (200, 200)
        it "has some items" $ do
            EI.items myIndividual `shouldBe` [("some", "thing")]
        it "should be convertable to storage and from storage" $ do
            (Just myIndividual) `shouldBe`
                EC.fromStorage (EC.toStorage  myIndividual)

    describe "The room module" $ do
        it "has a key" $ do
            ER.key myRoom `shouldBe` "room"
        it "has a title" $ do
            ER.title myRoom `shouldBe` "A room"
        it "has a description" $ do
            ER.desc myRoom `shouldBe` "This is a room"
        it "has individuals" $ do
            ER.individual myRoom `shouldBe` ["ind"]
        it "has exits" $ do
            ER.exits myRoom `shouldBe` [("Garage", "garage"),
                                        ("Brewery", "brewery")]
        it "has items in it" $ do
            ER.items myRoom `shouldBe` [("Bitcoin", "coin"), ("Beer", "beer")]
        it "should be convertable to and from storage" $ do
            (Just myRoom) `shouldBe` EC.fromStorage (EC.toStorage myRoom)

    describe "The parser module" $ do
        it "should parse blocks" $ do
            let parseStr = "New thing thingie\n" ++
                           "key1: value1\n" ++
                           "key2: value2\n" ++
                           "\n\n\n" ++
                           "New thong thingie\n\n"
            let dest = [EC.Storage "thingie" "thing" 
                            [("key1", "value1"), ("key2", "value2")],
                        EC.Storage "thingie" "thong" []]
            EP.loadString [] "(source)" parseStr `shouldBe` (Right dest)
        it "should parse empty values" $ do
            let parseStr = "New thing thingie\n" ++
                           "key1:\n" ++
                           "key2:a\n\n\n"
            let dest = [EC.Storage "thingie" "thing"
                            [("key1", ""), ("key2", "a")]]
            EP.loadString [] "(source)" parseStr `shouldBe` (Right dest)
        it "should support custom block parsers" $ do
            let parseStr = "Foobar\n\n"
            let dest = [EC.Storage "foo" "bar" []]
            EP.loadString [foobarParser] "(source)" parseStr `shouldBe`
                                                                (Right dest)

    describe "The Base Module" $ do
        context "which can handle rooms and individuals" $ do
            let room2 = ER.Room "brewery" "Room 2" "..." ["anna"] [] []
                anna = EI.Individual "anna" "Anna" ".." (0, 0) (0, 0) []
                ingame = EC.storageInsert myIndividual $
                            EC.storageInsert myRoom $
                            EC.storageInsert room2 $
                            EC.storageInsert anna EC.defaultIngame
            it "knows which individuals are in a spacific room" $ do
                EB.individualsInRoom myRoom ingame `shouldBe`
                    Just [myIndividual]
            it "knows in which room an individual is" $ do
                EB.roomOfIndividual myIndividual ingame `shouldBe` myRoom
            it "should be able to switch a person to another room" $ do
                let eitherIngame = EB.beam myIndividual "brewery" ingame
                case eitherIngame of
                    Left err -> expectationFailure $ show err
                    Right ingame' -> do
                        let room = EB.roomOfIndividual
                                            myIndividual ingame' :: ER.Room
                            room' = ER.getRoom ingame' "room" :: ER.Room
                        ER.key room `shouldBe` "brewery"
                        ER.individual room' `shouldSatisfy`
                            (\x -> not $ elem "ind" x)
            it "will move a person only if its room is connected by exits" $ do
                let moveStorage = do
                        ingame' <- EB.move myIndividual "Brewery" ingame
                        return $ EC.storage ingame'
                    beamStorage = do
                        ingame' <- EB.beam myIndividual "brewery" ingame
                        return $ EC.storage ingame'
                moveStorage `shouldBe` beamStorage
            it "will let a person pick something up" $ do
                pendingWith "future version"
        context "which provides some acions" $ do
            let myAction ingame = EC.setIngameResponse "output" "foo" ingame
                ing = EC.defaultIngame
                test ingame = case EC.storageGet "ind" ingame of
                    Nothing -> False
                    Just _ -> True
            it "will provide in infinityAction" $ do
                let ingame = EC.scheduleAction act ing
                    act = EB.infinityAction myAction
                let ingame' = EC.step ingame
                EC.getIngameResponse "output" ingame' `shouldBe` "foo"
                let ingame'' = EC.step ingame'
                EC.getIngameResponse "output" ingame'' `shouldBe` "foo"
            it "will provide a conditional infinity action" $ do
                let ingame = EC.scheduleAction act 
                                $ EC.storageInsert myIndividual ing
                    act = EB.condInfinityAction test myAction
                let ingame' = EC.step ingame
                EC.getIngameResponse "output" ingame' `shouldBe` "foo"
                let ingame'' = EC.step ingame
                EC.getIngameResponse "output" ingame'' `shouldBe` "foo"
                let ingame'3 = EC.storageRemove "ind" ingame''
                let ingame'4 = EC.step ingame'3
                EC.getIngameResponse "output" ingame'4 `shouldBe` ""
                let ingame'5 = EC.step ingame'4
                EC.getIngameResponse "output" ingame'5 `shouldBe` ""
                let ingame'6 = EC.storageInsert myIndividual ingame'5
                let ingame'7 = EC.step ingame'6
                EC.getIngameResponse "output" ingame'7 `shouldBe` "foo"
            it "will provide a conditional dropable action" $ do
                let ingame = EC.scheduleAction act 
                                $ EC.storageInsert myIndividual ing
                    act = EB.condDropableAction test myAction
                let ingame' = EC.step ingame
                EC.getIngameResponse "output" ingame' `shouldBe` "foo"
                let ingame'' = EC.step ingame
                EC.getIngameResponse "output" ingame'' `shouldBe` "foo"
                let ingame'3 = EC.storageRemove "ind" ingame''
                let ingame'4 = EC.step ingame'3
                EC.getIngameResponse "output" ingame'4 `shouldBe` ""
                let ingame'5 = EC.step ingame'4
                EC.getIngameResponse "output" ingame'5 `shouldBe` ""
                let ingame'6 = EC.storageInsert myIndividual ingame'5
                let ingame'7 = EC.step ingame'6
                EC.getIngameResponse "output" ingame'7 `shouldBe` ""
            


    describe "The Run Module" $ do
        context "which provides an individual check" $ do
            let pCheckGood _ = Right ()
                pCheckBad _ = Left "Error message"
                pCheckHasInd ingame = case EC.storageGet "ind" ingame of
                    Nothing -> Left "ind not found"
                    Just _ -> Right ()
                emptyIngame = EC.defaultIngame
                indIngame = EC.storageInsert myIndividual emptyIngame
            it "should be no error if the check returns Right ()" $ do
                ERun.plausabilityCheck [pCheckGood] emptyIngame
                                                    `shouldBe` Right ()
            it "should return an errer of the check is bad" $ do
                ERun.plausabilityCheck [pCheckBad] emptyIngame
                                                `shouldBe` Left "Error message"
            it "should return an error since ind is not found" $ do
                ERun.plausabilityCheck [pCheckHasInd] emptyIngame
                                                `shouldBe` Left "ind not found"
            it "should return no error since ind is found now" $ do
                ERun.plausabilityCheck [pCheckHasInd] indIngame
                                                `shouldBe` Right ()
            it "should return an error if just one test fails" $ do
                ERun.plausabilityCheck [pCheckGood, pCheckHasInd] emptyIngame
                                                `shouldBe` Left "ind not found"
                ERun.plausabilityCheck [pCheckHasInd, pCheckGood] emptyIngame
                                                `shouldBe` Left "ind not found"
            it "should return no error if all tests pass" $ do
                ERun.plausabilityCheck [pCheckGood, pCheckHasInd] indIngame
                                                `shouldBe` Right ()

