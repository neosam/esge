{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Esge.Parser (loadFile, loadString) where


-- I import qualified so that it's clear which
-- functions are from the parsec library:
import qualified Text.Parsec as Parsec

-- I am the error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)

import qualified Esge.Core as EC

-- alias Parsec.parse for more concise usage in my examples:
parse rule text = Parsec.parse rule "(source)" text



loadFile filename = do
    storyFile <- readFile filename
    return (Parsec.parse parseFile filename storyFile)

loadString = Parsec.parse parseFile

eol :: Parsec.Parsec String () String
eol = Parsec.choice [Parsec.try (Parsec.string "\n\r"),
                     Parsec.string "\n",
                     Parsec.string "\r"] <?> "End of line"

dbleol :: Parsec.Parsec String () ()
dbleol = do
    eol
    Parsec.many (Parsec.oneOf "\n\r")
    return ()

parseFile :: Parsec.Parsec String () [EC.Storage]
parseFile = Parsec.manyTill storage Parsec.eof

storage :: Parsec.Parsec String () EC.Storage
storage = do
    (bId, bType) <- newLine
    m <- metas
    return $ EC.Storage bId bType m

newLine :: Parsec.Parsec String () (String, String)
newLine = do
    Parsec.string "New" <?> "New keyword"
    Parsec.spaces
    bType <- keyword <?> "Item identifier"
    Parsec.spaces
    bId <- keyword <?> "Type identifier"
    eol
    return (bId, bType)

metas :: Parsec.Parsec String () [(String, String)]
metas = Parsec.manyTill meta dbleol

meta :: Parsec.Parsec String () (String, String)
meta = do
    key <- keyword
    Parsec.char ':'
    Parsec.many (Parsec.oneOf " \t")
    value <- Parsec.many (Parsec.noneOf "\n\r")
    eol
    return (key, value)

keyword :: Parsec.Parsec String () String
keyword = Parsec.many1 $ Parsec.oneOf (['a'..'z']++['A'..'Z']++['0'..'9'])
