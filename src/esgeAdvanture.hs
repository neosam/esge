{-# OPTIONS -Wall #-} 
import qualified Esge.Run as ERun

main :: IO ()
main = do
    _ <- ERun.replRun "story.esge" [
            ERun.defaultRepl
        ]
    return ()
