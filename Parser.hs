import NXT.Parser
import NXT.Types

import System.Environment

main =
    do args <- getArgs
       case args of
         [filename] -> parseFile filename
         _ -> putStrLn "USAGE: ./Parser [filename]"

parseFile f =
    do r <- runFile pProg f
       mapM_ (\v -> putStrLn (prettyFD v)) r
