import qualified NXT.AttoParser as P
import NXT.Types

import System.Environment

main =
    do args <- getArgs
       case args of
         [filename] -> parseFile filename
         _ -> putStrLn "USAGE: ./Parser [filename]"

parseFile f =
    do pResult <- P.parseFile f
       case pResult of
         Left errorMsg ->
             putStrLn $ "Error: " ++ errorMsg
         Right funDefs ->
             mapM_ (\v -> putStrLn (prettyFD v)) funDefs
