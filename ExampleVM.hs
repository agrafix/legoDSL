import ExampleCode
import NXT.Interpretation

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Concurrent
import Control.Concurrent.STM

main =
    do t <- tree
       runResourceT $ do env <- runVM t
                         liftIO $ threadDelay 500000 -- 50 ms setup time
                         liftIO $ setLoop env
    where
      assertMotor name m expect =
          do got <- atomically $ do b <- readTVar (m_b m)
                                    c <- readTVar (m_c m)
                                    return (b, c)
             when (expect /= got) $ error $ "Assertion (" ++ name ++ ") failed. Expected motor values: "
                      ++ (show expect) ++ ". Got: " ++ (show got)

      setLoop t@(envQ, motor) =
          do atomically $ writeTBQueue envQ (RealEnv 55 55)
             threadDelay 100000 -- 10 ms
             assertMotor "light = 55" motor (37, 37)

             atomically $ writeTBQueue envQ (RealEnv 0 0)
             threadDelay 100000 -- 10 ms
             assertMotor "light = 0" motor (0, 75)

             atomically $ writeTBQueue envQ (RealEnv 100 0)
             threadDelay 100000 -- 10 ms
             assertMotor "light = 100" motor (75, 0)

             setLoop t
