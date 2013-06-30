module ExampleCode
( tree
) where

import NXT.Core
import NXT.Common
import NXT.Motor
import NXT.Sensor

import Prelude hiding ((<), (>), (<=), (>=), (==), (/=), (&&), (||))

black = 45
white = 65

setMotorVal sensorVal =
    do motorVal <- newVar
       ifThenElse (sensorVal < black) (motorVal $= 0) $
          ifThenElse (sensorVal > white) (motorVal $= 1) $
             motorVal $= ((castFloat (sensorVal - black)) / (castFloat range))

       left <- newVar
       right <- newVar

       left $= motorVal
       right $= ((-1.0) * motorVal + 1.0)

       whenBlock left right
       whenBlock right left

       vCallF2 onFwd _OUT_B_ $ castInt $ left * 75.0
       vCallF2 onFwd _OUT_C_ $ castInt $ right * 75.0
    where
      whenBlock a b =
          when ((a <= 0.2) && (a >= 0.8)) $
            do a $= (-0.5)
               b $= 0.5

      range = white - black


follow old =
    do new <- newVar
       new $= callF1 sensor _IN_3_
       when (old /= new) (setMotorVal new)
       vCallF1 wait 50
       ret new

tree = mkTree $
  do followF <- newFun
     def followF follow

     mainF <- newFun
     def mainF $ do vCallF1 setSensorLight _IN_3_
                    curVal <- newVar
                    curVal $= callF1 sensor _IN_3_
                    while (true) $
                          curVal $= callF1 followF curVal
                    ret void
     setMain mainF
