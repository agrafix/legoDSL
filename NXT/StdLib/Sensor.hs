{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : NXT.Sensor
-- Copyright   : Alexander Thiemann <mail@agrafix.net>
-- License     : BSD3
--
-- Maintainer  : Alexander Thiemann <mail@agrafix.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module NXT.Stdlib.Sensor where

import NXT.Core

import Data.Typeable

data Sensor = Sensor deriving (Typeable)

_IN_1_ :: V Sensor
_IN_1_ = mkLit 0

_IN_2_ :: V Sensor
_IN_2_ = mkLit 1

_IN_3_ :: V Sensor
_IN_3_ = mkLit 2

_IN_4_ :: V Sensor
_IN_4_ = mkLit 3

setSensorLight :: V (V Sensor -> V ())
setSensorLight = defExt "SetSensorLight"

sensor :: V (V Sensor -> V Int)
sensor = defExt "Sensor"
