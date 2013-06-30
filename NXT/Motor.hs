{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : NXT.Motor
-- Copyright   : Alexander Thiemann <mail@agrafix.net>
-- License     : BSD3
--
-- Maintainer  : Alexander Thiemann <mail@agrafix.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module NXT.Motor where

import NXT.Core
import Data.Typeable

data Motor = Motor deriving (Typeable)

_OUT_A_ :: V Motor
_OUT_A_ = mkLit 0

_OUT_B_ :: V Motor
_OUT_B_ = mkLit 1

_OUT_C_ :: V Motor
_OUT_C_ = mkLit 2

{--
_OUT_AB_ :: V Motor
_OUT_AB_ = mkLit 3

_OUT_AC_ :: V Motor
_OUT_AC_ = mkLit 4

_OUT_BC_ :: V Motor
_OUT_BC_ = mkLit 5

_OUT_ABC_ :: V Motor
_OUT_ABC_ = mkLit 6
--}

onFwd :: V (V Motor -> V Int -> V ())
onFwd = defExt "OnFwd"

onRev :: V (V Motor -> V Int -> V ())
onRev = defExt "OnRev"

off :: V (V Motor -> V ())
off = defExt "Off"
