{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : NXT.Interpretation
-- Copyright   : Alexander Thiemann <mail@agrafix.net>
-- License     : BSD3
--
-- Maintainer  : Alexander Thiemann <mail@agrafix.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module NXT.Common where

import NXT.Core
import Data.Typeable

wait :: V (V Integer -> V ())
wait = defExt "Wait"
