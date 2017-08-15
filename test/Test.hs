{-|
Module      : Main
Description : Tests for BLT
Copyright   : (c) Benjamin F Jones, 2017
License     : MIT
Maintainer  : bjones@galois.com
Stability   : experimental
Portability : POSIX

This module is the main test driver for unit and property-based
tests for BLT.
-}

module Main where

import qualified UnitTests as UT
import qualified QCTests as QC

main = do
    putStrLn "BLT: running unit tests ..."
    sequence_ UT.testSuite

    putStrLn "BLT: running QuickCheck tests ..."
    sequence_ QC.testSuite
