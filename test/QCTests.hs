{-|
Module      : QCTests
Description : QuickCheck tests for BLT
Copyright   : (c) Benjamin F Jones, 2017
License     : MIT
Maintainer  : bjones@galois.com
Stability   : experimental
Portability : POSIX

We generate SAT ILP instances (of varying shape and dimension)
for BLT to solve and validate the results.
-}

module QCTests
  ( testSuite
  )
where

import Control.Applicative ((<$>))
import Control.Monad (foldM, forM, forM_, zipWithM, replicateM)
import Data.List (delete)
import Data.Ratio
import System.IO
import Text.Printf (printf)

import Test.QuickCheck

import BLT.Binding


-- | Export the list of unit tests to run.
testSuite :: [IO ()]
testSuite = map quickCheck [ test1Prop ]


-- | Type for specifying BLT problems
data BLTProb = BLTProb
  { _rows :: [[Rational]]
  , _bounds :: [(Rational, Rational)]
  }
  deriving (Eq, Show)

-- | Generate random SAT instances for BLT. To do this we
-- a positive number of non-zero row vectors and then generate
-- the same number of lower/upper bound pairs (l,u) such that 0
-- is contained in the interval. Thus the zero vector is always
-- a solution to this system.
genSATProb
  :: Int  -- ^ bound on number of rows / cols
  -> Int  -- ^ bound on abs value of lower/upper bounds
  -> Gen BLTProb
genSATProb boundR boundB = do
  -- generate random coefficient matrix sizes bounded by 10
  n <- choose (1, boundR) :: Gen Int
  m <- choose (1, boundR) :: Gen Int
  rowsI <- replicateM n (vector m) :: Gen [[Int]]
  let rowsR = map (map fromIntegral) rowsI
  -- gaurantee that at least one row is non-zero
      (fstR:restR) = rowsR
      fstR'  = if all (==0) fstR then (1:tail fstR) else fstR
      rowsR' = fstR':restR
  -- generate upper/lower bounds. Always include (0,...,0) so the system
  -- will be SAT
  lbounds <- map fromIntegral <$>
               replicateM n (choose (-boundB, -1) :: Gen Int)
  ubounds <- map fromIntegral <$>
               replicateM n (choose (1, boundB) :: Gen Int)
  return (BLTProb rowsR' (zip lbounds ubounds))

-- | Shrink BLT problems by removing rows of coefficients.
shrinkBLTProb :: BLTProb -> [BLTProb]
shrinkBLTProb (BLTProb rows bounds) =
    if length rows > 1
    then
      -- shrink by deleting rows, but only if doing so results in
      -- a coefficient matrix with a non-zero row
      [ BLTProb (delete r rows) (delete b bounds)
      | (r,b) <- zip rows bounds, any nzr (delete r rows)]
    else
      []
  where
    nzr = not . all (==0)


-- | Set the parameters for test sizes. We have to stay relatively
-- small here or integer overflow will occur frequently due to the
-- randomness of the coefficients and bounds.
test1Prop :: Property
test1Prop = forAllShrink (genSATProb 20 10)
                         shrinkBLTProb test1

-- | Test a random SAT instance of BLT.
test1 :: BLTProb -> Property
test1 (BLTProb rs bs) = ioProperty $ do
  let names = map (printf "x%d") [0..(length bs - 1)]
  -- putStrLn (concat names)
  ctx <- bltInit RatID False
  zero <- bltConst ctx 0
  vs <- mapM (bltVar ctx) names

  -- putStrLn $ "rows: " ++ show rs
  -- putStrLn $ "bounds: " ++ show bs
  rows <- forM rs $ \c ->
    zipWithM (bltSmul ctx) c vs >>=
    foldM (bltAdd ctx) zero
  forM_ (zip rows bs) $ \(r, (l, h)) -> bltAssume ctx l r h

  rc <- bltCheck ctx
  if rc == bltSAT
  then do
    bltFree ctx
    return True
  else do
    bltFree ctx
    return False

