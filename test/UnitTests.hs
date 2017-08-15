module UnitTests
  ( testSuite
  )
where

import Control.Applicative ((<$>))
import Control.Monad (forM, forM_, foldM, zipWithM)
import Data.Maybe (fromJust)
import Data.Ratio
import System.Exit
import System.IO

import BLT.Binding


-- | Export the list of unit tests to run.
testSuite :: [IO ()]
testSuite = [ test1, test2, test_affine, test_affine_UNSAT ]


-- | 4 dimensional inverse DCT problem
test1 :: IO ()
test1 = do
  let coeff = [ [10703, 4433, -4433, -10703]
              , [4433, -10703, 10703, -4433]
              , [8192, -8192, -8192, 8192]
              , [8192, 8192, 8192, 8192]
              ]
  let bound = [ (16384, 344064)
              , (49152, 376832)
              , (32768, 360448)
              , (0, 327680)
              ]
  let names = words "x y z w"

  ctx <- bltInit RatID False

  zero <- bltConst ctx 0
  vs <- mapM (bltVar ctx) names
  rows <- forM coeff $ \c ->
    zipWithM (bltSmul ctx) c vs >>=
    foldM (bltAdd ctx) zero
  forM_ (zip rows bound) $ \(r, (l, h)) -> bltAssume ctx l r h

  rc <- bltCheck ctx
  sols <- mapM (bltModel ctx) names

  -- TODO validate
  bltFree ctx

-- | Test the higher level binding with a fun 2IP problem from the Russian
-- Mathematics Olympiad:
--
--      0 <= y       <= 2
--   -inf <= x       <= 3
--   -inf <= x - y   <= 2
--   -inf <= -2x + y <= 0
--
test2 :: IO ()
test2 = do
  ctx <- bltInit RatID False
  x <- bltVar ctx "x"
  y <- bltVar ctx "y"
  bltAssume ctx (0%1)    y 2
  bltAssume ctx (-100%1) x 3
  expr2 <- bltSmul ctx (-1) y >>= bltAdd ctx x
  bltAssume ctx (-100%1) expr2 2
  expr3 <- bltSmul ctx (-2) x >>= bltAdd ctx y
  bltAssume ctx (-100%1) expr3 0
  rc <- bltCheck ctx
  x0 <- fromJust <$> bltModel ctx "x"
  y0 <- fromJust <$> bltModel ctx "y"
  -- TODO validate
  bltFree ctx


-- Test solution given an affine system:
--
--   -100 <= x - y + 1/3 <= 2
--   -100 <= -2 x + y - 5/3 <= 0
--
test_affine :: IO ()
test_affine = do
  ctx <- bltInit RatID False
  x <- bltVar ctx "x"
  y <- bltVar ctx "y"
  c <- bltConst ctx (1%3)  -- constant term 1/3
  c2 <- bltConst ctx (-5%3)  -- constant term -5/3
  expr0 <- bltSmul ctx (-1) y >>= bltAdd ctx x >>= bltAdd ctx c
  bltAssume ctx (-100%1) expr0 2
  expr1 <- bltSmul ctx (-2) x >>= bltAdd ctx y >>= bltAdd ctx c2
  bltAssume ctx (-100%1) expr1 0
  rc <- bltCheck ctx
  x0 <- fromIntegral . fromJust <$> bltModel ctx "x"
  y0 <- fromIntegral . fromJust <$> bltModel ctx "y"

  if (-100 > x0 - y0 + 1%3 || x0 - y0 + 1%3 > 2
     || -100 > -2*x0 + y0 - 5%3 || -2*x0 + y0 - 5%3 > 0)
  then do
    bltFree ctx
    hPutStrLn stderr "UnitTests: test_affine failed!"
    exitWith (ExitFailure 1)
  else do
    putStrLn "UnitTests: test_affine passed!"
    bltFree ctx

-- Test that a modification to the 'test_affine' system is UNSAT.
--
--   7/4 <= x - y + 1/3 <= 2
--  -1   <= -2 x + y - 5/3 <= 0
--
test_affine_UNSAT :: IO ()
test_affine_UNSAT = do
  ctx <- bltInit RatID False
  x <- bltVar ctx "x"
  y <- bltVar ctx "y"
  c <- bltConst ctx (1%3)  -- constant term 1/3
  c2 <- bltConst ctx (-5%3)  -- constant term -5/3
  expr0 <- bltSmul ctx (-1) y >>= bltAdd ctx x >>= bltAdd ctx c
  bltAssume ctx (7%4) expr0 2
  expr1 <- bltSmul ctx (-2) x >>= bltAdd ctx y >>= bltAdd ctx c2
  bltAssume ctx (-1%1) expr1 0
  rc <- bltCheck ctx
  x0 <- fromIntegral . fromJust <$> bltModel ctx "x"
  y0 <- fromIntegral . fromJust <$> bltModel ctx "y"

  if (7%4 > x0 - y0 + 1%3 || x0 - y0 + 1%3 > 2
     || -1 > -2*x0 + y0 - 5%3 || -2*x0 + y0 - 5%3 > 0)
  then do
    bltFree ctx
    putStrLn "UnitTests: test_affine_UNSAT passed!"
  else do
    hPutStrLn stderr $
      "UnitTests: test_affine_UNSAT failed! x=" ++ show x0 ++
      ", y=" ++ show y0
    bltFree ctx
    exitWith (ExitFailure 1)


-- TODO test scale parameter
-- TODO test overflow detection
