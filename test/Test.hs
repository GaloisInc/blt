module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM, forM_, foldM, zipWithM)
import Data.Maybe (fromJust)
import Data.Ratio
import System.IO

import BLT.Binding

-- XXX use tast-HUnit
main = do
    hPutStrLn stderr "BLT: running test program"
    test1
    test2
    test_affine


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

  hPutStrLn stderr "bltInit done"
  zero <- bltConst ctx 0
  vs <- mapM (bltVar ctx) names
  rows <- forM coeff $ \c ->
    zipWithM (bltSmul ctx) c vs >>=
    foldM (bltAdd ctx) zero
  forM_ (zip rows bound) $ \(r, (l, h)) -> bltAssume ctx l r h
  hPutStrLn stderr "bltAssume done"

  rc <- bltCheck ctx
  hPutStrLn stderr "bltCheck done"
  putStrLn $ if rc == bltSAT
               then "SAT"
               else "UNSAT"  -- or error?
  sols <- mapM (bltModel ctx) names
  putStrLn $ "Solution: " ++ show (map fromJust sols)

  bltFree ctx
  putStrLn "Done."

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
  hPutStrLn stderr "Starting test 2"
  ctx <- bltInit RatID False
  x <- bltVar ctx "x"
  y <- bltVar ctx "y"
  hPutStrLn stderr "bltInit done"
  bltAssume ctx (0%1)    y 2
  bltAssume ctx (-100%1) x 3
  expr2 <- bltSmul ctx (-1) y >>= bltAdd ctx x
  bltAssume ctx (-100%1) expr2 2
  expr3 <- bltSmul ctx (-2) x >>= bltAdd ctx y
  bltAssume ctx (-100%1) expr3 0
  hPutStrLn stderr "bltAssume done"
  rc <- bltCheck ctx
  hPutStrLn stderr "bltCheck done"
  putStrLn $ if rc == bltSAT
               then "SAT"
               else "UNSAT"  -- or error?
  x0 <- fromJust <$> bltModel ctx "x"
  y0 <- fromJust <$> bltModel ctx "y"
  putStrLn $ "Solution: " ++ show [x0, y0]
  -- XXX validate solution
  bltFree ctx
  putStrLn "test2 Done."


-- Test solution given an affine system:
--
--   -100 <= x - y + 1/3 <= 2
--   -100 <= -2 x + y - 5/3 <= 0
--
test_affine :: IO ()
test_affine = do
  hPutStrLn stderr "Starting test_affine"
  ctx <- bltInit RatID False
  x <- bltVar ctx "x"
  y <- bltVar ctx "y"
  c <- bltConst ctx (1%3)  -- constant term 1/3
  c2 <- bltConst ctx (-5%3)  -- constant term -5/3
  hPutStrLn stderr "bltInit done"
  expr0 <- bltSmul ctx (-1) y >>= bltAdd ctx x >>= bltAdd ctx c
  bltAssume ctx (-100%1) expr0 2
  expr1 <- bltSmul ctx (-2) x >>= bltAdd ctx y >>= bltAdd ctx c2
  bltAssume ctx (-100%1) expr1 0
  hPutStrLn stderr "bltAssume done"
  rc <- bltCheck ctx
  hPutStrLn stderr "bltCheck done"
  putStrLn $ if rc == bltSAT
               then "SAT"
               else "UNSAT"  -- or error?
  x0 <- fromJust <$> bltModel ctx "x"
  y0 <- fromJust <$> bltModel ctx "y"
  putStrLn $ "Solution: " ++ show [x0, y0]
  -- XXX validate solution
  bltFree ctx
  putStrLn "test_affine Done."


-- XXX test scale parameter
-- XXX test overflow detection
