{-
 - High level Haskell bindings to BLT, a lattice-based integer linear
 - programming solver.
-}

module BLT.Binding
    (
      -- * BLT Solver API
      bltInit
    , bltVar
    , bltConst
    , bltAdd
    , bltSmul
    , bltAssume
    , bltCheck
    , bltModel
    , bltSave
    , bltBacktrack
    , bltFree
    , HContext()
    , RatMode(..)
      -- * re-exports from BLT.Internal
    , BLT.Internal.CExpr
    , BLT.Internal.CSolvState
    , BLT.Internal.BLTCode
      -- * BLT return codes
    , bltSAT
    , bltUNSAT
    ) where

import Data.Int (Int64)
import Data.Ratio (Rational, numerator, denominator, (%))
import Foreign
import Foreign.C.String
import Foreign.Marshal.Utils (fromBool)

import BLT.Internal


-- | Rename some constants of type 'BLTCode' for better compatibility
bltSAT                = bltCheckSat
bltUNSAT              = bltCheckUnsat
bltSTATUS_OK          = bltStatusOk
bltSTATUS_ERROR       = bltStatusError
bltSTATUS_INPUT_ERROR = bltStatusInputError
bltSTATUS_LP_FAIL     = bltStatusLpFail

-- | Choice of modes for handling rational coefficients
data RatMode = RatID           -- ^ pass rat'l coefficients through to BLT

             | RatApprox Int   -- ^ use an approximation strategy which limits the
                               --   precision of denominators

             | RatFixed Int64  -- ^ use a fixed precision strategy which scales all
                               --   coefficients uniformly and then rounds to
                               --   the nearest integer (towards zero on ties)
  deriving (Eq, Show)

-- | A problem context
data HContext = HContext
  { getMode  :: RatMode    -- ^ a mode choice, see 'RatMode'
  , getCtx   :: CContext   -- ^ opaque pointer to the C context struct
  } deriving (Eq, Show)


-- BLT API -------------------------------------------------------------

-- | Create a new problem context
--
-- Each call to `bltInit` must be paired with a call to `bltFree` when its
-- problem context is no longer needed.
bltInit :: RatMode  -- ^ mode for handing high precision rationals
        -> Bool     -- ^ flag to enable Yices "sound mode" in BLT
        -> IO HContext
bltInit m b = do
  ctx <- c_blt_init (fromBool b)
  return HContext { getMode = m, getCtx = ctx }

-- | Create a fresh variable with given name in the context
bltVar :: HContext -> String -> IO CExpr
bltVar ctx name = withCString name $ c_blt_var (getCtx ctx)

-- | Create a constant term
bltConst :: HContext -> Rational -> IO CExpr
bltConst ctx k = c_blt_const (getCtx ctx) (fi nk) (fi dk)
  where
    (nk, dk) = case getMode ctx of
      RatApprox p -> reduceRat p k
      _ -> reduceRat defPrec k

-- | Add two expressions
bltAdd :: HContext -> CExpr -> CExpr -> IO CExpr
bltAdd ctx = c_blt_add (getCtx ctx)

-- | Multiply an expression by a scalar. The scalar is checked for overflow
-- before being passed to the C-API.
bltSmul :: HContext -> Rational -> CExpr -> IO CExpr
bltSmul ctx k = c_blt_smul (getCtx ctx) (fi nk') (fi dk')
  where
    (nk', dk') = chk $ case getMode ctx of
      RatApprox p -> reduceRat p k
      _ -> reduceRat defPrec k  -- both RatID and RatFix behave the same here
    chk :: (Integer, Integer) -> (Integer, Integer)
    chk (a, b) =
      if doesOverflow64 a || doesOverflow64 b
        then error $ unlines
          [ "ERROR: BLT: Integer overflow in bltSmul"
          , "  Try using a smaller, non-zero, precision value (-p), or"
          , "  fixed precision (scaling) mode (-s)" ]
        else (a, b)

-- | Push a new assumption onto the stack
bltAssume :: HContext   -- ^ context pointer
          -> Rational   -- ^ left hand side (lower bound)
          -> CExpr      -- ^ linear middle term
          -> Rational   -- ^ right hand side (upper bound)
          -> IO BLTCode -- ^ return code
bltAssume ctx l e u =
  case getMode ctx of
    RatID -> c_blt_assume ctx' nl dl e nu du

    RatApprox p -> c_blt_assume ctx' (fi nl') (fi dl') e (fi nu') (fi du')
      where
      (nl', dl') = reduceRat p l
      (nu', du') = reduceRat p u

    RatFixed s -> do
        e' <- c_blt_tof (getCtx ctx) (fi s) e
        c_blt_assume ctx' (fi il) 1 e' (fi iu) 1
      where
        il = fixRat s l
        iu = fixRat s u

  where
    ctx' = getCtx ctx
    nl = fi . numerator $ l
    dl = fi . denominator $ l
    nu = fi . numerator $ u
    du = fi . denominator $ u

-- | Check the current assumption stack for satisfiability
bltCheck :: HContext -> IO BLTCode
bltCheck ctx = c_blt_check (getCtx ctx)

-- | Get the value of a variable in the solver's model
--
-- Return the value assigned to variable with given name or Nothing if there
-- is no value or if the variable doesn't exist in the given context.
bltModel :: HContext -> String -> IO (Maybe Int64)
bltModel ctx name =
  do fptr <- mallocForeignPtr
     (rc, val) <- withForeignPtr fptr $ \ptr -> do
         r <- withCString name (\n -> c_blt_model (getCtx ctx) n ptr)
         v <- peek ptr
         return (r, v)
     return $ if rc == bltSTATUS_OK
                  then Just (fi val)
                  else Nothing

-- | Save the current solver state
--
-- Return an opaque pointer to the current assumption stack so that it may be
-- backtracked to later using `bltBacktrack`.
bltSave :: HContext -> IO CSolvState
bltSave ctx = c_blt_save (getCtx ctx)

-- | Load a previously saved solver state
--
-- Backtrack the current context to the state represented by the given
-- CSolvState value. This operation removes assumptions from the stack
-- but preserves the list of known variables.
bltBacktrack :: HContext -> CSolvState -> IO BLTCode
bltBacktrack ctx = c_blt_backtrack (getCtx ctx)

-- | Free memory associated with the given context
bltFree :: HContext -> IO ()
bltFree ctx = c_blt_free (getCtx ctx)


-- Misc ----------------------------------------------------------------

-- | Transform a rational into an integer by scaling and rounding
fixRat :: Int64 -> Rational -> Int64
fixRat s r = round (fi s * r)

-- | Default denominator precision to reduce to
defPrec :: Int
defPrec = 50

-- | Transform the given rational number into a ``reduced'' one
--
-- EITHER ('p' > 0) the closest rational whose denominator can be represented as
-- a 'p'-bit signed integer.
--
-- OR ('p' <= 0) itself.
--
reduceRat :: Int                 -- ^ 'p' precision
          -> Rational            -- ^ 'r' input rational number
          -> (Integer, Integer)  -- ^ (numerator, denominator) of transformed result
reduceRat p r | p <= 0    = (rn, rd)
              | otherwise = (round (rn % shift), round (rd % shift))
  where
    rn = numerator r
    rd = denominator r
    -- smallest power of 2 larger than |rd|
    l  = ceiling (logBase 2 (fromIntegral rd))
    -- powers of 2 to divide out (+2) is needed to guarant the representation
    -- claim case rd is a power of 2.
    n  = max 0 (l - p + 2)
    shift = 2^n :: Integer

-- | Alias for fromIntegral
fi :: (Num b, Integral a) => a -> b
fi = fromIntegral

-- | Check that the input will overflow when cast to a 64-bit signed integer
--
-- Return True if the argument will overflow a 64-bit signed integer (used
-- to check that we don't silently overflow when calling the C-API that
-- expects Int64 values).
--
-- Note: this doesn't guarantee that overflow won't occur in the rational
-- and/or integer arithmetic on the C side.
doesOverflow64 :: (Integral a) => a -> Bool
doesOverflow64 x = x' > m'
  where
    m = maxBound :: Int64
    -- cast m and x to Integer before comparison
    m' = fromIntegral m :: Integer
    x' = fromIntegral x :: Integer
