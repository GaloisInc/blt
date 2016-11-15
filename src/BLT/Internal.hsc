{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module BLT.Internal where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

#include "codes.h"


-- | Type wrapper for BLT return codes defined in "codes.h"
newtype BLTCode = BLTCode { _code :: Int }
  deriving (Eq, Ord)

instance Show BLTCode where
  show (BLTCode c) = show c


-- | Type whose C representation is guaranteed to be at least 64 bits. Note
-- the two LL's in CLLong.
type CInt64 = CLLong

-- | Types used to represent the opaque types from BLT; they have no concrete
-- values other than bottom.
data Ctx
data Expr
data SolvState

-- | Opaque pointers (of the same name) returned by BLT
type CContext = Ptr Ctx
type CExpr = Ptr Expr
type CSolvState = Ptr SolvState


-- | Defines Haskell constants: statusOK, statusError, ...
#{ enum BLTCode, BLTCode \
 , BLT_STATUS_OK      , BLT_STATUS_ERROR \
 , BLT_STATUS_LP_FAIL , BLT_STATUS_INPUT_ERROR \
 , BLT_CHECK_SAT      , BLT_CHECK_UNSAT \
 , LP_OK , LP_ERROR \
 }

-- | blt_init
foreign import ccall unsafe "blt.h blt_init"
  c_blt_init :: Int -> IO CContext

-- | blt_var
foreign import ccall unsafe "blt.h blt_var"
  c_blt_var :: CContext -> CString -> IO CExpr

-- | blt_const
foreign import ccall unsafe "blt.h blt_const"
  c_blt_const :: CContext -> CInt64 -> CInt64 -> IO CExpr

-- | blt_add
foreign import ccall unsafe "blt.h blt_add"
  c_blt_add :: CContext -> CExpr -> CExpr -> IO CExpr

-- | blt_smul
foreign import ccall unsafe "blt.h blt_smul"
  c_blt_smul :: CContext -> CInt64 -> CInt64 -> CExpr -> IO CExpr

-- | blt_tof
foreign import ccall unsafe "blt.h blt_tof"
  c_blt_tof :: CContext -> CInt64 -> CExpr -> IO CExpr

-- | blt_assume
foreign import ccall unsafe "blt.h blt_assume"
  c_blt_assume :: CContext -> CInt64 -> CInt64 -> CExpr -> CInt64 -> CInt64 -> IO BLTCode

-- | blt_check
foreign import ccall unsafe "blt.h blt_check"
  c_blt_check :: CContext -> IO BLTCode

-- | blt_model
foreign import ccall unsafe "blt.h blt_model"
  c_blt_model :: CContext -> CString -> Ptr CInt64 -> IO BLTCode

-- | blt_save
foreign import ccall unsafe "blt.h blt_save"
  c_blt_save :: CContext -> IO CSolvState

-- | blt_backtrack
foreign import ccall unsafe "blt.h blt_backtrack"
  c_blt_backtrack :: CContext -> CSolvState -> IO BLTCode

-- | blt_free
foreign import ccall unsafe "blt.h blt_free"
  c_blt_free :: CContext -> IO ()
