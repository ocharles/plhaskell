{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module PlHaskell where

import Control.Applicative
import Data.Bits
import Data.Constraint (Dict(..))
import Data.Int
import Data.Singletons
import Data.Singletons.TH
import Data.Typeable (Typeable)
import Foreign
import Foreign.C
import Language.Haskell.Interpreter

--------------------------------------------------------------------------------
$(singletons [d|
   data PgType
     = PgBool
     | PgInt
     | PgFloat4
     | PgFloat8
     | PgString
     deriving (Show)
  |])

type family InterpretPgType (t :: PgType) :: *
type instance InterpretPgType 'PgBool = Bool
type instance InterpretPgType 'PgFloat4 = Float
type instance InterpretPgType 'PgFloat8 = Double
type instance InterpretPgType 'PgInt = Int32
type instance InterpretPgType 'PgString = String

-- all interpretations of PgTypes are Typeable
pgTypeTypeable :: SPgType t -> Dict (Typeable (InterpretPgType t))
pgTypeTypeable SPgInt    = Dict
pgTypeTypeable SPgString = Dict
pgTypeTypeable SPgBool   = Dict
pgTypeTypeable SPgFloat4 = Dict
pgTypeTypeable SPgFloat8 = Dict

--------------------------------------------------------------------------------
$(singletons [d|
  data PgFunType
    = PgReturn PgType
    | PgArrow PgType PgFunType
    deriving (Show)

  isFunction :: PgFunType -> Bool
  isFunction (PgReturn _) = False
  isFunction (PgArrow _ _) = True

  functionHead :: PgFunType -> PgType
  functionHead (PgReturn r) = r
  functionHead (PgArrow t _) = t
  |])

type family InterpretFunction (t :: PgFunType) :: *
type instance InterpretFunction ('PgReturn t) = InterpretPgType t
type instance InterpretFunction ('PgArrow t ts) = InterpretPgType t -> InterpretFunction ts

-- All PgFunTypes are Typeable by virtue of being made from -> and
-- InterpretPgType (shown Typeable by pgTypeTypeable)
pgFunTypeTypeable :: SPgFunType t -> Dict (Typeable (InterpretFunction t))
pgFunTypeTypeable (SPgReturn (pgTypeTypeable -> Dict)) = Dict
pgFunTypeTypeable (SPgArrow (pgTypeTypeable -> Dict) (pgFunTypeTypeable -> Dict)) = Dict

spgFunType :: SPgFunType t -> InterpretFunction t
spgFunType (SPgReturn _) = undefined
spgFunType (SPgArrow _ bs) = \_ -> spgFunType bs

hasStorableDatum :: SPgType t -> Dict (Storable (Datum t))
hasStorableDatum SPgInt    = Dict
hasStorableDatum SPgString = Dict
hasStorableDatum SPgBool   = Dict
hasStorableDatum SPgFloat4 = Dict
hasStorableDatum SPgFloat8 = Dict

--------------------------------------------------------------------------------
data Datum :: PgType -> * where
  MkDatum :: InterpretPgType t -> Datum t

instance Storable (Datum 'PgInt) where
  sizeOf _ = sizeOf (0 :: IntPtr)
  alignment _ = alignment (0 :: IntPtr)

  peek ptr = do
    x <- peek (castPtr ptr)
    return (MkDatum $ fromIntegral (x :: IntPtr))

  poke ptr (MkDatum x) =
    poke (castPtr ptr) (fromIntegral x :: IntPtr)

instance Storable (Datum 'PgString) where
  sizeOf _ = sizeOf (0 :: IntPtr)
  alignment _ = alignment (0 :: IntPtr)

  peek ptr = do
    vardataPtr <- peek (castPtr ptr)
    sz <- fmap (`shiftR` 2) (peek vardataPtr)
    if sz > 4
      then do str <- peekCStringLen (vardataPtr `plusPtr` 4, fromIntegral $ (sz :: CUInt) - 4)
              return (MkDatum str)
      else return (MkDatum "")

  poke ptr (MkDatum x) = do
    (cStrPtr, cStrLen) <- newCStringLen x
    cStrPtr' <- reallocBytes cStrPtr (cStrLen + 4)
    moveBytes (cStrPtr' `plusPtr` 4) cStrPtr' cStrLen
    poke (castPtr cStrPtr') ((fromIntegral cStrLen + 4) `shiftL` 2 :: CUInt)
    poke (castPtr ptr) cStrPtr'

instance Storable (Datum 'PgBool) where
  sizeOf _ = sizeOf (0 :: IntPtr)
  alignment _ = alignment (0 :: IntPtr)

  peek ptr = do
    x <- peek (castPtr ptr)
    return $ MkDatum $ case fromIntegral (x :: IntPtr) :: Int of
      0 -> False
      1 -> True
      _ -> error "Impossible"

  poke ptr (MkDatum x) =
    poke (castPtr ptr) (if x then 1 else 0 :: IntPtr)

#define PASS_BY_REFERENCE(t)                \
  instance Storable (Datum 't) where           \
    sizeOf _ = sizeOf (0 :: IntPtr)           ; \
    alignment _ = alignment (0 :: IntPtr)     ; \
    peek ptr = MkDatum <$> peek (castPtr ptr) ; \
    poke ptr (MkDatum x) = poke (castPtr ptr) x

PASS_BY_REFERENCE (PgFloat4)
PASS_BY_REFERENCE (PgFloat8)

#undef PASS_BY_REFERENCE

oidToPgType :: CUInt -> PgType
oidToPgType oid = case oid of
  16 -> PgBool
  23 -> PgInt
  25 -> PgString
  700 -> PgFloat4
  701 -> PgFloat8
  n -> error $ "Type oid " ++ show n ++ " not supported"

--------------------------------------------------------------------------------
foreign export ccall
  plhaskell_test :: CString -> CUInt -> Ptr (Ptr ()) -> Ptr CUInt -> Int -> Ptr (Ptr Int64) -> Int -> IO CInt

plhaskell_test :: CString -> CUInt -> Ptr (Ptr ()) -> Ptr CUInt -> Int -> Ptr (Ptr Int64) -> Int -> IO CInt
plhaskell_test srcPtr returnType outputPtrPtr argTypes argCount argValues datumSize  = do
  src <- peekCString srcPtr

  let determineType ptr n t
        | n == 0 = return t
        | n > 0  = do oid <- peek ptr
                      tail <- determineType (ptr `plusPtr` sizeOf oid) (n - 1) t
                      return $ PgArrow (oidToPgType oid) tail

  types <- determineType argTypes argCount (PgReturn $ oidToPgType returnType)

  withSomeSing types $ \sTypes ->
    case pgFunTypeTypeable sTypes of
      Dict -> do
        interpreted <- runInterpreter $ do
          setImportsQ [ ("Prelude", Nothing), ("Data.Int", Nothing) ]
          interpret src (spgFunType sTypes)

        case interpreted of
          Left e -> do
            newCString (show e) >>= poke outputPtrPtr . castPtr
            return (-1)

          Right f ->
            case sTypes of
              SPgReturn{} -> 0 <$ apply sTypes f
              SPgArrow{} -> 0 <$ apply sTypes f (castPtr argValues)

  where

  apply
    :: SPgFunType t
    -> InterpretFunction t
    -> If (IsFunction t) (Ptr (Datum (FunctionHead t)) -> IO ())
                         (IO ())

  apply (SPgReturn t) x = do
    case hasStorableDatum t of
      Dict -> do
        let datum = mkDatumForSPgType t x
        poke (castPtr outputPtrPtr) datum

  apply (SPgArrow t ts) f = \arguments ->
    case hasStorableDatum t of
      Dict -> do
        MkDatum x <- peek arguments
        case ts of
          SPgReturn{} -> apply ts (f x)
          SPgArrow{} -> apply ts (f x) (arguments `plusPtr` datumSize)

  -- I feel like this should be inferrable, but I can't pull that off
  mkDatumForSPgType :: Storable (Datum t) => SPgType t -> InterpretPgType t -> Datum t
  mkDatumForSPgType _ x = MkDatum x
