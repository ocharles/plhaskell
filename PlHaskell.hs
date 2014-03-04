{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module PlHaskell where

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
     = PgInt
     | PgBool
     | PgString
     deriving (Show)
  |])

type family InterpretPgType (t :: PgType) :: *
type instance InterpretPgType 'PgInt = Int32
type instance InterpretPgType 'PgString = String
type instance InterpretPgType 'PgBool = Bool

-- all interpretations of PgTypes are Typeable
pgTypeTypeable :: SPgType t -> Dict (Typeable (InterpretPgType t))
pgTypeTypeable SPgInt    = Dict
pgTypeTypeable SPgString = Dict
pgTypeTypeable SPgBool   = Dict

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

--------------------------------------------------------------------------------
foreign export ccall
  plhaskell_test :: CString -> CUInt -> Ptr (Ptr ()) -> Ptr CUInt -> Int -> Ptr (Ptr Int64) -> Int -> IO CInt

oidToPgType :: CUInt -> PgType
oidToPgType oid = case oid of
                    16 -> PgBool
                    23 -> PgInt
                    25 -> PgString
                    n -> error $ "Type oid " ++ show n ++ " not supported"

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
              SPgReturn{} -> apply sTypes f
              SPgArrow{} -> apply sTypes f (castPtr argValues)

  where

  apply
    :: SPgFunType t
    -> InterpretFunction t
    -> If (IsFunction t) (Ptr (Datum (FunctionHead t)) -> IO CInt)
                         (IO CInt)

  apply (SPgReturn t) x = do
    case hasStorableDatum t of
      Dict -> do
        let datum = mkDatumForSPgType t x
        poke (castPtr outputPtrPtr) datum
        return (pgTypeSize t x)

  apply (SPgArrow t ts) f = \arguments ->
    case hasStorableDatum t of
      Dict -> do
        MkDatum x <- peek arguments
        case ts of
          SPgReturn{} -> apply ts (f x)
          SPgArrow{} -> apply ts (f x) (arguments `plusPtr` datumSize)

  pgTypeSize :: SPgType t -> InterpretPgType t -> CInt
  pgTypeSize SPgString x = fromIntegral $ length x
  pgTypeSize SPgInt _ = 0
  pgTypeSize SPgBool _ = 0

  -- I feel like this should be inferrable, but I can't pull that off
  mkDatumForSPgType :: Storable (Datum t) => SPgType t -> InterpretPgType t -> Datum t
  mkDatumForSPgType _ x = MkDatum x
