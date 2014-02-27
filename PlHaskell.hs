{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module PlHaskell where

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
     | PgString
     deriving (Show)
  |])

type family InterpretPgType (t :: PgType) :: *
type instance InterpretPgType 'PgInt = Int32
type instance InterpretPgType 'PgString = String

-- all interpretations of PgTypes are Typeable
pgTypeTypeable :: SPgType t -> Dict (Typeable (InterpretPgType t))
pgTypeTypeable SPgInt    = Dict
pgTypeTypeable SPgString = Dict

--------------------------------------------------------------------------------
$(singletons [d|
  data PgFunType
    = PgReturn PgType
    | PgArrow PgType PgFunType
    deriving (Show)
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
  plhaskell_test :: CString -> CString -> Ptr (Ptr ()) -> Ptr CUInt -> Int -> Ptr (Ptr ()) -> Int -> IO CInt

plhaskell_test :: CString -> CString -> Ptr (Ptr ()) -> Ptr CUInt -> Int -> Ptr (Ptr ()) -> Int -> IO CInt
plhaskell_test srcPtr typeNamePtr outputPtrPtr argTypes argCount argValues datumSize  = do
  src <- peekCString srcPtr
  typeName <- peekCString typeNamePtr

  let determineType ptr n t
        | n == 0 = return t
        | n > 0  = do oid <- peek ptr
                      tail <- determineType (ptr `plusPtr` sizeOf oid) (n - 1) t
                      return $ PgArrow
                        (case oid of
                           23 -> PgInt
                           n -> error $ "Input OID " ++ show n)
                        tail

  types <- determineType argTypes argCount (PgReturn $ case typeName of
                                               "int4" -> PgInt
                                               "int8" -> PgInt
                                               "text" -> PgString
                                               t -> error t)

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
            apply sTypes f argValues

  where

  apply :: SPgFunType t -> InterpretFunction t -> Ptr (Ptr ()) -> IO CInt
  apply (SPgReturn SPgInt) x _ = do
    poke outputPtrPtr (nullPtr `plusPtr` (fromIntegral x))
    return 0

  apply (SPgReturn SPgString) x _ = do
    (cStrPtr, cStrLen) <- newCStringLen x
    cStrPtr' <- reallocBytes cStrPtr (cStrLen + 4)
    moveBytes (cStrPtr' `plusPtr` 4) cStrPtr' cStrLen
    poke outputPtrPtr (castPtr cStrPtr)
    return (fromIntegral cStrLen)

  apply (SPgArrow t ts) f arguments =
    case t of
      SPgInt -> do
        x <- fmap (fromIntegral . ptrToIntPtr) (peek arguments)
        apply ts (f x) (arguments `plusPtr` datumSize)
