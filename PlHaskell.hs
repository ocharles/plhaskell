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
  plhaskell_test :: CString -> CString -> Ptr (Ptr ()) -> Ptr () -> IO CChar

plhaskell_test :: CString -> CString -> Ptr (Ptr ()) -> Ptr () -> IO CChar
plhaskell_test srcPtr typeNamePtr outputPtrPtr arguments = do
  src <- peekCString srcPtr
  typeName <- peekCString typeNamePtr
  types <- return (PgArrow PgInt (PgReturn $ case typeName of
                                               "int8" -> PgInt
                                               "text" -> PgString))

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
            apply sTypes f arguments

  where

  apply :: SPgFunType t -> InterpretFunction t -> Ptr () -> IO CChar
  apply (SPgReturn SPgInt) x _ = do
    poke outputPtrPtr (nullPtr `plusPtr` (fromIntegral x))
    return 1

  apply (SPgReturn SPgString) x _ = do
    newCString x >>= poke outputPtrPtr . castPtr
    return 0

  apply (SPgArrow t ts) f args =
    case t of
      SPgInt -> do
        x <- peek (castPtr arguments)
        apply ts (f x) args
