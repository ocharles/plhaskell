{-# LANGUAGE ForeignFunctionInterface #-}
module PlHaskell where

import Data.Int
import Foreign
import Foreign.C
import Language.Haskell.Interpreter

foreign export ccall plhaskell_test :: CString -> CString -> Ptr (Ptr ()) -> IO CChar

plhaskell_test :: CString -> CString -> Ptr (Ptr ()) -> IO CChar
plhaskell_test srcPtr typeNamePtr outputPtrPtr = do
  src <- peekCString srcPtr
  typeName <- peekCString typeNamePtr

  interpreted <- runInterpreter $ do
    setImportsQ [ ("Prelude", Nothing)
                , ("Data.Int", Nothing)
                ]

    case typeName of
      "text" -> do
        str <- interpret src (as :: String)
        liftIO $ newCString str >>= poke outputPtrPtr . castPtr
        return 0

      "int4" -> do
        x <- interpret src (as :: Data.Int.Int32)
        liftIO $ poke outputPtrPtr (nullPtr `plusPtr` (fromIntegral x))
        return 1

      _ -> error $ "Whoops, don't know what to do with " ++ typeName

  case interpreted of
    Left e -> do
      newCString (show e) >>= poke outputPtrPtr . castPtr
      return (-1)

    Right x -> return x
