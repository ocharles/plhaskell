{-# LANGUAGE ForeignFunctionInterface #-}
module PlHaskell where

test :: IO Int
test = return 5

foreign export ccall test :: IO Int
