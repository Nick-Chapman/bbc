module Bbc.Mem (
  Effect(..)
  ) where

import Control.Monad (ap,liftM)

import Bbc.Addr
import Bbc.Byte

instance Functor Effect where fmap = liftM
instance Applicative Effect where pure = return; (<*>) = ap
instance Monad Effect where return = Ret; (>>=) = Bind

data Effect a where
  Ret :: a -> Effect a
  Bind :: Effect a -> (a -> Effect b) -> Effect b
  Read :: Addr -> Effect Byte
  Write :: Addr -> Byte -> Effect ()
