module Mecanism.Types where

type Output a = Array a
type Input a = Array a

type System a = Input a -> Output a
