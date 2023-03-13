{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveGeneric #-}
module Language.Java.Syntax.Exp where

import Data.Data
import GHC.Generics (Generic)

-- | A literal denotes a fixed, unchanging value.
data Literal a
    = Int Integer a
    | Word Integer a
    | Float Double a
    | Double Double a
    | Boolean Bool a
    | Char Char a
    | String String a
    | Null a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | A binary infix operator.
data Op a = Mult a| Div a| Rem a| Add a | Sub a | LShift a | RShift a | RRShift a
        | LThan a | GThan a | LThanE a | GThanE a | Equal a | NotEq a
        | And a| Or a| Xor a| CAnd a| COr a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | An assignment operator.
data AssignOp a = EqualA a | MultA a | DivA a | RemA a | AddA a | SubA a
              | LShiftA a| RShiftA a| RRShiftA a| AndA a| XorA a| OrA a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)
