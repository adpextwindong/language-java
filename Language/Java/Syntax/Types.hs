{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Language.Java.Syntax.Types where

import Data.Data
import GHC.Generics (Generic)

-- | There are two kinds of types in the Java programming language: primitive types and reference types.
data Type a
    = PrimType (PrimType a) a
    | RefType (RefType a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor, Foldable, Traversable)

-- | There are three kinds of reference types: class types, interface types, and array types.
--   Reference types may be parameterized with type arguments.
--   Type variables cannot be syntactically distinguished from class type identifiers,
--   and are thus represented uniformly as single ident class types.
data RefType a
    = ClassRefType (ClassType a) a
    {- | TypeVariable Ident -}
    | ArrayType (Type a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor, Foldable, Traversable)

-- | A class or interface type consists of a type declaration specifier,
--   optionally followed by type arguments (in which case it is a parameterized type).
data ClassType a
    = ClassType a [(Ident a, [TypeArgument a])]
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor, Foldable, Traversable)

-- | Type arguments may be either reference types or wildcards.
data TypeArgument a
    = Wildcard a (Maybe (WildcardBound a))
    | ActualType a (RefType a)
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor, Foldable, Traversable)

data TypeDeclSpecifier a
    = TypeDeclSpecifier (ClassType a) a
    | TypeDeclSpecifierWithDiamond (ClassType a) (Ident a) (Diamond a) a
    | TypeDeclSpecifierUnqualifiedWithDiamond (Ident a) (Diamond a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor, Foldable, Traversable)

data Diamond a = Diamond a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor, Foldable, Traversable)

-- | Wildcards may be given explicit bounds, either upper (@extends@) or lower (@super@) bounds.
data WildcardBound a
    = ExtendsBound a (RefType a)
    | SuperBound a (RefType a)
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor, Foldable, Traversable)

-- | A primitive type is predefined by the Java programming language and named by its reserved keyword.
data PrimType a
    = BooleanT a
    | ByteT a
    | ShortT a
    | IntT a
    | LongT a
    | CharT a
    | FloatT a
    | DoubleT a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor, Foldable, Traversable)


-- | A class is generic if it declares one or more type variables. These type variables are known
--   as the type parameters of the class.
data TypeParam a = TypeParam (Ident a) [RefType a] a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor, Foldable, Traversable)

-----------------------------------------------------------------------
-- Names and identifiers

-- | A single identifier.
data Ident a = Ident String a
    deriving (Eq,Ord,Show,Read,Typeable,Generic,Data, Functor, Foldable, Traversable)

-- | A name, i.e. a period-separated list of identifiers.
data Name a = Name [Ident a] a
    deriving (Eq,Ord,Show,Read,Typeable,Generic,Data, Functor, Foldable, Traversable)
