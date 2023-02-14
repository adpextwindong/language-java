{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Language.Java.Syntax
    ( CompilationUnit(..)
    , PackageDecl(..)
    , ImportDecl(..)
    , TypeDecl(..)
    , ClassDecl(..)
    , ClassBody(..)
    , EnumBody(..)
    , EnumConstant(..)
    , InterfaceDecl(..)
    , InterfaceBody(..)
    , InterfaceKind(..)
    , Decl(..)
    , MemberDecl(..)
    , VarDecl(..)
    , VarDeclId(..)
    , VarInit(..)
    , FormalParam(..)
    , MethodBody(..)
    , ConstructorBody(..)
    , ExplConstrInv(..)
    , Modifier(..)
    , Annotation(..)
    , desugarAnnotation
    , desugarAnnotation'
    , ElementValue(..)
    , Block(..)
    , BlockStmt(..)
    , Stmt(..)
    , Catch(..)
    , SwitchBlock(..)
    , SwitchLabel(..)
    , ForInit(..)
    , ExceptionType
    , Argument
    , Exp(..)
    , Lhs(..)
    , ArrayIndex(..)
    , FieldAccess(..)
    , LambdaParams(..)
    , LambdaExpression(..)
    , ArrayInit(..)
    , MethodInvocation(..)
    , module Language.Java.Syntax.Exp
    , module Language.Java.Syntax.Types
    ) where

import Data.Data
import Data.Tuple.Extra
import GHC.Generics (Generic)

import Language.Java.Syntax.Types
import Language.Java.Syntax.Exp

-----------------------------------------------------------------------
-- Packages


-- | A compilation unit is the top level syntactic goal symbol of a Java program.
data CompilationUnit a = CompilationUnit (Maybe (PackageDecl a)) [ImportDecl a] [TypeDecl a] a
  deriving (Eq,Show,Read,Typeable,Generic,Data)


-- | A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
data PackageDecl a = PackageDecl (Name a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An import declaration allows a static member or a named type to be referred to by a single unqualified identifier.
--   The first argument signals whether the declaration only imports static members.
--   The last argument signals whether the declaration brings all names in the named type or package, or only brings
--   a single name into scope.
data ImportDecl a
    = ImportDecl Bool {- static? -} (Name a) Bool {- .*? -} a
  deriving (Eq,Show,Read,Typeable,Generic,Data)


-----------------------------------------------------------------------
-- Declarations

-- | A type declaration declares a class type or an interface type.
data TypeDecl a
    = ClassTypeDecl (ClassDecl a) a
    | InterfaceTypeDecl (InterfaceDecl a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A class declaration specifies a new named reference type.
data ClassDecl a
    = ClassDecl [Modifier a] (Ident a) [TypeParam a] (Maybe (RefType a)) [RefType a] (ClassBody a) a
    | EnumDecl  [Modifier a] (Ident a)                             [RefType a] (EnumBody a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A class body may contain declarations of members of the class, that is,
--   fields, classes, interfaces and methods.
--   A class body may also contain instance initializers, static
--   initializers, and declarations of constructors for the class.
data ClassBody a = ClassBody [Decl a] a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | The body of an enum type may contain enum constants.
data EnumBody a = EnumBody [EnumConstant a] [Decl a] a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An enum constant defines an instance of the enum type.
data EnumConstant a = EnumConstant (Ident a) [Argument a] (Maybe (ClassBody a)) a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An interface declaration introduces a new reference type whose members
--   are classes, interfaces, constants and abstract methods. This type has
--   no implementation, but otherwise unrelated classes can implement it by
--   providing implementations for its abstract methods.
data InterfaceDecl a
    = InterfaceDecl InterfaceKind [Modifier a] (Ident a) [TypeParam a] [RefType a] (InterfaceBody a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Interface can declare either a normal interface or an annotation
data InterfaceKind = InterfaceNormal | InterfaceAnnotation
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | The body of an interface may declare members of the interface.
data InterfaceBody a
    = InterfaceBody [MemberDecl a] a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A declaration is either a member declaration, or a declaration of an
--   initializer, which may be static.
data Decl a
    = MemberDecl (MemberDecl a) a
    | InitDecl Bool (Block a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data)


-- | A class or interface member can be an inner class or interface, a field or
--   constant, or a method or constructor. An interface may only have as members
--   constants (not fields), abstract methods, and no constructors.
data MemberDecl a
    -- | The variables of a class type are introduced by field declarations.
    = FieldDecl [Modifier a] (Type a) [VarDecl a] a
    -- | A method declares executable code that can be invoked, passing a fixed number of values as arguments.
    | MethodDecl      [Modifier a] [TypeParam a] (Maybe (Type a)) (Ident a) [FormalParam a] [ExceptionType a] (Maybe (Exp a)) (MethodBody a) a
    -- | A constructor is used in the creation of an object that is an instance of a class.
    | ConstructorDecl [Modifier a] [TypeParam a]              (Ident a) [FormalParam a] [ExceptionType a] (ConstructorBody a) a
    -- | A member class is a class whose declaration is directly enclosed in another class or interface declaration.
    | MemberClassDecl (ClassDecl a) a
    -- | A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
    | MemberInterfaceDecl (InterfaceDecl a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data)


-- | A declaration of a variable, which may be explicitly initialized.
data VarDecl a
    = VarDecl (VarDeclId a) (Maybe (VarInit a)) a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | The name of a variable in a declaration, which may be an array.
data VarDeclId a
    = VarId (Ident a) a
    | VarDeclArray (VarDeclId a) a
    -- ^ Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Explicit initializer for a variable declaration.
data VarInit a
    = InitExp (Exp a) a
    | InitArray (ArrayInit a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A formal parameter in method declaration. The last parameter
--   for a given declaration may be marked as variable arity,
--   indicated by the boolean argument.
data FormalParam a = FormalParam [Modifier a] (Type a) Bool (VarDeclId a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A method body is either a block of code that implements the method or simply a
--   semicolon, indicating the lack of an implementation (modelled by 'Nothing').
data MethodBody a = MethodBody (Maybe (Block a)) a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | The first statement of a constructor body may be an explicit invocation of
--   another constructor of the same class or of the direct superclass.
data ConstructorBody a = ConstructorBody (Maybe (ExplConstrInv a)) [BlockStmt a] a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An explicit constructor invocation invokes another constructor of the
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately
--   enclosing instance.
data ExplConstrInv a
    = ThisInvoke             [RefType a] [Argument a] a
    | SuperInvoke            [RefType a] [Argument a] a
    | PrimarySuperInvoke (Exp a) [RefType a] [Argument a] a
  deriving (Eq,Show,Read,Typeable,Generic,Data)


-- | A modifier specifying properties of a given declaration. In general only
--   a few of these modifiers are allowed for each declaration type, for instance
--   a member type declaration may only specify one of public, private or protected.
data Modifier a
    = Public a
    | Private a
    | Protected a
    | Abstract a
    | Final a
    | Static a
    | StrictFP a
    | Transient a
    | Volatile a
    | Native a
    | Annotation (Annotation a) a
    | Synchronized_ a
  deriving (Eq,Read,Typeable,Generic,Data)

instance Show a => Show (Modifier a) where
   show (Public _) = "public"
   show (Private _) = "private"
   show (Protected _)  = "protected"
   show (Abstract _) = "abstract"
   show (Final _) = "final"
   show (Static _) = "static"
   show (StrictFP _) = "strictfp"
   show (Transient _) = "transient"
   show (Volatile _) = "volatile"
   show (Native _) = "native"
   show (Annotation a _) = show a
   show (Synchronized_ _) = "synchronized"

-- | Annotations have three different forms: no-parameter, single-parameter or key-value pairs
data Annotation a = NormalAnnotation        { annName :: Name a-- Not type because not type generics not allowed
                                            , annKV   :: [(Ident a, ElementValue a)]
                                            , srcInfo :: a }

                  | SingleElementAnnotation { annName :: Name a
                                            , annValue:: ElementValue a
                                            , srcInfo :: a }

                  | MarkerAnnotation        { annName :: Name a
                                            , srcInfo :: a}

  deriving (Eq,Show,Read,Typeable,Generic,Data)


desugarAnnotation :: Annotation a -> (Name a, [(Ident a, ElementValue a)], a)
desugarAnnotation (MarkerAnnotation n a)          = (n, [], a)
desugarAnnotation (SingleElementAnnotation n e a) = (n, [(Ident "value" (evInfo e), e)], a)
desugarAnnotation (NormalAnnotation n kv a)       = (n, kv, a)

desugarAnnotation' :: Annotation a -> Annotation a
desugarAnnotation' = uncurry3 NormalAnnotation . desugarAnnotation


-- | Annotations may contain  annotations or (loosely) expressions
data ElementValue a = EVVal (VarInit a) a
                  | EVAnn (Annotation a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

evInfo :: ElementValue a -> a
evInfo (EVVal _ a) = a
evInfo (EVAnn _ a) = a
-----------------------------------------------------------------------
-- Statements

-- | A block is a sequence of statements, local class declarations
--   and local variable declaration statements within braces.
data Block a = Block [BlockStmt a] a
  deriving (Eq,Show,Read,Typeable,Generic,Data)



-- | A block statement is either a normal statement, a local
--   class declaration or a local variable declaration.
data BlockStmt a
    = BlockStmt (Stmt a) a
    | LocalClass (ClassDecl a) a
    | LocalVars [Modifier a] (Type a) [VarDecl a] a
  deriving (Eq,Show,Read,Typeable,Generic,Data)


-- | A Java statement.
data Stmt a
    -- | A statement can be a nested block.
    = StmtBlock (Block a) a
    -- | The @if-then@ statement allows conditional execution of a statement.
    | IfThen (Exp a) (Stmt a) a
    -- | The @if-then-else@ statement allows conditional choice of two statements, executing one or the other but not both.
    | IfThenElse (Exp a) (Stmt a) (Stmt a) a
    -- | The @while@ statement executes an expression and a statement repeatedly until the value of the expression is false.
    | While (Exp a) (Stmt a) a
    -- | The basic @for@ statement executes some initialization code, then executes an expression, a statement, and some
    --   update code repeatedly until the value of the expression is false.
    | BasicFor (Maybe (ForInit a)) (Maybe (Exp a)) (Maybe [Exp a]) (Stmt a) a
    -- | The enhanced @for@ statement iterates over an array or a value of a class that implements the @iterator@ interface.
    | EnhancedFor [Modifier a] (Type a) (Ident a) (Exp a) (Stmt a) a
    -- | An empty statement does nothing.
    | Empty a
    -- | Certain kinds of expressions may be used as statements by following them with semicolons:
    --   assignments, pre- or post-inc- or decrementation, method invocation or class instance
    --   creation expressions.
    | ExpStmt (Exp a) a
    -- | An assertion is a statement containing a boolean expression, where an error is reported if the expression
    --   evaluates to false.
    | Assert (Exp a) (Maybe (Exp a)) a
    -- | The switch statement transfers control to one of several statements depending on the value of an expression.
    | Switch (Exp a) [SwitchBlock a] a
    -- | The @do@ statement executes a statement and an expression repeatedly until the value of the expression is false.
    | Do (Stmt a) (Exp a) a
    -- | A @break@ statement transfers control out of an enclosing statement.
    | Break (Maybe (Ident a)) a
    -- | A @continue@ statement may occur only in a while, do, or for statement. Control passes to the loop-continuation
    --   point of that statement.
    | Continue (Maybe (Ident a)) a
    -- A @return@ statement returns control to the invoker of a method or constructor.
    | Return (Maybe (Exp a)) a
    -- | A @synchronized@ statement acquires a mutual-exclusion lock on behalf of the executing thread, executes a block,
    --   then releases the lock. While the executing thread owns the lock, no other thread may acquire the lock.
    | Synchronized (Exp a) (Block a) a
    -- | A @throw@ statement causes an exception to be thrown.
    | Throw (Exp a) a
    -- | A try statement executes a block. If a value is thrown and the try statement has one or more catch clauses that
    --   can catch it, then control will be transferred to the first such catch clause. If the try statement has a finally
    --   clause, then another block of code is executed, no matter whether the try block completes normally or abruptly,
    --   and no matter whether a catch clause is first given control.
    | Try (Block a) [Catch a] (Maybe {- finally -} (Block a)) a
    -- | Statements may have label prefixes.
    | Labeled (Ident a) (Stmt a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | If a value is thrown and the try statement has one or more catch clauses that can catch it, then control will be
--   transferred to the first such catch clause.
data Catch a = Catch (FormalParam a) (Block a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A block of code labelled with a @case@ or @default@ within a @switch@ statement.
data SwitchBlock a
    = SwitchBlock (SwitchLabel a) [BlockStmt a] a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A label within a @switch@ statement.
data SwitchLabel a
    -- | The expression contained in the @case@ must be a 'Lit' or an @enum@ constant.
    = SwitchCase (Exp a) a
    | Default a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Initialization code for a basic @for@ statement.
data ForInit a
    = ForLocalVars [Modifier a] (Type a) [VarDecl a] a
    | ForInitExps [Exp a] a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An exception type has to be a class type or a type variable.
type ExceptionType a = RefType a -- restricted to ClassType or TypeVariable

-- | Arguments to methods and constructors are expressions.
type Argument a = Exp a

-- | A Java expression.
data Exp a
    -- | A literal denotes a fixed, unchanging value.
    = Lit (Literal a) a
    -- | A class literal, which is an expression consisting of the name of a class, interface, array,
    --   or primitive type, or the pseudo-type void (modelled by 'Nothing'), followed by a `.' and the token class.
    | ClassLit (Maybe (Type a)) a
    -- | The keyword @this@ denotes a value that is a reference to the object for which the instance method
    --   was invoked, or to the object being constructed.
    | This a
    -- | Any lexically enclosing instance can be referred to by explicitly qualifying the keyword this.
    | ThisClass (Name a) a
    -- | A class instance creation expression is used to create new objects that are instances of classes.
    -- | The first argument is a list of non-wildcard type arguments to a generic constructor.
    --   What follows is the type to be instantiated, the list of arguments passed to the constructor, and
    --   optionally a class body that makes the constructor result in an object of an /anonymous/ class.
    | InstanceCreation [TypeArgument a] (TypeDeclSpecifier a) [Argument a] (Maybe (ClassBody a)) a
    -- | A qualified class instance creation expression enables the creation of instances of inner member classes
    --   and their anonymous subclasses.
    | QualInstanceCreation (Exp a) [TypeArgument a] (Ident a) [Argument a] (Maybe (ClassBody a)) a
    -- | An array instance creation expression is used to create new arrays. The last argument denotes the number
    --   of dimensions that have no explicit length given. These dimensions must be given last.
    | ArrayCreate (Type a) [Exp a] Int a
    -- | An array instance creation expression may come with an explicit initializer. Such expressions may not
    --   be given explicit lengths for any of its dimensions.
    | ArrayCreateInit (Type a) Int (ArrayInit a) a
    -- | A field access expression.
    | FieldAccess (FieldAccess a) a
    -- | A method invocation expression.
    | MethodInv (MethodInvocation a) a
    -- | An array access expression refers to a variable that is a component of an array.
    | ArrayAccess (ArrayIndex a) a
{-    | ArrayAccess Exp Exp -- Should this be made into a datatype, for consistency and use with Lhs? -}
    -- | An expression name, e.g. a variable.
    | ExpName (Name a) a
    -- | Post-incrementation expression, i.e. an expression followed by @++@.
    | PostIncrement a (Exp a)
    -- | Post-decrementation expression, i.e. an expression followed by @--@.
    | PostDecrement a (Exp a)
    -- | Pre-incrementation expression, i.e. an expression preceded by @++@.
    | PreIncrement  a (Exp a)
    -- | Pre-decrementation expression, i.e. an expression preceded by @--@.
    | PreDecrement  a (Exp a)
    -- | Unary plus, the promotion of the value of the expression to a primitive numeric type.
    | PrePlus  a (Exp a)
    -- | Unary minus, the promotion of the negation of the value of the expression to a primitive numeric type.
    | PreMinus a (Exp a)
    -- | Unary bitwise complementation: note that, in all cases, @~x@ equals @(-x)-1@.
    | PreBitCompl a (Exp a)
    -- | Logical complementation of boolean values.
    | PreNot  a (Exp a)
    -- | A cast expression converts, at run time, a value of one numeric type to a similar value of another
    --   numeric type; or confirms, at compile time, that the type of an expression is boolean; or checks,
    --   at run time, that a reference value refers to an object whose class is compatible with a specified
    --   reference type.
    | Cast  (Type a) (Exp a) a
    -- | The application of a binary operator to two operand expressions.
    | BinOp (Exp a) (Op a) (Exp a) a
    -- | Testing whether the result of an expression is an instance of some reference type.
    | InstanceOf (Exp a) (RefType a) a
    -- | The conditional operator @? :@ uses the boolean value of one expression to decide which of two other
    --   expressions should be evaluated.
    | Cond (Exp a) (Exp a) (Exp a) a
    -- | Assignment of the result of an expression to a variable.
    | Assign (Lhs a) (AssignOp a) (Exp a) a
    -- | Lambda expression
    | Lambda (LambdaParams a) (LambdaExpression a) a
    -- | Method reference
    | MethodRef (Name a) (Ident a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | The left-hand side of an assignment expression. This operand may be a named variable, such as a local
--   variable or a field of the current object or class, or it may be a computed variable, as can result from
--   a field access or an array access.
data Lhs a
    = NameLhs (Name a) a          -- ^ Assign to a variable
    | FieldLhs (FieldAccess a) a  -- ^ Assign through a field access
    | ArrayLhs (ArrayIndex a) a   -- ^ Assign to an array
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Array access
data ArrayIndex a = ArrayIndex (Exp a) [Exp a] a    -- ^ Index into an array
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A field access expression may access a field of an object or array, a reference to which is the value
--   of either an expression or the special keyword super.
data FieldAccess a
    = PrimaryFieldAccess (Exp a) (Ident a) a      -- ^ Accessing a field of an object or array computed from an expression.
    | SuperFieldAccess (Ident a) a            -- ^ Accessing a field of the superclass.
    | ClassFieldAccess (Name a) (Ident a) a       -- ^ Accessing a (static) field of a named class.
  deriving (Eq,Show,Read,Typeable,Generic,Data)


-- ¦ A lambda parameter can be a single parameter, or mulitple formal or mulitple inferred parameters
data LambdaParams a
  = LambdaSingleParam (Ident a) a
  | LambdaFormalParams [FormalParam a] a
  | LambdaInferredParams [Ident a] a
    deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Lambda expression, starting from java 8
data LambdaExpression a
    = LambdaExpression (Exp a) a
    | LambdaBlock (Block a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data)


-- | A method invocation expression is used to invoke a class or instance method.
data MethodInvocation a
    -- | Invoking a specific named method.
    = MethodCall (Name a) [Argument a] a
    -- | Invoking a method of a class computed from a primary expression, giving arguments for any generic type parameters.
    | PrimaryMethodCall (Exp a) [RefType a] (Ident a) [Argument a] a
    -- | Invoking a method of the super class, giving arguments for any generic type parameters.
    | SuperMethodCall [RefType a] (Ident a) [Argument a] a
    -- | Invoking a method of the superclass of a named class, giving arguments for any generic type parameters.
    | ClassMethodCall (Name a) [RefType a] (Ident a) [Argument a] a
    -- | Invoking a method of a named type, giving arguments for any generic type parameters.
    | TypeMethodCall  (Name a) [RefType a] (Ident a) [Argument a] a
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An array initializer may be specified in a declaration, or as part of an array creation expression, creating an
--   array and providing some initial values
data ArrayInit a
    = ArrayInit [VarInit a] a
  deriving (Eq,Show,Read,Typeable,Generic,Data)
