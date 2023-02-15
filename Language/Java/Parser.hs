{-# LANGUAGE CPP #-}
module Language.Java.Parser (
    parser, SourceInfo(..),

    compilationUnit, packageDecl, importDecl, typeDecl,

    classDecl, interfaceDecl,

    memberDecl, fieldDecl, methodDecl, constrDecl,
    interfaceMemberDecl, absMethodDecl,

    formalParams, formalParam,

    modifier,

    varDecls, varDecl,

    block, blockStmt, stmt,

    stmtExp, exp, primary, literal,

    ttype, primType, refType, classType, resultType,

    lambdaExp, methodRef,

    typeParams, typeParam,

    name, ident,


    empty, list, list1, seplist, seplist1, opt, bopt, lopt,

    comma, semiColon, period, colon

    ) where

import Language.Java.Lexer ( L(..), Token(..), lexer)
import Language.Java.Syntax
import Language.Java.Pretty (pretty)

import Text.Parsec hiding ( Empty )
import Text.Parsec.Pos (newPos)

import Prelude hiding ( exp, catch, (>>), (>>=) )
import qualified Prelude as P ( (>>), (>>=) )
import Data.Maybe ( isJust, catMaybes )
import Control.Monad ( ap )

#if __GLASGOW_HASKELL__ < 707
import Control.Applicative ( (<$>), (<$), (<*) )
-- Since I cba to find the instance Monad m => Applicative m declaration.
(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap
infixl 4 <*>
#else
import Control.Applicative ( (<$>), (<$), (<*), (<*>) )
#endif

type P = Parsec [L Token] ()

-- SourceInfo Patch Stuff
data SourceInfo = SourceSpan SourcePos SourcePos
                | SourceSpot SourcePos
                | InaccSpot SourcePos
  deriving Show

sourcePos = statePos <$> getParserState
sourceSpot = SourceSpot <$> sourcePos

withSourceSpan :: P (SourceInfo -> a) -> P a
withSourceSpan p = do
  pos1 <- getPosition
  p' <- p
  pos2 <- getPosition
  return $ p' (SourceSpan pos1 pos2)


-- A trick to allow >> and >>=, normally infixr 1, to be
-- used inside branches of <|>, which is declared as infixl 1.
-- There are no clashes with other operators of precedence 2.
(>>) = (P.>>)
(>>=) = (P.>>=)
infixr 2 >>, >>=
-- Note also when reading that <$> is infixl 4 and thus has
-- lower precedence than all the others (>>, >>=, and <|>).

----------------------------------------------------------------------------
-- Top-level parsing

parseCompilationUnit :: String -> Either ParseError (CompilationUnit SourceInfo)
parseCompilationUnit inp =
    runParser compilationUnit () "" (lexer inp)

parser p = runParser p () "" . lexer

--class Parse a where
--  parse :: String -> a

----------------------------------------------------------------------------
-- Packages and compilation units

compilationUnit :: P (CompilationUnit SourceInfo)
compilationUnit = withSourceSpan $ do
    mpd <- opt packageDecl
    ids <- list importDecl
    tds <- list typeDecl
    eof
    return $ CompilationUnit mpd ids (catMaybes tds)

packageDecl :: P (PackageDecl SourceInfo)
packageDecl = withSourceSpan $ do
    tok KW_Package
    n <- name
    semiColon
    return $ PackageDecl n

importDecl :: P (ImportDecl SourceInfo)
importDecl = withSourceSpan $ do
    tok KW_Import
    st <- bopt $ tok KW_Static
    n  <- name
    ds <- bopt $ period >> tok Op_Star
    semiColon
    return $ ImportDecl st n ds

typeDecl :: P (Maybe (TypeDecl SourceInfo))
typeDecl = Just <$> classOrInterfaceDecl <|>
            const Nothing <$> semiColon

----------------------------------------------------------------------------
-- Declarations

-- Class declarations

classOrInterfaceDecl :: P (TypeDecl SourceInfo)
classOrInterfaceDecl = do
    pos1 <- getPosition
    ms <- list modifier
    de <- (do cd <- classDecl
              return $ \ms -> ClassTypeDecl (cd ms)) <|>
          (do id <- annInterfaceDecl <|> interfaceDecl
              return $ \ms -> InterfaceTypeDecl (id ms))
    pos2 <- getPosition
    let srcInfo = SourceSpan pos1 pos2
    return $ de ms srcInfo

classDecl :: P (Mod (ClassDecl SourceInfo))
classDecl = normalClassDecl <|> enumClassDecl

normalClassDecl :: P (Mod (ClassDecl SourceInfo))
normalClassDecl = do
    pos1 <- getPosition
    tok KW_Class
    i   <- ident
    tps <- lopt typeParams
    mex <- opt extends
    imp <- lopt implements
    bod <- classBody
    pos2 <- getPosition
    let srcInfo = SourceSpan pos1 pos2
    return $ \ms -> ClassDecl ms i tps ((fmap head) mex) imp bod srcInfo

extends :: P [RefType SourceInfo]
extends = tok KW_Extends >> refTypeList

implements :: P [RefType SourceInfo]
implements = tok KW_Implements >> refTypeList

enumClassDecl :: P (Mod (ClassDecl SourceInfo))
enumClassDecl = do
    pos1 <- getPosition
    tok KW_Enum
    i   <- ident
    imp <- lopt implements
    bod <- enumBody
    pos2 <- getPosition
    let srcInfo = SourceSpan pos1 pos2
    return $ \ms -> EnumDecl ms i imp bod srcInfo

classBody :: P (ClassBody SourceInfo)
classBody = withSourceSpan $ ClassBody <$> braces classBodyStatements

enumBody :: P (EnumBody SourceInfo)
enumBody = withSourceSpan . braces $ do
    ecs <- seplist enumConst comma
    optional comma
    eds <- lopt enumBodyDecls
    return $ EnumBody ecs eds

enumConst :: P (EnumConstant SourceInfo)
enumConst = withSourceSpan $ do
    id  <- ident
    as  <- lopt args
    mcb <- opt classBody
    return $ EnumConstant id as mcb

enumBodyDecls :: P [Decl SourceInfo]
enumBodyDecls = semiColon >> classBodyStatements

classBodyStatements :: P [Decl SourceInfo]
classBodyStatements = catMaybes <$> list classBodyStatement

-- Interface declarations

annInterfaceDecl :: P (Mod (InterfaceDecl SourceInfo))
annInterfaceDecl = do
    pos1 <- getPosition
    tok KW_AnnInterface
    id  <- ident
    tps <- lopt typeParams
    exs <- lopt extends
    bod <- interfaceBody
    pos2 <- getPosition
    let srcInfo = SourceSpan pos1 pos2
    return $ \ms -> InterfaceDecl InterfaceAnnotation ms id tps exs bod srcInfo

interfaceDecl :: P (Mod (InterfaceDecl SourceInfo))
interfaceDecl = do
    pos1 <- getPosition
    tok KW_Interface
    id  <- ident
    tps <- lopt typeParams
    exs <- lopt extends
    bod <- interfaceBody
    pos2 <- getPosition
    let srcInfo = SourceSpan pos1 pos2
    return $ \ms -> InterfaceDecl InterfaceNormal ms id tps exs bod srcInfo

interfaceBody :: P (InterfaceBody SourceInfo)
interfaceBody = do
    pos1 <- getPosition
    body <- list interfaceBodyDecl
    pos2 <- getPosition
    let srcInfo = SourceSpan pos1 pos2
    return $ (`InterfaceBody` srcInfo) . catMaybes $ body

-- Declarations

classBodyStatement :: P (Maybe (Decl SourceInfo))
classBodyStatement =
    (try $ do
       list1 semiColon
       return Nothing) <|>
    (try $ do
       pos1 <- getPosition
       mst <- bopt (tok KW_Static)
       blk <- block
       pos2 <- getPosition
       let srcInfo = SourceSpan pos1 pos2
       return $ Just $ InitDecl mst blk srcInfo) <|>
    (do
        pos1 <- getPosition
        ms  <- list modifier
        dec <- memberDecl
        pos2 <- getPosition
        let srcInfo = SourceSpan pos1 pos2
        return $ Just $ MemberDecl (dec ms) srcInfo)

memberDecl :: P (Mod (MemberDecl SourceInfo))
memberDecl =
    (try $ do
        pos1 <- getPosition
        cd  <- classDecl
        pos2 <- getPosition
        let srcInfo = SourceSpan pos1 pos2
        return $ \ms -> MemberClassDecl (cd ms) srcInfo) <|>
    (try $ do
        pos1 <- getPosition
        id  <- try annInterfaceDecl <|> try interfaceDecl
        pos2 <- getPosition
        let srcInfo = SourceSpan pos1 pos2
        return $ \ms -> MemberInterfaceDecl (id ms) srcInfo) <|>

    try fieldDecl <|>
    try methodDecl <|>
    constrDecl

fieldDecl :: P (Mod (MemberDecl SourceInfo))
fieldDecl = endSemi $ do
    pos1 <- getPosition
    typ <- ttype
    vds <- varDecls
    pos2 <- getPosition
    let srcInfo = SourceSpan pos1 pos2
    return $ \ms -> FieldDecl ms typ vds srcInfo

methodDecl :: P (Mod (MemberDecl SourceInfo))
methodDecl = do
    pos1 <- getPosition
    tps <- lopt typeParams
    rt  <- resultType
    id  <- ident
    fps <- formalParams
    thr <- lopt throws
    bod <- methodBody
    pos2 <- getPosition
    let srcInfo = SourceSpan pos1 pos2
    return $ \ms -> MethodDecl ms tps rt id fps thr Nothing bod srcInfo

methodBody :: P (MethodBody SourceInfo)
methodBody = withSourceSpan (MethodBody <$>
    (const Nothing <$> semiColon <|> Just <$> block))


constrDecl :: P (Mod (MemberDecl SourceInfo))
constrDecl = do
    pos1 <- getPosition
    tps <- lopt typeParams
    id  <- ident
    fps <- formalParams
    thr <- lopt throws
    bod <- constrBody
    pos2 <- getPosition
    let srcInfo = SourceSpan pos1 pos2
    return $ \ms -> ConstructorDecl ms tps id fps thr bod srcInfo

constrBody :: P (ConstructorBody SourceInfo)
constrBody = withSourceSpan $ braces $ do
    mec <- opt (try explConstrInv)
    bss <- list blockStmt
    return $ ConstructorBody mec bss

explConstrInv :: P (ExplConstrInv SourceInfo)
explConstrInv = endSemi $
    withSourceSpan
    (try $ do
        tas <- lopt refTypeArgs
        tok KW_This
        as  <- args
        return $ ThisInvoke tas as) <|>
    withSourceSpan
    (try $ do
        tas <- lopt refTypeArgs
        tok KW_Super
        as  <- args
        return $ SuperInvoke tas as) <|>
    withSourceSpan
    (do pri <- primary
        period
        tas <- lopt refTypeArgs
        tok KW_Super
        as  <- args
        return $ PrimarySuperInvoke pri tas as)

-- TODO: This should be parsed like class bodies, and post-checked.
--       That would give far better error messages.
interfaceBodyDecl :: P (Maybe (MemberDecl SourceInfo))
interfaceBodyDecl = semiColon >> return Nothing <|>
    do ms  <- list modifier
       imd <- interfaceMemberDecl
       return $ Just (imd ms)

interfaceMemberDecl :: P (Mod (MemberDecl SourceInfo))
interfaceMemberDecl =
    (do
        pos1 <- getPosition
        cd  <- classDecl
        pos2 <- getPosition
        let srcInfo = SourceSpan pos1 pos2
        return $ \ms -> MemberClassDecl (cd ms) srcInfo) <|>
    (do
        pos1 <- getPosition
        id  <- try annInterfaceDecl <|> try interfaceDecl
        pos2 <- getPosition
        let srcInfo = SourceSpan pos1 pos2
        return $ \ms -> MemberInterfaceDecl (id ms) srcInfo) <|>
    try fieldDecl <|>
    absMethodDecl

absMethodDecl :: P (Mod (MemberDecl SourceInfo))
absMethodDecl = do
    pos1 <- getPosition
    tps <- lopt typeParams
    rt  <- resultType
    id  <- ident
    fps <- formalParams
    thr <- lopt throws
    def <- opt defaultValue
    semiColon
    pos2 <- getPosition
    let noMethodSpot = InaccSpot pos2
    let srcInfo = SourceSpan pos1 pos2
    return $ \ms -> MethodDecl ms tps rt id fps thr def (MethodBody Nothing noMethodSpot) srcInfo

defaultValue :: P (Exp SourceInfo)
defaultValue = tok KW_Default >> exp

throws :: P [RefType SourceInfo]
throws = tok KW_Throws >> refTypeList

-- Formal parameters

formalParams :: P [FormalParam SourceInfo]
formalParams = parens $ do
    fps <- seplist formalParam comma
    if validateFPs fps
     then return fps
     else fail "Only the last formal parameter may be of variable arity"
  where validateFPs :: [FormalParam SourceInfo] -> Bool
        validateFPs [] = True
        validateFPs [_] = True
        validateFPs (FormalParam _ _ b _ _ :xs) = not b

formalParam :: P (FormalParam SourceInfo)
formalParam = withSourceSpan $ do
    ms  <- list modifier
    typ <- ttype
    var <- bopt ellipsis
    vid <- varDeclId
    return $ FormalParam ms typ var vid

ellipsis :: P ()
ellipsis = period >> period >> period

-- Modifiers

modifier :: P (Modifier SourceInfo)
modifier = withSourceSpan $
        tok KW_Public      >> return Public
    <|> tok KW_Protected   >> return Protected
    <|> tok KW_Private     >> return Private
    <|> tok KW_Abstract    >> return Abstract
    <|> tok KW_Static      >> return Static
    <|> tok KW_Strictfp    >> return StrictFP
    <|> tok KW_Final       >> return Final
    <|> tok KW_Native      >> return Native
    <|> tok KW_Transient   >> return Transient
    <|> tok KW_Volatile    >> return Volatile
    <|> tok KW_Synchronized >> return Synchronized_
    <|> Annotation <$> annotation

annotation :: P (Annotation SourceInfo)
annotation = withSourceSpan $ flip ($) <$ tok Op_AtSign <*> name <*> (
               try (flip NormalAnnotation <$> parens evlist)
           <|> try (flip SingleElementAnnotation <$> parens elementValue)
           <|> try (MarkerAnnotation <$ return ())
        )

evlist :: P [(Ident SourceInfo, ElementValue SourceInfo)]
evlist = seplist1 elementValuePair comma

elementValuePair :: P (Ident SourceInfo, ElementValue SourceInfo)
elementValuePair = (,) <$> ident <* tok Op_Equal <*> elementValue

elementValue :: P (ElementValue SourceInfo)
elementValue =
    withSourceSpan (
    EVVal <$> (    withSourceSpan (InitArray <$> arrayInit)
               <|> withSourceSpan (InitExp   <$> condExp )))
    <|> withSourceSpan (EVAnn <$> annotation)


----------------------------------------------------------------------------
-- Variable declarations

varDecls :: P [VarDecl SourceInfo]
varDecls = seplist1 varDecl comma

varDecl :: P (VarDecl SourceInfo)
varDecl = withSourceSpan $ do
    vid <- varDeclId
    mvi <- opt $ tok Op_Equal >> varInit
    return $ VarDecl vid mvi

varDeclId :: P (VarDeclId SourceInfo)
varDeclId = do
    pos1 <- getPosition
    id  <- ident
    pos2 <- getPosition
    let idSrcInfo = SourceSpan pos1 pos2
    let spotHack = InaccSpot pos2

    abs <- list arrBrackets
    return $ foldl (\f _ -> (`VarDeclArray` spotHack) . f) (`VarId` idSrcInfo) abs id

arrBrackets :: P ()
arrBrackets = brackets $ return ()

localVarDecl :: P ([Modifier SourceInfo], Type SourceInfo, [VarDecl SourceInfo])
localVarDecl = do
    ms  <- list modifier
    typ <- ttype
    vds <- varDecls
    return (ms, typ, vds)

varInit :: P (VarInit SourceInfo)
varInit = withSourceSpan $
    InitArray <$> arrayInit <|>
    InitExp   <$> exp

arrayInit :: P (ArrayInit SourceInfo)
arrayInit = withSourceSpan $ braces $ do
    vis <- seplist varInit comma
    opt comma
    return $ ArrayInit vis

----------------------------------------------------------------------------
-- Statements

block :: P (Block SourceInfo)
block = withSourceSpan $ braces $ Block <$> list blockStmt


blockStmt :: P (BlockStmt SourceInfo)
blockStmt =
    (try $ do
        pos1 <- getPosition
        ms  <- list modifier
        cd  <- classDecl
        pos2 <- getPosition
        let srcInfo = SourceSpan pos1 pos2
        return $ LocalClass (cd ms) srcInfo) <|>
    withSourceSpan (try $ do
        (m,t,vds) <- endSemi $ localVarDecl
        return $ LocalVars m t vds) <|>
    withSourceSpan (BlockStmt <$> stmt)

stmt :: P (Stmt SourceInfo)
stmt = withSourceSpan ifStmt <|>
       withSourceSpan whileStmt <|>
       withSourceSpan forStmt <|>
       withSourceSpan labeledStmt <|>
       stmtNoTrail
  where
    ifStmt = do
        tok KW_If
        e   <- parens exp
        (try $
            do th <- stmtNSI
               tok KW_Else
               el <- stmt
               return $ IfThenElse e th el) <|>
           (do th <- stmt
               return $ IfThen e th)
    whileStmt = do
        tok KW_While
        e   <- parens exp
        s   <- stmt
        return $ While e s
    forStmt = do
        tok KW_For
        f <- parens $
            (try $ do
                fi <- opt forInit
                semiColon
                e  <- opt exp
                semiColon
                fu <- opt forUp
                return $ BasicFor fi e fu) <|>
            (do ms <- list modifier
                t  <- ttype
                i  <- ident
                colon
                e  <- exp
                return $ EnhancedFor ms t i e)
        s <- stmt
        return $ f s
    labeledStmt = try $ do
        lbl <- ident
        colon
        s   <- stmt
        return $ Labeled lbl s

stmtNSI :: P (Stmt SourceInfo)
stmtNSI =
  withSourceSpan ifStmt <|>
  withSourceSpan whileStmt <|>
  withSourceSpan forStmt <|>
  withSourceSpan labeledStmt <|>
  stmtNoTrail
  where
    ifStmt = do
        tok KW_If
        e  <- parens exp
        th <- stmtNSI
        tok KW_Else
        el <- stmtNSI
        return $ IfThenElse e th el
    whileStmt = do
        tok KW_While
        e <- parens exp
        s <- stmtNSI
        return $ While e s
    forStmt = do
        tok KW_For
        f <- parens $ (try $ do
            fi <- opt forInit
            semiColon
            e  <- opt exp
            semiColon
            fu <- opt forUp
            return $ BasicFor fi e fu)
            <|> (do
            ms <- list modifier
            t  <- ttype
            i  <- ident
            colon
            e  <- exp
            return $ EnhancedFor ms t i e)
        s <- stmtNSI
        return $ f s
    labeledStmt = try $ do
        i <- ident
        colon
        s <- stmtNSI
        return $ Labeled i s

stmtNoTrail :: P (Stmt SourceInfo)
stmtNoTrail = withSourceSpan (
    -- empty statement
    const Empty <$> semiColon <|>
    -- inner block
    StmtBlock <$> block <|>
    -- assertions
    (endSemi $ do
        tok KW_Assert
        e   <- exp
        me2 <- opt $ colon >> exp
        return $ Assert e me2) <|>
    -- switch stmts
    (do tok KW_Switch
        e  <- parens exp
        sb <- switchBlock
        return $ Switch e sb) <|>
    -- do-while loops
    (endSemi $ do
        tok KW_Do
        s <- stmt
        tok KW_While
        e <- parens exp
        return $ Do s e) <|>
    -- break
    (endSemi $ do
        tok KW_Break
        mi <- opt ident
        return $ Break mi) <|>
    -- continue
    (endSemi $ do
        tok KW_Continue
        mi <- opt ident
        return $ Continue mi) <|>
    -- return
    (endSemi $ do
        tok KW_Return
        me <- opt exp
        return $ Return me) <|>
    -- synchronized
    (do tok KW_Synchronized
        e <- parens exp
        b <- block
        return $ Synchronized e b) <|>
    -- throw
    (endSemi $ do
        tok KW_Throw
        e <- exp
        return $ Throw e) <|>
    -- try-catch, both with and without a finally clause
    (do tok KW_Try
        b <- block
        c <- list catch
        mf <- opt $ tok KW_Finally >> block
        -- TODO: here we should check that there exists at
        -- least one catch or finally clause
        return $ Try b c mf) <|>
    -- expressions as stmts
    ExpStmt <$> endSemi stmtExp )

-- For loops

forInit :: P (ForInit SourceInfo)
forInit = withSourceSpan (do
    try (do (m,t,vds) <- localVarDecl
            return $ ForLocalVars m t vds)) <|>
    withSourceSpan (seplist1 stmtExp comma >>= return . ForInitExps)

forUp :: P [Exp SourceInfo]
forUp = seplist1 stmtExp comma

-- Switches

switchBlock :: P [SwitchBlock SourceInfo]
switchBlock = braces $ list switchStmt

switchStmt :: P (SwitchBlock SourceInfo)
switchStmt = withSourceSpan $ do
    lbl <- switchLabel
    bss <- list blockStmt
    return $ SwitchBlock lbl bss

switchLabel :: P (SwitchLabel SourceInfo)
switchLabel = withSourceSpan (tok KW_Default >> colon >> return Default) <|>
    withSourceSpan
    (do tok KW_Case
        e <- exp
        colon
        return $ SwitchCase e)

-- Try-catch clauses

catch :: P (Catch SourceInfo)
catch = withSourceSpan $ do
    tok KW_Catch
    fp <- parens formalParam
    b  <- block
    return $ Catch fp b

----------------------------------------------------------------------------
-- Expressions

stmtExp :: P (Exp SourceInfo)
stmtExp = try preIncDec
    <|> try postIncDec
    <|> try assignment
    -- There are sharing gains to be made by unifying these two
    <|> try methodInvocationExp
    <|> try lambdaExp
    <|> try methodRef
    <|> instanceCreation

preIncDec :: P (Exp SourceInfo)
preIncDec = do
    op <- preIncDecOp
    e <- unaryExp
    return $ op e

postIncDec :: P (Exp SourceInfo)
postIncDec = do
    e <- postfixExpNES
    ops <- list1 postfixOp
    return $ foldl (\a s -> s a) e ops

assignment :: P (Exp SourceInfo)
assignment = withSourceSpan $ do
    lh <- lhs
    op <- assignOp
    e  <- assignExp
    return $ Assign lh op e

lhs :: P (Lhs SourceInfo)
lhs = withSourceSpan $
    try (FieldLhs <$> fieldAccess)
    <|> try (ArrayLhs <$> arrayAccess)
    <|> NameLhs <$> name



exp :: P (Exp SourceInfo)
exp = assignExp

assignExp :: P (Exp SourceInfo)
assignExp = try methodRef <|> try lambdaExp <|> try assignment <|> condExp

condExp :: P (Exp SourceInfo)
condExp = do
    ie <- infixExp
    ces <- list condExpSuffix
    return $ foldl (\a s -> s a) ie ces

condExpSuffix :: P (Exp SourceInfo -> Exp SourceInfo)
condExpSuffix = do
    pos1 <- getPosition
    tok Op_Query
    th <- exp
    colon
    el <- condExp
    pos2 <- getPosition
    let srcInfo = SourceSpan pos1 pos2
    return $ \ce -> Cond ce th el srcInfo

infixExp :: P (Exp SourceInfo)
infixExp = do
    ue <- unaryExp
    ies <- list infixExpSuffix
    return $ foldl (\a s -> s a) ue ies

infixExpSuffix :: P (Exp SourceInfo -> Exp SourceInfo)
infixExpSuffix =
    (do
      pos1 <- getPosition
      op <- infixCombineOp
      ie2 <- infixExp
      pos2 <- getPosition
      let srcInfo = SourceSpan pos1 pos2
      return $ \ie1 -> BinOp ie1 op ie2 srcInfo) <|>
    (do
        pos1 <- getPosition
        op <- infixOp
        e2 <- unaryExp
        pos2 <- getPosition
        let srcInfo = SourceSpan pos1 pos2
        return $ \e1 -> BinOp e1 op e2 srcInfo) <|>
    (do
        pos1 <- getPosition
        tok KW_Instanceof
        t  <- refType
        pos2 <- getPosition
        let srcInfo = SourceSpan pos1 pos2
        return $ \e1 -> InstanceOf e1 t srcInfo)

unaryExp :: P (Exp SourceInfo)
unaryExp = try preIncDec <|>
    try (do
        op <- prefixOp
        ue <- unaryExp
        return $ op ue) <|>
    try (withSourceSpan $ do
        t <- parens ttype
        e <- unaryExp
        return $ Cast t e) <|>
    postfixExp

postfixExpNES :: P (Exp SourceInfo)
postfixExpNES = -- try postIncDec <|>
    try primary <|>
    withSourceSpan (ExpName <$> name)

postfixExp :: P (Exp SourceInfo)
postfixExp = do
    pe <- postfixExpNES
    ops <- list postfixOp
    return $ foldl (\a s -> s a) pe ops


primary :: P (Exp SourceInfo)
primary = primaryNPS |>> primarySuffix

primaryNPS :: P (Exp SourceInfo)
primaryNPS = try arrayCreation <|> primaryNoNewArrayNPS

--TODO TYPE SIG
primaryNoNewArray = startSuff primaryNoNewArrayNPS primarySuffix

primaryNoNewArrayNPS :: P (Exp SourceInfo)
primaryNoNewArrayNPS =
    withSourceSpan (Lit <$> literal) <|>
    withSourceSpan (const This <$> tok KW_This) <|>
    parens exp <|>
    -- TODO: These two following should probably be merged more
    (try $ withSourceSpan $ do
        rt <- resultType
        period >> tok KW_Class
        return $ ClassLit rt) <|>
    (try $ withSourceSpan $ do
        n <- name
        period >> tok KW_This
        return $ ThisClass n) <|>
    try instanceCreationNPS <|>
    try (withSourceSpan (MethodInv <$> methodInvocationNPS)) <|>
    try (withSourceSpan (FieldAccess <$> fieldAccessNPS)) <|>
    withSourceSpan (ArrayAccess <$> arrayAccessNPS)

--TODO make this a span not spot
primarySuffix :: P (Exp SourceInfo -> Exp SourceInfo)
primarySuffix = try instanceCreationSuffix <|>
    try (sourceSpot >>= (\info -> (flip ArrayAccess info .) <$> arrayAccessSuffix)) <|>
    try (sourceSpot >>= (\info -> (flip MethodInv info .) <$> methodInvocationSuffix)) <|>
      sourceSpot >>= (\info -> (flip FieldAccess info .) <$> fieldAccessSuffix)


instanceCreationNPS :: P (Exp SourceInfo)
instanceCreationNPS =
    withSourceSpan $
    do tok KW_New
       tas <- lopt typeArgs
       tds <- typeDeclSpecifier
       as  <- args
       mcb <- opt classBody
       return $ InstanceCreation tas tds as mcb

typeDeclSpecifier :: P (TypeDeclSpecifier SourceInfo)
typeDeclSpecifier =
    withSourceSpan
    (try $ do ct <- classType
              period
              i <- ident
              pos1 <- getPosition
              tok Op_LThan
              tok Op_GThan
              pos2 <- getPosition
              let diamondInfo = SourceSpan pos1 pos2
              return $ TypeDeclSpecifierWithDiamond ct i (Diamond diamondInfo)
    ) <|>
    withSourceSpan
    (try $ do i <- ident
              pos1 <- getPosition
              tok Op_LThan
              tok Op_GThan
              pos2 <- getPosition
              let diamondInfo = SourceSpan pos1 pos2
              return $ TypeDeclSpecifierUnqualifiedWithDiamond i (Diamond diamondInfo)
    ) <|>
    withSourceSpan
    (do ct <- classType
        return $ TypeDeclSpecifier ct
    )

instanceCreationSuffix :: P (Exp SourceInfo -> Exp SourceInfo)
instanceCreationSuffix =
     do
        pos1 <- getPosition
        period >> tok KW_New
        tas <- lopt typeArgs
        i   <- ident
        as  <- args
        mcb <- opt classBody
        pos2 <- getPosition
        let srcInfo = SourceSpan pos1 pos2
        return $ \p -> QualInstanceCreation p tas i as mcb srcInfo

instanceCreation :: P (Exp SourceInfo)
instanceCreation = try instanceCreationNPS <|> do
    p <- primaryNPS
    ss <- list primarySuffix
    let icp = foldl (\a s -> s a) p ss
    case icp of
     QualInstanceCreation {} -> return icp
     _ -> fail ""


lambdaParams :: P (LambdaParams SourceInfo)
lambdaParams = try (withSourceSpan $ LambdaSingleParam <$> ident)
               <|> withSourceSpan (try (parens $ LambdaFormalParams <$> (seplist formalParam comma)))
               <|> withSourceSpan (parens $ LambdaInferredParams <$> (seplist ident comma))

lambdaExp :: P (Exp SourceInfo)
lambdaExp = withSourceSpan $
            Lambda
            <$> (lambdaParams <* (tok LambdaArrow))
            <*> (withSourceSpan (LambdaBlock <$> (try block))
                 <|> withSourceSpan (LambdaExpression <$> exp))

methodRef :: P (Exp SourceInfo)
methodRef = withSourceSpan $
            MethodRef
            <$> (name <*  (tok MethodRefSep))
            <*> ident

--TODO REMOVE?
{-
instanceCreation =
    (do tok KW_New
        tas <- lopt typeArgs
        ct  <- classType
        as  <- args
        mcb <- opt classBody
        return $ InstanceCreation tas ct as mcb) <|>
    (do p   <- primary
        period >> tok KW_New
        tas <- lopt typeArgs
        i   <- ident
        as  <- args
        mcb <- opt classBody
        return $ QualInstanceCreation p tas i as mcb)
-}

fieldAccessNPS :: P (FieldAccess SourceInfo)
fieldAccessNPS =
    withSourceSpan
    (do tok KW_Super >> period
        i <- ident
        return $ SuperFieldAccess i) <|>
    withSourceSpan
    (do n <- name
        period >> tok KW_Super >> period
        i <- ident
        return $ ClassFieldAccess n i)

fieldAccessSuffix :: P (Exp SourceInfo -> FieldAccess SourceInfo)
fieldAccessSuffix = do
    pos1 <- getPosition
    period
    i <- ident
    pos2 <- getPosition
    let srcInfo = SourceSpan pos1 pos2
    return $ \p -> PrimaryFieldAccess p i srcInfo

fieldAccess :: P (FieldAccess SourceInfo)
fieldAccess = try fieldAccessNPS <|> do
    p <- primaryNPS
    ss <- list primarySuffix
    let fap = foldl (\a s -> s a) p ss
    case fap of
     FieldAccess fa _ -> return fa
     _ -> fail ""

methodInvocationNPS :: P (MethodInvocation SourceInfo)
methodInvocationNPS =
    withSourceSpan
    (do tok KW_Super >> period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ SuperMethodCall rts i as) <|>
    withSourceSpan
    (do n <- name
        f <- (do as <- args
                 return $ \n -> MethodCall n as) <|>
             (period >> do
                msp <- opt (tok KW_Super >> period)
                rts <- lopt refTypeArgs
                i   <- ident
                as  <- args
                let mc = maybe TypeMethodCall (const ClassMethodCall) msp
                return $ \n -> mc n rts i as)
        return $ f n)

methodInvocationSuffix :: P (Exp SourceInfo -> MethodInvocation SourceInfo)
methodInvocationSuffix = do
        pos1 <- getPosition
        period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        pos2 <- getPosition
        let srcInfo = SourceSpan pos1 pos2
        return $ \p -> PrimaryMethodCall p [] i as srcInfo

methodInvocationExp :: P (Exp SourceInfo)
methodInvocationExp = try (do
    p <- primaryNPS
    ss <- list primarySuffix
    let mip = foldl (\a s -> s a) p ss
    case mip of
     MethodInv _ e -> return mip
     _ -> fail "") <|>
     withSourceSpan (MethodInv <$> methodInvocationNPS)

--TODO REMOVE?
{-
methodInvocation :: P MethodInvocation
methodInvocation =
    (do tok KW_Super >> period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ SuperMethodCall rts i as) <|>
    (do p <- primary
        period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ PrimaryMethodCall p rts i as) <|>
    (do n <- name
        f <- (do as <- args
                 return $ \n -> MethodCall n as) <|>
             (period >> do
                msp <- opt (tok KW_Super >> period)
                rts <- lopt refTypeArgs
                i   <- ident
                as  <- args
                let mc = maybe TypeMethodCall (const ClassMethodCall) msp
                return $ \n -> mc n rts i as)
        return $ f n)
-}

args :: P [Argument SourceInfo]
args = parens $ seplist exp comma

-- Arrays

arrayAccessNPS :: P (ArrayIndex SourceInfo)
arrayAccessNPS = withSourceSpan $ do
    pos1 <- getPosition
    n <- name
    pos2 <- getPosition
    e <- list1 $ brackets exp
    return $ ArrayIndex (ExpName n (SourceSpan pos1 pos2)) e

arrayAccessSuffix :: P (Exp SourceInfo -> ArrayIndex SourceInfo)
arrayAccessSuffix = do
    pos1 <- getPosition
    e <- list1 $ brackets exp
    pos2 <- getPosition
    return $ \ref -> ArrayIndex ref e (SourceSpan pos1 pos2)

--TODO TYPE SIG
arrayAccess = try arrayAccessNPS <|> do
    p <- primaryNoNewArrayNPS
    ss <- list primarySuffix
    let aap = foldl (\a s -> s a) p ss
    case aap of
     ArrayAccess ain info -> return ain
     _ -> fail ""

{-
 - TODO REMOVE?
arrayAccess :: P (Exp, Exp)
arrayAccess = do
    ref <- arrayRef
    e   <- brackets exp
    return (ref, e)

arrayRef :: P Exp
arrayRef = ExpName <$> name <|> primaryNoNewArray
-}

arrayCreation :: P (Exp SourceInfo)
arrayCreation = withSourceSpan $ do
    tok KW_New
    t <- nonArrayType
    f <- (try $ do
             ds <- list1 $ brackets empty
             ai <- arrayInit
             return $ \t -> ArrayCreateInit t (length ds) ai) <|>
         (do des <- list1 $ try $ brackets exp
             ds  <- list  $ brackets empty
             return $ \t -> ArrayCreate t des (length ds))
    return $ f t

literal :: P (Literal SourceInfo)
literal = do
    srcSpot <- sourceSpot
    javaToken $ \t -> case t of
        IntTok     i -> Just (Int i srcSpot)
        LongTok    l -> Just (Word l srcSpot)
        DoubleTok  d -> Just (Double d srcSpot)
        FloatTok   f -> Just (Float f srcSpot)
        CharTok    c -> Just (Char c srcSpot)
        StringTok  s -> Just (String s srcSpot)
        BoolTok    b -> Just (Boolean b srcSpot)
        NullTok      -> Just $ Null srcSpot
        _ -> Nothing

-- Operators

preIncDecOp, prefixOp, postfixOp :: P (Exp SourceInfo -> Exp SourceInfo)
preIncDecOp =
  sourceSpot >>= (\srcSpot ->
    (tok Op_PPlus >> return (PreIncrement srcSpot))<|>
    (tok Op_MMinus >> return (PreDecrement srcSpot)))

prefixOp =
  sourceSpot >>= (\srcSpot ->
    (tok Op_Bang  >> return (PreNot srcSpot)) <|>
    (tok Op_Tilde >> return (PreBitCompl srcSpot)) <|>
    (tok Op_Plus  >> return (PrePlus srcSpot)) <|>
    (tok Op_Minus >> return (PreMinus srcSpot)))
postfixOp =
  sourceSpot >>= (\srcSpot ->
    (tok Op_PPlus  >> return (PostIncrement srcSpot)) <|>
    (tok Op_MMinus >> return (PostDecrement srcSpot)))

assignOp :: P (AssignOp SourceInfo)
assignOp =
  sourceSpot >>= (\srcSpot ->
    (tok Op_Equal    >> return (EqualA srcSpot)) <|>
    (tok Op_StarE    >> return (MultA srcSpot)) <|>
    (tok Op_SlashE   >> return (DivA srcSpot)) <|>
    (tok Op_PercentE >> return (RemA srcSpot)) <|>
    (tok Op_PlusE    >> return (AddA srcSpot)) <|>
    (tok Op_MinusE   >> return (SubA srcSpot)) <|>
    (tok Op_LShiftE  >> return (LShiftA srcSpot)) <|>
    (tok Op_RShiftE  >> return (RShiftA srcSpot)) <|>
    (tok Op_RRShiftE >> return (RRShiftA srcSpot)) <|>
    (tok Op_AndE     >> return (AndA srcSpot)) <|>
    (tok Op_CaretE   >> return (XorA srcSpot)) <|>
    (tok Op_OrE      >> return (OrA srcSpot)))

infixCombineOp :: P (Op SourceInfo)
infixCombineOp =
  sourceSpot >>= (\srcSpot ->
    (tok Op_And     >> return (And srcSpot)) <|>
    (tok Op_Caret   >> return (Xor srcSpot)) <|>
    (tok Op_Or      >> return (Or srcSpot)) <|>
    (tok Op_AAnd    >> return (CAnd srcSpot)) <|>
    (tok Op_OOr     >> return (COr srcSpot)))


infixOp :: P (Op SourceInfo)
infixOp = sourceSpot >>= (\srcSpot ->
    (tok Op_Star    >> return (Mult srcSpot))   <|>
    (tok Op_Slash   >> return (Div srcSpot))    <|>
    (tok Op_Percent >> return (Rem srcSpot))    <|>
    (tok Op_Plus    >> return (Add srcSpot))     <|>
    (tok Op_Minus   >> return (Sub srcSpot))    <|>
    (tok Op_LShift  >> return (LShift srcSpot)) <|>
    (tok Op_LThan   >> return (LThan srcSpot))  <|>
    (try $ withSourceSpan (do
       tok Op_GThan
       tok Op_GThan
       tok Op_GThan
       return RRShift   )) <|>

    (try $ withSourceSpan (do
       tok Op_GThan
       tok Op_GThan
       return RShift    )) <|>

    (tok Op_GThan   >> return (GThan srcSpot)) <|>
    (tok Op_LThanE  >> return (LThanE srcSpot)) <|>
    (tok Op_GThanE  >> return (GThanE srcSpot)) <|>
    (tok Op_Equals  >> return (Equal srcSpot)) <|>
    (tok Op_BangE   >> return (NotEq srcSpot)))


----------------------------------------------------------------------------
-- Types

ttype :: P (Type SourceInfo)
ttype = try (withSourceSpan (RefType <$> refType))
    <|> withSourceSpan (PrimType <$> primType)

primType :: P (PrimType SourceInfo)
primType =
    sourceSpot >>= (\srcSpot ->
      tok KW_Boolean >> return (BooleanT srcSpot) <|>
      tok KW_Byte    >> return (ByteT srcSpot)     <|>
      tok KW_Short   >> return (ShortT srcSpot)    <|>
      tok KW_Int     >> return (IntT srcSpot)      <|>
      tok KW_Long    >> return (LongT srcSpot)     <|>
      tok KW_Char    >> return (CharT srcSpot)     <|>
      tok KW_Float   >> return (FloatT srcSpot)    <|>
      tok KW_Double  >> return (DoubleT srcSpot))

refType :: P (RefType SourceInfo)
refType =
    (do
        ptSpot <- InaccSpot <$> getPosition
        pt <- primType
        arrSpot <- InaccSpot <$> getPosition
        (_:bs) <- list1 arrBrackets
        return $ foldl (\f _ -> (`ArrayType` arrSpot) . (`RefType` ptSpot) . f)
                        ((`ArrayType` arrSpot) . (`PrimType` ptSpot)) bs pt) <|>
    (do
        ctSpot <- InaccSpot <$> getPosition
        ct <- classType
        arrSpot <- InaccSpot <$> getPosition
        bs <- list arrBrackets
        return $ foldl (\f _ -> (`ArrayType` arrSpot) . (`RefType` ctSpot). f)
                            (`ClassRefType` ctSpot) bs ct) <?> "refType"

nonArrayType :: P (Type SourceInfo)
nonArrayType = withSourceSpan (PrimType <$> primType)
    <|> withSourceSpan (RefType <$> withSourceSpan (
        do ClassRefType <$> classType))

classType :: P (ClassType SourceInfo)
classType = do
  pos1 <- getPosition
  spl <- seplist1 classTypeSpec period
  pos2 <- getPosition
  return $ ClassType (SourceSpan pos1 pos2) spl

classTypeSpec :: P (Ident SourceInfo, [TypeArgument SourceInfo])
classTypeSpec = do
    i   <- ident
    tas <- lopt typeArgs
    return (i, tas)

resultType :: P (Maybe (Type SourceInfo))
resultType = tok KW_Void >> return Nothing <|> Just <$> ttype <?> "resultType"

refTypeList :: P [RefType SourceInfo]
refTypeList = seplist1 refType comma

----------------------------------------------------------------------------
-- Type parameters and arguments

typeParams :: P [TypeParam SourceInfo]
typeParams = angles $ seplist1 typeParam comma

typeParam :: P (TypeParam SourceInfo)
typeParam = withSourceSpan $ do
    i  <- ident
    bs <- lopt bounds
    return $ TypeParam i bs

bounds :: P [RefType SourceInfo]
bounds = tok KW_Extends >> seplist1 refType (tok Op_And)

typeArgs :: P [TypeArgument SourceInfo]
typeArgs = angles $ seplist1 typeArg comma

typeArg :: P (TypeArgument SourceInfo)
typeArg = tok Op_Query >> sourceSpot >>= (\wSpot -> Wildcard wSpot <$> opt wildcardBound)
    <|> sourceSpot >>= (\aSpot -> ActualType aSpot <$> refType)

wildcardBound :: P (WildcardBound SourceInfo)
wildcardBound = sourceSpot >>= (\spot ->
       (tok KW_Extends >> (ExtendsBound spot) <$> refType
    <|> tok KW_Super >> (SuperBound spot) <$> refType))

refTypeArgs :: P [RefType SourceInfo]
refTypeArgs = angles refTypeList

----------------------------------------------------------------------------
-- Names

name :: P (Name SourceInfo)
name = do
  pos1 <- getPosition
  pname <- seplist1 ident period
  pos2 <- getPosition
  return $ Name pname $ SourceSpan pos1 pos2

ident :: P (Ident SourceInfo)
ident = do
  pos <- SourceSpot <$> sourcePos
  javaToken $ \t -> case t of
    IdentTok s -> Just $ Ident s pos
    _ -> Nothing

------------------------------------------------------------

empty :: P ()
empty = return ()

opt :: P a -> P (Maybe a)
opt = optionMaybe

bopt :: P a -> P Bool
bopt p = opt p >>= \ma -> return $ isJust ma

lopt :: P [a] -> P [a]
lopt p = do mas <- opt p
            case mas of
             Nothing -> return []
             Just as -> return as

list :: P a -> P [a]
list = option [] . list1

list1 :: P a -> P [a]
list1 = many1

seplist :: P a -> P sep -> P [a]
--seplist = sepBy
seplist p sep = option [] $ seplist1 p sep

seplist1 :: P a -> P sep -> P [a]
--seplist1 = sepBy1
seplist1 p sep =
    p >>= \a ->
        try (do sep
                as <- seplist1 p sep
                return (a:as))
        <|> return [a]

startSuff, (|>>) :: P a -> P (a -> a) -> P a
startSuff start suffix = do
    x <- start
    ss <- list suffix
    return $ foldl (\a s -> s a) x ss

(|>>) = startSuff

------------------------------------------------------------

javaToken :: (Token -> Maybe a) -> P a
javaToken test = token showT posT testT
  where showT (L _ t) = show t
        posT  (L p _) = pos2sourcePos p
        testT (L _ t) = test t

tok, matchToken :: Token -> P ()
tok = matchToken
matchToken t = javaToken (\r -> if r == t then Just () else Nothing)

pos2sourcePos :: (Int, Int) -> SourcePos
pos2sourcePos (l,c) = newPos "" l c

type Mod a = [Modifier SourceInfo] -> a

parens, braces, brackets, angles :: P a -> P a
parens   = between (tok OpenParen)  (tok CloseParen)
braces   = between (tok OpenCurly)  (tok CloseCurly)
brackets = between (tok OpenSquare) (tok CloseSquare)
angles   = between (tok Op_LThan)   (tok Op_GThan)

endSemi :: P a -> P a
endSemi p = p >>= \a -> semiColon >> return a

comma, colon, semiColon, period :: P ()
comma     = tok Comma
colon     = tok Op_Colon
semiColon = tok SemiColon
period    = tok Period

------------------------------------------------------------

test = "public class Foo { }"
testFile file = do
  i <- readFile file
  let r = parseCompilationUnit i
  putStrLn$ either (("Parsing error:\n"++) . show) (show . pretty) r
