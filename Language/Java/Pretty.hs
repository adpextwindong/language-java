{-# LANGUAGE CPP #-}
module Language.Java.Pretty where

import Text.PrettyPrint
import Text.Printf (printf)
import Data.Char (toLower)
import Data.List (intersperse)

import Language.Java.Syntax

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

prettyPrint :: Pretty a => a -> String
prettyPrint = show . pretty

parenPrec :: Int -> Int -> Doc -> Doc
parenPrec inheritedPrec currentPrec t
    | inheritedPrec <= 0          = t
    | inheritedPrec < currentPrec = parens t
    | otherwise                   = t

class Pretty a where
  pretty :: a -> Doc
  pretty = prettyPrec 0
  
  prettyPrec :: Int -> a -> Doc
  prettyPrec _ = pretty

-----------------------------------------------------------------------
-- Packages

-- SourceInfo param a note: Leaving this unpretty printed for now
instance Show a => Pretty (CompilationUnit a) where
  prettyPrec p (CompilationUnit mpd ids tds _) =
    vcat $ ((maybePP p mpd): map (prettyPrec p) ids) ++ map (prettyPrec p) tds

instance Show a => Pretty (PackageDecl a) where
  prettyPrec p (PackageDecl name _) = text "package" <+> prettyPrec p name <> semi

instance Show a => Pretty (ImportDecl a) where
  prettyPrec p (ImportDecl st name wc a) =
    text "import" <+> opt st (text "static")
                  <+> prettyPrec p name <> opt wc (text ".*")
                  <> semi

-----------------------------------------------------------------------
-- Declarations

instance Show a => Pretty (TypeDecl a) where
  prettyPrec p (ClassTypeDecl     cd _) = prettyPrec p cd
  prettyPrec p (InterfaceTypeDecl id _) = prettyPrec p id

instance Show a => Pretty (ClassDecl a) where
  prettyPrec p (EnumDecl mods ident impls body _) =
    hsep [hsep (map (prettyPrec p) mods)
          , text "enum"
          , prettyPrec p ident
          , ppImplements p impls
         ] $$ prettyPrec p body

  prettyPrec p (ClassDecl mods ident tParams mSuper impls body _) =
    hsep [hsep (map (prettyPrec p) mods)
          , text "class"
          , prettyPrec p ident
          , ppTypeParams p tParams
          , ppExtends p (maybe [] return mSuper)
          , ppImplements p impls
         ] $$ prettyPrec p body

instance Show a => Pretty (ClassBody a) where
  prettyPrec p (ClassBody ds _) =
    braceBlock (map (prettyPrec p) ds)

instance Show a => Pretty (EnumBody a) where
  prettyPrec p (EnumBody cs ds _) =
    braceBlock $
        punctuate comma (map (prettyPrec p) cs) ++
        opt (not $ null ds) semi : map (prettyPrec p) ds

instance Show a => Pretty (EnumConstant a) where
  prettyPrec p (EnumConstant ident args mBody _) =
    prettyPrec p ident
        -- needs special treatment since even the parens are optional
        <> opt (not $ null args) (ppArgs p args)
      $$ maybePP p mBody

instance Show a => Pretty (InterfaceDecl a) where
  prettyPrec p (InterfaceDecl kind mods ident tParams impls body _) =
    hsep [hsep (map (prettyPrec p) mods)
          , text (if kind == InterfaceNormal then "interface" else "@interface")
          , prettyPrec p ident
          , ppTypeParams p tParams
          , ppExtends p impls
         ] $$ prettyPrec p body

instance Show a => Pretty (InterfaceBody a) where
  prettyPrec p (InterfaceBody mds _) =
    braceBlock (map (prettyPrec p) mds)

instance Show a => Pretty (Decl a) where
  prettyPrec p (MemberDecl md _) = prettyPrec p md
  prettyPrec p (InitDecl b bl _) =
    opt b (text "static") <+> prettyPrec p bl

instance Show a => Pretty (MemberDecl a) where
  prettyPrec p (FieldDecl mods t vds _) =
    hsep (map (prettyPrec p) mods ++ prettyPrec p t:punctuate (text ",") (map (prettyPrec p) vds)) <> semi

  prettyPrec p (MethodDecl mods tParams mt ident fParams throws def body _) =
    hsep [hsep (map (prettyPrec p) mods)
          , ppTypeParams p tParams
          , ppResultType p mt
          , prettyPrec p ident
          , ppArgs p fParams
          , ppThrows p throws
          , ppDefault p def
         ] $$ prettyPrec p body

  prettyPrec p (ConstructorDecl mods tParams ident fParams throws body _) =
    hsep [hsep (map (prettyPrec p) mods)
          , ppTypeParams p tParams
          , prettyPrec p ident
          , ppArgs p fParams
          , ppThrows p throws
         ] $$ prettyPrec p body

  prettyPrec p (MemberClassDecl cd _) = prettyPrec p cd
  prettyPrec p (MemberInterfaceDecl id _) = prettyPrec p id

instance Show a => Pretty (VarDecl a) where
  prettyPrec p (VarDecl vdId Nothing _) = prettyPrec p vdId
  prettyPrec p (VarDecl vdId (Just ie) _) =
    (prettyPrec p vdId <+> char '=') <+> prettyPrec p ie

instance Show a => Pretty (VarDeclId a) where
  prettyPrec p (VarId ident _) = prettyPrec p ident
  prettyPrec p (VarDeclArray vId _) = prettyPrec p vId <> text "[]"

instance Show a => Pretty (VarInit a) where
  prettyPrec p (InitExp e _) = prettyPrec p e
  prettyPrec p (InitArray (ArrayInit ai _) _) =
    text "{" <+> hsep (punctuate comma (map (prettyPrec p) ai)) <+> text "}"

instance Show a => Pretty (FormalParam a) where
  prettyPrec p (FormalParam mods t b vId _) =
    hsep [hsep (map (prettyPrec p) mods)
          , prettyPrec p t <> opt b (text "...")
          , prettyPrec p vId
         ]

instance Show a => Pretty (MethodBody a) where
  prettyPrec p (MethodBody mBlock _) = maybe semi (prettyPrec p) mBlock

instance Show a => Pretty (ConstructorBody a) where
  prettyPrec p (ConstructorBody mECI stmts _) =
    braceBlock $ maybePP p mECI : map (prettyPrec p) stmts

instance Show a => Pretty (ExplConstrInv a) where
  prettyPrec p (ThisInvoke rts args _) =
    ppTypeParams p rts <+> text "this" <> ppArgs p args <> semi
  prettyPrec p (SuperInvoke rts args _) =
    ppTypeParams p rts <+> text "super" <> ppArgs p args <> semi
  prettyPrec p (PrimarySuperInvoke e rts args _) =
    prettyPrec p e <> char '.' <>
      ppTypeParams p rts <+> text "super" <> ppArgs p args <> semi

instance Show a => Pretty (Modifier a) where
  prettyPrec p (Annotation ann _) = prettyPrec p ann $+$ nest (-1) ( text "")
  prettyPrec p mod = text . map toLower $ show mod

instance Show a => Pretty (Annotation a) where
  prettyPrec p x = text "@" <> prettyPrec p (annName x) <> case x of
         MarkerAnnotation {} -> text ""
         SingleElementAnnotation {} -> text "(" <> prettyPrec p (annValue x) <> text ")"
         NormalAnnotation {} -> text "(" <> ppEVList p (annKV x) <> text ")"

ppEVList p = hsep . punctuate comma . map (\(k,v) -> prettyPrec p k <+> text "=" <+> prettyPrec p v)

instance Show a => Pretty (ElementValue a) where
  prettyPrec p (EVVal vi _) = prettyPrec p vi
  prettyPrec p (EVAnn ann _) = prettyPrec p ann

-----------------------------------------------------------------------
-- Statements


instance Show a => Pretty (Block a) where
  prettyPrec p (Block stmts _) = braceBlock $ map (prettyPrec p) stmts

instance Show a => Pretty (BlockStmt a) where
  prettyPrec p (BlockStmt stmt _) = prettyPrec p stmt
  prettyPrec p (LocalClass cd _) = prettyPrec p cd
  prettyPrec p (LocalVars mods t vds _) =
    hsep (map (prettyPrec p) mods) <+> prettyPrec p t <+>
      hsep (punctuate comma $ map (prettyPrec p) vds) <> semi

instance Show a => Pretty (Stmt a) where
  prettyPrec p (StmtBlock block _) = prettyPrec p block
  prettyPrec p (IfThen c th _) =
    text "if" <+> parens (prettyPrec 0 c) $+$ prettyNestedStmt 0 th

  prettyPrec p (IfThenElse c th el _) =
    text "if" <+> parens (prettyPrec p c) $+$ prettyNestedStmt 0 th $+$ text "else" $+$ prettyNestedStmt 0 el

  prettyPrec p (While c stmt _) =
    text "while" <+> parens (prettyPrec p c) $+$ prettyNestedStmt 0 stmt

  prettyPrec p (BasicFor mInit mE mUp stmt _) =
    text "for" <+> (parens $ hsep [maybePP p mInit, semi
                           , maybePP p mE, semi
                           , maybe empty (hsep . punctuate comma . map (prettyPrec p)) mUp
                          ]) $+$ prettyNestedStmt p stmt

  prettyPrec p (EnhancedFor mods t ident e stmt _) =
    hsep [text "for"
          , parens $ hsep [
                  hsep (map (prettyPrec p) mods)
                , prettyPrec p t
                , prettyPrec p ident
                , colon
                , prettyPrec p e
               ]
          , prettyPrec p stmt
         ]

  prettyPrec p (Empty _) = semi

  prettyPrec p (ExpStmt e _) = prettyPrec p e <> semi

  prettyPrec p (Assert ass mE _) =
    text "assert" <+> prettyPrec p ass
      <+> maybe empty ((colon <>) . prettyPrec p) mE <> semi

  prettyPrec p (Switch e sBlocks _) =
    text "switch" <+> parens (prettyPrec p e)
      $$ braceBlock (map (prettyPrec p) sBlocks)

  prettyPrec p (Do stmt e _) =
    text "do" $+$ prettyPrec p stmt <+> text "while" <+> parens (prettyPrec p e) <> semi

  prettyPrec p (Break mIdent _) =
    text "break" <+> maybePP p mIdent <> semi

  prettyPrec p (Continue mIdent _) =
    text "continue" <+> maybePP p mIdent <> semi

  prettyPrec p (Return mE _) =
    text "return" <+> maybePP p mE <> semi

  prettyPrec p (Synchronized e block _) =
    text "synchronized" <+> parens (prettyPrec p e) $$ prettyPrec p block

  prettyPrec p (Throw e _) =
    text "throw" <+> prettyPrec p e <> semi

  prettyPrec p (Try block catches mFinally _) =
    text "try" $$ prettyPrec p block $$
      vcat (map (prettyPrec p) catches ++ [ppFinally mFinally])
   where ppFinally Nothing = empty
         ppFinally (Just bl) = text "finally" <+> prettyPrec p bl

  prettyPrec p (Labeled ident stmt _) =
    prettyPrec p ident <> colon <+> prettyPrec p stmt

instance Show a => Pretty (Catch a) where
  prettyPrec p (Catch fParam block _) =
    hsep [text "catch", parens (prettyPrec p fParam)] $$ prettyPrec p block

instance Show a => Pretty (SwitchBlock a) where
  prettyPrec p (SwitchBlock lbl stmts _) =
    vcat (prettyPrec p lbl : map (nest 2 . prettyPrec p) stmts)

instance Show a => Pretty (SwitchLabel a) where
  prettyPrec p (SwitchCase e _) =
    text "case" <+> prettyPrec p e <> colon
  prettyPrec p (Default _) = text "default:"

instance Show a => Pretty (ForInit a) where
  prettyPrec p (ForLocalVars mods t vds _) =
    hsep $ map (prettyPrec p) mods ++
            prettyPrec p t: punctuate comma (map (prettyPrec p) vds)
  prettyPrec p (ForInitExps es _) =
    hsep $ punctuate comma (map (prettyPrec p) es)


-----------------------------------------------------------------------
-- Expressions

instance Show a => Pretty (Exp a) where
  prettyPrec p (Lit l _) = prettyPrec p l

  prettyPrec p (ClassLit mT _) =
    ppResultType p mT <> text ".class"

  prettyPrec _ (This _) = text "this"

  prettyPrec p (ThisClass name _) =
    prettyPrec p name <> text ".this"

  prettyPrec p (InstanceCreation tArgs tds args mBody _) =
    hsep [text "new"
          , ppTypeParams p tArgs
          , prettyPrec p tds <> ppArgs p args
         ] $$ maybePP p mBody

  prettyPrec p (QualInstanceCreation e tArgs ident args mBody _) =
    hsep [prettyPrec p e <> char '.' <> text "new"
          , ppTypeParams p tArgs
          , prettyPrec p ident <> ppArgs p args
         ] $$ maybePP p mBody

  prettyPrec p (ArrayCreate t es k _) =
    text "new" <+>
      hcat (prettyPrec p t : map (brackets . prettyPrec p) es
                ++ replicate k (text "[]"))

  prettyPrec p (ArrayCreateInit t k init _) =
    text "new"
      <+> hcat (prettyPrec p t : replicate k (text "[]"))
      <+> prettyPrec p init

  prettyPrec p (FieldAccess fa _) = parenPrec p 1 $ prettyPrec 1 fa

  prettyPrec p (MethodInv mi _) = parenPrec p 1 $ prettyPrec 1 mi

  prettyPrec p (ArrayAccess ain _) = parenPrec p 1 $ prettyPrec 1 ain

  prettyPrec p (ExpName name _) = prettyPrec p name

  prettyPrec p (PostIncrement _ e) = parenPrec p 1 $ prettyPrec 2 e <> text "++"

  prettyPrec p (PostDecrement _ e) = parenPrec p 1 $ prettyPrec 2 e <> text "--"

  prettyPrec p (PreIncrement _ e)  = parenPrec p 1 $ text "++" <> prettyPrec 2 e

  prettyPrec p (PreDecrement _ e)  = parenPrec p 1 $ text "--" <> prettyPrec 2 e

  prettyPrec p (PrePlus _ e) = parenPrec p 2 $ char '+' <> prettyPrec 2 e

  prettyPrec p (PreMinus _ e) = parenPrec p 2 $ char '-' <> prettyPrec 2 e

  prettyPrec p (PreBitCompl _ e) = parenPrec p 2 $ char '~' <> prettyPrec 2 e

  prettyPrec p (PreNot _ e) = parenPrec p 2 $ char '!' <> prettyPrec 2 e

  prettyPrec p (Cast t e _) = parenPrec p 2 $ parens (prettyPrec p t) <+> prettyPrec 2 e

  prettyPrec p (BinOp e1 op e2 _) =
    let prec = opPrec op in
    parenPrec p prec (prettyPrec prec e1 <+> prettyPrec p op <+> prettyPrec prec e2)

  prettyPrec p (InstanceOf e rt _) =
    let cp = opPrec (LThan ()) in
    parenPrec p cp $ prettyPrec cp e
                   <+> text "instanceof" <+> prettyPrec cp rt

  prettyPrec p (Cond c th el _) =
    parenPrec p 13 $ prettyPrec 13 c <+> char '?'
                   <+> prettyPrec p th <+> colon <+> prettyPrec 13 el

  prettyPrec p (Assign lhs aop e _) =
    hsep [prettyPrec p lhs, prettyPrec p aop, prettyPrec p e]

  prettyPrec p (Lambda params body _) =
    prettyPrec p params <+> text "->" <+> prettyPrec p body

  prettyPrec p (MethodRef i1 i2 _) =
    prettyPrec p i1 <+> text "::" <+> prettyPrec p i2

instance Show a => Pretty (LambdaParams a) where
  prettyPrec p (LambdaSingleParam ident _) = prettyPrec p ident
  prettyPrec p (LambdaFormalParams params _) = ppArgs p params
  prettyPrec p (LambdaInferredParams idents _) = ppArgs p idents

instance Show a => Pretty (LambdaExpression a) where
  prettyPrec p (LambdaExpression exp _) = prettyPrec p exp
  prettyPrec p (LambdaBlock block _) = prettyPrec p block

instance Pretty (Literal a) where
  prettyPrec p (Int i _) = text (show i)
  prettyPrec p (Word i _) = text (show i) <> char 'L'
  prettyPrec p (Float f _) = text (show f) <> char 'F'
  prettyPrec p (Double d _) = text (show d)
  prettyPrec p (Boolean b _) = text . map toLower $ show b
  prettyPrec p (Char c _) = quotes $ text (escapeChar c)
  prettyPrec p (String s _) = doubleQuotes $ text (concatMap escapeString s)
  prettyPrec p (Null _) = text "null"

instance Pretty (Op a) where
  prettyPrec p op = text $ case op of
    Mult _    -> "*"
    Div _     -> "/"
    Rem _     -> "%"
    Add _     -> "+"
    Sub _     -> "-"
    LShift _  -> "<<"
    RShift _  -> ">>"
    RRShift _ -> ">>>"
    LThan _   -> "<"
    GThan _   -> ">"
    LThanE _  -> "<="
    GThanE _  -> ">="
    Equal _   -> "=="
    NotEq _   -> "!="
    And _     -> "&"
    Xor _     -> "^"
    Or _      -> "|"
    CAnd _    -> "&&"
    COr _     -> "||"

instance Pretty (AssignOp a) where
  prettyPrec p aop = text $ case aop of
    EqualA _  -> "="
    MultA _   -> "*="
    DivA _    -> "/="
    RemA _    -> "%="
    AddA _    -> "+="
    SubA _    -> "-="
    LShiftA _ -> "<<="
    RShiftA _ -> ">>="
    RRShiftA _ -> ">>>="
    AndA _    -> "&="
    XorA _    -> "^="
    OrA _     -> "|="

instance Show a => Pretty (Lhs a) where
  prettyPrec p (NameLhs name _) = prettyPrec p name
  prettyPrec p (FieldLhs fa _) = prettyPrec p fa
  prettyPrec p (ArrayLhs ain _) = prettyPrec p ain

instance Show a => Pretty (ArrayIndex a) where
  prettyPrec p (ArrayIndex ref e _) = prettyPrec p ref <> (hcat $ map (brackets . (prettyPrec p)) e)

instance Show a => Pretty (FieldAccess a) where
  prettyPrec p (PrimaryFieldAccess e ident _) =
    prettyPrec p e <> char '.' <> prettyPrec p ident
  prettyPrec p (SuperFieldAccess ident _) =
    text "super." <> prettyPrec p ident
  prettyPrec p (ClassFieldAccess name ident _) =
    prettyPrec p name <> text "." <> prettyPrec p ident

instance Show a => Pretty (MethodInvocation a) where
  prettyPrec p (MethodCall name args _) =
    prettyPrec p name <> ppArgs p args

  prettyPrec p (PrimaryMethodCall e tArgs ident args _) =
    hcat [prettyPrec p e, char '.', ppTypeParams p tArgs,
           prettyPrec p ident, ppArgs p args]

  prettyPrec p (SuperMethodCall tArgs ident args _) =
    hcat [text "super.", ppTypeParams p tArgs,
           prettyPrec p ident, ppArgs p args]

  prettyPrec p (ClassMethodCall name tArgs ident args _) =
    hcat [prettyPrec p name, text ".super.", ppTypeParams p tArgs,
           prettyPrec p ident, ppArgs p args]

  prettyPrec p (TypeMethodCall name tArgs ident args _) =
    hcat [prettyPrec p name, char '.', ppTypeParams p tArgs,
           prettyPrec p ident, ppArgs p args]

instance Show a => Pretty (ArrayInit a) where
  prettyPrec p (ArrayInit vInits _) =
    braceBlock $ map (\v -> prettyPrec p v <> comma) vInits
    --braces $ hsep (punctuate comma (map (prettyPrec p) vInits))


ppArgs :: Pretty a => Int -> [a] -> Doc
ppArgs p = parens . hsep . punctuate comma . map (prettyPrec p)

-----------------------------------------------------------------------
-- Types

instance Pretty (Type a) where
  prettyPrec p (PrimType pt _) = prettyPrec p pt
  prettyPrec p (RefType rt _) = prettyPrec p rt

instance Pretty (RefType a) where
  prettyPrec p (ClassRefType ct _) = prettyPrec p ct
  prettyPrec p (ArrayType t _) = prettyPrec p t <> text "[]"

instance Pretty (ClassType a) where
  prettyPrec p (ClassType _ itas) =
    hcat . punctuate (char '.') $
      map (\(i,tas) -> prettyPrec p i <> ppTypeParams p tas) itas

instance Pretty (TypeArgument a) where
  prettyPrec p (ActualType _ rt) = prettyPrec p rt
  prettyPrec p (Wildcard _ mBound) = char '?' <+> maybePP p mBound

instance Pretty (TypeDeclSpecifier a) where
  prettyPrec p (TypeDeclSpecifier ct _) = prettyPrec p ct
  prettyPrec p (TypeDeclSpecifierWithDiamond ct i d _) =  prettyPrec p ct <> char '.' <> prettyPrec p i <> prettyPrec p d
  prettyPrec p (TypeDeclSpecifierUnqualifiedWithDiamond i d _) = prettyPrec p i <> prettyPrec p d

instance Pretty (Diamond a) where
  prettyPrec p (Diamond _) = text "<>"

instance Pretty (WildcardBound a) where
  prettyPrec p (ExtendsBound _ rt) = text "extends" <+> prettyPrec p rt
  prettyPrec p (SuperBound   _ rt) = text "super"   <+> prettyPrec p rt

instance Pretty (PrimType a) where
  prettyPrec p (BooleanT _) = text "boolean"
  prettyPrec p (ByteT _)    = text "byte"
  prettyPrec p (ShortT _)   = text "short"
  prettyPrec p (IntT _)     = text "int"
  prettyPrec p (LongT _)    = text "long"
  prettyPrec p (CharT _)    = text "char"
  prettyPrec p (FloatT _)   = text "float"
  prettyPrec p (DoubleT _)  = text "double"

instance Pretty (TypeParam a) where
  prettyPrec p (TypeParam ident rts _) =
    prettyPrec p ident
      <+> opt (not $ null rts)
           (hsep $ text "extends":
                    punctuate (text " &") (map (prettyPrec p) rts))

ppTypeParams :: Pretty a => Int -> [a] -> Doc
ppTypeParams _ [] = empty
ppTypeParams p tps = char '<'
    <> hsep (punctuate comma (map (prettyPrec p) tps))
    <> char '>'

ppImplements :: Int -> [RefType a] -> Doc
ppImplements _ [] = empty
ppImplements p rts = text "implements"
    <+> hsep (punctuate comma (map (prettyPrec p) rts))

ppExtends :: Int -> [RefType a] -> Doc
ppExtends _ [] = empty
ppExtends p rts = text "extends"
    <+> hsep (punctuate comma (map (prettyPrec p) rts))

ppThrows :: Int -> [ExceptionType a] -> Doc
ppThrows _ [] = empty
ppThrows p ets = text "throws"
    <+> hsep (punctuate comma (map (prettyPrec p) ets))

ppDefault :: Show a => Int -> Maybe (Exp a) -> Doc
ppDefault _ Nothing = empty
ppDefault p (Just exp) = text "default" <+> prettyPrec p exp

ppResultType :: Int -> Maybe (Type a) -> Doc
ppResultType _ Nothing = text "void"
ppResultType p (Just a) = prettyPrec p a

-----------------------------------------------------------------------
-- Names and identifiers

instance Pretty (Name a) where
  prettyPrec p (Name is _) =
    hcat (punctuate (char '.') $ map (prettyPrec p) is)

instance Pretty (Ident a) where
  prettyPrec p (Ident s _) = text s


-----------------------------------------------------------------------
-- Help functionality
prettyNestedStmt :: Show a => Int -> Stmt a -> Doc
prettyNestedStmt prio p@(StmtBlock b _) = prettyPrec prio p
prettyNestedStmt prio p = nest 2 (prettyPrec prio p)

maybePP :: Pretty a => Int -> Maybe a -> Doc
maybePP p = maybe empty (prettyPrec p)

opt :: Bool -> Doc -> Doc
opt x a = if x then a else empty

braceBlock :: [Doc] -> Doc
braceBlock xs = char '{'
    $+$ nest 2 (vcat xs)
    $+$ char '}'

opPrec (Mult _)    = 3
opPrec (Div _)     = 3
opPrec (Rem _)     = 3
opPrec (Add _)     = 4
opPrec (Sub _)     = 4
opPrec (LShift _)  = 5
opPrec (RShift _)  = 5
opPrec (RRShift _) = 5
opPrec (LThan _)   = 6
opPrec (GThan _)   = 6
opPrec (LThanE _)  = 6
opPrec (GThanE _)  = 6
opPrec (Equal _)   = 7
opPrec (NotEq _)   = 7
opPrec (And _)     = 8
opPrec (Xor _)     = 9
opPrec (Or _)      = 10
opPrec (CAnd _)    = 11
opPrec (COr _)     = 12

escapeGeneral :: Char -> String
escapeGeneral '\b' = "\\b"
escapeGeneral '\t' = "\\t"
escapeGeneral '\n' = "\\n"
escapeGeneral '\f' = "\\f"
escapeGeneral '\r' = "\\r"
escapeGeneral '\\' = "\\\\"
escapeGeneral c | c >= ' ' && c < '\DEL' = [c]
                | c <= '\xFFFF' = printf "\\u%04x" (fromEnum c)
                | otherwise = error $ "Language.Java.Pretty.escapeGeneral: Char " ++ show c ++ " too large for Java char"

escapeChar :: Char -> String
escapeChar '\'' = "\\'"
escapeChar c = escapeGeneral c

escapeString :: Char -> String
escapeString '"' = "\\\""
escapeString c | c <= '\xFFFF' = escapeGeneral c
               | otherwise = escapeGeneral lead ++ escapeGeneral trail
                   where c' = fromEnum c - 0x010000
                         lead = toEnum $ 0xD800 + c' `div` 0x0400
                         trail = toEnum $ 0xDC00 + c' `mod` 0x0400
