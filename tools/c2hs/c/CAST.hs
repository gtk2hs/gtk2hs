--  C -> Haskell Compiler: Abstract Syntax for Header Files
--
--  Author : Manuel M T Chakravarty
--  Created: 7 March 99
--
--  Version $Revision: 1.3 $ from $Date: 2005/06/22 16:01:20 $
--
--  Copyright (c) [1999..2004] Manuel M T Chakravarty
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  Abstract syntax of C header files.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  The tree structure corresponds to the grammar in Appendix A of K&R.  This
--  abstract syntax simplifies the concrete syntax by merging similar concrete
--  constructs into a single type of abstract tree structure: declarations are
--  merged with structure declarations, parameter declarations and type names,
--  and declarators are merged with abstract declarators.
--
--  With K&R we refer to ``The C Programming Language'', second edition, Brain
--  W. Kernighan and Dennis M. Ritchie, Prentice Hall, 1988.  This module
--  supports the C99 `restrict' extension
--  <http://www.lysator.liu.se/c/restrict.html>, `inline' functions, and also
--  the GNU C `alignof' extension.
--
--- TODO ----------------------------------------------------------------------
--

module CAST (CHeader(..), CExtDecl(..), CFunDef(..), CStat(..), CDecl(..),
	     CDeclSpec(..), CStorageSpec(..), CTypeSpec(..), CTypeQual(..),
	     CStructUnion(..),  CStructTag(..), CEnum(..), CDeclr(..),
	     CInit(..), CExpr(..), CAssignOp(..), CBinaryOp(..), CUnaryOp(..),
	     CConst (..)) 
where

import Common     (Position, Pos(posOf), nopos)
import Idents     (Ident)
import Attributes (Attrs)
import Binary     (Binary(..), putByte, getByte)


-- a complete C header file (K&R A10) (EXPORTED)
--
data CHeader = CHeader [CExtDecl]
		       Attrs

instance Pos CHeader where
  posOf (CHeader _ at) = posOf at

instance Eq CHeader where
  (CHeader _ at1) == (CHeader _ at2) = at1 == at2

-- external C declaration (K&R A10) (EXPORTED)
--
data CExtDecl = CDeclExt CDecl
	      | CFDefExt CFunDef

instance Pos CExtDecl where
  posOf (CDeclExt decl) = posOf decl
  posOf (CFDefExt fdef) = posOf fdef

instance Eq CExtDecl where
  CDeclExt decl1 == CDeclExt decl2 = decl1 == decl2
  CFDefExt fdef1 == CFDefExt fdef2 = fdef1 == fdef2

-- C function definition (K&R A10.1) (EXPORTED)
--
-- * The only type specifiers allowed are `extern' and `static'.
--
-- * The declarator must specify explicitly that the declared identifier has
--   function type.
--
-- * The optional declaration list is for old-style function declarations.
--
-- * The statement must be a compound statement.
--
data CFunDef = CFunDef [CDeclSpec]	-- type specifier and qualifier
		       CDeclr		-- declarator
		       [CDecl]		-- optional declaration list
		       CStat		-- compound statement
		       Attrs

instance Pos CFunDef where
  posOf (CFunDef _ _ _ _ at) = posOf at

instance Eq CFunDef where
  CFunDef _ _ _ _ at1 == CFunDef _ _ _ _ at2 = at1 == at2

-- C statement (A9) (EXPORTED)
--
data CStat = CLabel    Ident		-- label
		       CStat
		       Attrs
           | CCase     CExpr		-- constant expression
		       CStat
		       Attrs
           | CDefault  CStat		-- default case
		       Attrs
           | CExpr     (Maybe CExpr)	-- expression statement, maybe empty
		       Attrs
           | CCompound [CDecl]		-- optional declaration list
		       [CStat]		-- optional statement list
		       Attrs
           | CIf       CExpr		-- conditional expression
		       CStat
		       (Maybe CStat)    -- optional "else" case
		       Attrs
           | CSwitch   CExpr	        -- selector
		       CStat
		       Attrs
           | CWhile    CExpr
		       CStat
		       Bool		-- `True' implies "do-while" statement
		       Attrs
           | CFor      (Maybe CExpr)
		       (Maybe CExpr)
		       (Maybe CExpr)
		       CStat
		       Attrs
           | CGoto     Ident		-- label
		       Attrs
           | CCont     Attrs		-- continue statement
	   | CBreak    Attrs		-- break statement
	   | CReturn   (Maybe CExpr)
		       Attrs

instance Pos CStat where
  posOf (CLabel    _ _     at) = posOf at
  posOf (CCase     _ _     at) = posOf at
  posOf (CDefault  _       at) = posOf at
  posOf (CExpr     _       at) = posOf at
  posOf (CCompound _ _     at) = posOf at
  posOf (CIf       _ _ _   at) = posOf at
  posOf (CSwitch   _ _     at) = posOf at
  posOf (CWhile    _ _ _   at) = posOf at
  posOf (CFor      _ _ _ _ at) = posOf at
  posOf (CGoto     _	   at) = posOf at
  posOf (CCont     	   at) = posOf at
  posOf (CBreak    	   at) = posOf at
  posOf (CReturn   _   	   at) = posOf at

instance Eq CStat where
  (CLabel    _ _     at1) == (CLabel    _ _     at2) = at1 == at2
  (CCase     _ _     at1) == (CCase     _ _     at2) = at1 == at2
  (CDefault  _       at1) == (CDefault  _       at2) = at1 == at2
  (CExpr     _       at1) == (CExpr     _       at2) = at1 == at2
  (CCompound _ _     at1) == (CCompound _ _     at2) = at1 == at2
  (CIf       _ _ _   at1) == (CIf       _ _ _   at2) = at1 == at2
  (CSwitch   _ _     at1) == (CSwitch   _ _     at2) = at1 == at2
  (CWhile    _ _ _   at1) == (CWhile    _ _ _   at2) = at1 == at2
  (CFor      _ _ _ _ at1) == (CFor      _ _ _ _ at2) = at1 == at2
  (CGoto     _	     at1) == (CGoto     _	at2) = at1 == at2
  (CCont	     at1) == (CCont		at2) = at1 == at2
  (CBreak	     at1) == (CBreak		at2) = at1 == at2
  (CReturn   _	     at1) == (CReturn   _	at2) = at1 == at2

-- C declaration (K&R A8), structure declaration (K&R A8.3), parameter
-- declaration (K&R A8.6.3), and type name (K&R A8.8) (EXPORTED) 
--
-- * Toplevel declarations (K&R A8): 
--
--   - they require that the type specifier and qualifier list is not empty,
--     but gcc allows it and just issues a warning; for the time being, we
--     also allow it;
--   - at most one storage class specifier is allowed per declaration;
--   - declarators must be present and size expressions are not allowed, ie,
--     the elements of K&R's init-declarator-list are represented by triples
--     of the form `(Just declr, oinit, Nothing)', where `oinit' maybe
--     `Nothing' or `Just init'; and
--   - abstract declarators are not allowed.
--
-- * Structure declarations (K&R A8.3):
--
--   - do not allow storage specifiers;
--   - do not allow initializers; 
--   - require a non-empty declarator-triple list, where abstract declarators 
--     are not allowed; and
--   - each of the declarator-triples has to contain either a declarator or a
--     size expression, or both, ie, it has the form `(Just decl, Nothing,
--     Nothing)', `(Nothing, Nothing, Just size)', or `(Just decl, Nothing,
--     Just size)'.
--
-- * Parameter declarations (K&R A8.6.3):
--
--   - allow neither initializers nor size expressions;
--   - allow at most one declarator triple of the form `(Just declr, Nothing, 
--     Nothing)' (in case of an empty declarator, the list must be empty); and
--   - allow abstract declarators.
--
-- * Type names (A8.8):
--
--   - do not allow storage specifiers;
--   - allow neither initializers nor size expressions; and
--   - allow at most one declarator triple of the form `(Just declr, Nothing, 
--     Nothing)' (in case of an empty declarator, the list must be empty),
--     where the declarator must be abstract, ie, must not contain a declared
--     identifier. 
--
data CDecl = CDecl [CDeclSpec]		-- type specifier and qualifier
		   [(Maybe CDeclr,	-- declarator (may be omitted)
		     Maybe CInit,	-- optional initializer
		     Maybe CExpr)]	-- optional size (const expr)
		   Attrs

instance Pos CDecl where
  posOf (CDecl _ _ at) = posOf at

instance Eq CDecl where
  (CDecl _ _ at1) == (CDecl _ _ at2) = at1 == at2

-- C declaration specifiers and qualifiers (EXPORTED)
--
data CDeclSpec = CStorageSpec CStorageSpec
	       | CTypeSpec    CTypeSpec
	       | CTypeQual    CTypeQual
	       deriving (Eq)

instance Pos CDeclSpec where
  posOf (CStorageSpec sspec) = posOf sspec
  posOf (CTypeSpec    tspec) = posOf tspec
  posOf (CTypeQual    tqual) = posOf tqual

-- C storage class specifier (K&R A8.1) (EXPORTED)
--
data CStorageSpec = CAuto     Attrs
		  | CRegister Attrs
		  | CStatic   Attrs
		  | CExtern   Attrs
		  | CTypedef  Attrs	-- syntactic awkwardness of C

instance Pos CStorageSpec where
  posOf (CAuto     at) = posOf at
  posOf (CRegister at) = posOf at
  posOf (CStatic   at) = posOf at
  posOf (CExtern   at) = posOf at
  posOf (CTypedef  at) = posOf at

instance Eq CStorageSpec where
  (CAuto     at1) == (CAuto     at2) = at1 == at2
  (CRegister at1) == (CRegister at2) = at1 == at2
  (CStatic   at1) == (CStatic   at2) = at1 == at2
  (CExtern   at1) == (CExtern   at2) = at1 == at2
  (CTypedef  at1) == (CTypedef  at2) = at1 == at2

-- C type specifier (K&R A8.2) (EXPORTED)
--
data CTypeSpec = CVoidType    Attrs
	       | CCharType    Attrs
	       | CShortType   Attrs
	       | CIntType     Attrs
	       | CLongType    Attrs
	       | CFloatType   Attrs
	       | CDoubleType  Attrs
	       | CSignedType  Attrs
	       | CUnsigType   Attrs
	       | CSUType      CStructUnion
			      Attrs
	       | CEnumType    CEnum
			      Attrs
	       | CTypeDef     Ident		-- typedef name
			      Attrs

instance Pos CTypeSpec where
  posOf (CVoidType      at) = posOf at
  posOf (CCharType      at) = posOf at
  posOf (CShortType     at) = posOf at
  posOf (CIntType       at) = posOf at
  posOf (CLongType      at) = posOf at
  posOf (CFloatType     at) = posOf at
  posOf (CDoubleType    at) = posOf at
  posOf (CSignedType    at) = posOf at
  posOf (CUnsigType     at) = posOf at
  posOf (CSUType     _  at) = posOf at
  posOf (CEnumType   _  at) = posOf at
  posOf (CTypeDef    _  at) = posOf at

instance Eq CTypeSpec where
  (CVoidType     at1) == (CVoidType     at2) = at1 == at2
  (CCharType     at1) == (CCharType     at2) = at1 == at2
  (CShortType    at1) == (CShortType    at2) = at1 == at2
  (CIntType      at1) == (CIntType      at2) = at1 == at2
  (CLongType     at1) == (CLongType     at2) = at1 == at2
  (CFloatType    at1) == (CFloatType    at2) = at1 == at2
  (CDoubleType   at1) == (CDoubleType   at2) = at1 == at2
  (CSignedType   at1) == (CSignedType   at2) = at1 == at2
  (CUnsigType    at1) == (CUnsigType    at2) = at1 == at2
  (CSUType     _ at1) == (CSUType     _ at2) = at1 == at2
  (CEnumType   _ at1) == (CEnumType   _ at2) = at1 == at2
  (CTypeDef    _ at1) == (CTypeDef    _ at2) = at1 == at2

-- C type qualifier (K&R A8.2) (EXPORTED)
--
-- * plus `restrict' from C99 and `inline'
--
data CTypeQual = CConstQual Attrs
	       | CVolatQual Attrs
	       | CRestrQual Attrs
	       | CInlinQual Attrs

instance Pos CTypeQual where
 posOf (CConstQual at) = posOf at
 posOf (CVolatQual at) = posOf at
 posOf (CRestrQual at) = posOf at
 posOf (CInlinQual at) = posOf at

instance Eq CTypeQual where
  (CConstQual at1) == (CConstQual at2) = at1 == at2
  (CVolatQual at1) == (CVolatQual at2) = at1 == at2
  (CRestrQual at1) == (CRestrQual at2) = at1 == at2
  (CInlinQual at1) == (CInlinQual at2) = at1 == at2

-- C structure of union declaration (K&R A8.3) (EXPORTED)
--
-- * in both case, either the identifier is present or the list must be
--   non-empty 
--
data CStructUnion = CStruct CStructTag
			    (Maybe Ident)
			    [CDecl]	-- *structure* declaration
			    Attrs

instance Pos CStructUnion where
  posOf (CStruct _ _ _ at) = posOf at

instance Eq CStructUnion where
  (CStruct _ _ _ at1) == (CStruct _ _ _ at2) = at1 == at2

-- (EXPORTED)
--
data CStructTag = CStructTag
		| CUnionTag
		deriving (Eq)

-- C enumeration declaration (K&R A8.4) (EXPORTED)
--
data CEnum = CEnum (Maybe Ident)
		   [(Ident,			-- variant name
		     Maybe CExpr)]		-- explicit variant value
		   Attrs

instance Pos CEnum where
  posOf (CEnum _ _ at) = posOf at

instance Eq CEnum where
  (CEnum _ _ at1) == (CEnum _ _ at2) = at1 == at2

-- C declarator (K&R A8.5) and abstract declarator (K&R A8.8) (EXPORTED)
--
-- * We have one type qualifer list `[CTypeQual]' for each indirection (ie,
--   each occurrence of `*' in the concrete syntax).
--
-- * We unfold K&R's direct-declarators nonterminal into declarators.  Note
--   that `*(*x)' is equivalent to `**x'.
--
-- * Declarators (A8.5) and abstract declarators (A8.8) are represented in the 
--   same structure.  In the case of a declarator, the identifier in
--   `CVarDeclr' must be present; in an abstract declarator it misses.
--   `CVarDeclr Nothing ...' on its own is meaningless, it may only occur as
--   part of a larger type (ie, there must be a pointer, an array, or function
--   declarator around).
--
-- * The qualifiers list in a `CPtrDeclr' may not be empty.
--
-- * Old and new style function definitions are merged into a single case
--   `CFunDeclr'.  In case of an old style definition, the parameter list is
--   empty and the variadic flag is `False' (ie, the parameter names are not
--   stored in the tree).  Remember, a new style definition with no parameters 
--   requires a single `void' in the argument list (according to the standard).
--
-- * We unfold K&R's parameter-type-list nonterminal into the declarator
--   variant for functions.
--
data CDeclr = CVarDeclr (Maybe Ident)		-- declared identifier
		        Attrs
	    | CPtrDeclr [[CTypeQual]]		-- indirections (non-empty)
		        CDeclr
		        Attrs
            | CArrDeclr CDeclr
			(Maybe CExpr)		-- array size
			Attrs
	    | CFunDeclr CDeclr
			[CDecl]			-- *parameter* declarations
			Bool			-- is variadic?
			Attrs

instance Pos CDeclr where
  posOf (CVarDeclr _     at) = posOf at
  posOf (CPtrDeclr _ _   at) = posOf at
  posOf (CArrDeclr _ _   at) = posOf at
  posOf (CFunDeclr _ _ _ at) = posOf at

instance Eq CDeclr where
  (CVarDeclr _     at1) == (CVarDeclr _     at2) = at1 == at2
  (CPtrDeclr _ _   at1) == (CPtrDeclr _ _   at2) = at1 == at2
  (CArrDeclr _ _   at1) == (CArrDeclr _ _   at2) = at1 == at2
  (CFunDeclr _ _ _ at1) == (CFunDeclr _ _ _ at2) = at1 == at2

-- C initializer (K&R A8.7) (EXPORTED)
--
data CInit = CInitExpr CExpr
		       Attrs		-- assignment expression
           | CInitList [CInit]
		       Attrs

instance Pos CInit where
  posOf (CInitExpr _ at) = posOf at
  posOf (CInitList _ at) = posOf at

instance Eq CInit where
  (CInitExpr _ at1) == (CInitExpr _ at2) = at1 == at2
  (CInitList _ at1) == (CInitList _ at2) = at1 == at2

-- C expression (K&R A7) (EXPORTED)
--
-- * these can be arbitrary expression, as the argument of `sizeof' can be
--   arbitrary, even if appearing in a constant expression
--
-- * GNU C extension: `alignof'
--
data CExpr = CComma       [CExpr]	-- comma expression list, n >= 2
		          Attrs
	   | CAssign      CAssignOp	-- assignment operator
		          CExpr		-- l-value
		          CExpr		-- r-value
		          Attrs
	   | CCond        CExpr		-- conditional
		          CExpr		-- true-expression
		          CExpr		-- false-expression
		          Attrs
	   | CBinary      CBinaryOp	-- binary operator
		          CExpr		-- lhs
		          CExpr		-- rhs
		          Attrs
	   | CCast        CDecl		-- type name
		          CExpr
		          Attrs
           | CUnary       CUnaryOp	-- unary operator
		          CExpr
		          Attrs
	   | CSizeofExpr  CExpr
			  Attrs
	   | CSizeofType  CDecl		-- type name
			  Attrs
	   | CAlignofExpr CExpr
			  Attrs
	   | CAlignofType CDecl		-- type name
			  Attrs
	   | CIndex       CExpr		-- array
			  CExpr		-- index
			  Attrs
	   | CCall	  CExpr		-- function
			  [CExpr]	-- arguments
			  Attrs
	   | CMember	  CExpr		-- structure
			  Ident		-- member name
			  Bool		-- deref structure? (True for `->')
			  Attrs
	   | CVar	  Ident		-- identifier (incl. enumeration const)
			  Attrs
           | CConst       CConst		-- includes strings
			  Attrs

instance Pos CExpr where
  posOf (CComma       _     at) = posOf at
  posOf (CAssign      _ _ _ at) = posOf at
  posOf (CCond        _ _ _ at) = posOf at
  posOf (CBinary      _ _ _ at) = posOf at
  posOf (CCast        _ _   at) = posOf at
  posOf (CUnary       _ _   at) = posOf at
  posOf (CSizeofExpr  _     at) = posOf at
  posOf (CSizeofType  _     at) = posOf at
  posOf (CAlignofExpr _     at) = posOf at
  posOf (CAlignofType _     at) = posOf at
  posOf (CIndex       _ _   at) = posOf at
  posOf (CCall        _ _   at) = posOf at
  posOf (CMember      _ _ _ at) = posOf at
  posOf (CVar         _     at) = posOf at
  posOf (CConst       _     at) = posOf at

instance Eq CExpr where
  (CComma      	_     at1) == (CComma       _     at2) = at1 == at2
  (CAssign     	_ _ _ at1) == (CAssign      _ _ _ at2) = at1 == at2
  (CCond       	_ _ _ at1) == (CCond        _ _ _ at2) = at1 == at2
  (CBinary     	_ _ _ at1) == (CBinary      _ _ _ at2) = at1 == at2
  (CCast       	_ _   at1) == (CCast        _ _   at2) = at1 == at2
  (CUnary      	_ _   at1) == (CUnary       _ _   at2) = at1 == at2
  (CSizeofExpr 	_     at1) == (CSizeofExpr  _     at2) = at1 == at2
  (CSizeofType 	_     at1) == (CSizeofType  _     at2) = at1 == at2
  (CAlignofExpr _     at1) == (CAlignofExpr _     at2) = at1 == at2
  (CAlignofType _     at1) == (CAlignofType _     at2) = at1 == at2
  (CIndex      	_ _   at1) == (CIndex       _ _   at2) = at1 == at2
  (CCall       	_ _   at1) == (CCall	    _ _   at2) = at1 == at2
  (CMember     	_ _ _ at1) == (CMember	    _ _ _ at2) = at1 == at2
  (CVar        	_     at1) == (CVar	    _     at2) = at1 == at2
  (CConst      	_     at1) == (CConst	    _	  at2) = at1 == at2

-- C assignment operators (K&R A7.17) (EXPORTED)
--
data CAssignOp = CAssignOp
	       | CMulAssOp
	       | CDivAssOp
	       | CRmdAssOp		-- remainder and assignment
	       | CAddAssOp
	       | CSubAssOp
	       | CShlAssOp
	       | CShrAssOp
	       | CAndAssOp
	       | CXorAssOp
	       | COrAssOp
	       deriving (Eq)

-- C binary operators (K&R A7.6-15) (EXPORTED)
--
data CBinaryOp = CMulOp
	       | CDivOp
	       | CRmdOp			-- remainder of division
	       | CAddOp
	       | CSubOp
	       | CShlOp			-- shift left
	       | CShrOp			-- shift right
	       | CLeOp			-- less
	       | CGrOp			-- greater
	       | CLeqOp			-- less or equal
	       | CGeqOp			-- greater or equal
	       | CEqOp			-- equal
	       | CNeqOp			-- not equal
	       | CAndOp			-- bitwise and
	       | CXorOp			-- exclusive bitwise or
	       | COrOp			-- inclusive bitwise or
	       | CLndOp			-- logical and
	       | CLorOp			-- logical or
	       deriving (Eq)

-- C unary operator (K&R A7.3-4) (EXPORTED)
--
data CUnaryOp = CPreIncOp		-- prefix increment operator
	      | CPreDecOp		-- prefix decrement operator
	      | CPostIncOp		-- postfix increment operator
	      | CPostDecOp		-- postfix decrement operator
	      | CAdrOp			-- address operator
	      | CIndOp			-- indirection operator
	      | CPlusOp			-- prefix plus
	      | CMinOp			-- prefix minus
	      | CCompOp			-- one's complement
	      | CNegOp			-- logical negation
	      deriving (Eq)

-- C constant (K&R A2.5 & A7.2) (EXPORTED)
--
-- * we do not list enumeration constants here, as they are identifiers
--
data CConst = CIntConst   Integer
		          Attrs
	    | CCharConst  Char
		          Attrs
	    | CFloatConst String
			  Attrs
	    | CStrConst   String
			  Attrs

instance Pos CConst where
  posOf (CIntConst   _ at) = posOf at
  posOf (CCharConst  _ at) = posOf at
  posOf (CFloatConst _ at) = posOf at
  posOf (CStrConst   _ at) = posOf at

instance Eq CConst where
  (CIntConst   _ at1) == (CIntConst   _ at2) = at1 == at2
  (CCharConst  _ at1) == (CCharConst  _ at2) = at1 == at2
  (CFloatConst _ at1) == (CFloatConst _ at2) = at1 == at2
  (CStrConst   _ at1) == (CStrConst   _ at2) = at1 == at2


{-! for CDecl derive : GhcBinary !-}
{-! for CEnum derive : GhcBinary !-}
{-! for CStructUnion derive : GhcBinary !-}
{-! for CStructTag derive : GhcBinary !-}
{-! for CExpr derive : GhcBinary !-}
{-! for CInit derive : GhcBinary !-}
{-! for CDeclr derive : GhcBinary !-}
{-! for CDeclSpec derive : GhcBinary !-}
{-! for CTypeSpec derive : GhcBinary !-}
{-! for CStorageSpec derive : GhcBinary !-}
{-! for CTypeQual derive : GhcBinary !-}
{-! for CConst derive : GhcBinary !-}
{-! for CUnaryOp derive : GhcBinary !-}
{-! for CBinaryOp derive : GhcBinary !-}
{-! for CAssignOp derive : GhcBinary !-}
{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance Binary CDecl where
    put_ bh (CDecl aa ab ac) = do
            put_ bh aa
            put_ bh ab
            put_ bh ac
    get bh = do
    aa <- get bh
    ab <- get bh
    ac <- get bh
    return (CDecl aa ab ac)

instance Binary CEnum where
    put_ bh (CEnum aa ab ac) = do
            put_ bh aa
            put_ bh ab
            put_ bh ac
    get bh = do
    aa <- get bh
    ab <- get bh
    ac <- get bh
    return (CEnum aa ab ac)

instance Binary CStructUnion where
    put_ bh (CStruct aa ab ac ad) = do
            put_ bh aa
            put_ bh ab
            put_ bh ac
            put_ bh ad
    get bh = do
    aa <- get bh
    ab <- get bh
    ac <- get bh
    ad <- get bh
    return (CStruct aa ab ac ad)

instance Binary CStructTag where
    put_ bh CStructTag = do
            putByte bh 0
    put_ bh CUnionTag = do
            putByte bh 1
    get bh = do
            h <- getByte bh
            case h of
              0 -> do
                    return CStructTag
              1 -> do
                    return CUnionTag

instance Binary CExpr where
    put_ bh (CComma aa ab) = do
            putByte bh 0
            put_ bh aa
            put_ bh ab
    put_ bh (CAssign ac ad ae af) = do
            putByte bh 1
            put_ bh ac
            put_ bh ad
            put_ bh ae
            put_ bh af
    put_ bh (CCond ag ah ai aj) = do
            putByte bh 2
            put_ bh ag
            put_ bh ah
            put_ bh ai
            put_ bh aj
    put_ bh (CBinary ak al am an) = do
            putByte bh 3
            put_ bh ak
            put_ bh al
            put_ bh am
            put_ bh an
    put_ bh (CCast ao ap aq) = do
            putByte bh 4
            put_ bh ao
            put_ bh ap
            put_ bh aq
    put_ bh (CUnary ar as at) = do
            putByte bh 5
            put_ bh ar
            put_ bh as
            put_ bh at
    put_ bh (CSizeofExpr au av) = do
            putByte bh 6
            put_ bh au
            put_ bh av
    put_ bh (CSizeofType aw ax) = do
            putByte bh 7
            put_ bh aw
            put_ bh ax
    put_ bh (CAlignofExpr ay az) = do
            putByte bh 8
            put_ bh ay
            put_ bh az
    put_ bh (CAlignofType aA aB) = do
            putByte bh 9
            put_ bh aA
            put_ bh aB
    put_ bh (CIndex aC aD aE) = do
            putByte bh 10
            put_ bh aC
            put_ bh aD
            put_ bh aE
    put_ bh (CCall aF aG aH) = do
            putByte bh 11
            put_ bh aF
            put_ bh aG
            put_ bh aH
    put_ bh (CMember aI aJ aK aL) = do
            putByte bh 12
            put_ bh aI
            put_ bh aJ
            put_ bh aK
            put_ bh aL
    put_ bh (CVar aM aN) = do
            putByte bh 13
            put_ bh aM
            put_ bh aN
    put_ bh (CConst aO aP) = do
            putByte bh 14
            put_ bh aO
            put_ bh aP
    get bh = do
            h <- getByte bh
            case h of
              0 -> do
                    aa <- get bh
                    ab <- get bh
                    return (CComma aa ab)
              1 -> do
                    ac <- get bh
                    ad <- get bh
                    ae <- get bh
                    af <- get bh
                    return (CAssign ac ad ae af)
              2 -> do
                    ag <- get bh
                    ah <- get bh
                    ai <- get bh
                    aj <- get bh
                    return (CCond ag ah ai aj)
              3 -> do
                    ak <- get bh
                    al <- get bh
                    am <- get bh
                    an <- get bh
                    return (CBinary ak al am an)
              4 -> do
                    ao <- get bh
                    ap <- get bh
                    aq <- get bh
                    return (CCast ao ap aq)
              5 -> do
                    ar <- get bh
                    as <- get bh
                    at <- get bh
                    return (CUnary ar as at)
              6 -> do
                    au <- get bh
                    av <- get bh
                    return (CSizeofExpr au av)
              7 -> do
                    aw <- get bh
                    ax <- get bh
                    return (CSizeofType aw ax)
              8 -> do
                    ay <- get bh
                    az <- get bh
                    return (CAlignofExpr ay az)
              9 -> do
                    aA <- get bh
                    aB <- get bh
                    return (CAlignofType aA aB)
              10 -> do
                    aC <- get bh
                    aD <- get bh
                    aE <- get bh
                    return (CIndex aC aD aE)
              11 -> do
                    aF <- get bh
                    aG <- get bh
                    aH <- get bh
                    return (CCall aF aG aH)
              12 -> do
                    aI <- get bh
                    aJ <- get bh
                    aK <- get bh
                    aL <- get bh
                    return (CMember aI aJ aK aL)
              13 -> do
                    aM <- get bh
                    aN <- get bh
                    return (CVar aM aN)
              14 -> do
                    aO <- get bh
                    aP <- get bh
                    return (CConst aO aP)

instance Binary CInit where
    put_ bh (CInitExpr aa ab) = do
            putByte bh 0
            put_ bh aa
            put_ bh ab
    put_ bh (CInitList ac ad) = do
            putByte bh 1
            put_ bh ac
            put_ bh ad
    get bh = do
            h <- getByte bh
            case h of
              0 -> do
                    aa <- get bh
                    ab <- get bh
                    return (CInitExpr aa ab)
              1 -> do
                    ac <- get bh
                    ad <- get bh
                    return (CInitList ac ad)

instance Binary CDeclr where
    put_ bh (CVarDeclr aa ab) = do
            putByte bh 0
            put_ bh aa
            put_ bh ab
    put_ bh (CPtrDeclr ac ad ae) = do
            putByte bh 1
            put_ bh ac
            put_ bh ad
            put_ bh ae
    put_ bh (CArrDeclr af ag ah) = do
            putByte bh 2
            put_ bh af
            put_ bh ag
            put_ bh ah
    put_ bh (CFunDeclr ai aj ak al) = do
            putByte bh 3
            put_ bh ai
            put_ bh aj
            put_ bh ak
            put_ bh al
    get bh = do
            h <- getByte bh
            case h of
              0 -> do
                    aa <- get bh
                    ab <- get bh
                    return (CVarDeclr aa ab)
              1 -> do
                    ac <- get bh
                    ad <- get bh
                    ae <- get bh
                    return (CPtrDeclr ac ad ae)
              2 -> do
                    af <- get bh
                    ag <- get bh
                    ah <- get bh
                    return (CArrDeclr af ag ah)
              3 -> do
                    ai <- get bh
                    aj <- get bh
                    ak <- get bh
                    al <- get bh
                    return (CFunDeclr ai aj ak al)

instance Binary CDeclSpec where
    put_ bh (CStorageSpec aa) = do
            putByte bh 0
            put_ bh aa
    put_ bh (CTypeSpec ab) = do
            putByte bh 1
            put_ bh ab
    put_ bh (CTypeQual ac) = do
            putByte bh 2
            put_ bh ac
    get bh = do
            h <- getByte bh
            case h of
              0 -> do
                    aa <- get bh
                    return (CStorageSpec aa)
              1 -> do
                    ab <- get bh
                    return (CTypeSpec ab)
              2 -> do
                    ac <- get bh
                    return (CTypeQual ac)

instance Binary CTypeSpec where
    put_ bh (CVoidType aa) = do
            putByte bh 0
            put_ bh aa
    put_ bh (CCharType ab) = do
            putByte bh 1
            put_ bh ab
    put_ bh (CShortType ac) = do
            putByte bh 2
            put_ bh ac
    put_ bh (CIntType ad) = do
            putByte bh 3
            put_ bh ad
    put_ bh (CLongType ae) = do
            putByte bh 4
            put_ bh ae
    put_ bh (CFloatType af) = do
            putByte bh 5
            put_ bh af
    put_ bh (CDoubleType ag) = do
            putByte bh 6
            put_ bh ag
    put_ bh (CSignedType ah) = do
            putByte bh 7
            put_ bh ah
    put_ bh (CUnsigType ai) = do
            putByte bh 8
            put_ bh ai
    put_ bh (CSUType aj ak) = do
            putByte bh 9
            put_ bh aj
            put_ bh ak
    put_ bh (CEnumType al am) = do
            putByte bh 10
            put_ bh al
            put_ bh am
    put_ bh (CTypeDef an ao) = do
            putByte bh 11
            put_ bh an
            put_ bh ao
    get bh = do
            h <- getByte bh
            case h of
              0 -> do
                    aa <- get bh
                    return (CVoidType aa)
              1 -> do
                    ab <- get bh
                    return (CCharType ab)
              2 -> do
                    ac <- get bh
                    return (CShortType ac)
              3 -> do
                    ad <- get bh
                    return (CIntType ad)
              4 -> do
                    ae <- get bh
                    return (CLongType ae)
              5 -> do
                    af <- get bh
                    return (CFloatType af)
              6 -> do
                    ag <- get bh
                    return (CDoubleType ag)
              7 -> do
                    ah <- get bh
                    return (CSignedType ah)
              8 -> do
                    ai <- get bh
                    return (CUnsigType ai)
              9 -> do
                    aj <- get bh
                    ak <- get bh
                    return (CSUType aj ak)
              10 -> do
                    al <- get bh
                    am <- get bh
                    return (CEnumType al am)
              11 -> do
                    an <- get bh
                    ao <- get bh
                    return (CTypeDef an ao)

instance Binary CStorageSpec where
    put_ bh (CAuto aa) = do
            putByte bh 0
            put_ bh aa
    put_ bh (CRegister ab) = do
            putByte bh 1
            put_ bh ab
    put_ bh (CStatic ac) = do
            putByte bh 2
            put_ bh ac
    put_ bh (CExtern ad) = do
            putByte bh 3
            put_ bh ad
    put_ bh (CTypedef ae) = do
            putByte bh 4
            put_ bh ae
    get bh = do
            h <- getByte bh
            case h of
              0 -> do
                    aa <- get bh
                    return (CAuto aa)
              1 -> do
                    ab <- get bh
                    return (CRegister ab)
              2 -> do
                    ac <- get bh
                    return (CStatic ac)
              3 -> do
                    ad <- get bh
                    return (CExtern ad)
              4 -> do
                    ae <- get bh
                    return (CTypedef ae)

instance Binary CTypeQual where
    put_ bh (CConstQual aa) = do
            putByte bh 0
            put_ bh aa
    put_ bh (CVolatQual ab) = do
            putByte bh 1
            put_ bh ab
    put_ bh (CRestrQual ac) = do
            putByte bh 2
            put_ bh ac
    put_ bh (CInlinQual ad) = do
            putByte bh 3
            put_ bh ad
    get bh = do
            h <- getByte bh
            case h of
              0 -> do
                    aa <- get bh
                    return (CConstQual aa)
              1 -> do
                    ab <- get bh
                    return (CVolatQual ab)
              2 -> do
                    ac <- get bh
                    return (CRestrQual ac)
              3 -> do
                    ad <- get bh
                    return (CInlinQual ad)

instance Binary CConst where
    put_ bh (CIntConst aa ab) = do
            putByte bh 0
            put_ bh aa
            put_ bh ab
    put_ bh (CCharConst ac ad) = do
            putByte bh 1
            put_ bh ac
            put_ bh ad
    put_ bh (CFloatConst ae af) = do
            putByte bh 2
            put_ bh ae
            put_ bh af
    put_ bh (CStrConst ag ah) = do
            putByte bh 3
            put_ bh ag
            put_ bh ah
    get bh = do
            h <- getByte bh
            case h of
              0 -> do
                    aa <- get bh
                    ab <- get bh
                    return (CIntConst aa ab)
              1 -> do
                    ac <- get bh
                    ad <- get bh
                    return (CCharConst ac ad)
              2 -> do
                    ae <- get bh
                    af <- get bh
                    return (CFloatConst ae af)
              3 -> do
                    ag <- get bh
                    ah <- get bh
                    return (CStrConst ag ah)

instance Binary CUnaryOp where
    put_ bh CPreIncOp = putByte bh 0
    put_ bh CPreDecOp = putByte bh 1
    put_ bh CPostIncOp = putByte bh 2
    put_ bh CPostDecOp = putByte bh 3
    put_ bh CAdrOp = putByte bh 4
    put_ bh CIndOp = putByte bh 5
    put_ bh CPlusOp = putByte bh 6
    put_ bh CMinOp = putByte bh 7
    put_ bh CCompOp = putByte bh 8
    put_ bh CNegOp = putByte bh 9
    get bh = do
            h <- getByte bh
            case h of
              0 -> return CPreIncOp
              1 -> return CPreDecOp
              2 -> return CPostIncOp
              3 -> return CPostDecOp
              4 -> return CAdrOp
              5 -> return CIndOp
              6 -> return CPlusOp
              7 -> return CMinOp
              8 -> return CCompOp
              9 -> return CNegOp

instance Binary CBinaryOp where
    put_ bh CMulOp = putByte bh 0
    put_ bh CDivOp = putByte bh 1
    put_ bh CRmdOp = putByte bh 2
    put_ bh CAddOp = putByte bh 3
    put_ bh CSubOp = putByte bh 4
    put_ bh CShlOp = putByte bh 5
    put_ bh CShrOp = putByte bh 6
    put_ bh CLeOp = putByte bh 7
    put_ bh CGrOp = putByte bh 8
    put_ bh CLeqOp = putByte bh 9
    put_ bh CGeqOp = putByte bh 10
    put_ bh CEqOp = putByte bh 11
    put_ bh CNeqOp = putByte bh 12
    put_ bh CAndOp = putByte bh 13
    put_ bh CXorOp = putByte bh 14
    put_ bh COrOp = putByte bh 15
    put_ bh CLndOp = putByte bh 16
    put_ bh CLorOp = putByte bh 17
    get bh = do
            h <- getByte bh
            case h of
              0 -> return CMulOp
              1 -> return CDivOp
              2 -> return CRmdOp
              3 -> return CAddOp
              4 -> return CSubOp
              5 -> return CShlOp
              6 -> return CShrOp
              7 -> return CLeOp
              8 -> return CGrOp
              9 -> return CLeqOp
              10 -> return CGeqOp
              11 -> return CEqOp
              12 -> return CNeqOp
              13 -> return CAndOp
              14 -> return CXorOp
              15 -> return COrOp
              16 -> return CLndOp
              17 -> return CLorOp

instance Binary CAssignOp where
    put_ bh CAssignOp = putByte bh 0
    put_ bh CMulAssOp = putByte bh 1
    put_ bh CDivAssOp = putByte bh 2
    put_ bh CRmdAssOp = putByte bh 3
    put_ bh CAddAssOp = putByte bh 4
    put_ bh CSubAssOp = putByte bh 5
    put_ bh CShlAssOp = putByte bh 6
    put_ bh CShrAssOp = putByte bh 7
    put_ bh CAndAssOp = putByte bh 8
    put_ bh CXorAssOp = putByte bh 9
    put_ bh COrAssOp = putByte bh 10
    get bh = do
            h <- getByte bh
            case h of
              0 -> return CAssignOp
              1 -> return CMulAssOp
              2 -> return CDivAssOp
              3 -> return CRmdAssOp
              4 -> return CAddAssOp
              5 -> return CSubAssOp
              6 -> return CShlAssOp
              7 -> return CShrAssOp
              8 -> return CAndAssOp
              9 -> return CXorAssOp
              10 -> return COrAssOp
