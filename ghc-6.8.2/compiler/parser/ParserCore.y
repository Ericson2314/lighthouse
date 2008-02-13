{
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module ParserCore ( parseCore ) where

import IfaceSyn
import ForeignCall
import RdrHsSyn
import HsSyn
import RdrName
import OccName
import Type ( Kind,
              liftedTypeKindTyCon, openTypeKindTyCon, unliftedTypeKindTyCon,
              argTypeKindTyCon, ubxTupleKindTyCon, mkArrowKind, mkTyConApp
            )
import Name( Name, nameOccName, nameModule, mkExternalName )
import Module
import PackageConfig	( mainPackageId, stringToPackageId )
import ParserCoreUtils
import LexCore
import Literal
import SrcLoc
import TysPrim( wordPrimTyCon, intPrimTyCon, charPrimTyCon, 
		floatPrimTyCon, doublePrimTyCon, addrPrimTyCon )
import TyCon ( TyCon, tyConName )
import FastString
import Outputable
import Char
import Unique

#include "../HsVersions.h"

}

%name parseCore
%tokentype { Token }

%token
 '%module'	{ TKmodule }
 '%data'	{ TKdata }
 '%newtype'	{ TKnewtype }
 '%forall'	{ TKforall }
 '%rec'		{ TKrec }
 '%let'		{ TKlet }
 '%in'		{ TKin }
 '%case'	{ TKcase }
 '%of'		{ TKof }
 '%cast'	{ TKcast }
 '%note'	{ TKnote }
 '%external'	{ TKexternal }
 '%local'	{ TKlocal }
 '%_'		{ TKwild }
 '('		{ TKoparen }
 ')'		{ TKcparen }
 '{'		{ TKobrace }
 '}'		{ TKcbrace }
 '#' 		{ TKhash}
 '='		{ TKeq }
 ':'		{ TKcolon }
 '::'		{ TKcoloncolon }
 ':=:'		{ TKcoloneqcolon }
 '*'		{ TKstar }
 '->'		{ TKrarrow }
 '\\'		{ TKlambda}
 '@'		{ TKat }
 '.'		{ TKdot }
 '?'		{ TKquestion}
 ';'            { TKsemicolon }
 NAME		{ TKname $$ }
 CNAME 		{ TKcname $$ }
 INTEGER	{ TKinteger $$ }
 RATIONAL	{ TKrational $$ }
 STRING		{ TKstring $$ }
 CHAR		{ TKchar $$ }

%monad { P } { thenP } { returnP }
%lexer { lexer } { TKEOF }

%%

module	:: { HsExtCore RdrName }
	-- : '%module' modid tdefs vdefgs	{ HsExtCore $2 $3 $4 }
	: '%module' modid tdefs vdefgs	{ HsExtCore $2 [] [] }


-------------------------------------------------------------
--     Names: the trickiest bit in here

-- A name of the form A.B.C could be:
--   module A.B.C
--   dcon C in module A.B
--   tcon C in module A.B
modid	:: { Module }
	: NAME ':' mparts		{ undefined }

q_dc_name :: { Name }
	  : NAME ':' mparts		{ undefined }

q_tc_name :: { Name }
 	  : NAME ':' mparts		{ undefined }

q_var_occ :: { Name }
          : NAME ':' vparts             { undefined }

mparts	:: { [String] }
	: CNAME				{ [$1] }
	| CNAME '.' mparts		{ $1:$3 }

vparts  :: { [String] }
        : var_occ                       { [$1] }
        | CNAME '.' vparts              { $1:$3 }

-------------------------------------------------------------
--     Type and newtype declarations are in HsSyn syntax

tdefs	:: { [TyClDecl RdrName] }
	: {- empty -}	{[]}
	| tdef tdefs	{$1:$2}

tdef	:: { TyClDecl RdrName }
	: '%data' q_tc_name tv_bndrs '=' '{' cons '}' ';'
                { mkTyData DataType ( noLoc []
				    , noLoc (ifaceExtRdrName $2)
				    , map toHsTvBndr $3
				    , Nothing
				    ) Nothing $6 Nothing }
	| '%newtype' q_tc_name tv_bndrs trep ';'
		{ let tc_rdr = ifaceExtRdrName $2 in
                  mkTyData NewType ( noLoc []
				   , noLoc tc_rdr
				   , map toHsTvBndr $3
				   , Nothing
				   ) Nothing ($4 (rdrNameOcc tc_rdr)) Nothing }

-- For a newtype we have to invent a fake data constructor name
-- It doesn't matter what it is, because it won't be used
trep    :: { OccName -> [LConDecl RdrName] }
        : {- empty -}   { (\ tc_occ -> []) }
        | '=' ty        { (\ tc_occ -> let { dc_name  = mkRdrUnqual (setOccNameSpace dataName tc_occ) ;
			                     con_info = PrefixCon [toHsType $2] }
			                in [noLoc $ ConDecl (noLoc dc_name) Explicit []
					   (noLoc []) con_info ResTyH98 Nothing]) }

cons	:: { [LConDecl RdrName] }
	: {- empty -}	{ [] } -- 20060420 Empty data types allowed. jds
        | con           { [$1] }
	| con ';' cons	{ $1:$3 }

con	:: { LConDecl RdrName }
	: d_pat_occ attv_bndrs hs_atys 
		{ noLoc $ ConDecl (noLoc (mkRdrUnqual $1)) Explicit $2 (noLoc []) (PrefixCon $3) ResTyH98 Nothing }
        | d_pat_occ '::' ty
                -- XXX - audreyt - $3 needs to be split into argument and return types!
                -- also not sure whether the [] below (quantified vars) appears.
                -- also the "PrefixCon []" is wrong.
                -- also we want to munge $3 somehow.
                -- extractWhatEver to unpack ty into the parts to ConDecl
                -- XXX - define it somewhere in RdrHsSyn
		{ noLoc $ ConDecl (noLoc (mkRdrUnqual $1)) Explicit [] (noLoc []) (PrefixCon []) (undefined $3) Nothing }

attv_bndrs :: { [LHsTyVarBndr RdrName] }
	: {- empty -} 	         { [] }
	| '@' tv_bndr attv_bndrs {  toHsTvBndr $2 : $3 }

hs_atys :: { [LHsType RdrName] }
         : atys               { map toHsType $1 }


---------------------------------------
--                 Types
---------------------------------------

atys	:: { [IfaceType] }
	: {- empty -}   { [] }
	| aty atys      { $1:$2 }

aty	:: { IfaceType }
	: fs_var_occ { IfaceTyVar $1 }
	| q_tc_name  { IfaceTyConApp (IfaceTc $1) [] }
	| '(' ty ')' { $2 }

bty	:: { IfaceType }
	: fs_var_occ atys { foldl IfaceAppTy (IfaceTyVar $1) $2 }
        | q_var_occ atys  { undefined }
        | q_tc_name atys  { IfaceTyConApp (IfaceTc $1) $2 }
        | '(' ty ')' { $2 }

ty	:: { IfaceType }
	: bty	                     { $1 }
	| bty '->' ty                { IfaceFunTy $1 $3 }
	| '%forall' tv_bndrs '.' ty  { foldr IfaceForAllTy $4 $2 }

----------------------------------------------
--        Bindings are in Iface syntax

vdefgs	:: { [IfaceBinding] }
	: {- empty -}	        { [] }
	| let_bind ';' vdefgs	{ $1 : $3 }

let_bind :: { IfaceBinding }
	: '%rec' '{' vdefs1 '}' { IfaceRec $3 } -- Can be empty. Do we care?
	|  vdef                 { let (b,r) = $1
				  in IfaceNonRec b r }

vdefs1	:: { [(IfaceLetBndr, IfaceExpr)] }
	: vdef  	        { [$1] }
	| vdef ';' vdefs1       { $1:$3 }

vdef	:: { (IfaceLetBndr, IfaceExpr) }
	: fs_var_occ '::' ty '=' exp { (IfLetBndr $1 $3 NoInfo, $5) }
        | '%local' vdef              { $2 }

  -- NB: qd_occ includes data constructors, because
  --     we allow data-constructor wrappers at top level
  -- But we discard the module name, because it must be the
  -- same as the module being compiled, and Iface syntax only
  -- has OccNames in binding positions. Ah, but it has Names now!

---------------------------------------
--  Binders
bndr	:: { IfaceBndr }
        : '@' tv_bndr 	{ IfaceTvBndr $2 }
	| id_bndr	{ IfaceIdBndr $1 }

bndrs 	:: { [IfaceBndr] }
	: bndr		{ [$1] }
	| bndr bndrs	{ $1:$2 }

id_bndr	:: { IfaceIdBndr }
	: '(' fs_var_occ '::' ty ')'	{ ($2,$4) }

tv_bndr	:: { IfaceTvBndr }
	:  fs_var_occ                    { ($1, ifaceLiftedTypeKind) }
	|  '(' fs_var_occ '::' akind ')' { ($2, $4) }

tv_bndrs 	:: { [IfaceTvBndr] }
	: {- empty -}	{ [] }
	| tv_bndr tv_bndrs	{ $1:$2 }

akind	:: { IfaceKind }
	: '*' 		   { ifaceLiftedTypeKind }	
	| '#'		   { ifaceUnliftedTypeKind }
	| '?'		   { ifaceOpenTypeKind }
        | '(' kind ')'	   { $2 }

kind 	:: { IfaceKind }
	: akind 	   { $1 }
	| akind '->' kind  { ifaceArrow $1 $3 }
        | ty ':=:' ty      { ifaceEq $1 $3 }

-----------------------------------------
--             Expressions

aexp    :: { IfaceExpr }
	: fs_var_occ    { IfaceLcl $1 }
        | q_var_occ    	{ IfaceExt $1 }
	| q_dc_name	{ IfaceExt $1 }
	| lit		{ IfaceLit $1 }
	| '(' exp ')' 	{ $2 }

fexp	:: { IfaceExpr }
	: fexp aexp	{ IfaceApp $1 $2 }
	| fexp '@' aty	{ IfaceApp $1 (IfaceType $3) }
	| aexp		{ $1 }

exp	:: { IfaceExpr }
	: fexp		              { $1 }
	| '\\' bndrs '->' exp 	      { foldr IfaceLam $4 $2 }
	| '%let' let_bind '%in' exp   { IfaceLet $2 $4 }
-- gaw 2004
	| '%case' '(' ty ')' aexp '%of' id_bndr
	  '{' alts1 '}'		      { IfaceCase $5 (fst $7) $3 $9 }
        | '%cast' aexp aty { IfaceCast $2 $3 }
	| '%note' STRING exp 	   
	    { case $2 of
	       --"SCC"      -> IfaceNote (IfaceSCC "scc") $3
	       "InlineMe"   -> IfaceNote IfaceInlineMe $3
            }
        | '%external' STRING aty   { IfaceFCall (ForeignCall.CCall 
                                                    (CCallSpec (StaticTarget (mkFastString $2)) 
                                                               CCallConv (PlaySafe False))) 
                                                 $3 }

alts1	:: { [IfaceAlt] }
	: alt		{ [$1] }
	| alt ';' alts1	{ $1:$3 }

alt	:: { IfaceAlt }
	: q_dc_name bndrs '->' exp 
		{ (IfaceDataAlt $1, map ifaceBndrName $2, $4) } 
                       -- The external syntax currently includes the types of the
		       -- the args, but they aren't needed internally
                       -- Nor is the module qualifier
	| q_dc_name '->' exp 
		{ (IfaceDataAlt $1, [], $3) } 
	| lit '->' exp
		{ (IfaceLitAlt $1, [], $3) }
	| '%_' '->' exp
		{ (IfaceDefault, [], $3) }

lit	:: { Literal }
	: '(' INTEGER '::' aty ')'	{ convIntLit $2 $4 }
	| '(' RATIONAL '::' aty ')'	{ convRatLit $2 $4 }
	| '(' CHAR '::' aty ')'		{ MachChar $2 }
	| '(' STRING '::' aty ')'	{ MachStr (mkFastString $2) }

fs_var_occ	:: { FastString }
		: NAME	{ mkFastString $1 }

var_occ	:: { String }
	: NAME	{ $1 }


-- Data constructor in a pattern or data type declaration; use the dataName, 
-- because that's what we expect in Core case patterns
d_pat_occ :: { OccName }
        : CNAME      { mkOccName dataName $1 }

{

ifaceKind kc = IfaceTyConApp kc []

ifaceBndrName (IfaceIdBndr (n,_)) = n
ifaceBndrName (IfaceTvBndr (n,_)) = n

convIntLit :: Integer -> IfaceType -> Literal
convIntLit i (IfaceTyConApp tc [])
  | tc `eqTc` intPrimTyCon  = MachInt  i  
  | tc `eqTc` wordPrimTyCon = MachWord i
  | tc `eqTc` charPrimTyCon = MachChar (chr (fromInteger i))
  | tc `eqTc` addrPrimTyCon && i == 0 = MachNullAddr
convIntLit i aty
  = pprPanic "Unknown integer literal type" (ppr aty)

convRatLit :: Rational -> IfaceType -> Literal
convRatLit r (IfaceTyConApp tc [])
  | tc `eqTc` floatPrimTyCon  = MachFloat  r
  | tc `eqTc` doublePrimTyCon = MachDouble r
convRatLit i aty
  = pprPanic "Unknown rational literal type" (ppr aty)

eqTc :: IfaceTyCon -> TyCon -> Bool   -- Ugh!
eqTc (IfaceTc name) tycon = name == tyConName tycon

-- Tiresomely, we have to generate both HsTypes (in type/class decls) 
-- and IfaceTypes (in Core expressions).  So we parse them as IfaceTypes,
-- and convert to HsTypes here.  But the IfaceTypes we can see here
-- are very limited (see the productions for 'ty', so the translation
-- isn't hard
toHsType :: IfaceType -> LHsType RdrName
toHsType (IfaceTyVar v)        		 = noLoc $ HsTyVar (mkRdrUnqual (mkTyVarOcc v))
toHsType (IfaceAppTy t1 t2)    		 = noLoc $ HsAppTy (toHsType t1) (toHsType t2)
toHsType (IfaceFunTy t1 t2)    		 = noLoc $ HsFunTy (toHsType t1) (toHsType t2)
toHsType (IfaceTyConApp (IfaceTc tc) ts) = foldl mkHsAppTy (noLoc $ HsTyVar (ifaceExtRdrName tc)) (map toHsType ts) 
toHsType (IfaceForAllTy tv t)            = add_forall (toHsTvBndr tv) (toHsType t)

-- We also need to convert IfaceKinds to Kinds (now that they are different).
-- Only a limited form of kind will be encountered... hopefully
toKind :: IfaceKind -> Kind
toKind (IfaceFunTy ifK1 ifK2)  = mkArrowKind (toKind ifK1) (toKind ifK2)
toKind (IfaceTyConApp ifKc []) = mkTyConApp (toKindTc ifKc) []
toKind other                   = pprPanic "toKind" (ppr other)

toKindTc :: IfaceTyCon -> TyCon
toKindTc IfaceLiftedTypeKindTc   = liftedTypeKindTyCon
toKindTc IfaceOpenTypeKindTc     = openTypeKindTyCon
toKindTc IfaceUnliftedTypeKindTc = unliftedTypeKindTyCon
toKindTc IfaceUbxTupleKindTc     = ubxTupleKindTyCon
toKindTc IfaceArgTypeKindTc      = argTypeKindTyCon
toKindTc other                   = pprPanic "toKindTc" (ppr other)

ifaceTcType ifTc = IfaceTyConApp ifTc []

ifaceLiftedTypeKind   = ifaceTcType IfaceLiftedTypeKindTc
ifaceOpenTypeKind     = ifaceTcType IfaceOpenTypeKindTc
ifaceUnliftedTypeKind = ifaceTcType IfaceUnliftedTypeKindTc

ifaceArrow ifT1 ifT2 = IfaceFunTy ifT1 ifT2

ifaceEq ifT1 ifT2 = IfacePredTy (IfaceEqPred ifT1 ifT2)

toHsTvBndr :: IfaceTvBndr -> LHsTyVarBndr RdrName
toHsTvBndr (tv,k) = noLoc $ KindedTyVar (mkRdrUnqual (mkTyVarOcc tv)) (toKind k)

ifaceExtRdrName :: Name -> RdrName
ifaceExtRdrName name = mkOrig (nameModule name) (nameOccName name)
ifaceExtRdrName other = pprPanic "ParserCore.ifaceExtRdrName" (ppr other)

add_forall tv (L _ (HsForAllTy exp tvs cxt t))
  = noLoc $ HsForAllTy exp (tv:tvs) cxt t
add_forall tv t
  = noLoc $ HsForAllTy Explicit [tv] (noLoc []) t
  
happyError :: P a 
happyError s l = failP (show l ++ ": Parse error\n") (take 100 s) l
}

