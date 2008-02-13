%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[GHC_Stats]{Statistics for per-module compilations}

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module HscStats ( ppSourceStats ) where

#include "HsVersions.h"

import HsSyn
import Outputable
import SrcLoc		( unLoc, Located(..) )
import Char		( isSpace )
import Bag		( bagToList )
import Util             ( count )
\end{code}

%************************************************************************
%*									*
\subsection{Statistics}
%*									*
%************************************************************************

\begin{code}
ppSourceStats short (L _ (HsModule _ exports imports ldecls _ _ _ _))
 = (if short then hcat else vcat)
        (map pp_val
	       [("ExportAll        ", export_all), -- 1 if no export list
		("ExportDecls      ", export_ds),
		("ExportModules    ", export_ms),
		("Imports          ", import_no),
		("  ImpQual        ", import_qual),
		("  ImpAs          ", import_as),
		("  ImpAll         ", import_all),
		("  ImpPartial     ", import_partial),
		("  ImpHiding      ", import_hiding),
		("FixityDecls      ", fixity_sigs),
		("DefaultDecls     ", default_ds),
	      	("TypeDecls        ", type_ds),
	      	("DataDecls        ", data_ds),
	      	("NewTypeDecls     ", newt_ds),
	      	("TypeFamilyDecls  ", type_fam_ds),
	      	("FamilyInstDecls  ", fam_inst_ds),
	      	("DataConstrs      ", data_constrs),
		("DataDerivings    ", data_derivs),
	      	("ClassDecls       ", class_ds),
	      	("ClassMethods     ", class_method_ds),
	      	("DefaultMethods   ", default_method_ds),
	      	("InstDecls        ", inst_ds),
	      	("InstMethods      ", inst_method_ds),
	      	("InstType         ", inst_type_ds),
	      	("InstData         ", inst_data_ds),
	      	("TypeSigs         ", bind_tys),
	      	("ValBinds         ", val_bind_ds),
	      	("FunBinds         ", fn_bind_ds),
	      	("InlineMeths      ", method_inlines),
		("InlineBinds      ", bind_inlines),
--	      	("SpecialisedData  ", data_specs),
--	      	("SpecialisedInsts ", inst_specs),
	      	("SpecialisedMeths ", method_specs),
	      	("SpecialisedBinds ", bind_specs)
	       ])
  where
    decls = map unLoc ldecls

    pp_val (str, 0) = empty
    pp_val (str, n) 
      | not short   = hcat [text str, int n]
      | otherwise   = hcat [text (trim str), equals, int n, semi]
    
    trim ls     = takeWhile (not.isSpace) (dropWhile isSpace ls)

    (fixity_sigs, bind_tys, bind_specs, bind_inlines) 
	= count_sigs [d | SigD d <- decls]
		-- NB: this omits fixity decls on local bindings and
		-- in class decls.  ToDo

    tycl_decls  = [d | TyClD d <- decls]
    (class_ds, type_ds, data_ds, newt_ds, type_fam_ds, fam_inst_ds) = 
      countTyClDecls tycl_decls

    inst_decls  = [d | InstD d <- decls]
    inst_ds     = length inst_decls
    default_ds  = count (\ x -> case x of { DefD{} -> True; _ -> False}) decls
    val_decls   = [d | ValD d <- decls]

    real_exports = case exports of { Nothing -> []; Just es -> es }
    n_exports  	 = length real_exports
    export_ms  	 = count (\ e -> case unLoc e of { IEModuleContents{} -> True;_ -> False})
                         real_exports
    export_ds  	 = n_exports - export_ms
    export_all 	 = case exports of { Nothing -> 1; other -> 0 }

    (val_bind_ds, fn_bind_ds)
	= foldr add2 (0,0) (map count_bind val_decls)

    (import_no, import_qual, import_as, import_all, import_partial, import_hiding)
	= foldr add6 (0,0,0,0,0,0) (map import_info imports)
    (data_constrs, data_derivs)
	= foldr add2 (0,0) (map data_info tycl_decls)
    (class_method_ds, default_method_ds)
	= foldr add2 (0,0) (map class_info tycl_decls)
    (inst_method_ds, method_specs, method_inlines, inst_type_ds, inst_data_ds)
	= foldr add5 (0,0,0,0,0) (map inst_info inst_decls)

    count_bind (PatBind { pat_lhs = L _ (VarPat n) }) = (1,0)
    count_bind (PatBind {})                           = (0,1)
    count_bind (FunBind {})                           = (0,1)

    count_sigs sigs = foldr add4 (0,0,0,0) (map sig_info sigs)

    sig_info (FixSig _)		= (1,0,0,0)
    sig_info (TypeSig _ _)      = (0,1,0,0)
    sig_info (SpecSig _ _ _)    = (0,0,1,0)
    sig_info (InlineSig _ _)    = (0,0,0,1)
    sig_info _                  = (0,0,0,0)

    import_info (L _ (ImportDecl _ _ qual as spec))
	= add6 (1, qual_info qual, as_info as, 0,0,0) (spec_info spec)
    qual_info False  = 0
    qual_info True   = 1
    as_info Nothing  = 0
    as_info (Just _) = 1
    spec_info Nothing 	        = (0,0,0,1,0,0)
    spec_info (Just (False, _)) = (0,0,0,0,1,0)
    spec_info (Just (True, _))  = (0,0,0,0,0,1)

    data_info (TyData {tcdCons = cs, tcdDerivs = derivs})
	= (length cs, case derivs of Nothing -> 0
				     Just ds -> length ds)
    data_info other = (0,0)

    class_info decl@(ClassDecl {})
	= case count_sigs (map unLoc (tcdSigs decl)) of
	    (_,classops,_,_) ->
	       (classops, addpr (foldr add2 (0,0) (map (count_bind.unLoc) (bagToList (tcdMeths decl)))))
    class_info other = (0,0)

    inst_info (InstDecl _ inst_meths inst_sigs ats)
	= case count_sigs (map unLoc inst_sigs) of
	    (_,_,ss,is) ->
	      case foldr add2 (0, 0) (map (countATDecl . unLoc) ats) of
	        (tyDecl, dtDecl) ->
	          (addpr (foldr add2 (0,0) 
			   (map (count_bind.unLoc) (bagToList inst_meths))), 
                   ss, is, tyDecl, dtDecl)
        where
	  countATDecl (TyData    {}) = (0, 1)
	  countATDecl (TySynonym {}) = (1, 0)

    addpr :: (Int,Int) -> Int
    add2  :: (Int,Int) -> (Int,Int) -> (Int, Int)
    add3  :: (Int,Int,Int) -> (Int,Int,Int) -> (Int, Int, Int)
    add4  :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> (Int, Int, Int, Int)
    add5  :: (Int,Int,Int,Int,Int) -> (Int,Int,Int,Int,Int) -> (Int, Int, Int, Int, Int)
    add6  :: (Int,Int,Int,Int,Int,Int) -> (Int,Int,Int,Int,Int,Int) -> (Int, Int, Int, Int, Int, Int)

    addpr (x,y) = x+y
    add2 (x1,x2) (y1,y2) = (x1+y1,x2+y2)
    add3 (x1,x2,x3) (y1,y2,y3) = (x1+y1,x2+y2,x3+y3)
    add4 (x1,x2,x3,x4) (y1,y2,y3,y4) = (x1+y1,x2+y2,x3+y3,x4+y4)
    add5 (x1,x2,x3,x4,x5) (y1,y2,y3,y4,y5) = (x1+y1,x2+y2,x3+y3,x4+y4,x5+y5)
    add6 (x1,x2,x3,x4,x5,x6) (y1,y2,y3,y4,y5,y6) = (x1+y1,x2+y2,x3+y3,x4+y4,x5+y5,x6+y6)
\end{code}









