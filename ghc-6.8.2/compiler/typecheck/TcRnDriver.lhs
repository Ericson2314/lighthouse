%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcModule]{Typechecking a whole module}

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module TcRnDriver (
#ifdef GHCI
	tcRnStmt, tcRnExpr, tcRnType,
	tcRnLookupRdrName,
	tcRnLookupName,
	tcRnGetInfo,
	getModuleExports, 
#endif
	tcRnModule, 
	tcTopSrcDecls,
	tcRnExtCore
    ) where

#include "HsVersions.h"

import IO
#ifdef GHCI
import {-# SOURCE #-} TcSplice ( tcSpliceDecls )
#endif

import DynFlags
import StaticFlags
import HsSyn
import RdrHsSyn

import PrelNames
import RdrName
import TcHsSyn
import TcExpr
import TcRnMonad
import TcType
import Inst
import FamInst
import InstEnv
import FamInstEnv
import TcBinds
import TcDefaults
import TcEnv
import TcRules
import TcForeign
import TcInstDcls
import TcIface
import MkIface
import IfaceSyn
import TcSimplify
import TcTyClsDecls
import LoadIface
import RnNames
import RnEnv
import RnSource
import RnHsDoc
import PprCore
import CoreSyn
import ErrUtils
import Id
import Var
import Module
import UniqFM
import Name
import NameEnv
import NameSet
import TyCon
import SrcLoc
import HscTypes
import ListSetOps
import Outputable

#ifdef GHCI
import Linker
import DataCon
import TcHsType
import TcMType
import TcMatches
import TcGadt
import RnTypes
import RnExpr
import IfaceEnv
import MkId
import TysWiredIn
import IdInfo
import {- Kind parts of -} Type
import BasicTypes
import Foreign.Ptr( Ptr )
#endif

import FastString
import Maybes
import Util
import Bag

import Control.Monad    ( unless )
import Data.Maybe	( isJust )

\end{code}



%************************************************************************
%*									*
	Typecheck and rename a module
%*									*
%************************************************************************


\begin{code}
tcRnModule :: HscEnv 
	   -> HscSource
	   -> Bool 		-- True <=> save renamed syntax
	   -> Located (HsModule RdrName)
	   -> IO (Messages, Maybe TcGblEnv)

tcRnModule hsc_env hsc_src save_rn_syntax
	 (L loc (HsModule maybe_mod export_ies 
			  import_decls local_decls mod_deprec _ 
			  module_info maybe_doc))
 = do { showPass (hsc_dflags hsc_env) "Renamer/typechecker" ;

   let { this_pkg = thisPackage (hsc_dflags hsc_env) ;
	 this_mod = case maybe_mod of
			Nothing  -> mAIN	-- 'module M where' is omitted
			Just (L _ mod) -> mkModule this_pkg mod } ;
						-- The normal case
		
   initTc hsc_env hsc_src save_rn_syntax this_mod $ 
   setSrcSpan loc $
   do {		-- Deal with imports;
	tcg_env <- tcRnImports hsc_env this_mod import_decls ;
	setGblEnv tcg_env		$ do {

		-- Load the hi-boot interface for this module, if any
		-- We do this now so that the boot_names can be passed
		-- to tcTyAndClassDecls, because the boot_names are 
		-- automatically considered to be loop breakers
		--
		-- Do this *after* tcRnImports, so that we know whether
		-- a module that we import imports us; and hence whether to
		-- look for a hi-boot file
	boot_iface <- tcHiBootIface hsc_src this_mod ;

		-- Rename and type check the declarations
	traceRn (text "rn1a") ;
	tcg_env <- if isHsBoot hsc_src then
			tcRnHsBootDecls local_decls
		   else	
			tcRnSrcDecls boot_iface local_decls ;
	setGblEnv tcg_env		$ do {

		-- Report the use of any deprecated things
		-- We do this *before* processsing the export list so
		-- that we don't bleat about re-exporting a deprecated
		-- thing (especially via 'module Foo' export item)
		-- That is, only uses in the *body* of the module are complained about
	traceRn (text "rn3") ;
	failIfErrsM ;	-- finishDeprecations crashes sometimes 
			-- as a result of typechecker repairs (e.g. unboundNames)
	tcg_env <- finishDeprecations (hsc_dflags hsc_env) mod_deprec tcg_env ;

		-- Process the export list
	tcg_env <- rnExports (isJust maybe_mod) export_ies tcg_env ;
	traceRn (text "rn4") ;

	-- Compare the hi-boot iface (if any) with the real thing
	-- Must be done after processing the exports
 	tcg_env <- checkHiBootIface tcg_env boot_iface ;

	-- Make the new type env available to stuff slurped from interface files
	-- Must do this after checkHiBootIface, because the latter might add new
	-- bindings for boot_dfuns, which may be mentioned in imported unfoldings
	writeMutVar (tcg_type_env_var tcg_env) (tcg_type_env tcg_env) ;

		-- Rename the Haddock documentation 
	tcg_env <- rnHaddock module_info maybe_doc tcg_env ;

		-- Report unused names
 	reportUnusedNames export_ies tcg_env ;

		-- Dump output and return
	tcDump tcg_env ;
	return tcg_env
    }}}}
\end{code}


%************************************************************************
%*									*
		Import declarations
%*									*
%************************************************************************

\begin{code}
tcRnImports :: HscEnv -> Module -> [LImportDecl RdrName] -> TcM TcGblEnv
tcRnImports hsc_env this_mod import_decls
  = do	{ (rn_imports, rdr_env, imports,hpc_info) <- rnImports import_decls ;

	; let { dep_mods :: ModuleNameEnv (ModuleName, IsBootInterface)
	      ; dep_mods = imp_dep_mods imports

		-- We want instance declarations from all home-package
		-- modules below this one, including boot modules, except
		-- ourselves.  The 'except ourselves' is so that we don't
		-- get the instances from this module's hs-boot file
	      ; want_instances :: ModuleName -> Bool
	      ; want_instances mod = mod `elemUFM` dep_mods
				   && mod /= moduleName this_mod
	      ; (home_insts, home_fam_insts) = hptInstances hsc_env 
                                                            want_instances
	      } ;

		-- Record boot-file info in the EPS, so that it's 
		-- visible to loadHiBootInterface in tcRnSrcDecls,
		-- and any other incrementally-performed imports
	; updateEps_ (\eps -> eps { eps_is_boot = dep_mods }) ;

		-- Update the gbl env
	; updGblEnv ( \ gbl -> 
	    gbl { 
              tcg_rdr_env      = plusOccEnv (tcg_rdr_env gbl) rdr_env,
	      tcg_imports      = tcg_imports gbl `plusImportAvails` imports,
              tcg_rn_imports   = fmap (const rn_imports) (tcg_rn_imports gbl),
	      tcg_inst_env     = extendInstEnvList (tcg_inst_env gbl) home_insts,
	      tcg_fam_inst_env = extendFamInstEnvList (tcg_fam_inst_env gbl) 
                                                      home_fam_insts,
	      tcg_hpc          = hpc_info
	    }) $ do {

	; traceRn (text "rn1" <+> ppr (imp_dep_mods imports))
		-- Fail if there are any errors so far
		-- The error printing (if needed) takes advantage 
		-- of the tcg_env we have now set
-- 	; traceIf (text "rdr_env: " <+> ppr rdr_env)
	; failIfErrsM

		-- Load any orphan-module and family instance-module
		-- interfaces, so that their rules and instance decls will be
		-- found.
	; loadOrphanModules (imp_orphs  imports) False
	; loadOrphanModules (imp_finsts imports) True 

		-- Check type-familily consistency
	; traceRn (text "rn1: checking family instance consistency")
	; let { dir_imp_mods = map (\ (mod, _) -> mod) 
			     . moduleEnvElts 
			     . imp_mods 
			     $ imports }
	; checkFamInstConsistency (imp_finsts imports) dir_imp_mods ;

	; getGblEnv } }
\end{code}


%************************************************************************
%*									*
	Type-checking external-core modules
%*									*
%************************************************************************

\begin{code}
tcRnExtCore :: HscEnv 
	    -> HsExtCore RdrName
	    -> IO (Messages, Maybe ModGuts)
	-- Nothing => some error occurred 

tcRnExtCore hsc_env (HsExtCore this_mod decls src_binds)
	-- The decls are IfaceDecls; all names are original names
 = do { showPass (hsc_dflags hsc_env) "Renamer/typechecker" ;

   initTc hsc_env ExtCoreFile False this_mod $ do {

   let { ldecls  = map noLoc decls } ;

	-- Deal with the type declarations; first bring their stuff
	-- into scope, then rname them, then type check them
   tcg_env  <- importsFromLocalDecls (mkFakeGroup ldecls) ;

   setGblEnv tcg_env $ do {

   rn_decls <- checkNoErrs $ rnTyClDecls ldecls ;

	-- Dump trace of renaming part
   rnDump (ppr rn_decls) ;

	-- Typecheck them all together so that
	-- any mutually recursive types are done right
   tcg_env <- tcTyAndClassDecls emptyModDetails rn_decls ;
	-- Make the new type env available to stuff slurped from interface files

   setGblEnv tcg_env $ do {
   
	-- Now the core bindings
   core_binds <- initIfaceExtCore (tcExtCoreBindings src_binds) ;

	-- Wrap up
   let {
	bndrs 	   = bindersOfBinds core_binds ;
	my_exports = map (Avail . idName) bndrs ;
		-- ToDo: export the data types also?

	final_type_env = extendTypeEnvWithIds (tcg_type_env tcg_env) bndrs ;

	mod_guts = ModGuts {	mg_module    = this_mod,
				mg_boot	     = False,
				mg_used_names    = emptyNameSet,		-- ToDo: compute usage
				mg_dir_imps  = emptyModuleEnv,		-- ??
				mg_deps      = noDependencies,	-- ??
				mg_exports   = my_exports,
				mg_types     = final_type_env,
				mg_insts     = tcg_insts tcg_env,
				mg_fam_insts = tcg_fam_insts tcg_env,
				mg_inst_env  = tcg_inst_env tcg_env,
				mg_fam_inst_env = tcg_fam_inst_env tcg_env,
				mg_rules     = [],
				mg_binds     = core_binds,

				-- Stubs
				mg_rdr_env   = emptyGlobalRdrEnv,
				mg_fix_env   = emptyFixityEnv,
				mg_deprecs   = NoDeprecs,
				mg_foreign   = NoStubs,
				mg_hpc_info  = emptyHpcInfo False,
                                mg_modBreaks = emptyModBreaks,
                                mg_vect_info = noVectInfo
		    } } ;

   tcCoreDump mod_guts ;

   return mod_guts
   }}}}

mkFakeGroup decls -- Rather clumsy; lots of unused fields
  = emptyRdrGroup { hs_tyclds = decls }
\end{code}


%************************************************************************
%*									*
	Type-checking the top level of a module
%*									*
%************************************************************************

\begin{code}
tcRnSrcDecls :: ModDetails -> [LHsDecl RdrName] -> TcM TcGblEnv
	-- Returns the variables free in the decls
	-- Reason: solely to report unused imports and bindings
tcRnSrcDecls boot_iface decls
 = do {   	-- Do all the declarations
	(tc_envs, lie) <- getLIE $ tc_rn_src_decls boot_iface decls ;

	     -- 	Finish simplifying class constraints
	     -- 
	     -- tcSimplifyTop deals with constant or ambiguous InstIds.  
	     -- How could there be ambiguous ones?  They can only arise if a
	     -- top-level decl falls under the monomorphism restriction
	     -- and no subsequent decl instantiates its type.
	     --
	     -- We do this after checkMain, so that we use the type info 
	     -- thaat checkMain adds
	     -- 
	     -- We do it with both global and local env in scope:
	     --	 * the global env exposes the instances to tcSimplifyTop
	     --  * the local env exposes the local Ids to tcSimplifyTop, 
	     --    so that we get better error messages (monomorphism restriction)
        traceTc (text "Tc8") ;
	inst_binds <- setEnvs tc_envs (tcSimplifyTop lie) ;

	    -- Backsubstitution.  This must be done last.
	    -- Even tcSimplifyTop may do some unification.
        traceTc (text "Tc9") ;
	let { (tcg_env, _) = tc_envs
	    ; TcGblEnv { tcg_type_env = type_env, tcg_binds = binds, 
		         tcg_rules = rules, tcg_fords = fords } = tcg_env
	    ; all_binds = binds `unionBags` inst_binds } ;

	failIfErrsM ;	-- Don't zonk if there have been errors
			-- It's a waste of time; and we may get debug warnings
			-- about strangely-typed TyCons!

	(bind_ids, binds', fords', rules') <- zonkTopDecls all_binds rules fords ;

	let { final_type_env = extendTypeEnvWithIds type_env bind_ids
	    ; tcg_env' = tcg_env { tcg_type_env = final_type_env,
			 	   tcg_binds = binds',
				   tcg_rules = rules', 
				   tcg_fords = fords' } } ;

	return (tcg_env' { tcg_binds = tcg_binds tcg_env' }) 
   }

tc_rn_src_decls :: ModDetails -> [LHsDecl RdrName] -> TcM (TcGblEnv, TcLclEnv)
-- Loops around dealing with each top level inter-splice group 
-- in turn, until it's dealt with the entire module
tc_rn_src_decls boot_details ds
 = do { let { (first_group, group_tail) = findSplice ds } ;
		-- If ds is [] we get ([], Nothing)

	-- Deal with decls up to, but not including, the first splice
	(tcg_env, rn_decls) <- rnTopSrcDecls first_group ;
		-- rnTopSrcDecls fails if there are any errors

	(tcg_env, tcl_env) <- setGblEnv tcg_env $ 
			      tcTopSrcDecls boot_details rn_decls ;

	-- If there is no splice, we're nearly done
	setEnvs (tcg_env, tcl_env) $ 
	case group_tail of {
	   Nothing -> do { tcg_env <- checkMain ;	-- Check for `main'
			   return (tcg_env, tcl_env) 
		      } ;

	-- If there's a splice, we must carry on
	   Just (SpliceDecl splice_expr, rest_ds) -> do {
#ifndef GHCI
	failWithTc (text "Can't do a top-level splice; need a bootstrapped compiler")
#else

	-- Rename the splice expression, and get its supporting decls
	(rn_splice_expr, splice_fvs) <- checkNoErrs (rnLExpr splice_expr) ;
		-- checkNoErrs: don't typecheck if renaming failed
	rnDump (ppr rn_splice_expr) ;

	-- Execute the splice
	spliced_decls <- tcSpliceDecls rn_splice_expr ;

	-- Glue them on the front of the remaining decls and loop
	setGblEnv (tcg_env `addTcgDUs` usesOnly splice_fvs) $
	tc_rn_src_decls boot_details (spliced_decls ++ rest_ds)
#endif /* GHCI */
    } } }
\end{code}

%************************************************************************
%*									*
	Compiling hs-boot source files, and
	comparing the hi-boot interface with the real thing
%*									*
%************************************************************************

\begin{code}
tcRnHsBootDecls :: [LHsDecl RdrName] -> TcM TcGblEnv
tcRnHsBootDecls decls
   = do { let { (first_group, group_tail) = findSplice decls }

	; case group_tail of
	     Just stuff -> spliceInHsBootErr stuff
	     Nothing    -> return ()

		-- Rename the declarations
	; (tcg_env, rn_group) <- rnTopSrcDecls first_group
	; setGblEnv tcg_env $ do {

	-- Todo: check no foreign decls, no rules, no default decls

		-- Typecheck type/class decls
	; traceTc (text "Tc2")
	; let tycl_decls = hs_tyclds rn_group
	; tcg_env <- tcTyAndClassDecls emptyModDetails tycl_decls
	; setGblEnv tcg_env	$ do {

		-- Typecheck instance decls
	; traceTc (text "Tc3")
	; (tcg_env, inst_infos, _deriv_binds) 
            <- tcInstDecls1 tycl_decls (hs_instds rn_group) (hs_derivds rn_group)
	; setGblEnv tcg_env	$ do {

		-- Typecheck value declarations
	; traceTc (text "Tc5") 
	; val_ids <- tcHsBootSigs (hs_valds rn_group)

		-- Wrap up
		-- No simplification or zonking to do
	; traceTc (text "Tc7a")
	; gbl_env <- getGblEnv 
	
		-- Make the final type-env
		-- Include the dfun_ids so that their type sigs
		-- are written into the interface file
	; let { type_env0 = tcg_type_env gbl_env
	      ; type_env1 = extendTypeEnvWithIds type_env0 val_ids
	      ; type_env2 = extendTypeEnvWithIds type_env1 dfun_ids 
	      ; dfun_ids = map iDFunId inst_infos }
	; return (gbl_env { tcg_type_env = type_env2 }) 
   }}}}

spliceInHsBootErr (SpliceDecl (L loc _), _)
  = addErrAt loc (ptext SLIT("Splices are not allowed in hs-boot files"))
\end{code}

Once we've typechecked the body of the module, we want to compare what
we've found (gathered in a TypeEnv) with the hi-boot details (if any).

\begin{code}
checkHiBootIface :: TcGblEnv -> ModDetails -> TcM TcGblEnv
-- Compare the hi-boot file for this module (if there is one)
-- with the type environment we've just come up with
-- In the common case where there is no hi-boot file, the list
-- of boot_names is empty.
--
-- The bindings we return give bindings for the dfuns defined in the
-- hs-boot file, such as 	$fbEqT = $fEqT

checkHiBootIface
	tcg_env@(TcGblEnv { tcg_src = hs_src, tcg_binds = binds,
			    tcg_insts = local_insts, tcg_fam_insts = local_fam_insts,
			    tcg_type_env = local_type_env, tcg_exports = local_exports })
	(ModDetails { md_insts = boot_insts, md_fam_insts = boot_fam_insts,
		      md_types = boot_type_env, md_exports = boot_exports })
  | isHsBoot hs_src	-- Current module is already a hs-boot file!
  = return tcg_env	

  | otherwise
  = do	{ traceTc (text "checkHiBootIface" <+> (ppr boot_type_env $$ ppr boot_insts $$ 
				ppr boot_exports)) ;

		-- Check the exports of the boot module, one by one
	; mapM_ check_export boot_exports

		-- Check instance declarations
	; mb_dfun_prs <- mapM check_inst boot_insts
	; let tcg_env' = tcg_env { tcg_binds    = binds `unionBags` dfun_binds,
				   tcg_type_env = extendTypeEnvWithIds local_type_env boot_dfuns }
	      dfun_prs   = catMaybes mb_dfun_prs
	      boot_dfuns = map fst dfun_prs
	      dfun_binds = listToBag [ noLoc $ VarBind boot_dfun (nlHsVar dfun)
				     | (boot_dfun, dfun) <- dfun_prs ]

		-- Check for no family instances
	; unless (null boot_fam_insts) $
	    panic ("TcRnDriver.checkHiBootIface: Cannot handle family " ++
		   "instances in boot files yet...")
            -- FIXME: Why?  The actual comparison is not hard, but what would
            --	      be the equivalent to the dfun bindings returned for class
            --	      instances?  We can't easily equate tycons...

	; return tcg_env' }
  where
    check_export boot_avail	-- boot_avail is exported by the boot iface
      | name `elem` dfun_names = return ()	
      | isWiredInName name     = return ()	-- No checking for wired-in names.  In particular,
						-- 'error' is handled by a rather gross hack
						-- (see comments in GHC.Err.hs-boot)

	-- Check that the actual module exports the same thing
      | not (null missing_names)
      = addErrTc (missingBootThing (head missing_names) "exported by")

	-- If the boot module does not *define* the thing, we are done
	-- (it simply re-exports it, and names match, so nothing further to do)
      | isNothing mb_boot_thing = return ()

	-- Check that the actual module also defines the thing, and 
	-- then compare the definitions
      | Just real_thing <- lookupTypeEnv local_type_env name
      = do { let boot_decl = tyThingToIfaceDecl (fromJust mb_boot_thing)
	         real_decl = tyThingToIfaceDecl real_thing
	   ; checkTc (checkBootDecl boot_decl real_decl)
		     (bootMisMatch real_thing boot_decl real_decl) }
		-- The easiest way to check compatibility is to convert to
		-- iface syntax, where we already have good comparison functions

      | otherwise
      = addErrTc (missingBootThing name "defined in")
      where
	name          = availName boot_avail
	mb_boot_thing = lookupTypeEnv boot_type_env name
	missing_names = case lookupNameEnv local_export_env name of
			  Nothing    -> [name]
			  Just avail -> availNames boot_avail `minusList` availNames avail
		 
    dfun_names = map getName boot_insts

    local_export_env :: NameEnv AvailInfo
    local_export_env = availsToNameEnv local_exports

    check_inst :: Instance -> TcM (Maybe (Id, Id))
	-- Returns a pair of the boot dfun in terms of the equivalent real dfun
    check_inst boot_inst
	= case [dfun | inst <- local_insts, 
		       let dfun = instanceDFunId inst,
		       idType dfun `tcEqType` boot_inst_ty ] of
	    [] -> do { addErrTc (instMisMatch boot_inst); return Nothing }
	    (dfun:_) -> return (Just (local_boot_dfun, dfun))
	where
	  boot_dfun = instanceDFunId boot_inst
	  boot_inst_ty = idType boot_dfun
	  local_boot_dfun = Id.mkExportedLocalId (idName boot_dfun) boot_inst_ty


----------------
missingBootThing thing what
  = ppr thing <+> ptext SLIT("is exported by the hs-boot file, but not") 
	      <+> text what <+> ptext SLIT("the module")

bootMisMatch thing boot_decl real_decl
  = vcat [ppr thing <+> ptext SLIT("has conflicting definitions in the module and its hs-boot file"),
	  ptext SLIT("Main module:") <+> ppr real_decl,
	  ptext SLIT("Boot file:  ") <+> ppr boot_decl]

instMisMatch inst
  = hang (ppr inst)
       2 (ptext SLIT("is defined in the hs-boot file, but not in the module itself"))
\end{code}


%************************************************************************
%*									*
	Type-checking the top level of a module
%*									*
%************************************************************************

tcRnGroup takes a bunch of top-level source-code declarations, and
 * renames them
 * gets supporting declarations from interface files
 * typechecks them
 * zonks them
 * and augments the TcGblEnv with the results

In Template Haskell it may be called repeatedly for each group of
declarations.  It expects there to be an incoming TcGblEnv in the
monad; it augments it and returns the new TcGblEnv.

\begin{code}
------------------------------------------------
rnTopSrcDecls :: HsGroup RdrName -> TcM (TcGblEnv, HsGroup Name)
rnTopSrcDecls group
 = do { 	-- Bring top level binders into scope
	tcg_env <- importsFromLocalDecls group ;
	setGblEnv tcg_env $ do {

	failIfErrsM ;	-- No point in continuing if (say) we have duplicate declarations

		-- Rename the source decls
	(tcg_env, rn_decls) <- checkNoErrs (rnSrcDecls group) ;

		-- save the renamed syntax, if we want it
	let { tcg_env'
	        | Just grp <- tcg_rn_decls tcg_env
	          = tcg_env{ tcg_rn_decls = Just (appendGroups grp rn_decls) }
	        | otherwise
	           = tcg_env };

		-- Dump trace of renaming part
	rnDump (ppr rn_decls) ;

	return (tcg_env', rn_decls)
   }}

------------------------------------------------
tcTopSrcDecls :: ModDetails -> HsGroup Name -> TcM (TcGblEnv, TcLclEnv)
tcTopSrcDecls boot_details
	(HsGroup { hs_tyclds = tycl_decls, 
		   hs_instds = inst_decls,
                   hs_derivds = deriv_decls,
		   hs_fords  = foreign_decls,
		   hs_defds  = default_decls,
		   hs_ruleds = rule_decls,
		   hs_valds  = val_binds })
 = do {		-- Type-check the type and class decls, and all imported decls
		-- The latter come in via tycl_decls
        traceTc (text "Tc2") ;

	tcg_env <- tcTyAndClassDecls boot_details tycl_decls ;
		-- If there are any errors, tcTyAndClassDecls fails here
	
	-- Make these type and class decls available to stuff slurped from interface files
	writeMutVar (tcg_type_env_var tcg_env) (tcg_type_env tcg_env) ;


	setGblEnv tcg_env	$ do {
		-- Source-language instances, including derivings,
		-- and import the supporting declarations
        traceTc (text "Tc3") ;
	(tcg_env, inst_infos, deriv_binds) 
            <- tcInstDecls1 tycl_decls inst_decls deriv_decls;
	setGblEnv tcg_env	$ do {

	        -- Foreign import declarations next.  No zonking necessary
		-- here; we can tuck them straight into the global environment.
        traceTc (text "Tc4") ;
	(fi_ids, fi_decls) <- tcForeignImports foreign_decls ;
	tcExtendGlobalValEnv fi_ids	$ do {

		-- Default declarations
        traceTc (text "Tc4a") ;
	default_tys <- tcDefaults default_decls ;
	updGblEnv (\gbl -> gbl { tcg_default = default_tys }) $ do {
	
		-- Value declarations next
		-- We also typecheck any extra binds that came out 
		-- of the "deriving" process (deriv_binds)
        traceTc (text "Tc5") ;
	(tc_val_binds,   tcl_env) <- tcTopBinds val_binds ;
	setLclTypeEnv tcl_env 	$ do {

		-- Now GHC-generated derived bindings and generics.
		-- Do not generate warnings from compiler-generated code.
	(tc_deriv_binds, tcl_env) <- discardWarnings $
                                 tcTopBinds deriv_binds ;

	     	-- Second pass over class and instance declarations, 
        traceTc (text "Tc6") ;
	(inst_binds, tcl_env)     <- setLclTypeEnv tcl_env $ tcInstDecls2 tycl_decls inst_infos ;
	showLIE (text "after instDecls2") ;

		-- Foreign exports
		-- They need to be zonked, so we return them
        traceTc (text "Tc7") ;
	(foe_binds, foe_decls) <- tcForeignExports foreign_decls ;

		-- Rules
	rules <- tcRules rule_decls ;

		-- Wrap up
        traceTc (text "Tc7a") ;
	tcg_env <- getGblEnv ;
	let { all_binds = tc_val_binds	 `unionBags`
			  tc_deriv_binds `unionBags`
			  inst_binds	 `unionBags`
			  foe_binds  ;

		-- Extend the GblEnv with the (as yet un-zonked) 
		-- bindings, rules, foreign decls
	      tcg_env' = tcg_env {  tcg_binds = tcg_binds tcg_env `unionBags` all_binds,
				    tcg_rules = tcg_rules tcg_env ++ rules,
				    tcg_fords = tcg_fords tcg_env ++ foe_decls ++ fi_decls } } ;
  	return (tcg_env', tcl_env)
    }}}}}}
\end{code}


%************************************************************************
%*									*
	Checking for 'main'
%*									*
%************************************************************************

\begin{code}
checkMain :: TcM TcGblEnv
-- If we are in module Main, check that 'main' is defined.
checkMain 
  = do { tcg_env   <- getGblEnv ;
	 dflags    <- getDOpts ;
	 check_main dflags tcg_env
    }

check_main dflags tcg_env
 | mod /= main_mod
 = traceTc (text "checkMain not" <+> ppr main_mod <+> ppr mod) >>
   return tcg_env

 | otherwise
 = do	{ mb_main <- lookupSrcOcc_maybe main_fn
		-- Check that 'main' is in scope
		-- It might be imported from another module!
	; case mb_main of {
	     Nothing -> do { traceTc (text "checkMain fail" <+> ppr main_mod <+> ppr main_fn)
			   ; complain_no_main	
			   ; return tcg_env } ;
	     Just main_name -> do

	{ traceTc (text "checkMain found" <+> ppr main_mod <+> ppr main_fn)
	; let { rhs = nlHsApp (nlHsVar runMainIOName) (nlHsVar main_name) }
		   	-- :Main.main :: IO () = runMainIO main 

	; (main_expr, ty) <- addErrCtxt mainCtxt			$
   			     setSrcSpan (srcLocSpan (getSrcLoc main_name)) $
			     tcInferRho rhs

		-- See Note [Root-main Id]
	; let { root_main_name =  mkExternalName rootMainKey rOOT_MAIN 
				   (mkVarOccFS FSLIT("main")) 
				   (getSrcSpan main_name)
	      ; root_main_id = Id.mkExportedLocalId root_main_name ty
	      ; main_bind    = noLoc (VarBind root_main_id main_expr) }

	; return (tcg_env { tcg_binds = tcg_binds tcg_env 
					`snocBag` main_bind,
			    tcg_dus   = tcg_dus tcg_env
				        `plusDU` usesOnly (unitFV main_name)
			-- Record the use of 'main', so that we don't 
			-- complain about it being defined but not used
		 }) 
    }}}
  where
    mod 	 = tcg_mod tcg_env
    main_mod     = mainModIs dflags
    main_is_flag = mainFunIs dflags

    main_fn  = case main_is_flag of
		  Just fn -> mkRdrUnqual (mkVarOccFS (mkFastString fn))
		  Nothing -> main_RDR_Unqual

    complain_no_main | ghcLink dflags == LinkInMemory = return ()
		     | otherwise = failWithTc noMainMsg
	-- In interactive mode, don't worry about the absence of 'main'
	-- In other modes, fail altogether, so that we don't go on
	-- and complain a second time when processing the export list.

    mainCtxt  = ptext SLIT("When checking the type of the") <+> pp_main_fn
    noMainMsg = ptext SLIT("The") <+> pp_main_fn
		<+> ptext SLIT("is not defined in module") <+> quotes (ppr main_mod)
    pp_main_fn | isJust main_is_flag = ptext SLIT("main function") <+> quotes (ppr main_fn)
	       | otherwise 	     = ptext SLIT("function") <+> quotes (ppr main_fn)
\end{code}

Note [Root-main Id]
~~~~~~~~~~~~~~~~~~~
The function that the RTS invokes is always :Main.main, which we call
root_main_id.  (Because GHC allows the user to have a module not
called Main as the main module, we can't rely on the main function
being called "Main.main".  That's why root_main_id has a fixed module
":Main".)  

This is unusual: it's a LocalId whose Name has a Module from another
module.  Tiresomely, we must filter it out again in MkIface, les we
get two defns for 'main' in the interface file!


%*********************************************************
%*						 	 *
		GHCi stuff
%*							 *
%*********************************************************

\begin{code}
#ifdef GHCI
setInteractiveContext :: HscEnv -> InteractiveContext -> TcRn a -> TcRn a
setInteractiveContext hsc_env icxt thing_inside 
  = let -- Initialise the tcg_inst_env with instances from all home modules.  
        -- This mimics the more selective call to hptInstances in tcRnModule.
	(home_insts, home_fam_insts) = hptInstances hsc_env (\mod -> True)
    in
    updGblEnv (\env -> env { 
	tcg_rdr_env      = ic_rn_gbl_env icxt,
	tcg_inst_env     = extendInstEnvList    (tcg_inst_env env) home_insts,
	tcg_fam_inst_env = extendFamInstEnvList (tcg_fam_inst_env env) 
                                                home_fam_insts 
      }) $

    tcExtendGhciEnv (ic_tmp_ids icxt) $
        -- tcExtendGhciEnv does lots: 
        --   - it extends the local type env (tcl_env) with the given Ids,
        --   - it extends the local rdr env (tcl_rdr) with the Names from 
        --     the given Ids
        --   - it adds the free tyvars of the Ids to the tcl_tyvars
        --     set.
        --
        -- later ids in ic_tmp_ids must shadow earlier ones with the same
        -- OccName, and tcExtendIdEnv implements this behaviour.

    do	{ traceTc (text "setIC" <+> ppr (ic_tmp_ids icxt))
 	; thing_inside }
\end{code}


\begin{code}
tcRnStmt :: HscEnv
	 -> InteractiveContext
	 -> LStmt RdrName
	 -> IO (Maybe ([Id], LHsExpr Id))
		-- The returned [Id] is the list of new Ids bound by
                -- this statement.  It can be used to extend the
                -- InteractiveContext via extendInteractiveContext.
		--
		-- The returned TypecheckedHsExpr is of type IO [ () ],
		-- a list of the bound values, coerced to ().

tcRnStmt hsc_env ictxt rdr_stmt
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    setInteractiveContext hsc_env ictxt $ do {

    -- Rename; use CmdLineMode because tcRnStmt is only used interactively
    (([rn_stmt], _), fvs) <- rnStmts DoExpr [rdr_stmt] (return ((), emptyFVs)) ;
    traceRn (text "tcRnStmt" <+> vcat [ppr rdr_stmt, ppr rn_stmt, ppr fvs]) ;
    failIfErrsM ;
    rnDump (ppr rn_stmt) ;
    
    -- The real work is done here
    (bound_ids, tc_expr) <- mkPlan rn_stmt ;
    zonked_expr <- zonkTopLExpr tc_expr ;
    zonked_ids  <- zonkTopBndrs bound_ids ;
    
	-- None of the Ids should be of unboxed type, because we
	-- cast them all to HValues in the end!
    mappM bad_unboxed (filter (isUnLiftedType . idType) zonked_ids) ;

    traceTc (text "tcs 1") ;
    let { global_ids = map globaliseAndTidy zonked_ids } ;
    
{- ---------------------------------------------
   At one stage I removed any shadowed bindings from the type_env;
   they are inaccessible but might, I suppose, cause a space leak if we leave them there.
   However, with Template Haskell they aren't necessarily inaccessible.  Consider this
   GHCi session
	 Prelude> let f n = n * 2 :: Int
	 Prelude> fName <- runQ [| f |]
	 Prelude> $(return $ AppE fName (LitE (IntegerL 7)))
	 14
	 Prelude> let f n = n * 3 :: Int
	 Prelude> $(return $ AppE fName (LitE (IntegerL 7)))
   In the last line we use 'fName', which resolves to the *first* 'f'
   in scope. If we delete it from the type env, GHCi crashes because
   it doesn't expect that.
 
   Hence this code is commented out

-------------------------------------------------- -}

    dumpOptTcRn Opt_D_dump_tc 
    	(vcat [text "Bound Ids" <+> pprWithCommas ppr global_ids,
    	       text "Typechecked expr" <+> ppr zonked_expr]) ;

    returnM (global_ids, zonked_expr)
    }
  where
    bad_unboxed id = addErr (sep [ptext SLIT("GHCi can't bind a variable of unlifted type:"),
				  nest 2 (ppr id <+> dcolon <+> ppr (idType id))])

globaliseAndTidy :: Id -> Id
globaliseAndTidy id	-- Note [Interactively-bound Ids in GHCi]
  = Id.setIdType (globaliseId VanillaGlobal id) tidy_type
  where
    tidy_type = tidyTopType (idType id)
\end{code}

Note [Interactively-bound Ids in GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Ids bound by previous Stmts in Template Haskell are currently
	a) GlobalIds
	b) with an Internal Name (not External)
	c) and a tidied type

 (a) They must be GlobalIds (not LocalIds) otherwise when we come to
     compile an expression using these ids later, the byte code
     generator will consider the occurrences to be free rather than
     global.

 (b) They retain their Internal names becuase we don't have a suitable
     Module to name them with.  We could revisit this choice.

 (c) Their types are tidied.  This is important, because :info may ask
     to look at them, and :info expects the things it looks up to have
     tidy types
	

--------------------------------------------------------------------------
		Typechecking Stmts in GHCi

Here is the grand plan, implemented in tcUserStmt

	What you type			The IO [HValue] that hscStmt returns
	-------------			------------------------------------
	let pat = expr		==> 	let pat = expr in return [coerce HVal x, coerce HVal y, ...]
					bindings: [x,y,...]

	pat <- expr		==> 	expr >>= \ pat -> return [coerce HVal x, coerce HVal y, ...]
					bindings: [x,y,...]

	expr (of IO type)	==>	expr >>= \ it -> return [coerce HVal it]
	  [NB: result not printed]	bindings: [it]
	  
	expr (of non-IO type,	==>	let it = expr in print it >> return [coerce HVal it]
	  result showable)		bindings: [it]

	expr (of non-IO type, 
	  result not showable)	==>	error


\begin{code}
---------------------------
type PlanResult = ([Id], LHsExpr Id)
type Plan = TcM PlanResult

runPlans :: [Plan] -> TcM PlanResult
-- Try the plans in order.  If one fails (by raising an exn), try the next.
-- If one succeeds, take it.
runPlans []     = panic "runPlans"
runPlans [p]    = p
runPlans (p:ps) = tryTcLIE_ (runPlans ps) p

--------------------
mkPlan :: LStmt Name -> TcM PlanResult
mkPlan (L loc (ExprStmt expr _ _))	-- An expression typed at the prompt 
  = do	{ uniq <- newUnique		-- is treated very specially
	; let fresh_it  = itName uniq
	      the_bind  = L loc $ mkFunBind (L loc fresh_it) matches
	      matches   = [mkMatch [] expr emptyLocalBinds]
	      let_stmt  = L loc $ LetStmt (HsValBinds (ValBindsOut [(NonRecursive,unitBag the_bind)] []))
	      bind_stmt = L loc $ BindStmt (nlVarPat fresh_it) expr
					   (HsVar bindIOName) noSyntaxExpr 
	      print_it  = L loc $ ExprStmt (nlHsApp (nlHsVar printName) (nlHsVar fresh_it))
			          	   (HsVar thenIOName) placeHolderType

	-- The plans are:
	--	[it <- e; print it]	but not if it::()
	--	[it <- e]		
	--	[let it = e; print it]	
	; runPlans [	-- Plan A
		    do { stuff@([it_id], _) <- tcGhciStmts [bind_stmt, print_it]
		       ; it_ty <- zonkTcType (idType it_id)
		       ; ifM (isUnitTy it_ty) failM
		       ; return stuff },

			-- Plan B; a naked bind statment
		    tcGhciStmts [bind_stmt],	

			-- Plan C; check that the let-binding is typeable all by itself.
			-- If not, fail; if so, try to print it.
			-- The two-step process avoids getting two errors: one from
			-- the expression itself, and one from the 'print it' part
			-- This two-step story is very clunky, alas
		    do { checkNoErrs (tcGhciStmts [let_stmt]) 
				--- checkNoErrs defeats the error recovery of let-bindings
		       ; tcGhciStmts [let_stmt, print_it] }
	  ]}

mkPlan stmt@(L loc (BindStmt {}))
  | [L _ v] <- collectLStmtBinders stmt		-- One binder, for a bind stmt 
  = do	{ let print_v  = L loc $ ExprStmt (nlHsApp (nlHsVar printName) (nlHsVar v))
			          	   (HsVar thenIOName) placeHolderType

	; print_bind_result <- doptM Opt_PrintBindResult
	; let print_plan = do
		  { stuff@([v_id], _) <- tcGhciStmts [stmt, print_v]
		  ; v_ty <- zonkTcType (idType v_id)
		  ; ifM (isUnitTy v_ty || not (isTauTy v_ty)) failM
		  ; return stuff }

	-- The plans are:
	--	[stmt; print v]		but not if v::()
	--	[stmt]
	; runPlans ((if print_bind_result then [print_plan] else []) ++
		    [tcGhciStmts [stmt]])
	}

mkPlan stmt
  = tcGhciStmts [stmt]

---------------------------
tcGhciStmts :: [LStmt Name] -> TcM PlanResult
tcGhciStmts stmts
 = do { ioTyCon <- tcLookupTyCon ioTyConName ;
	ret_id  <- tcLookupId returnIOName ;		-- return @ IO
	let {
	    io_ty     = mkTyConApp ioTyCon [] ;
	    ret_ty    = mkListTy unitTy ;
	    io_ret_ty = mkTyConApp ioTyCon [ret_ty] ;
	    tc_io_stmts stmts = tcStmts DoExpr (tcDoStmt io_ty) stmts 
				        (emptyRefinement, io_ret_ty) ;

	    names = map unLoc (collectLStmtsBinders stmts) ;

		-- mk_return builds the expression
		--	returnIO @ [()] [coerce () x, ..,  coerce () z]
		--
		-- Despite the inconvenience of building the type applications etc,
		-- this *has* to be done in type-annotated post-typecheck form
		-- because we are going to return a list of *polymorphic* values
		-- coerced to type (). If we built a *source* stmt
		--	return [coerce x, ..., coerce z]
		-- then the type checker would instantiate x..z, and we wouldn't
		-- get their *polymorphic* values.  (And we'd get ambiguity errs
		-- if they were overloaded, since they aren't applied to anything.)
	    mk_return ids = nlHsApp (nlHsTyApp ret_id [ret_ty]) 
			 	    (noLoc $ ExplicitList unitTy (map mk_item ids)) ;
	    mk_item id = nlHsApp (nlHsTyApp unsafeCoerceId [idType id, unitTy])
		    	         (nlHsVar id) 
	 } ;

	-- OK, we're ready to typecheck the stmts
	traceTc (text "TcRnDriver.tcGhciStmts: tc stmts") ;
	((tc_stmts, ids), lie) <- getLIE $ tc_io_stmts stmts $ \ _ ->
					   mappM tcLookupId names ;
					-- Look up the names right in the middle,
					-- where they will all be in scope

	-- Simplify the context
	traceTc (text "TcRnDriver.tcGhciStmts: simplify ctxt") ;
	const_binds <- checkNoErrs (tcSimplifyInteractive lie) ;
		-- checkNoErrs ensures that the plan fails if context redn fails

	traceTc (text "TcRnDriver.tcGhciStmts: done") ;
	return (ids, mkHsDictLet const_binds $
		     noLoc (HsDo DoExpr tc_stmts (mk_return ids) io_ret_ty))
    }
\end{code}


tcRnExpr just finds the type of an expression

\begin{code}
tcRnExpr :: HscEnv
	 -> InteractiveContext
	 -> LHsExpr RdrName
	 -> IO (Maybe Type)
tcRnExpr hsc_env ictxt rdr_expr
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    setInteractiveContext hsc_env ictxt $ do {

    (rn_expr, fvs) <- rnLExpr rdr_expr ;
    failIfErrsM ;

	-- Now typecheck the expression; 
	-- it might have a rank-2 type (e.g. :t runST)
    ((tc_expr, res_ty), lie)	   <- getLIE (tcInferRho rn_expr) ;
    ((qtvs, dict_insts, _), lie_top) <- getLIE (tcSimplifyInfer smpl_doc (tyVarsOfType res_ty) lie)  ;
    tcSimplifyInteractive lie_top ;

    let { all_expr_ty = mkForAllTys qtvs $
    		        mkFunTys (map (idType . instToId) dict_insts)	$
    		        res_ty } ;
    zonkTcType all_expr_ty
    }
  where
    smpl_doc = ptext SLIT("main expression")
\end{code}

tcRnType just finds the kind of a type

\begin{code}
tcRnType :: HscEnv
	 -> InteractiveContext
	 -> LHsType RdrName
	 -> IO (Maybe Kind)
tcRnType hsc_env ictxt rdr_type
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    setInteractiveContext hsc_env ictxt $ do {

    rn_type <- rnLHsType doc rdr_type ;
    failIfErrsM ;

	-- Now kind-check the type
    (ty', kind) <- kcHsType rn_type ;
    return kind
    }
  where
    doc = ptext SLIT("In GHCi input")

#endif /* GHCi */
\end{code}


%************************************************************************
%*									*
	More GHCi stuff, to do with browsing and getting info
%*									*
%************************************************************************

\begin{code}
#ifdef GHCI
-- ASSUMES that the module is either in the HomePackageTable or is
-- a package module with an interface on disk.  If neither of these is
-- true, then the result will be an error indicating the interface
-- could not be found.
getModuleExports :: HscEnv -> Module -> IO (Messages, Maybe [AvailInfo])
getModuleExports hsc_env mod
  = let
      ic        = hsc_IC hsc_env
      checkMods = ic_toplev_scope ic ++ ic_exports ic
    in
    initTc hsc_env HsSrcFile False iNTERACTIVE (tcGetModuleExports mod checkMods)

-- Get the export avail info and also load all orphan and family-instance
-- modules.  Finally, check that the family instances of all modules in the
-- interactive context are consistent (these modules are in the second
-- argument).
tcGetModuleExports :: Module -> [Module] -> TcM [AvailInfo]
tcGetModuleExports mod directlyImpMods
  = do { let doc = ptext SLIT("context for compiling statements")
       ; iface <- initIfaceTcRn $ loadSysInterface doc mod

  		-- Load any orphan-module and family instance-module
  		-- interfaces, so their instances are visible.
       ; loadOrphanModules (dep_orphs (mi_deps iface)) False 
       ; loadOrphanModules (dep_finsts (mi_deps iface)) True

                -- Check that the family instances of all directly loaded
                -- modules are consistent.
       ; checkFamInstConsistency (dep_finsts (mi_deps iface)) directlyImpMods

       ; ifaceExportNames (mi_exports iface)
       }

tcRnLookupRdrName :: HscEnv -> RdrName -> IO (Maybe [Name])
tcRnLookupRdrName hsc_env rdr_name 
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    setInteractiveContext hsc_env (hsc_IC hsc_env) $ 
    lookup_rdr_name rdr_name

lookup_rdr_name rdr_name = do {
	-- If the identifier is a constructor (begins with an
	-- upper-case letter), then we need to consider both
	-- constructor and type class identifiers.
    let { rdr_names = dataTcOccs rdr_name } ;

	-- results :: [Either Messages Name]
    results <- mapM (tryTcErrs . lookupOccRn) rdr_names ;

    traceRn (text "xx" <+> vcat [ppr rdr_names, ppr (map snd results)]);
	-- The successful lookups will be (Just name)
    let { (warns_s, good_names) = unzip [ (msgs, name) 
					| (msgs, Just name) <- results] ;
	  errs_s = [msgs | (msgs, Nothing) <- results] } ;

	-- Fail if nothing good happened, else add warnings
    if null good_names then
		-- No lookup succeeded, so
		-- pick the first error message and report it
		-- ToDo: If one of the errors is "could be Foo.X or Baz.X",
		--	 while the other is "X is not in scope", 
		--	 we definitely want the former; but we might pick the latter
	do { addMessages (head errs_s) ; failM }
      else 			-- Add deprecation warnings
	mapM_ addMessages warns_s ;
    
    return good_names
 }

tcRnLookupName :: HscEnv -> Name -> IO (Maybe TyThing)
tcRnLookupName hsc_env name
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    setInteractiveContext hsc_env (hsc_IC hsc_env) $
    tcRnLookupName' name

-- To look up a name we have to look in the local environment (tcl_lcl)
-- as well as the global environment, which is what tcLookup does. 
-- But we also want a TyThing, so we have to convert:

tcRnLookupName' :: Name -> TcRn TyThing
tcRnLookupName' name = do
   tcthing <- tcLookup name
   case tcthing of
     AGlobal thing    -> return thing
     ATcId{tct_id=id} -> return (AnId id)
     _ -> panic "tcRnLookupName'"

tcRnGetInfo :: HscEnv
	    -> Name
	    -> IO (Maybe (TyThing, Fixity, [Instance]))

-- Used to implemnent :info in GHCi
--
-- Look up a RdrName and return all the TyThings it might be
-- A capitalised RdrName is given to us in the DataName namespace,
-- but we want to treat it as *both* a data constructor 
--  *and* as a type or class constructor; 
-- hence the call to dataTcOccs, and we return up to two results
tcRnGetInfo hsc_env name
  = initTcPrintErrors hsc_env iNTERACTIVE $ 
    let ictxt = hsc_IC hsc_env in
    setInteractiveContext hsc_env ictxt $ do

	-- Load the interface for all unqualified types and classes
	-- That way we will find all the instance declarations
	-- (Packages have not orphan modules, and we assume that
	--  in the home package all relevant modules are loaded.)
    loadUnqualIfaces ictxt

    thing  <- tcRnLookupName' name
    fixity <- lookupFixityRn name
    ispecs <- lookupInsts thing
    return (thing, fixity, ispecs)

lookupInsts :: TyThing -> TcM [Instance]
lookupInsts (AClass cls)
  = do	{ inst_envs <- tcGetInstEnvs
	; return (classInstances inst_envs cls) }

lookupInsts (ATyCon tc)
  = do 	{ eps <- getEps	-- Load all instances for all classes that are
			-- in the type environment (which are all the ones
			-- we've seen in any interface file so far)
	; (pkg_ie, home_ie) <- tcGetInstEnvs	-- Search all
	; return [ ispec
		 | ispec <- instEnvElts home_ie ++ instEnvElts pkg_ie
		 , let dfun = instanceDFunId ispec
		 , relevant dfun ] } 
  where
    relevant df = tc_name `elemNameSet` tyClsNamesOfDFunHead (idType df)
    tc_name     = tyConName tc		  

lookupInsts other = return []

loadUnqualIfaces :: InteractiveContext -> TcM ()
-- Load the home module for everything that is in scope unqualified
-- This is so that we can accurately report the instances for 
-- something
loadUnqualIfaces ictxt
  = initIfaceTcRn $
    mapM_ (loadSysInterface doc) (moduleSetElts (mkModuleSet unqual_mods))
  where
    unqual_mods = [ nameModule name
		  | gre <- globalRdrEnvElts (ic_rn_gbl_env ictxt),
		    let name = gre_name gre,
                    not (isInternalName name),
		    isTcOcc (nameOccName name),  -- Types and classes only
		    unQualOK gre ]		 -- In scope unqualified
    doc = ptext SLIT("Need interface for module whose export(s) are in scope unqualified")
#endif /* GHCI */
\end{code}

%************************************************************************
%*									*
		Degugging output
%*									*
%************************************************************************

\begin{code}
rnDump :: SDoc -> TcRn ()
-- Dump, with a banner, if -ddump-rn
rnDump doc = do { dumpOptTcRn Opt_D_dump_rn (mkDumpDoc "Renamer" doc) }

tcDump :: TcGblEnv -> TcRn ()
tcDump env
 = do { dflags <- getDOpts ;

	-- Dump short output if -ddump-types or -ddump-tc
	ifM (dopt Opt_D_dump_types dflags || dopt Opt_D_dump_tc dflags)
	    (dumpTcRn short_dump) ;

	-- Dump bindings if -ddump-tc
	dumpOptTcRn Opt_D_dump_tc (mkDumpDoc "Typechecker" full_dump)
   }
  where
    short_dump = pprTcGblEnv env
    full_dump  = pprLHsBinds (tcg_binds env)
	-- NB: foreign x-d's have undefined's in their types; 
	--     hence can't show the tc_fords

tcCoreDump mod_guts
 = do { dflags <- getDOpts ;
	ifM (dopt Opt_D_dump_types dflags || dopt Opt_D_dump_tc dflags)
 	    (dumpTcRn (pprModGuts mod_guts)) ;

	-- Dump bindings if -ddump-tc
	dumpOptTcRn Opt_D_dump_tc (mkDumpDoc "Typechecker" full_dump) }
  where
    full_dump = pprCoreBindings (mg_binds mod_guts)

-- It's unpleasant having both pprModGuts and pprModDetails here
pprTcGblEnv :: TcGblEnv -> SDoc
pprTcGblEnv (TcGblEnv { tcg_type_env  = type_env, 
		        tcg_insts     = insts, 
		        tcg_fam_insts = fam_insts, 
		        tcg_rules     = rules,
			tcg_imports   = imports })
  = vcat [ ppr_types insts type_env
	 , ppr_tycons fam_insts type_env
	 , ppr_insts insts
	 , ppr_fam_insts fam_insts
	 , vcat (map ppr rules)
	 , ppr_gen_tycons (typeEnvTyCons type_env)
	 , ptext SLIT("Dependent modules:") <+> ppr (eltsUFM (imp_dep_mods imports))
	 , ptext SLIT("Dependent packages:") <+> ppr (imp_dep_pkgs imports)]

pprModGuts :: ModGuts -> SDoc
pprModGuts (ModGuts { mg_types = type_env,
		      mg_rules = rules })
  = vcat [ ppr_types [] type_env,
	   ppr_rules rules ]

ppr_types :: [Instance] -> TypeEnv -> SDoc
ppr_types insts type_env
  = text "TYPE SIGNATURES" $$ nest 4 (ppr_sigs ids)
  where
    dfun_ids = map instanceDFunId insts
    ids = [id | id <- typeEnvIds type_env, want_sig id]
    want_sig id | opt_PprStyle_Debug = True
	        | otherwise	     = isLocalId id && 
				       isExternalName (idName id) && 
				       not (id `elem` dfun_ids)
	-- isLocalId ignores data constructors, records selectors etc.
	-- The isExternalName ignores local dictionary and method bindings
	-- that the type checker has invented.  Top-level user-defined things 
	-- have External names.

ppr_tycons :: [FamInst] -> TypeEnv -> SDoc
ppr_tycons fam_insts type_env
  = text "TYPE CONSTRUCTORS" $$ nest 4 (ppr_tydecls tycons)
  where
    fi_tycons = map famInstTyCon fam_insts
    tycons = [tycon | tycon <- typeEnvTyCons type_env, want_tycon tycon]
    want_tycon tycon | opt_PprStyle_Debug = True
	             | otherwise	  = not (isImplicitTyCon tycon) &&
					    isExternalName (tyConName tycon) &&
				            not (tycon `elem` fi_tycons)

ppr_insts :: [Instance] -> SDoc
ppr_insts []     = empty
ppr_insts ispecs = text "INSTANCES" $$ nest 2 (pprInstances ispecs)

ppr_fam_insts :: [FamInst] -> SDoc
ppr_fam_insts []        = empty
ppr_fam_insts fam_insts = 
  text "FAMILY INSTANCES" $$ nest 2 (pprFamInsts fam_insts)

ppr_sigs :: [Var] -> SDoc
ppr_sigs ids
	-- Print type signatures; sort by OccName 
  = vcat (map ppr_sig (sortLe le_sig ids))
  where
    le_sig id1 id2 = getOccName id1 <= getOccName id2
    ppr_sig id = ppr id <+> dcolon <+> ppr (tidyTopType (idType id))

ppr_tydecls :: [TyCon] -> SDoc
ppr_tydecls tycons
	-- Print type constructor info; sort by OccName 
  = vcat (map ppr_tycon (sortLe le_sig tycons))
  where
    le_sig tycon1 tycon2 = getOccName tycon1 <= getOccName tycon2
    ppr_tycon tycon 
      | isCoercionTyCon tycon = ptext SLIT("coercion") <+> ppr tycon
      | otherwise             = ppr (tyThingToIfaceDecl (ATyCon tycon))

ppr_rules :: [CoreRule] -> SDoc
ppr_rules [] = empty
ppr_rules rs = vcat [ptext SLIT("{-# RULES"),
		      nest 4 (pprRules rs),
		      ptext SLIT("#-}")]

ppr_gen_tycons []  = empty
ppr_gen_tycons tcs = vcat [ptext SLIT("Tycons with generics:"),
			   nest 2 (fsep (map ppr (filter tyConHasGenerics tcs)))]
\end{code}
