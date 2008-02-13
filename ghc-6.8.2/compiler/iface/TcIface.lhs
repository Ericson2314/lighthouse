%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Type checking of type signatures in interface files

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module TcIface ( 
	tcImportDecl, checkWiredInTyCon, tcHiBootIface, typecheckIface, 
	tcIfaceDecl, tcIfaceInst, tcIfaceFamInst, tcIfaceRules,
	tcIfaceVectInfo, tcIfaceGlobal, tcExtCoreBindings
 ) where

#include "HsVersions.h"

import IfaceSyn
import LoadIface
import IfaceEnv
import BuildTyCl
import TcRnMonad
import Type
import TypeRep
import HscTypes
import InstEnv
import FamInstEnv
import CoreSyn
import CoreUtils
import CoreUnfold
import CoreLint
import WorkWrap
import Id
import MkId
import IdInfo
import Class
import TyCon
import DataCon
import TysWiredIn
import Var              ( TyVar )
import qualified Var
import VarEnv
import Name
import NameEnv
import OccName
import Module
import UniqFM
import UniqSupply
import Outputable	
import ErrUtils
import Maybes
import SrcLoc
import DynFlags
import Control.Monad

import Data.List
import Data.Maybe
\end{code}

This module takes

	IfaceDecl -> TyThing
	IfaceType -> Type
	etc

An IfaceDecl is populated with RdrNames, and these are not renamed to
Names before typechecking, because there should be no scope errors etc.

	-- For (b) consider: f = $(...h....)
	-- where h is imported, and calls f via an hi-boot file.  
	-- This is bad!  But it is not seen as a staging error, because h
	-- is indeed imported.  We don't want the type-checker to black-hole 
	-- when simplifying and compiling the splice!
	--
	-- Simple solution: discard any unfolding that mentions a variable
	-- bound in this module (and hence not yet processed).
	-- The discarding happens when forkM finds a type error.

%************************************************************************
%*									*
%*	tcImportDecl is the key function for "faulting in" 		*
%*	imported things
%*									*
%************************************************************************

The main idea is this.  We are chugging along type-checking source code, and
find a reference to GHC.Base.map.  We call tcLookupGlobal, which doesn't find
it in the EPS type envt.  So it 
	1 loads GHC.Base.hi
	2 gets the decl for GHC.Base.map
	3 typechecks it via tcIfaceDecl
	4 and adds it to the type env in the EPS

Note that DURING STEP 4, we may find that map's type mentions a type 
constructor that also 

Notice that for imported things we read the current version from the EPS
mutable variable.  This is important in situations like
	...$(e1)...$(e2)...
where the code that e1 expands to might import some defns that 
also turn out to be needed by the code that e2 expands to.

\begin{code}
tcImportDecl :: Name -> TcM TyThing
-- Entry point for *source-code* uses of importDecl
tcImportDecl name 
  | Just thing <- wiredInNameTyThing_maybe name
  = do	{ initIfaceTcRn (loadWiredInHomeIface name) 
		-- See Note [Loading instances] in LoadIface
	; return thing }
  | otherwise
  = do 	{ traceIf (text "tcImportDecl" <+> ppr name)
	; mb_thing <- initIfaceTcRn (importDecl name)
	; case mb_thing of
	    Succeeded thing -> return thing
	    Failed err      -> failWithTc err }

checkWiredInTyCon :: TyCon -> TcM ()
-- Ensure that the home module of the TyCon (and hence its instances)
-- are loaded. See See Note [Loading instances] in LoadIface
-- It might not be a wired-in tycon (see the calls in TcUnify),
-- in which case this is a no-op.
checkWiredInTyCon tc	
  | not (isWiredInName tc_name) 
  = return ()
  | otherwise
  = do	{ mod <- getModule
	; unless (mod == nameModule tc_name)
		 (initIfaceTcRn (loadWiredInHomeIface tc_name))
		-- Don't look for (non-existent) Float.hi when
		-- compiling Float.lhs, which mentions Float of course
	  	-- A bit yukky to call initIfaceTcRn here
	}
  where
    tc_name = tyConName tc

importDecl :: Name -> IfM lcl (MaybeErr Message TyThing)
-- Get the TyThing for this Name from an interface file
-- It's not a wired-in thing -- the caller caught that
importDecl name
  = ASSERT( not (isWiredInName name) )
    do	{ traceIf nd_doc

	-- Load the interface, which should populate the PTE
	; mb_iface <- loadInterface nd_doc (nameModule name) ImportBySystem
	; case mb_iface of {
		Failed err_msg  -> return (Failed err_msg) ;
		Succeeded iface -> do

	-- Now look it up again; this time we should find it
	{ eps <- getEps	
	; case lookupTypeEnv (eps_PTE eps) name of
	    Just thing -> return (Succeeded thing)
	    Nothing    -> return (Failed not_found_msg)
    }}}
  where
    nd_doc = ptext SLIT("Need decl for") <+> ppr name
    not_found_msg = hang (ptext SLIT("Can't find interface-file declaration for") <+>
				pprNameSpace (occNameSpace (nameOccName name)) <+> ppr name)
	  	       2 (vcat [ptext SLIT("Probable cause: bug in .hi-boot file, or inconsistent .hi file"),
		                ptext SLIT("Use -ddump-if-trace to get an idea of which file caused the error")])
\end{code}

%************************************************************************
%*									*
		Type-checking a complete interface
%*									*
%************************************************************************

Suppose we discover we don't need to recompile.  Then we must type
check the old interface file.  This is a bit different to the
incremental type checking we do as we suck in interface files.  Instead
we do things similarly as when we are typechecking source decls: we
bring into scope the type envt for the interface all at once, using a
knot.  Remember, the decls aren't necessarily in dependency order --
and even if they were, the type decls might be mutually recursive.

\begin{code}
typecheckIface :: ModIface 	-- Get the decls from here
	       -> TcRnIf gbl lcl ModDetails
typecheckIface iface
  = initIfaceTc iface $ \ tc_env_var -> do
	-- The tc_env_var is freshly allocated, private to 
	-- type-checking this particular interface
	{ 	-- Get the right set of decls and rules.  If we are compiling without -O
		-- we discard pragmas before typechecking, so that we don't "see"
		-- information that we shouldn't.  From a versioning point of view
		-- It's not actually *wrong* to do so, but in fact GHCi is unable 
		-- to handle unboxed tuples, so it must not see unfoldings.
	  ignore_prags <- doptM Opt_IgnoreInterfacePragmas

		-- Typecheck the decls.  This is done lazily, so that the knot-tying
		-- within this single module work out right.  In the If monad there is
		-- no global envt for the current interface; instead, the knot is tied
		-- through the if_rec_types field of IfGblEnv
	; names_w_things <- loadDecls ignore_prags (mi_decls iface)
	; let type_env = mkNameEnv names_w_things
	; writeMutVar tc_env_var type_env

		-- Now do those rules and instances
	; insts     <- mapM tcIfaceInst    (mi_insts     iface)
	; fam_insts <- mapM tcIfaceFamInst (mi_fam_insts iface)
	; rules     <- tcIfaceRules ignore_prags (mi_rules iface)

                -- Vectorisation information
        ; vect_info <- tcIfaceVectInfo (mi_module iface) type_env 
                                       (mi_vect_info iface)

		-- Exports
	; exports <- ifaceExportNames (mi_exports iface)

		-- Finished
	; traceIf (vcat [text "Finished typechecking interface for" <+> ppr (mi_module iface),
			 text "Type envt:" <+> ppr type_env])
	; return $ ModDetails { md_types     = type_env
			      , md_insts     = insts
			      , md_fam_insts = fam_insts
			      , md_rules     = rules
                              , md_vect_info = vect_info
			      , md_exports   = exports
			      }
    }
\end{code}


%************************************************************************
%*									*
		Type and class declarations
%*									*
%************************************************************************

\begin{code}
tcHiBootIface :: HscSource -> Module -> TcRn ModDetails
-- Load the hi-boot iface for the module being compiled,
-- if it indeed exists in the transitive closure of imports
-- Return the ModDetails, empty if no hi-boot iface
tcHiBootIface hsc_src mod
  | isHsBoot hsc_src		-- Already compiling a hs-boot file
  = return emptyModDetails
  | otherwise
  = do 	{ traceIf (text "loadHiBootInterface" <+> ppr mod)

	; mode <- getGhcMode
	; if not (isOneShot mode)
		-- In --make and interactive mode, if this module has an hs-boot file
		-- we'll have compiled it already, and it'll be in the HPT
		-- 
		-- We check wheher the interface is a *boot* interface.
		-- It can happen (when using GHC from Visual Studio) that we
		-- compile a module in TypecheckOnly mode, with a stable, 
		-- fully-populated HPT.  In that case the boot interface isn't there
		-- (it's been replaced by the mother module) so we can't check it.
		-- And that's fine, because if M's ModInfo is in the HPT, then 
		-- it's been compiled once, and we don't need to check the boot iface
	  then do { hpt <- getHpt
		  ; case lookupUFM hpt (moduleName mod) of
		      Just info | mi_boot (hm_iface info) 
				-> return (hm_details info)
		      other -> return emptyModDetails }
	  else do

	-- OK, so we're in one-shot mode.  
	-- In that case, we're read all the direct imports by now, 
	-- so eps_is_boot will record if any of our imports mention us by 
	-- way of hi-boot file
	{ eps <- getEps
	; case lookupUFM (eps_is_boot eps) (moduleName mod) of {
	    Nothing -> return emptyModDetails ;	-- The typical case

	    Just (_, False) -> failWithTc moduleLoop ;
 		-- Someone below us imported us!
		-- This is a loop with no hi-boot in the way
		
	    Just (_mod, True) -> 	-- There's a hi-boot interface below us
		
    do	{ read_result <- findAndReadIface 
				need mod
				True	-- Hi-boot file

	; case read_result of
		Failed err               -> failWithTc (elaborate err)
		Succeeded (iface, _path) -> typecheckIface iface
    }}}}
  where
    need = ptext SLIT("Need the hi-boot interface for") <+> ppr mod
		 <+> ptext SLIT("to compare against the Real Thing")

    moduleLoop = ptext SLIT("Circular imports: module") <+> quotes (ppr mod) 
		     <+> ptext SLIT("depends on itself")

    elaborate err = hang (ptext SLIT("Could not find hi-boot interface for") <+> 
		          quotes (ppr mod) <> colon) 4 err
\end{code}


%************************************************************************
%*									*
		Type and class declarations
%*									*
%************************************************************************

When typechecking a data type decl, we *lazily* (via forkM) typecheck
the constructor argument types.  This is in the hope that we may never
poke on those argument types, and hence may never need to load the
interface files for types mentioned in the arg types.

E.g.	
	data Foo.S = MkS Baz.T
Mabye we can get away without even loading the interface for Baz!

This is not just a performance thing.  Suppose we have
	data Foo.S = MkS Baz.T
	data Baz.T = MkT Foo.S
(in different interface files, of course).
Now, first we load and typecheck Foo.S, and add it to the type envt.  
If we do explore MkS's argument, we'll load and typecheck Baz.T.
If we explore MkT's argument we'll find Foo.S already in the envt.  

If we typechecked constructor args eagerly, when loading Foo.S we'd try to
typecheck the type Baz.T.  So we'd fault in Baz.T... and then need Foo.S...
which isn't done yet.

All very cunning. However, there is a rather subtle gotcha which bit
me when developing this stuff.  When we typecheck the decl for S, we
extend the type envt with S, MkS, and all its implicit Ids.  Suppose
(a bug, but it happened) that the list of implicit Ids depended in
turn on the constructor arg types.  Then the following sequence of
events takes place:
	* we build a thunk <t> for the constructor arg tys
	* we build a thunk for the extended type environment (depends on <t>)
	* we write the extended type envt into the global EPS mutvar
	
Now we look something up in the type envt
	* that pulls on <t>
	* which reads the global type envt out of the global EPS mutvar
	* but that depends in turn on <t>

It's subtle, because, it'd work fine if we typechecked the constructor args 
eagerly -- they don't need the extended type envt.  They just get the extended
type envt by accident, because they look at it later.

What this means is that the implicitTyThings MUST NOT DEPEND on any of
the forkM stuff.


\begin{code}
tcIfaceDecl :: Bool	-- True <=> discard IdInfo on IfaceId bindings
	    -> IfaceDecl
	    -> IfL TyThing

tcIfaceDecl ignore_prags (IfaceId {ifName = occ_name, ifType = iface_type, ifIdInfo = info})
  = do	{ name <- lookupIfaceTop occ_name
	; ty <- tcIfaceType iface_type
	; info <- tcIdInfo ignore_prags name ty info
	; return (AnId (mkVanillaGlobal name ty info)) }

tcIfaceDecl ignore_prags 
	    (IfaceData {ifName = occ_name, 
			ifTyVars = tv_bndrs, 
			ifCtxt = ctxt, ifGadtSyntax = gadt_syn,
			ifCons = rdr_cons, 
			ifRec = is_rec, 
			ifGeneric = want_generic,
			ifFamInst = mb_family })
  = do	{ tc_name <- lookupIfaceTop occ_name
	; bindIfaceTyVars tv_bndrs $ \ tyvars -> do

	{ tycon <- fixM ( \ tycon -> do
	    { stupid_theta <- tcIfaceCtxt ctxt
	    ; famInst <- 
	        case mb_family of
		  Nothing         -> return Nothing
		  Just (fam, tys) -> 
		    do { famTyCon <- tcIfaceTyCon fam
		       ; insttys <- mapM tcIfaceType tys
		       ; return $ Just (famTyCon, insttys)
		       }
	    ; cons <- tcIfaceDataCons tc_name tycon tyvars rdr_cons
	    ; buildAlgTyCon tc_name tyvars stupid_theta
			    cons is_rec want_generic gadt_syn famInst
	    })
        ; traceIf (text "tcIfaceDecl4" <+> ppr tycon)
	; return (ATyCon tycon)
    }}

tcIfaceDecl ignore_prags 
	    (IfaceSyn {ifName = occ_name, ifTyVars = tv_bndrs, 
		       ifOpenSyn = isOpen, ifSynRhs = rdr_rhs_ty,
		       ifFamInst = mb_family})
   = bindIfaceTyVars tv_bndrs $ \ tyvars -> do
     { tc_name <- lookupIfaceTop occ_name
     ; rhs_tyki <- tcIfaceType rdr_rhs_ty
     ; let rhs = if isOpen then OpenSynTyCon rhs_tyki Nothing
			   else SynonymTyCon rhs_tyki
     ; famInst <- case mb_family of
		    Nothing         -> return Nothing
		    Just (fam, tys) -> 
		      do { famTyCon <- tcIfaceTyCon fam
		         ; insttys <- mapM tcIfaceType tys
		         ; return $ Just (famTyCon, insttys)
		         }
     ; tycon <- buildSynTyCon tc_name tyvars rhs famInst
     ; return $ ATyCon tycon
     }

tcIfaceDecl ignore_prags
	    (IfaceClass {ifCtxt = rdr_ctxt, ifName = occ_name, 
			 ifTyVars = tv_bndrs, ifFDs = rdr_fds, 
			 ifATs = rdr_ats, ifSigs = rdr_sigs, 
			 ifRec = tc_isrec })
-- ToDo: in hs-boot files we should really treat abstract classes specially,
--	 as we do abstract tycons
  = bindIfaceTyVars tv_bndrs $ \ tyvars -> do
    { cls_name <- lookupIfaceTop occ_name
    ; ctxt <- tcIfaceCtxt rdr_ctxt
    ; sigs <- mappM tc_sig rdr_sigs
    ; fds  <- mappM tc_fd rdr_fds
    ; ats'  <- mappM (tcIfaceDecl ignore_prags) rdr_ats
    ; let ats = zipWith setTyThingPoss ats' (map ifTyVars rdr_ats)
    ; cls  <- buildClass cls_name tyvars ctxt fds ats sigs tc_isrec
    ; return (AClass cls) }
  where
   tc_sig (IfaceClassOp occ dm rdr_ty)
     = do { op_name <- lookupIfaceTop occ
	  ; op_ty   <- forkM (mk_doc op_name rdr_ty) (tcIfaceType rdr_ty)
		-- Must be done lazily for just the same reason as the 
		-- type of a data con; to avoid sucking in types that
		-- it mentions unless it's necessray to do so
	  ; return (op_name, dm, op_ty) }

   mk_doc op_name op_ty = ptext SLIT("Class op") <+> sep [ppr op_name, ppr op_ty]

   tc_fd (tvs1, tvs2) = do { tvs1' <- mappM tcIfaceTyVar tvs1
			   ; tvs2' <- mappM tcIfaceTyVar tvs2
			   ; return (tvs1', tvs2') }

   -- For each AT argument compute the position of the corresponding class
   -- parameter in the class head.  This will later serve as a permutation
   -- vector when checking the validity of instance declarations.
   setTyThingPoss (ATyCon tycon) atTyVars = 
     let classTyVars = map fst tv_bndrs
	 poss        =   catMaybes 
		       . map ((`elemIndex` classTyVars) . fst) 
		       $ atTyVars
		    -- There will be no Nothing, as we already passed renaming
     in 
     ATyCon (setTyConArgPoss tycon poss)
   setTyThingPoss _		  _ = panic "TcIface.setTyThingPoss"

tcIfaceDecl ignore_prags (IfaceForeign {ifName = rdr_name, ifExtName = ext_name})
  = do	{ name <- lookupIfaceTop rdr_name
	; return (ATyCon (mkForeignTyCon name ext_name 
					 liftedTypeKind 0)) }

tcIfaceDataCons tycon_name tycon tc_tyvars if_cons
  = case if_cons of
	IfAbstractTyCon	 -> return mkAbstractTyConRhs
	IfOpenDataTyCon	 -> return mkOpenDataTyConRhs
	IfDataTyCon cons -> do 	{ data_cons <- mappM tc_con_decl cons
				; return (mkDataTyConRhs data_cons) }
	IfNewTyCon con	 -> do 	{ data_con <- tc_con_decl con
				; mkNewTyConRhs tycon_name tycon data_con }
  where
    tc_con_decl (IfCon { ifConInfix = is_infix, 
			 ifConUnivTvs = univ_tvs, ifConExTvs = ex_tvs,
			 ifConOcc = occ, ifConCtxt = ctxt, ifConEqSpec = spec,
			 ifConArgTys = args, ifConFields = field_lbls,
			 ifConStricts = stricts})
     = bindIfaceTyVars univ_tvs $ \ univ_tyvars -> do
       bindIfaceTyVars ex_tvs	 $ \ ex_tyvars -> do
	{ name  <- lookupIfaceTop occ
        ; eq_spec <- tcIfaceEqSpec spec
	; theta <- tcIfaceCtxt ctxt	-- Laziness seems not worth the bother here
	 	-- At one stage I thought that this context checking *had*
		-- to be lazy, because of possible mutual recursion between the
		-- type and the classe: 
		-- E.g. 
		--	class Real a where { toRat :: a -> Ratio Integer }
		--	data (Real a) => Ratio a = ...
		-- But now I think that the laziness in checking class ops breaks 
		-- the loop, so no laziness needed

	-- Read the argument types, but lazily to avoid faulting in
	-- the component types unless they are really needed
 	; arg_tys <- forkM (mk_doc name) (mappM tcIfaceType args)
	; lbl_names <- mappM lookupIfaceTop field_lbls

	; buildDataCon name is_infix {- Not infix -}
		       stricts lbl_names
		       univ_tyvars ex_tyvars 
                       eq_spec theta 
		       arg_tys tycon
	}
    mk_doc con_name = ptext SLIT("Constructor") <+> ppr con_name

tcIfaceEqSpec spec
  = mapM do_item spec
  where
    do_item (occ, if_ty) = do { tv <- tcIfaceTyVar (occNameFS occ)
                              ; ty <- tcIfaceType if_ty
                              ; return (tv,ty) }
\end{code}


%************************************************************************
%*									*
		Instances
%*									*
%************************************************************************

\begin{code}
tcIfaceInst :: IfaceInst -> IfL Instance
tcIfaceInst (IfaceInst { ifDFun = dfun_occ, ifOFlag = oflag,
			 ifInstCls = cls, ifInstTys = mb_tcs,
			 ifInstOrph = orph })
  = do	{ dfun    <- forkM (ptext SLIT("Dict fun") <+> ppr dfun_occ) $
		     tcIfaceExtId dfun_occ
        ; let mb_tcs' = map (fmap ifaceTyConName) mb_tcs
	; return (mkImportedInstance cls mb_tcs' dfun oflag) }

tcIfaceFamInst :: IfaceFamInst -> IfL FamInst
tcIfaceFamInst (IfaceFamInst { ifFamInstTyCon = tycon, 
			       ifFamInstFam = fam, ifFamInstTys = mb_tcs })
--  = do	{ tycon'  <- forkM (ptext SLIT("Inst tycon") <+> ppr tycon) $
-- ^^^this line doesn't work, but vvv this does => CPP in Haskell = evil!
  = do	{ tycon'  <- forkM (text ("Inst tycon") <+> ppr tycon) $
		     tcIfaceTyCon tycon
        ; let mb_tcs' = map (fmap ifaceTyConName) mb_tcs
	; return (mkImportedFamInst fam mb_tcs' tycon') }
\end{code}


%************************************************************************
%*									*
		Rules
%*									*
%************************************************************************

We move a IfaceRule from eps_rules to eps_rule_base when all its LHS free vars
are in the type environment.  However, remember that typechecking a Rule may 
(as a side effect) augment the type envt, and so we may need to iterate the process.

\begin{code}
tcIfaceRules :: Bool 		-- True <=> ignore rules
	     -> [IfaceRule]
	     -> IfL [CoreRule]
tcIfaceRules ignore_prags if_rules
  | ignore_prags = return []
  | otherwise    = mapM tcIfaceRule if_rules

tcIfaceRule :: IfaceRule -> IfL CoreRule
tcIfaceRule (IfaceRule {ifRuleName = name, ifActivation = act, ifRuleBndrs = bndrs,
			ifRuleHead = fn, ifRuleArgs = args, ifRuleRhs = rhs,
			ifRuleOrph = orph })
  = do	{ ~(bndrs', args', rhs') <- 
		-- Typecheck the payload lazily, in the hope it'll never be looked at
		forkM (ptext SLIT("Rule") <+> ftext name) $
		bindIfaceBndrs bndrs 			  $ \ bndrs' ->
		do { args' <- mappM tcIfaceExpr args
		   ; rhs'  <- tcIfaceExpr rhs
		   ; return (bndrs', args', rhs') }
	; let mb_tcs = map ifTopFreeName args
	; returnM (Rule { ru_name = name, ru_fn = fn, ru_act = act, 
			  ru_bndrs = bndrs', ru_args = args', 
			  ru_rhs = rhs', 
			  ru_rough = mb_tcs,
			  ru_local = False }) }	-- An imported RULE is never for a local Id
						-- or, even if it is (module loop, perhaps)
						-- we'll just leave it in the non-local set
  where
	-- This function *must* mirror exactly what Rules.topFreeName does
	-- We could have stored the ru_rough field in the iface file
	-- but that would be redundant, I think.
	-- The only wrinkle is that we must not be deceived by
	-- type syononyms at the top of a type arg.  Since
	-- we can't tell at this point, we are careful not
	-- to write them out in coreRuleToIfaceRule
    ifTopFreeName :: IfaceExpr -> Maybe Name
    ifTopFreeName (IfaceType (IfaceTyConApp tc _ )) = Just (ifaceTyConName tc)
    ifTopFreeName (IfaceApp f a)                    = ifTopFreeName f
    ifTopFreeName (IfaceExt n)                      = Just n
    ifTopFreeName other                             = Nothing
\end{code}


%************************************************************************
%*									*
		Vectorisation information
%*									*
%************************************************************************

\begin{code}
tcIfaceVectInfo :: Module -> TypeEnv  -> IfaceVectInfo -> IfL VectInfo
tcIfaceVectInfo mod typeEnv (IfaceVectInfo 
                             { ifaceVectInfoVar        = vars
                             , ifaceVectInfoTyCon      = tycons
                             , ifaceVectInfoTyConReuse = tyconsReuse
                             })
  = do { vVars     <- mapM vectVarMapping vars
       ; tyConRes1 <- mapM vectTyConMapping      tycons
       ; tyConRes2 <- mapM vectTyConReuseMapping tycons
       ; let (vTyCons, vDataCons, vPAs, vIsos) = unzip4 (tyConRes1 ++ tyConRes2)
       ; return $ VectInfo 
                  { vectInfoVar     = mkVarEnv  vVars
                  , vectInfoTyCon   = mkNameEnv vTyCons
                  , vectInfoDataCon = mkNameEnv (concat vDataCons)
                  , vectInfoPADFun  = mkNameEnv vPAs
                  , vectInfoIso     = mkNameEnv vIsos
                  }
       }
  where
    vectVarMapping name 
      = do { vName <- lookupOrig mod (mkVectOcc (nameOccName name))
           ; let { var  = lookupVar name
                 ; vVar = lookupVar vName
                 }
           ; return (var, (var, vVar))
           }
    vectTyConMapping name 
      = do { vName   <- lookupOrig mod (mkVectTyConOcc (nameOccName name))
           ; paName  <- lookupOrig mod (mkPADFunOcc    (nameOccName name))
           ; isoName <- lookupOrig mod (mkVectIsoOcc   (nameOccName name))
           ; let { tycon    = lookupTyCon name
                 ; vTycon   = lookupTyCon vName
                 ; paTycon  = lookupVar paName
                 ; isoTycon = lookupVar isoName
                 }
           ; vDataCons <- mapM vectDataConMapping (tyConDataCons tycon)
           ; return ((name, (tycon, vTycon)),    -- (T, T_v)
                     vDataCons,                  -- list of (Ci, Ci_v)
                     (vName, (vTycon, paTycon)), -- (T_v, paT)
                     (name, (tycon, isoTycon)))  -- (T, isoT)
           }
    vectTyConReuseMapping name 
      = do { paName  <- lookupOrig mod (mkPADFunOcc    (nameOccName name))
           ; isoName <- lookupOrig mod (mkVectIsoOcc   (nameOccName name))
           ; let { tycon      = lookupTyCon name
                 ; paTycon    = lookupVar paName
                 ; isoTycon   = lookupVar isoName
                 ; vDataCons  = [ (dataConName dc, (dc, dc)) 
                                | dc <- tyConDataCons tycon]
                 }
           ; return ((name, (tycon, tycon)),     -- (T, T)
                     vDataCons,                  -- list of (Ci, Ci)
                     (name, (tycon, paTycon)),   -- (T, paT)
                     (name, (tycon, isoTycon)))  -- (T, isoT)
           }
    vectDataConMapping datacon
      = do { let name = dataConName datacon
           ; vName <- lookupOrig mod (mkVectDataConOcc (nameOccName name))
           ; let vDataCon = lookupDataCon vName
           ; return (name, (datacon, vDataCon))
           }
    --
    lookupVar name = case lookupTypeEnv typeEnv name of
                       Just (AnId var) -> var
                       Just _         -> 
                         panic "TcIface.tcIfaceVectInfo: not an id"
                       Nothing        ->
                         panic "TcIface.tcIfaceVectInfo: unknown name"
    lookupTyCon name = case lookupTypeEnv typeEnv name of
                         Just (ATyCon tc) -> tc
                         Just _         -> 
                           panic "TcIface.tcIfaceVectInfo: not a tycon"
                         Nothing        ->
                           panic "TcIface.tcIfaceVectInfo: unknown name"
    lookupDataCon name = case lookupTypeEnv typeEnv name of
                           Just (ADataCon dc) -> dc
                           Just _         -> 
                             panic "TcIface.tcIfaceVectInfo: not a datacon"
                           Nothing        ->
                             panic "TcIface.tcIfaceVectInfo: unknown name"
\end{code}

%************************************************************************
%*									*
			Types
%*									*
%************************************************************************

\begin{code}
tcIfaceType :: IfaceType -> IfL Type
tcIfaceType (IfaceTyVar n)        = do { tv <- tcIfaceTyVar n; return (TyVarTy tv) }
tcIfaceType (IfaceAppTy t1 t2)    = do { t1' <- tcIfaceType t1; t2' <- tcIfaceType t2; return (AppTy t1' t2') }
tcIfaceType (IfaceFunTy t1 t2)    = do { t1' <- tcIfaceType t1; t2' <- tcIfaceType t2; return (FunTy t1' t2') }
tcIfaceType (IfaceTyConApp tc ts) = do { tc' <- tcIfaceTyCon tc; ts' <- tcIfaceTypes ts; return (mkTyConApp tc' ts') }
tcIfaceType (IfaceForAllTy tv t)  = bindIfaceTyVar tv $ \ tv' -> do { t' <- tcIfaceType t; return (ForAllTy tv' t') }
tcIfaceType (IfacePredTy st)      = do { st' <- tcIfacePredType st; return (PredTy st') }

tcIfaceTypes tys = mapM tcIfaceType tys

-----------------------------------------
tcIfacePredType :: IfacePredType -> IfL PredType
tcIfacePredType (IfaceClassP cls ts) = do { cls' <- tcIfaceClass cls; ts' <- tcIfaceTypes ts; return (ClassP cls' ts') }
tcIfacePredType (IfaceIParam ip t)   = do { ip' <- newIPName ip; t' <- tcIfaceType t; return (IParam ip' t') }
tcIfacePredType (IfaceEqPred t1 t2)  = do { t1' <- tcIfaceType t1; t2' <- tcIfaceType t2; return (EqPred t1' t2') }

-----------------------------------------
tcIfaceCtxt :: IfaceContext -> IfL ThetaType
tcIfaceCtxt sts = mappM tcIfacePredType sts
\end{code}


%************************************************************************
%*									*
			Core
%*									*
%************************************************************************

\begin{code}
tcIfaceExpr :: IfaceExpr -> IfL CoreExpr
tcIfaceExpr (IfaceType ty)
  = tcIfaceType ty		`thenM` \ ty' ->
    returnM (Type ty')

tcIfaceExpr (IfaceLcl name)
  = tcIfaceLclId name 	`thenM` \ id ->
    returnM (Var id)

tcIfaceExpr (IfaceTick modName tickNo)
  = tcIfaceTick modName tickNo	`thenM` \ id ->
    returnM (Var id)

tcIfaceExpr (IfaceExt gbl)
  = tcIfaceExtId gbl 	`thenM` \ id ->
    returnM (Var id)

tcIfaceExpr (IfaceLit lit)
  = returnM (Lit lit)

tcIfaceExpr (IfaceFCall cc ty)
  = tcIfaceType ty 	`thenM` \ ty' ->
    newUnique		`thenM` \ u ->
    returnM (Var (mkFCallId u cc ty'))

tcIfaceExpr (IfaceTuple boxity args) 
  = mappM tcIfaceExpr args	`thenM` \ args' ->
    let
	-- Put the missing type arguments back in
	con_args = map (Type . exprType) args' ++ args'
    in
    returnM (mkApps (Var con_id) con_args)
  where
    arity = length args
    con_id = dataConWorkId (tupleCon boxity arity)
    

tcIfaceExpr (IfaceLam bndr body)
  = bindIfaceBndr bndr 		$ \ bndr' ->
    tcIfaceExpr body		`thenM` \ body' ->
    returnM (Lam bndr' body')

tcIfaceExpr (IfaceApp fun arg)
  = tcIfaceExpr fun		`thenM` \ fun' ->
    tcIfaceExpr arg		`thenM` \ arg' ->
    returnM (App fun' arg')

tcIfaceExpr (IfaceCase scrut case_bndr ty alts) 
  = tcIfaceExpr scrut		`thenM` \ scrut' ->
    newIfaceName (mkVarOccFS case_bndr)	`thenM` \ case_bndr_name ->
    let
	scrut_ty   = exprType scrut'
	case_bndr' = mkLocalId case_bndr_name scrut_ty
	tc_app     = splitTyConApp scrut_ty
		-- NB: Won't always succeed (polymoprhic case)
		--     but won't be demanded in those cases
		-- NB: not tcSplitTyConApp; we are looking at Core here
		--     look through non-rec newtypes to find the tycon that
		--     corresponds to the datacon in this case alternative
    in
    extendIfaceIdEnv [case_bndr']	$
    mappM (tcIfaceAlt scrut' tc_app) alts	`thenM` \ alts' ->
    tcIfaceType ty				`thenM` \ ty' ->
    returnM (Case scrut' case_bndr' ty' alts')

tcIfaceExpr (IfaceLet (IfaceNonRec bndr rhs) body)
  = do	{ rhs' <- tcIfaceExpr rhs
	; id   <- tcIfaceLetBndr bndr
	; body' <- extendIfaceIdEnv [id] (tcIfaceExpr body)
	; return (Let (NonRec id rhs') body') }

tcIfaceExpr (IfaceLet (IfaceRec pairs) body)
  = do	{ ids <- mapM tcIfaceLetBndr bndrs
	; extendIfaceIdEnv ids $ do
	{ rhss' <- mapM tcIfaceExpr rhss
	; body' <- tcIfaceExpr body
	; return (Let (Rec (ids `zip` rhss')) body') } }
  where
    (bndrs, rhss) = unzip pairs

tcIfaceExpr (IfaceCast expr co) = do
  expr' <- tcIfaceExpr expr
  co' <- tcIfaceType co
  returnM (Cast expr' co')

tcIfaceExpr (IfaceNote note expr) 
  = tcIfaceExpr expr		`thenM` \ expr' ->
    case note of
	IfaceInlineMe     -> returnM (Note InlineMe   expr')
	IfaceSCC cc       -> returnM (Note (SCC cc)   expr')
	IfaceCoreNote n   -> returnM (Note (CoreNote n) expr')

-------------------------
tcIfaceAlt _ _ (IfaceDefault, names, rhs)
  = ASSERT( null names )
    tcIfaceExpr rhs		`thenM` \ rhs' ->
    returnM (DEFAULT, [], rhs')
  
tcIfaceAlt _ _ (IfaceLitAlt lit, names, rhs)
  = ASSERT( null names )
    tcIfaceExpr rhs		`thenM` \ rhs' ->
    returnM (LitAlt lit, [], rhs')

-- A case alternative is made quite a bit more complicated
-- by the fact that we omit type annotations because we can
-- work them out.  True enough, but its not that easy!
tcIfaceAlt scrut (tycon, inst_tys) (IfaceDataAlt data_occ, arg_strs, rhs)
  = do	{ con <- tcIfaceDataCon data_occ
#ifdef DEBUG
	; ifM (not (con `elem` tyConDataCons tycon))
	      (failIfM (ppr scrut $$ ppr con $$ ppr tycon $$ ppr (tyConDataCons tycon)))
#endif
	; tcIfaceDataAlt con inst_tys arg_strs rhs }
		  
tcIfaceAlt _ (tycon, inst_tys) (IfaceTupleAlt boxity, arg_occs, rhs)
  = ASSERT( isTupleTyCon tycon )
    do	{ let [data_con] = tyConDataCons tycon
	; tcIfaceDataAlt data_con inst_tys arg_occs rhs }

tcIfaceDataAlt con inst_tys arg_strs rhs
  = do	{ us <- newUniqueSupply
	; let uniqs = uniqsFromSupply us
	; let (ex_tvs, co_tvs, arg_ids)
	              = dataConRepFSInstPat arg_strs uniqs con inst_tys
              all_tvs = ex_tvs ++ co_tvs

	; rhs' <- extendIfaceTyVarEnv all_tvs	$
		  extendIfaceIdEnv arg_ids	$
		  tcIfaceExpr rhs
	; return (DataAlt con, all_tvs ++ arg_ids, rhs') }
\end{code}


\begin{code}
tcExtCoreBindings :: [IfaceBinding] -> IfL [CoreBind]	-- Used for external core
tcExtCoreBindings []     = return []
tcExtCoreBindings (b:bs) = do_one b (tcExtCoreBindings bs)

do_one :: IfaceBinding -> IfL [CoreBind] -> IfL [CoreBind]
do_one (IfaceNonRec bndr rhs) thing_inside
  = do	{ rhs' <- tcIfaceExpr rhs
	; bndr' <- newExtCoreBndr bndr
	; extendIfaceIdEnv [bndr'] $ do 
	{ core_binds <- thing_inside
	; return (NonRec bndr' rhs' : core_binds) }}

do_one (IfaceRec pairs) thing_inside
  = do	{ bndrs' <- mappM newExtCoreBndr bndrs
	; extendIfaceIdEnv bndrs' $ do
 	{ rhss' <- mappM tcIfaceExpr rhss
	; core_binds <- thing_inside
	; return (Rec (bndrs' `zip` rhss') : core_binds) }}
  where
    (bndrs,rhss) = unzip pairs
\end{code}


%************************************************************************
%*									*
		IdInfo
%*									*
%************************************************************************

\begin{code}
tcIdInfo :: Bool -> Name -> Type -> IfaceIdInfo -> IfL IdInfo
tcIdInfo ignore_prags name ty info 
  | ignore_prags = return vanillaIdInfo
  | otherwise    = case info of
			NoInfo       -> return vanillaIdInfo
			HasInfo info -> foldlM tcPrag init_info info
  where
    -- Set the CgInfo to something sensible but uninformative before
    -- we start; default assumption is that it has CAFs
    init_info = vanillaIdInfo

    tcPrag info HsNoCafRefs         = returnM (info `setCafInfo`   NoCafRefs)
    tcPrag info (HsArity arity)     = returnM (info `setArityInfo` arity)
    tcPrag info (HsStrictness str)  = returnM (info `setAllStrictnessInfo` Just str)

	-- The next two are lazy, so they don't transitively suck stuff in
    tcPrag info (HsWorker nm arity) = tcWorkerInfo ty info nm arity
    tcPrag info (HsInline inline_prag) = returnM (info `setInlinePragInfo` inline_prag)
    tcPrag info (HsUnfold expr)
	= tcPragExpr name expr 	`thenM` \ maybe_expr' ->
	  let
		-- maybe_expr' doesn't get looked at if the unfolding
		-- is never inspected; so the typecheck doesn't even happen
		unfold_info = case maybe_expr' of
				Nothing    -> noUnfolding
				Just expr' -> mkTopUnfolding expr' 
	  in
 	  returnM (info `setUnfoldingInfoLazily` unfold_info)
\end{code}

\begin{code}
tcWorkerInfo ty info wkr arity
  = do 	{ mb_wkr_id <- forkM_maybe doc (tcIfaceExtId wkr)

	-- We return without testing maybe_wkr_id, but as soon as info is
	-- looked at we will test it.  That's ok, because its outside the
	-- knot; and there seems no big reason to further defer the
	-- tcIfaceId lookup.  (Contrast with tcPragExpr, where postponing walking
	-- over the unfolding until it's actually used does seem worth while.)
	; us <- newUniqueSupply

	; returnM (case mb_wkr_id of
		     Nothing     -> info
		     Just wkr_id -> add_wkr_info us wkr_id info) }
  where
    doc = text "Worker for" <+> ppr wkr
    add_wkr_info us wkr_id info
	= info `setUnfoldingInfoLazily`  mk_unfolding us wkr_id
	       `setWorkerInfo`           HasWorker wkr_id arity

    mk_unfolding us wkr_id = mkTopUnfolding (initUs_ us (mkWrapper ty strict_sig) wkr_id)

    	-- We are relying here on strictness info always appearing 
	-- before worker info,  fingers crossed ....
    strict_sig = case newStrictnessInfo info of
		   Just sig -> sig
		   Nothing  -> pprPanic "Worker info but no strictness for" (ppr wkr)
\end{code}

For unfoldings we try to do the job lazily, so that we never type check
an unfolding that isn't going to be looked at.

\begin{code}
tcPragExpr :: Name -> IfaceExpr -> IfL (Maybe CoreExpr)
tcPragExpr name expr
  = forkM_maybe doc $
    tcIfaceExpr expr		`thenM` \ core_expr' ->

		-- Check for type consistency in the unfolding
    ifOptM Opt_DoCoreLinting (
	get_in_scope_ids			`thenM` \ in_scope -> 
	case lintUnfolding noSrcLoc in_scope core_expr' of
	  Nothing       -> returnM ()
	  Just fail_msg -> pprPanic "Iface Lint failure" (hang doc 2 fail_msg)
    )				`thenM_`

   returnM core_expr'	
  where
    doc = text "Unfolding of" <+> ppr name
    get_in_scope_ids 	-- Urgh; but just for linting
	= setLclEnv () $ 
	  do	{ env <- getGblEnv 
		; case if_rec_types env of {
			  Nothing -> return [] ;
			  Just (_, get_env) -> do
		{ type_env <- get_env
		; return (typeEnvIds type_env) }}}
\end{code}



%************************************************************************
%*									*
		Getting from Names to TyThings
%*									*
%************************************************************************

\begin{code}
tcIfaceGlobal :: Name -> IfL TyThing
tcIfaceGlobal name
  | Just thing <- wiredInNameTyThing_maybe name
	-- Wired-in things include TyCons, DataCons, and Ids
  = do { ifCheckWiredInThing name; return thing }
  | otherwise
  = do	{ env <- getGblEnv
	; case if_rec_types env of {	-- Note [Tying the knot]
	    Just (mod, get_type_env) 
		| nameIsLocalOrFrom mod name
		-> do 		-- It's defined in the module being compiled
	  	{ type_env <- setLclEnv () get_type_env		-- yuk
		; case lookupNameEnv type_env name of
			Just thing -> return thing
			Nothing	   -> pprPanic "tcIfaceGlobal (local): not found:"  
						(ppr name $$ ppr type_env) }

	  ; other -> do

	{ (eps,hpt) <- getEpsAndHpt
 	; dflags <- getDOpts
	; case lookupType dflags hpt (eps_PTE eps) name of {
	    Just thing -> return thing ;
	    Nothing    -> do

	{ mb_thing <- importDecl name 	-- It's imported; go get it
	; case mb_thing of
	    Failed err      -> failIfM err
	    Succeeded thing -> return thing
    }}}}}

-- Note [Tying the knot]
-- ~~~~~~~~~~~~~~~~~~~~~
-- The if_rec_types field is used in two situations:
--
-- a) Compiling M.hs, which indiretly imports Foo.hi, which mentions M.T
--    Then we look up M.T in M's type environment, which is splatted into if_rec_types
--    after we've built M's type envt.
--
-- b) In ghc --make, during the upsweep, we encounter M.hs, whose interface M.hi
--    is up to date.  So we call typecheckIface on M.hi.  This splats M.T into 
--    if_rec_types so that the (lazily typechecked) decls see all the other decls
--
-- In case (b) it's important to do the if_rec_types check *before* looking in the HPT
-- Because if M.hs also has M.hs-boot, M.T will *already be* in the HPT, but in its
-- emasculated form (e.g. lacking data constructors).

ifCheckWiredInThing :: Name -> IfL ()
-- Even though we are in an interface file, we want to make
-- sure the instances of a wired-in thing are loaded (imagine f :: Double -> Double)
-- Ditto want to ensure that RULES are loaded too
-- See Note [Loading instances] in LoadIface
ifCheckWiredInThing name 
  = do	{ mod <- getIfModule
		-- Check whether we are typechecking the interface for this
		-- very module.  E.g when compiling the base library in --make mode
		-- we may typecheck GHC.Base.hi. At that point, GHC.Base is not in
		-- the HPT, so without the test we'll demand-load it into the PIT!
		-- C.f. the same test in checkWiredInTyCon above
	; unless (mod == nameModule name)
		 (loadWiredInHomeIface name) }

tcIfaceTyCon :: IfaceTyCon -> IfL TyCon
tcIfaceTyCon IfaceIntTc       	= tcWiredInTyCon intTyCon
tcIfaceTyCon IfaceBoolTc      	= tcWiredInTyCon boolTyCon
tcIfaceTyCon IfaceCharTc      	= tcWiredInTyCon charTyCon
tcIfaceTyCon IfaceListTc      	= tcWiredInTyCon listTyCon
tcIfaceTyCon IfacePArrTc      	= tcWiredInTyCon parrTyCon
tcIfaceTyCon (IfaceTupTc bx ar) = tcWiredInTyCon (tupleTyCon bx ar)
tcIfaceTyCon (IfaceTc name)     = do { thing <- tcIfaceGlobal name 
				     ; return (check_tc (tyThingTyCon thing)) }
  where
#ifdef DEBUG
    check_tc tc = case toIfaceTyCon tc of
		   IfaceTc _ -> tc
		   other     -> pprTrace "check_tc" (ppr tc) tc
#else
    check_tc tc = tc
#endif
-- we should be okay just returning Kind constructors without extra loading
tcIfaceTyCon IfaceLiftedTypeKindTc   = return liftedTypeKindTyCon
tcIfaceTyCon IfaceOpenTypeKindTc     = return openTypeKindTyCon
tcIfaceTyCon IfaceUnliftedTypeKindTc = return unliftedTypeKindTyCon
tcIfaceTyCon IfaceArgTypeKindTc      = return argTypeKindTyCon
tcIfaceTyCon IfaceUbxTupleKindTc     = return ubxTupleKindTyCon

-- Even though we are in an interface file, we want to make
-- sure the instances and RULES of this tycon are loaded 
-- Imagine: f :: Double -> Double
tcWiredInTyCon :: TyCon -> IfL TyCon
tcWiredInTyCon tc = do { ifCheckWiredInThing (tyConName tc)
		       ; return tc }

tcIfaceClass :: Name -> IfL Class
tcIfaceClass name = do { thing <- tcIfaceGlobal name
		       ; return (tyThingClass thing) }

tcIfaceDataCon :: Name -> IfL DataCon
tcIfaceDataCon name = do { thing <- tcIfaceGlobal name
		 	 ; case thing of
				ADataCon dc -> return dc
				other   -> pprPanic "tcIfaceExtDC" (ppr name$$ ppr thing) }

tcIfaceExtId :: Name -> IfL Id
tcIfaceExtId name = do { thing <- tcIfaceGlobal name
		       ; case thing of
			  AnId id -> return id
			  other   -> pprPanic "tcIfaceExtId" (ppr name$$ ppr thing) }
\end{code}

%************************************************************************
%*									*
		Bindings
%*									*
%************************************************************************

\begin{code}
bindIfaceBndr :: IfaceBndr -> (CoreBndr -> IfL a) -> IfL a
bindIfaceBndr (IfaceIdBndr (fs, ty)) thing_inside
  = do	{ name <- newIfaceName (mkVarOccFS fs)
	; ty' <- tcIfaceType ty
	; let id = mkLocalId name ty'
	; extendIfaceIdEnv [id] (thing_inside id) }
bindIfaceBndr (IfaceTvBndr bndr) thing_inside
  = bindIfaceTyVar bndr thing_inside
    
bindIfaceBndrs :: [IfaceBndr] -> ([CoreBndr] -> IfL a) -> IfL a
bindIfaceBndrs []     thing_inside = thing_inside []
bindIfaceBndrs (b:bs) thing_inside
  = bindIfaceBndr b	$ \ b' ->
    bindIfaceBndrs bs	$ \ bs' ->
    thing_inside (b':bs')

-----------------------
tcIfaceLetBndr (IfLetBndr fs ty info)
  = do	{ name <- newIfaceName (mkVarOccFS fs)
	; ty' <- tcIfaceType ty
	; case info of
		NoInfo    -> return (mkLocalId name ty')
		HasInfo i -> return (mkLocalIdWithInfo name ty' (tc_info i)) } 
  where
	-- Similar to tcIdInfo, but much simpler
    tc_info [] = vanillaIdInfo
    tc_info (HsInline p     : i) = tc_info i `setInlinePragInfo` p 
    tc_info (HsArity a      : i) = tc_info i `setArityInfo` a 
    tc_info (HsStrictness s : i) = tc_info i `setAllStrictnessInfo` Just s 
    tc_info (other          : i) = pprTrace "tcIfaceLetBndr: discarding unexpected IdInfo" 
					    (ppr other) (tc_info i)

-----------------------
newExtCoreBndr :: IfaceLetBndr -> IfL Id
newExtCoreBndr (IfLetBndr var ty _)	-- Ignoring IdInfo for now
  = do	{ mod <- getIfModule
	; name <- newGlobalBinder mod (mkVarOccFS var) noSrcSpan
	; ty' <- tcIfaceType ty
	; return (mkLocalId name ty') }

-----------------------
bindIfaceTyVar :: IfaceTvBndr -> (TyVar -> IfL a) -> IfL a
bindIfaceTyVar (occ,kind) thing_inside
  = do	{ name <- newIfaceName (mkTyVarOcc occ)
   	; tyvar <- mk_iface_tyvar name kind
	; extendIfaceTyVarEnv [tyvar] (thing_inside tyvar) }

bindIfaceTyVars :: [IfaceTvBndr] -> ([TyVar] -> IfL a) -> IfL a
bindIfaceTyVars bndrs thing_inside
  = do	{ names <- newIfaceNames (map mkTyVarOcc occs)
  	; tyvars <- TcRnMonad.zipWithM mk_iface_tyvar names kinds
	; extendIfaceTyVarEnv tyvars (thing_inside tyvars) }
  where
    (occs,kinds) = unzip bndrs

mk_iface_tyvar :: Name -> IfaceKind -> IfL TyVar
mk_iface_tyvar name ifKind
   = do { kind <- tcIfaceType ifKind
	; if isCoercionKind kind then 
		return (Var.mkCoVar name kind)
	  else
		return (Var.mkTyVar name kind) }
\end{code}

