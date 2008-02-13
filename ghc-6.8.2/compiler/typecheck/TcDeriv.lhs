%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Handles @deriving@ clauses on @data@ declarations.

\begin{code}
module TcDeriv ( tcDeriving ) where

#include "HsVersions.h"

import HsSyn
import DynFlags

import Generics
import TcRnMonad
import TcEnv
import TcClassDcl( tcAddDeclCtxt )	-- Small helper
import TcGenDeriv			-- Deriv stuff
import InstEnv
import Inst
import TcHsType
import TcMType
import TcSimplify

import RnBinds
import RnEnv
import HscTypes

import Class
import Type
import ErrUtils
import MkId
import DataCon
import Maybes
import RdrName
import Name
import NameSet
import TyCon
import TcType
import Var
import VarSet
import PrelNames
import SrcLoc
import Util
import ListSetOps
import Outputable
import Bag
\end{code}

%************************************************************************
%*									*
		Overview
%*									*
%************************************************************************

Overall plan
~~~~~~~~~~~~
1.  Convert the decls (i.e. data/newtype deriving clauses, 
    plus standalone deriving) to [EarlyDerivSpec]

2.  Infer the missing contexts for the Left DerivSpecs

3.  Add the derived bindings, generating InstInfos

\begin{code}
-- DerivSpec is purely  local to this module
data DerivSpec  = DS { ds_loc     :: SrcSpan 
		     , ds_orig    :: InstOrigin 
		     , ds_name    :: Name
		     , ds_tvs     :: [TyVar] 
		     , ds_theta   :: ThetaType
		     , ds_cls     :: Class
		     , ds_tys     :: [Type]
		     , ds_newtype :: Bool }
	-- This spec implies a dfun declaration of the form
	--	 df :: forall tvs. theta => C tys
	-- The Name is the name for the DFun we'll build
	-- The tyvars bind all the variables in the theta
	-- For family indexes, the tycon is the *family* tycon
	--		(not the representation tycon)

	-- ds_newtype = True  <=> Newtype deriving
	--		False <=> Vanilla deriving

type EarlyDerivSpec = Either DerivSpec DerivSpec
	-- Left  ds => the context for the instance should be inferred
	--	       In this case ds_theta is the list of all the 
	--		  constraints needed, such as (Eq [a], Eq a)
	--		  The inference process is to reduce this to a 
	--		  simpler form (e.g. Eq a)
	-- 
	-- Right ds => the exact context for the instance is supplied 
	--	       by the programmer; it is ds_theta

pprDerivSpec :: DerivSpec -> SDoc
pprDerivSpec (DS { ds_loc = l, ds_name = n, ds_tvs = tvs, 
		   ds_cls = c, ds_tys = tys, ds_theta = rhs })
  = parens (hsep [ppr l, ppr n, ppr tvs, ppr c, ppr tys]
	    <+> equals <+> ppr rhs)
\end{code}


Inferring missing contexts 
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

	data T a b = C1 (Foo a) (Bar b)
		   | C2 Int (T b a)
		   | C3 (T a a)
		   deriving (Eq)

[NOTE: See end of these comments for what to do with 
	data (C a, D b) => T a b = ...
]

We want to come up with an instance declaration of the form

	instance (Ping a, Pong b, ...) => Eq (T a b) where
		x == y = ...

It is pretty easy, albeit tedious, to fill in the code "...".  The
trick is to figure out what the context for the instance decl is,
namely @Ping@, @Pong@ and friends.

Let's call the context reqd for the T instance of class C at types
(a,b, ...)  C (T a b).  Thus:

	Eq (T a b) = (Ping a, Pong b, ...)

Now we can get a (recursive) equation from the @data@ decl:

	Eq (T a b) = Eq (Foo a) u Eq (Bar b)	-- From C1
		   u Eq (T b a) u Eq Int	-- From C2
		   u Eq (T a a)			-- From C3

Foo and Bar may have explicit instances for @Eq@, in which case we can
just substitute for them.  Alternatively, either or both may have
their @Eq@ instances given by @deriving@ clauses, in which case they
form part of the system of equations.

Now all we need do is simplify and solve the equations, iterating to
find the least fixpoint.  Notice that the order of the arguments can
switch around, as here in the recursive calls to T.

Let's suppose Eq (Foo a) = Eq a, and Eq (Bar b) = Ping b.

We start with:

	Eq (T a b) = {}		-- The empty set

Next iteration:
	Eq (T a b) = Eq (Foo a) u Eq (Bar b)	-- From C1
		   u Eq (T b a) u Eq Int	-- From C2
		   u Eq (T a a)			-- From C3

	After simplification:
		   = Eq a u Ping b u {} u {} u {}
		   = Eq a u Ping b

Next iteration:

	Eq (T a b) = Eq (Foo a) u Eq (Bar b)	-- From C1
		   u Eq (T b a) u Eq Int	-- From C2
		   u Eq (T a a)			-- From C3

	After simplification:
		   = Eq a u Ping b
		   u (Eq b u Ping a)
		   u (Eq a u Ping a)

		   = Eq a u Ping b u Eq b u Ping a

The next iteration gives the same result, so this is the fixpoint.  We
need to make a canonical form of the RHS to ensure convergence.  We do
this by simplifying the RHS to a form in which

	- the classes constrain only tyvars
	- the list is sorted by tyvar (major key) and then class (minor key)
	- no duplicates, of course

So, here are the synonyms for the ``equation'' structures:


Note [Data decl contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

	data (RealFloat a) => Complex a = !a :+ !a deriving( Read )

We will need an instance decl like:

	instance (Read a, RealFloat a) => Read (Complex a) where
	  ...

The RealFloat in the context is because the read method for Complex is bound
to construct a Complex, and doing that requires that the argument type is
in RealFloat. 

But this ain't true for Show, Eq, Ord, etc, since they don't construct
a Complex; they only take them apart.

Our approach: identify the offending classes, and add the data type
context to the instance decl.  The "offending classes" are

	Read, Enum?

FURTHER NOTE ADDED March 2002.  In fact, Haskell98 now requires that
pattern matching against a constructor from a data type with a context
gives rise to the constraints for that context -- or at least the thinned
version.  So now all classes are "offending".

Note [Newtype deriving]
~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
    class C a b
    instance C [a] Char
    newtype T = T Char deriving( C [a] )

Notice the free 'a' in the deriving.  We have to fill this out to 
    newtype T = T Char deriving( forall a. C [a] )

And then translate it to:
    instance C [a] Char => C [a] T where ...
    
	
Note [Newtype deriving superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The 'tys' here come from the partial application in the deriving
clause. The last arg is the new instance type.

We must pass the superclasses; the newtype might be an instance
of them in a different way than the representation type
E.g.		newtype Foo a = Foo a deriving( Show, Num, Eq )
Then the Show instance is not done via isomorphism; it shows
	Foo 3 as "Foo 3"
The Num instance is derived via isomorphism, but the Show superclass
dictionary must the Show instance for Foo, *not* the Show dictionary
gotten from the Num dictionary. So we must build a whole new dictionary
not just use the Num one.  The instance we want is something like:
     instance (Num a, Show (Foo a), Eq (Foo a)) => Num (Foo a) where
     	(+) = ((+)@a)
     	...etc...
There may be a coercion needed which we get from the tycon for the newtype
when the dict is constructed in TcInstDcls.tcInstDecl2




%************************************************************************
%*									*
\subsection[TcDeriv-driver]{Top-level function for \tr{derivings}}
%*									*
%************************************************************************

\begin{code}
tcDeriving  :: [LTyClDecl Name]  -- All type constructors
            -> [LInstDecl Name]  -- All instance declarations
            -> [LDerivDecl Name] -- All stand-alone deriving declarations
	    -> TcM ([InstInfo],		-- The generated "instance decls"
		    HsValBinds Name)	-- Extra generated top-level bindings

tcDeriving tycl_decls inst_decls deriv_decls
  = recoverM (returnM ([], emptyValBindsOut)) $
    do	{   	-- Fish the "deriving"-related information out of the TcEnv
		-- And make the necessary "equations".
	; early_specs <- makeDerivSpecs tycl_decls inst_decls deriv_decls

	; overlap_flag <- getOverlapFlag
	; let (infer_specs, given_specs) = splitEithers early_specs
	; (insts1, aux_binds1) <- mapAndUnzipM (genInst overlap_flag) given_specs

	; final_specs <- extendLocalInstEnv (map iSpec insts1) $
			 inferInstanceContexts overlap_flag infer_specs

	; (insts2, aux_binds2) <- mapAndUnzipM (genInst overlap_flag) final_specs

	; is_boot <- tcIsHsBoot
	; rn_binds <- makeAuxBinds is_boot tycl_decls
				   (concat aux_binds1 ++ concat aux_binds2)

	; let inst_info = insts1 ++ insts2

	; dflags <- getDOpts
	; ioToTcRn (dumpIfSet_dyn dflags Opt_D_dump_deriv "Derived instances" 
	  	   (ddump_deriving inst_info rn_binds))

	; return (inst_info, rn_binds) }
  where
    ddump_deriving :: [InstInfo] -> HsValBinds Name -> SDoc
    ddump_deriving inst_infos extra_binds
      = vcat (map pprInstInfoDetails inst_infos) $$ ppr extra_binds

makeAuxBinds :: Bool -> [LTyClDecl Name] -> DerivAuxBinds -> TcM (HsValBinds Name)
makeAuxBinds is_boot tycl_decls deriv_aux_binds
  | is_boot	-- If we are compiling a hs-boot file, 
		-- don't generate any derived bindings
  = return emptyValBindsOut

  | otherwise
  = do	{ let aux_binds = listToBag (map genAuxBind (rm_dups [] deriv_aux_binds))
		-- Generate any extra not-one-inst-decl-specific binds, 
		-- notably "con2tag" and/or "tag2con" functions.  

	-- Generate the generic to/from functions from each type declaration
	; gen_binds <- mkGenericBinds tycl_decls

	-- Rename these extra bindings, discarding warnings about unused bindings etc
	-- Type signatures in patterns are used in the generic binds
	; discardWarnings $
          setOptM Opt_PatternSignatures $
          do	{ (rn_deriv, _dus1) <- rnTopBinds (ValBindsIn aux_binds [])
		; (rn_gen, dus_gen) <- rnTopBinds (ValBindsIn gen_binds [])
		; keepAliveSetTc (duDefs dus_gen)	-- Mark these guys to
							-- be kept alive
		; return (rn_deriv `plusHsValBinds` rn_gen) } }
  where
	-- Remove duplicate requests for auxilliary bindings
    rm_dups acc [] = acc
    rm_dups acc (b:bs) | any (isDupAux b) acc = rm_dups acc bs
    		       | otherwise	      = rm_dups (b:acc) bs

-----------------------------------------
mkGenericBinds :: [LTyClDecl Name] -> TcM (LHsBinds RdrName)
mkGenericBinds tycl_decls
  = do	{ tcs <- mapM tcLookupTyCon 
			[ tc_name | 
			  L _ (TyData { tcdLName = L _ tc_name }) <- tycl_decls]
		-- We are only interested in the data type declarations
	; return (unionManyBags [ mkTyConGenericBinds tc | 
				  tc <- tcs, tyConHasGenerics tc ]) }
		-- And then only in the ones whose 'has-generics' flag is on
\end{code}


%************************************************************************
%*									*
		From HsSyn to DerivSpec
%*									*
%************************************************************************

@makeDerivSpecs@ fishes around to find the info about needed derived
instances.  Complicating factors:
\begin{itemize}
\item
We can only derive @Enum@ if the data type is an enumeration
type (all nullary data constructors).

\item
We can only derive @Ix@ if the data type is an enumeration {\em
or} has just one data constructor (e.g., tuples).
\end{itemize}

[See Appendix~E in the Haskell~1.2 report.] This code here deals w/
all those.

\begin{code}
makeDerivSpecs :: [LTyClDecl Name] 
               -> [LInstDecl Name]
	       -> [LDerivDecl Name] 
	       -> TcM [EarlyDerivSpec]

makeDerivSpecs tycl_decls inst_decls deriv_decls
  = do	{ eqns1 <- mapAndRecoverM deriveTyData $
                     extractTyDataPreds tycl_decls ++
		     [ pd                        -- traverse assoc data families
                     | L _ (InstDecl _ _ _ ats) <- inst_decls
		     , pd <- extractTyDataPreds ats ]
	; eqns2 <- mapAndRecoverM deriveStandalone deriv_decls
	; return (catMaybes (eqns1 ++ eqns2)) }
  where
    extractTyDataPreds decls = 		   
      [(p, d) | d@(L _ (TyData {tcdDerivs = Just preds})) <- decls, p <- preds]


------------------------------------------------------------------
deriveStandalone :: LDerivDecl Name -> TcM (Maybe EarlyDerivSpec)
-- Standalone deriving declarations
--  e.g.   deriving instance show a => Show (T a)
-- Rather like tcLocalInstDecl
deriveStandalone (L loc (DerivDecl deriv_ty))
  = setSrcSpan loc                   $
    addErrCtxt (standaloneCtxt deriv_ty)  $
    do { traceTc (text "standalone deriving decl for" <+> ppr deriv_ty)
       ; (tvs, theta, tau) <- tcHsInstHead deriv_ty
       ; traceTc (text "standalone deriving;"
              <+> text "tvs:" <+> ppr tvs
              <+> text "theta:" <+> ppr theta
              <+> text "tau:" <+> ppr tau)
       ; (cls, inst_tys) <- checkValidInstHead tau
       ; let cls_tys = take (length inst_tys - 1) inst_tys
             inst_ty = last inst_tys

       ; traceTc (text "standalone deriving;"
              <+> text "class:" <+> ppr cls
              <+> text "class types:" <+> ppr cls_tys
              <+> text "type:" <+> ppr inst_ty)
       ; mkEqnHelp StandAloneDerivOrigin tvs cls cls_tys inst_ty
                   (Just theta) }

------------------------------------------------------------------
deriveTyData :: (LHsType Name, LTyClDecl Name) -> TcM (Maybe EarlyDerivSpec)
deriveTyData (deriv_pred, L loc decl@(TyData { tcdLName = L _ tycon_name, 
					       tcdTyVars = tv_names, 
				    	       tcdTyPats = ty_pats }))
  = setSrcSpan loc                   $
    tcAddDeclCtxt decl		     $
    do	{ let hs_ty_args = ty_pats `orElse` map (nlHsTyVar . hsLTyVarName) tv_names
	      hs_app     = nlHsTyConApp tycon_name hs_ty_args
		-- We get kinding info for the tyvars by typechecking (T a b)
		-- Hence forming a tycon application and then dis-assembling it
	; (tvs, tc_app) <- tcHsQuantifiedType tv_names hs_app
	; tcExtendTyVarEnv tvs $	-- Deriving preds may (now) mention
					-- the type variables for the type constructor
    do	{ (deriv_tvs, cls, cls_tys) <- tcHsDeriv deriv_pred
		-- The "deriv_pred" is a LHsType to take account of the fact that for
		-- newtype deriving we allow deriving (forall a. C [a]).
	; mkEqnHelp DerivOrigin (tvs++deriv_tvs) cls cls_tys tc_app Nothing } }

deriveTyData _other
  = panic "derivTyData"	-- Caller ensures that only TyData can happen

------------------------------------------------------------------
mkEqnHelp :: InstOrigin -> [TyVar] -> Class -> [Type] -> Type
          -> Maybe ThetaType	-- Just    => context supplied (standalone deriving)
				-- Nothing => context inferred (deriving on data decl)
          -> TcRn (Maybe EarlyDerivSpec)
mkEqnHelp orig tvs cls cls_tys tc_app mtheta
  | Just (tycon, tc_args) <- tcSplitTyConApp_maybe tc_app
  = do	{
	-- For standalone deriving (mtheta /= Nothing), 
	-- check that all the data constructors are in scope
	-- By this time we know that the thing is algebraic
	--	because we've called checkInstHead in derivingStandalone
	  rdr_env <- getGlobalRdrEnv
	; let hidden_data_cons = filter not_in_scope (tyConDataCons tycon)
	      not_in_scope dc = null (lookupGRE_Name rdr_env (dataConName dc))
	; checkTc (isNothing mtheta || null hidden_data_cons) 
		  (derivingHiddenErr tycon)

	; mayDeriveDataTypeable <- doptM Opt_DeriveDataTypeable
	; newtype_deriving <- doptM Opt_GeneralizedNewtypeDeriving

	; (rep_tc, rep_tc_args) <- tcLookupFamInstExact tycon tc_args

          -- Be careful to test rep_tc here: in the case of families, we want
          -- to check the instance tycon, not the family tycon
	; if isDataTyCon rep_tc then
		mkDataTypeEqn orig mayDeriveDataTypeable tvs cls cls_tys 
			      tycon tc_args rep_tc rep_tc_args mtheta
	  else
		mkNewTypeEqn orig mayDeriveDataTypeable newtype_deriving
		             tvs cls cls_tys 
			     tycon tc_args rep_tc rep_tc_args mtheta }
  | otherwise
  = baleOut (derivingThingErr cls cls_tys tc_app
		(ptext SLIT("Last argument of the instance must be a type application")))

baleOut :: Message -> TcM (Maybe a)
baleOut err = do { addErrTc err;  return Nothing }
\end{code}

Auxiliary lookup wrapper which requires that looked up family instances are
not type instances.  If called with a vanilla tycon, the old type application
is simply returned.

\begin{code}
tcLookupFamInstExact :: TyCon -> [Type] -> TcM (TyCon, [Type])
tcLookupFamInstExact tycon tys
  | not (isOpenTyCon tycon)
  = return (tycon, tys)
  | otherwise
  = do { maybeFamInst <- tcLookupFamInst tycon tys
       ; case maybeFamInst of
           Nothing                     -> famInstNotFound tycon tys False
           Just famInst@(_, rep_tys)
             | not variable_only_subst -> famInstNotFound tycon tys True
             | otherwise               -> return famInst
             where
               tvs		    = map (Type.getTyVar 
                                             "TcDeriv.tcLookupFamInstExact") 
					  rep_tys
	       variable_only_subst  = all Type.isTyVarTy rep_tys &&
				      sizeVarSet (mkVarSet tvs) == length tvs
					-- renaming may have no repetitions
       }
\end{code}


%************************************************************************
%*									*
		Deriving data types
%*									*
%************************************************************************

\begin{code}
mkDataTypeEqn :: InstOrigin -> Bool -> [Var] -> Class -> [Type]
              -> TyCon -> [Type] -> TyCon -> [Type] -> Maybe ThetaType
              -> TcRn (Maybe EarlyDerivSpec)	-- Return 'Nothing' if error
		
mkDataTypeEqn orig mayDeriveDataTypeable tvs cls cls_tys
              tycon tc_args rep_tc rep_tc_args mtheta
  | Just err <- checkSideConditions mayDeriveDataTypeable cls cls_tys rep_tc
	-- NB: pass the *representation* tycon to checkSideConditions
  = baleOut (derivingThingErr cls cls_tys (mkTyConApp tycon tc_args) err)

  | otherwise 
  = ASSERT( null cls_tys )
    mk_data_eqn orig tvs cls tycon tc_args rep_tc rep_tc_args mtheta

mk_data_eqn, mk_typeable_eqn
   :: InstOrigin -> [TyVar] -> Class 
   -> TyCon -> [TcType] -> TyCon -> [TcType] -> Maybe ThetaType
   -> TcM (Maybe EarlyDerivSpec)
mk_data_eqn orig tvs cls tycon tc_args rep_tc rep_tc_args mtheta
  | getName cls `elem` typeableClassNames
  = mk_typeable_eqn orig tvs cls tycon tc_args rep_tc rep_tc_args mtheta

  | otherwise
  = do	{ dfun_name <- new_dfun_name cls tycon
  	; loc <- getSrcSpanM
	; let ordinary_constraints
	        = [ mkClassPred cls [arg_ty] 
	          | data_con <- tyConDataCons rep_tc,
	            arg_ty   <- ASSERT( isVanillaDataCon data_con )
				dataConInstOrigArgTys data_con rep_tc_args,
	            not (isUnLiftedType arg_ty) ] -- No constraints for unlifted types?

			-- See Note [Superclasses of derived instance]
	      sc_constraints = substTheta (zipOpenTvSubst (classTyVars cls) inst_tys)
					  (classSCTheta cls)
	      inst_tys =  [mkTyConApp tycon tc_args]

	      stupid_subst = zipTopTvSubst (tyConTyVars rep_tc) rep_tc_args
	      stupid_constraints = substTheta stupid_subst (tyConStupidTheta rep_tc)
	      all_constraints = stupid_constraints ++ sc_constraints ++ ordinary_constraints

	      spec = DS { ds_loc = loc, ds_orig = orig
			, ds_name = dfun_name, ds_tvs = tvs 
			, ds_cls = cls, ds_tys = inst_tys
			, ds_theta =  mtheta `orElse` all_constraints
			, ds_newtype = False }

  	; return (if isJust mtheta then Just (Right spec)	-- Specified context
				   else Just (Left spec)) }	-- Infer context

mk_typeable_eqn orig tvs cls tycon tc_args rep_tc _rep_tc_args mtheta
	-- The Typeable class is special in several ways
	-- 	  data T a b = ... deriving( Typeable )
	-- gives
	--	  instance Typeable2 T where ...
	-- Notice that:
	-- 1. There are no constraints in the instance
	-- 2. There are no type variables either
	-- 3. The actual class we want to generate isn't necessarily
	--	Typeable; it depends on the arity of the type
  | isNothing mtheta	-- deriving on a data type decl
  = do	{ checkTc (cls `hasKey` typeableClassKey)
		  (ptext SLIT("Use deriving( Typeable ) on a data type declaration"))
	; real_cls <- tcLookupClass (typeableClassNames !! tyConArity tycon)
	; mk_typeable_eqn orig tvs real_cls tycon [] rep_tc [] (Just []) }

  | otherwise		-- standaone deriving
  = do	{ checkTc (null tc_args)
		  (ptext SLIT("Derived typeable instance must be of form (Typeable") 
			<> int (tyConArity tycon) <+> ppr tycon <> rparen)
	; dfun_name <- new_dfun_name cls tycon
  	; loc <- getSrcSpanM
	; return (Just $ Right $
		  DS { ds_loc = loc, ds_orig = orig, ds_name = dfun_name, ds_tvs = []
		     , ds_cls = cls, ds_tys = [mkTyConApp tycon []] 
		     , ds_theta = mtheta `orElse` [], ds_newtype = False })  }

------------------------------------------------------------------
-- Check side conditions that dis-allow derivability for particular classes
-- This is *apart* from the newtype-deriving mechanism
--
-- Here we get the representation tycon in case of family instances as it has
-- the data constructors - but we need to be careful to fall back to the
-- family tycon (with indexes) in error messages.

checkSideConditions :: Bool -> Class -> [TcType] -> TyCon -> Maybe SDoc
checkSideConditions mayDeriveDataTypeable cls cls_tys rep_tc
  | notNull cls_tys	
  = Just ty_args_why	-- e.g. deriving( Foo s )
  | otherwise
  = case sideConditions cls of
	Just cond -> cond (mayDeriveDataTypeable, rep_tc)
	Nothing   -> Just non_std_why
  where
    ty_args_why	= quotes (ppr (mkClassPred cls cls_tys)) <+> ptext SLIT("is not a class")
    non_std_why = quotes (ppr cls) <+> ptext SLIT("is not a derivable class")

sideConditions :: Class -> Maybe Condition
sideConditions cls
  | cls_key == eqClassKey   = Just cond_std
  | cls_key == ordClassKey  = Just cond_std
  | cls_key == readClassKey = Just cond_std
  | cls_key == showClassKey = Just cond_std
  | cls_key == enumClassKey = Just (cond_std `andCond` cond_isEnumeration)
  | cls_key == ixClassKey   = Just (cond_std `andCond` (cond_isEnumeration `orCond` cond_isProduct))
  | cls_key == boundedClassKey = Just (cond_std `andCond` (cond_isEnumeration `orCond` cond_isProduct))
  | cls_key == dataClassKey    = Just (cond_mayDeriveDataTypeable `andCond` cond_std)
  | getName cls `elem` typeableClassNames = Just (cond_mayDeriveDataTypeable `andCond` cond_typeableOK)
  | otherwise = Nothing
  where
    cls_key = getUnique cls

type Condition = (Bool, TyCon) -> Maybe SDoc
	-- Bool is whether or not we are allowed to derive Data and Typeable
	-- TyCon is the *representation* tycon if the 
	--	data type is an indexed one
	-- Nothing => OK

orCond :: Condition -> Condition -> Condition
orCond c1 c2 tc 
  = case c1 tc of
	Nothing -> Nothing		-- c1 succeeds
	Just x  -> case c2 tc of	-- c1 fails
		     Nothing -> Nothing
		     Just y  -> Just (x $$ ptext SLIT("  and") $$ y)
					-- Both fail

andCond :: Condition -> Condition -> Condition
andCond c1 c2 tc = case c1 tc of
		     Nothing -> c2 tc	-- c1 succeeds
		     Just x  -> Just x	-- c1 fails

cond_std :: Condition
cond_std (_, rep_tc)
  | any (not . isVanillaDataCon) data_cons = Just existential_why     
  | null data_cons		    	   = Just no_cons_why
  | otherwise      			   = Nothing
  where
    data_cons       = tyConDataCons rep_tc
    no_cons_why	    = quotes (pprSourceTyCon rep_tc) <+> 
		      ptext SLIT("has no data constructors")
    existential_why = quotes (pprSourceTyCon rep_tc) <+> 
		      ptext SLIT("has non-Haskell-98 constructor(s)")
  
cond_isEnumeration :: Condition
cond_isEnumeration (_, rep_tc)
  | isEnumerationTyCon rep_tc = Nothing
  | otherwise		      = Just why
  where
    why = quotes (pprSourceTyCon rep_tc) <+> 
	  ptext SLIT("has non-nullary constructors")

cond_isProduct :: Condition
cond_isProduct (_, rep_tc)
  | isProductTyCon rep_tc = Nothing
  | otherwise	          = Just why
  where
    why = quotes (pprSourceTyCon rep_tc) <+> 
	  ptext SLIT("has more than one constructor")

cond_typeableOK :: Condition
-- OK for Typeable class
-- Currently: (a) args all of kind *
--	      (b) 7 or fewer args
cond_typeableOK (_, rep_tc)
  | tyConArity rep_tc > 7	= Just too_many
  | not (all (isSubArgTypeKind . tyVarKind) (tyConTyVars rep_tc)) 
                                = Just bad_kind
  | isFamInstTyCon rep_tc	= Just fam_inst  -- no Typable for family insts
  | otherwise	  		= Nothing
  where
    too_many = quotes (pprSourceTyCon rep_tc) <+> 
	       ptext SLIT("has too many arguments")
    bad_kind = quotes (pprSourceTyCon rep_tc) <+> 
	       ptext SLIT("has arguments of kind other than `*'")
    fam_inst = quotes (pprSourceTyCon rep_tc) <+> 
	       ptext SLIT("is a type family")

cond_mayDeriveDataTypeable :: Condition
cond_mayDeriveDataTypeable (mayDeriveDataTypeable, _)
 | mayDeriveDataTypeable = Nothing
 | otherwise = Just why
  where
    why  = ptext SLIT("You need -XDeriveDataTypeable to derive an instance for this class")

std_class_via_iso :: Class -> Bool
std_class_via_iso clas	-- These standard classes can be derived for a newtype
			-- using the isomorphism trick *even if no -fglasgow-exts*
  = classKey clas `elem`  [eqClassKey, ordClassKey, ixClassKey, boundedClassKey]
	-- Not Read/Show because they respect the type
	-- Not Enum, because newtypes are never in Enum


new_dfun_name :: Class -> TyCon -> TcM Name
new_dfun_name clas tycon 	-- Just a simple wrapper
  = newDFunName clas [mkTyConApp tycon []] (getSrcSpan tycon)
	-- The type passed to newDFunName is only used to generate
	-- a suitable string; hence the empty type arg list
\end{code}

Note [Superclasses of derived instance] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, a derived instance decl needs the superclasses of the derived
class too.  So if we have
	data T a = ...deriving( Ord )
then the initial context for Ord (T a) should include Eq (T a).  Often this is 
redundant; we'll also generate an Ord constraint for each constructor argument,
and that will probably generate enough constraints to make the Eq (T a) constraint 
be satisfied too.  But not always; consider:

 data S a = S
 instance Eq (S a)
 instance Ord (S a)

 data T a = MkT (S a) deriving( Ord )
 instance Num a => Eq (T a)

The derived instance for (Ord (T a)) must have a (Num a) constraint!
Similarly consider:
	data T a = MkT deriving( Data, Typeable )
Here there *is* no argument field, but we must nevertheless generate
a context for the Data instances:
	instance Typable a => Data (T a) where ...


%************************************************************************
%*									*
		Deriving newtypes
%*									*
%************************************************************************

\begin{code}
mkNewTypeEqn :: InstOrigin -> Bool -> Bool -> [Var] -> Class
             -> [Type] -> TyCon -> [Type] -> TyCon -> [Type]
             -> Maybe ThetaType
             -> TcRn (Maybe EarlyDerivSpec)
mkNewTypeEqn orig mayDeriveDataTypeable newtype_deriving tvs
             cls cls_tys tycon tc_args rep_tycon rep_tc_args mtheta
  | can_derive_via_isomorphism && (newtype_deriving || std_class_via_iso cls)
  = do	{ traceTc (text "newtype deriving:" <+> ppr tycon <+> ppr rep_tys)
	; dfun_name <- new_dfun_name cls tycon
  	; loc <- getSrcSpanM
	; let spec = DS { ds_loc = loc, ds_orig = orig
			, ds_name = dfun_name, ds_tvs = dict_tvs 
			, ds_cls = cls, ds_tys = inst_tys
			, ds_theta =  mtheta `orElse` all_preds
			, ds_newtype = True }
	; return (if isJust mtheta then Just (Right spec)
				   else Just (Left spec)) }

  | isNothing mb_std_err	-- Use the standard H98 method
  = mk_data_eqn orig tvs cls tycon tc_args rep_tycon rep_tc_args mtheta

  	-- Otherwise we can't derive
  | newtype_deriving = baleOut cant_derive_err -- Too hard
  | otherwise        = baleOut std_err		-- Just complain about being a non-std instance
  where
	mb_std_err = checkSideConditions mayDeriveDataTypeable cls cls_tys rep_tycon
	std_err = derivingThingErr cls cls_tys tc_app $
		  vcat [fromJust mb_std_err,
			ptext SLIT("Try -XGeneralizedNewtypeDeriving for GHC's newtype-deriving extension")]

	-- Here is the plan for newtype derivings.  We see
	--	  newtype T a1...an = MkT (t ak+1...an) deriving (.., C s1 .. sm, ...)
	-- where t is a type,
	-- 	 ak+1...an is a suffix of a1..an, and are all tyars
	--	 ak+1...an do not occur free in t, nor in the s1..sm
	-- 	 (C s1 ... sm) is a  *partial applications* of class C 
	--			with the last parameter missing
	--	 (T a1 .. ak) matches the kind of C's last argument
	--		(and hence so does t)
	--
	-- We generate the instance
	--	 instance forall ({a1..ak} u fvs(s1..sm)).
	--		  C s1 .. sm t => C s1 .. sm (T a1...ak)
	-- where T a1...ap is the partial application of 
	-- 	 the LHS of the correct kind and p >= k
	--
	--	NB: the variables below are:
	--		tc_tvs = [a1, ..., an]
	--		tyvars_to_keep = [a1, ..., ak]
	--		rep_ty = t ak .. an
	--		deriv_tvs = fvs(s1..sm) \ tc_tvs
	--		tys = [s1, ..., sm]
	--		rep_fn' = t
	--
	-- Running example: newtype T s a = MkT (ST s a) deriving( Monad )
	-- We generate the instance
	--	instance Monad (ST s) => Monad (T s) where 

	cls_tyvars = classTyVars cls
	kind = tyVarKind (last cls_tyvars)
		-- Kind of the thing we want to instance
		--   e.g. argument kind of Monad, *->*

	(arg_kinds, _) = splitKindFunTys kind
	n_args_to_drop = length arg_kinds	
		-- Want to drop 1 arg from (T s a) and (ST s a)
		-- to get 	instance Monad (ST s) => Monad (T s)

	-- Note [newtype representation]
	-- Need newTyConRhs *not* newTyConRep to get the representation 
	-- type, because the latter looks through all intermediate newtypes
	-- For example
	--	newtype B = MkB Int
	--	newtype A = MkA B deriving( Num )
	-- We want the Num instance of B, *not* the Num instance of Int,
	-- when making the Num instance of A!
	rep_ty		      = newTyConInstRhs rep_tycon rep_tc_args
	(rep_fn, rep_ty_args) = tcSplitAppTys rep_ty

	n_tyargs_to_keep = tyConArity tycon - n_args_to_drop
	dropped_tc_args = drop n_tyargs_to_keep tc_args
	dropped_tvs     = tyVarsOfTypes dropped_tc_args

	n_args_to_keep = length rep_ty_args - n_args_to_drop
	args_to_drop   = drop n_args_to_keep rep_ty_args
	args_to_keep   = take n_args_to_keep rep_ty_args

	rep_fn'  = mkAppTys rep_fn args_to_keep
	rep_tys  = cls_tys ++ [rep_fn']
	rep_pred = mkClassPred cls rep_tys
		-- rep_pred is the representation dictionary, from where
		-- we are gong to get all the methods for the newtype
		-- dictionary 

	tc_app = mkTyConApp tycon (take n_tyargs_to_keep tc_args)

    -- Next we figure out what superclass dictionaries to use
    -- See Note [Newtype deriving superclasses] above

	inst_tys = cls_tys ++ [tc_app]
	sc_theta = substTheta (zipOpenTvSubst cls_tyvars inst_tys)
			      (classSCTheta cls)

		-- If there are no tyvars, there's no need
		-- to abstract over the dictionaries we need
		-- Example: 	newtype T = MkT Int deriving( C )
		-- We get the derived instance
		--		instance C T
		-- rather than
		--		instance C Int => C T
	dict_tvs = filterOut (`elemVarSet` dropped_tvs) tvs
	all_preds = rep_pred : sc_theta		-- NB: rep_pred comes first

	-------------------------------------------------------------------
	--  Figuring out whether we can only do this newtype-deriving thing

	right_arity = length cls_tys + 1 == classArity cls

		-- Never derive Read,Show,Typeable,Data this way 
	non_iso_classes = [readClassKey, showClassKey, typeableClassKey, dataClassKey]
	can_derive_via_isomorphism
	   =  not (getUnique cls `elem` non_iso_classes)
	   && right_arity 			-- Well kinded;
						-- eg not: newtype T ... deriving( ST )
						--	because ST needs *2* type params
	   && n_tyargs_to_keep >= 0		-- Type constructor has right kind:
						-- eg not: newtype T = T Int deriving( Monad )
	   && n_args_to_keep   >= 0		-- Rep type has right kind: 
						-- eg not: newtype T a = T Int deriving( Monad )
	   && eta_ok				-- Eta reduction works
	   && not (isRecursiveTyCon tycon)	-- Does not work for recursive tycons:
						--	newtype A = MkA [A]
						-- Don't want
						--	instance Eq [A] => Eq A !!
			-- Here's a recursive newtype that's actually OK
			--	newtype S1 = S1 [T1 ()]
			--	newtype T1 a = T1 (StateT S1 IO a ) deriving( Monad )
			-- It's currently rejected.  Oh well.
			-- In fact we generate an instance decl that has method of form
			--	meth @ instTy = meth @ repTy
			-- (no coerce's).  We'd need a coerce if we wanted to handle
			-- recursive newtypes too

	-- Check that eta reduction is OK
	eta_ok = (args_to_drop `tcEqTypes` dropped_tc_args)
		-- (a) the dropped-off args are identical in the source and rep type
		--	  newtype T a b = MkT (S [a] b) deriving( Monad )
		--     Here the 'b' must be the same in the rep type (S [a] b)

	      && (tyVarsOfType rep_fn' `disjointVarSet` dropped_tvs)
		-- (b) the remaining type args do not mention any of the dropped
		--     type variables 

	      && (tyVarsOfTypes cls_tys `disjointVarSet` dropped_tvs)
		-- (c) the type class args do not mention any of the dropped type
		--     variables 

	      && all isTyVarTy dropped_tc_args
		-- (d) in case of newtype family instances, the eta-dropped
		--      arguments must be type variables (not more complex indexes)

	cant_derive_err = derivingThingErr cls cls_tys tc_app
				(vcat [ptext SLIT("even with cunning newtype deriving:"),
					if isRecursiveTyCon tycon then
					  ptext SLIT("the newtype may be recursive")
					else empty,
					if not right_arity then 
					  quotes (ppr (mkClassPred cls cls_tys)) <+> ptext SLIT("does not have arity 1")
					else empty,
					if not (n_tyargs_to_keep >= 0) then 
					  ptext SLIT("the type constructor has wrong kind")
					else if not (n_args_to_keep >= 0) then
					  ptext SLIT("the representation type has wrong kind")
					else if not eta_ok then 
					  ptext SLIT("the eta-reduction property does not hold")
					else empty
				      ])
\end{code}


%************************************************************************
%*									*
\subsection[TcDeriv-fixpoint]{Finding the fixed point of \tr{deriving} equations}
%*									*
%************************************************************************

A ``solution'' (to one of the equations) is a list of (k,TyVarTy tv)
terms, which is the final correct RHS for the corresponding original
equation.
\begin{itemize}
\item
Each (k,TyVarTy tv) in a solution constrains only a type
variable, tv.

\item
The (k,TyVarTy tv) pairs in a solution are canonically
ordered by sorting on type varible, tv, (major key) and then class, k,
(minor key)
\end{itemize}

\begin{code}
inferInstanceContexts :: OverlapFlag -> [DerivSpec] -> TcM [DerivSpec]

inferInstanceContexts _ [] = return []

inferInstanceContexts oflag infer_specs
  = do	{ traceTc (text "inferInstanceContexts" <+> vcat (map pprDerivSpec infer_specs))
	; iterate_deriv 1 initial_solutions }
  where
    ------------------------------------------------------------------
	-- The initial solutions for the equations claim that each
	-- instance has an empty context; this solution is certainly
	-- in canonical form.
    initial_solutions :: [ThetaType]
    initial_solutions = [ [] | _ <- infer_specs ]

    ------------------------------------------------------------------
	-- iterate_deriv calculates the next batch of solutions,
	-- compares it with the current one; finishes if they are the
	-- same, otherwise recurses with the new solutions.
	-- It fails if any iteration fails
    iterate_deriv :: Int -> [ThetaType] -> TcM [DerivSpec]
    iterate_deriv n current_solns
      | n > 20 	-- Looks as if we are in an infinite loop
		-- This can happen if we have -fallow-undecidable-instances
		-- (See TcSimplify.tcSimplifyDeriv.)
      = pprPanic "solveDerivEqns: probable loop" 
		 (vcat (map pprDerivSpec infer_specs) $$ ppr current_solns)
      | otherwise
      =	do { 	  -- Extend the inst info from the explicit instance decls
		  -- with the current set of solutions, and simplify each RHS
	     let inst_specs = zipWithEqual "add_solns" (mkInstance2 oflag)
					   current_solns infer_specs
	   ; new_solns <- checkNoErrs $
	     		  extendLocalInstEnv inst_specs $
	     		  mapM gen_soln infer_specs

	   ; if (current_solns == new_solns) then
		return [ spec { ds_theta = soln } 
                       | (spec, soln) <- zip infer_specs current_solns ]
	     else
		iterate_deriv (n+1) new_solns }

    ------------------------------------------------------------------
    gen_soln :: DerivSpec  -> TcM [PredType]
    gen_soln (DS { ds_loc = loc, ds_orig = orig, ds_tvs = tyvars 
		 , ds_cls = clas, ds_tys = inst_tys, ds_theta = deriv_rhs })
      = setSrcSpan loc	$
	addErrCtxt (derivInstCtxt clas inst_tys) $ 
	do { theta <- tcSimplifyDeriv orig tyvars deriv_rhs
	   	-- checkValidInstance tyvars theta clas inst_tys
		-- Not necessary; see Note [Exotic derived instance contexts]
		-- 		  in TcSimplify

		  -- Check for a bizarre corner case, when the derived instance decl should
		  -- have form 	instance C a b => D (T a) where ...
		  -- Note that 'b' isn't a parameter of T.  This gives rise to all sorts
		  -- of problems; in particular, it's hard to compare solutions for
		  -- equality when finding the fixpoint.  So I just rule it out for now.
 	   ; let tv_set = mkVarSet tyvars
	         weird_preds = [pred | pred <- theta, not (tyVarsOfPred pred `subVarSet` tv_set)]  
	   ; mapM_ (addErrTc . badDerivedPred) weird_preds	

		-- Claim: the result instance declaration is guaranteed valid
		-- Hence no need to call:
		--   checkValidInstance tyvars theta clas inst_tys
	   ; return (sortLe (<=) theta) }	-- Canonicalise before returning the solution

------------------------------------------------------------------
mkInstance1 :: OverlapFlag -> DerivSpec -> Instance
mkInstance1 overlap_flag spec = mkInstance2 overlap_flag (ds_theta spec) spec

mkInstance2 :: OverlapFlag -> ThetaType -> DerivSpec -> Instance
mkInstance2 overlap_flag theta
	    (DS { ds_name = dfun_name
		, ds_tvs = tyvars, ds_cls = clas, ds_tys = tys })
  = mkLocalInstance dfun overlap_flag
  where
    dfun = mkDictFunId dfun_name tyvars theta clas tys


extendLocalInstEnv :: [Instance] -> TcM a -> TcM a
-- Add new locally-defined instances; don't bother to check
-- for functional dependency errors -- that'll happen in TcInstDcls
extendLocalInstEnv dfuns thing_inside
 = do { env <- getGblEnv
      ; let  inst_env' = extendInstEnvList (tcg_inst_env env) dfuns 
	     env'      = env { tcg_inst_env = inst_env' }
      ; setGblEnv env' thing_inside }
\end{code}


%************************************************************************
%*									*
\subsection[TcDeriv-normal-binds]{Bindings for the various classes}
%*									*
%************************************************************************

After all the trouble to figure out the required context for the
derived instance declarations, all that's left is to chug along to
produce them.  They will then be shoved into @tcInstDecls2@, which
will do all its usual business.

There are lots of possibilities for code to generate.  Here are
various general remarks.

PRINCIPLES:
\begin{itemize}
\item
We want derived instances of @Eq@ and @Ord@ (both v common) to be
``you-couldn't-do-better-by-hand'' efficient.

\item
Deriving @Show@---also pretty common--- should also be reasonable good code.

\item
Deriving for the other classes isn't that common or that big a deal.
\end{itemize}

PRAGMATICS:

\begin{itemize}
\item
Deriving @Ord@ is done mostly with the 1.3 @compare@ method.

\item
Deriving @Eq@ also uses @compare@, if we're deriving @Ord@, too.

\item
We {\em normally} generate code only for the non-defaulted methods;
there are some exceptions for @Eq@ and (especially) @Ord@...

\item
Sometimes we use a @_con2tag_<tycon>@ function, which returns a data
constructor's numeric (@Int#@) tag.  These are generated by
@gen_tag_n_con_binds@, and the heuristic for deciding if one of
these is around is given by @hasCon2TagFun@.

The examples under the different sections below will make this
clearer.

\item
Much less often (really just for deriving @Ix@), we use a
@_tag2con_<tycon>@ function.  See the examples.

\item
We use the renamer!!!  Reason: we're supposed to be
producing @LHsBinds Name@ for the methods, but that means
producing correctly-uniquified code on the fly.  This is entirely
possible (the @TcM@ monad has a @UniqueSupply@), but it is painful.
So, instead, we produce @MonoBinds RdrName@ then heave 'em through
the renamer.  What a great hack!
\end{itemize}

\begin{code}
-- Generate the InstInfo for the required instance paired with the
--   *representation* tycon for that instance,
-- plus any auxiliary bindings required
--
-- Representation tycons differ from the tycon in the instance signature in
-- case of instances for indexed families.
--
genInst :: OverlapFlag -> DerivSpec -> TcM (InstInfo, DerivAuxBinds)
genInst oflag spec
  | ds_newtype spec
  = return (InstInfo { iSpec = mkInstance1 oflag spec 
		     , iBinds = NewTypeDerived }, [])

  | otherwise
  = do	{ fix_env <- getFixityEnv
	; let
	    inst		    = mkInstance1 oflag spec
	    (tyvars,_,clas,[ty])    = instanceHead inst
	    clas_nm		    = className clas
	    (visible_tycon, tyArgs) = tcSplitTyConApp ty 

          -- In case of a family instance, we need to use the representation
          -- tycon (after all, it has the data constructors)
        ; (tycon, _) <- tcLookupFamInstExact visible_tycon tyArgs
	; let (meth_binds, aux_binds) = genDerivBinds clas fix_env tycon

	-- Bring the right type variables into 
	-- scope, and rename the method binds
	-- It's a bit yukky that we return *renamed* InstInfo, but
	-- *non-renamed* auxiliary bindings
	; (rn_meth_binds, _fvs) <- discardWarnings $ 
				   bindLocalNames (map Var.varName tyvars) $
			 	   rnMethodBinds clas_nm (\_ -> []) [] meth_binds

	-- Build the InstInfo
	; return (InstInfo { iSpec = inst, 
		  	     iBinds = VanillaInst rn_meth_binds [] },
		  aux_binds)
        }

genDerivBinds :: Class -> FixityEnv -> TyCon -> (LHsBinds RdrName, DerivAuxBinds)
genDerivBinds clas fix_env tycon
  | className clas `elem` typeableClassNames
  = (gen_Typeable_binds tycon, [])

  | otherwise
  = case assocMaybe gen_list (getUnique clas) of
	Just gen_fn -> gen_fn tycon
	Nothing	    -> pprPanic "genDerivBinds: bad derived class" (ppr clas)
  where
    gen_list :: [(Unique, TyCon -> (LHsBinds RdrName, DerivAuxBinds))]
    gen_list = [(eqClassKey,       gen_Eq_binds)
 	       ,(ordClassKey,      gen_Ord_binds)
 	       ,(enumClassKey,     gen_Enum_binds)
 	       ,(boundedClassKey,  gen_Bounded_binds)
 	       ,(ixClassKey,       gen_Ix_binds)
 	       ,(showClassKey,     gen_Show_binds fix_env)
 	       ,(readClassKey,     gen_Read_binds fix_env)
	       ,(dataClassKey,     gen_Data_binds fix_env)
 	       ]
\end{code}


%************************************************************************
%*									*
\subsection[TcDeriv-taggery-Names]{What con2tag/tag2con functions are available?}
%*									*
%************************************************************************

\begin{code}
derivingThingErr :: Class -> [Type] -> Type -> Message -> Message
derivingThingErr clas tys ty why
  = sep [hsep [ptext SLIT("Can't make a derived instance of"), 
	       quotes (ppr pred)],
	 nest 2 (parens why)]
  where
    pred = mkClassPred clas (tys ++ [ty])

derivingHiddenErr :: TyCon -> SDoc
derivingHiddenErr tc
  = hang (ptext SLIT("The data constructors of") <+> quotes (ppr tc) <+> ptext SLIT("are not all in scope"))
       2 (ptext SLIT("so you cannot derive an instance for it"))

standaloneCtxt :: LHsType Name -> SDoc
standaloneCtxt ty = hang (ptext SLIT("In the stand-alone deriving instance for")) 
		       2 (quotes (ppr ty))

derivInstCtxt :: Class -> [Type] -> Message
derivInstCtxt clas inst_tys
  = ptext SLIT("When deriving the instance for") <+> parens (pprClassPred clas inst_tys)

badDerivedPred :: PredType -> Message
badDerivedPred pred
  = vcat [ptext SLIT("Can't derive instances where the instance context mentions"),
	  ptext SLIT("type variables that are not data type parameters"),
	  nest 2 (ptext SLIT("Offending constraint:") <+> ppr pred)]

famInstNotFound :: TyCon -> [Type] -> Bool -> TcM a
famInstNotFound tycon tys notExact
  = failWithTc (msg <+> quotes (pprTypeApp tycon (ppr tycon) tys))
  where
    msg = ptext $ if notExact
		  then SLIT("No family instance exactly matching")
		  else SLIT("More than one family instance for")
\end{code}
