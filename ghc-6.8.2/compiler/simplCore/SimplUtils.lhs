%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[SimplUtils]{The simplifier utilities}

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module SimplUtils (
	-- Rebuilding
	mkLam, mkCase, prepareAlts, bindCaseBndr,

	-- Inlining,
	preInlineUnconditionally, postInlineUnconditionally, 
	activeInline, activeRule, inlineMode,

	-- The continuation type
	SimplCont(..), DupFlag(..), LetRhsFlag(..), 
	contIsDupable, contResultType, contIsTrivial, contArgs, dropArgs, 
	countValArgs, countArgs, splitInlineCont,
	mkBoringStop, mkLazyArgStop, mkRhsStop, contIsRhsOrArg,
	interestingCallContext, interestingArgContext,

	interestingArg, mkArgInfo,
	
	abstractFloats
    ) where

#include "HsVersions.h"

import SimplEnv
import DynFlags
import StaticFlags
import CoreSyn
import qualified CoreSubst
import PprCore
import CoreFVs
import CoreUtils
import Literal	
import CoreUnfold
import MkId
import Name
import Id
import Var	( isCoVar )
import NewDemand
import SimplMonad
import Type	hiding( substTy )
import TyCon
import DataCon
import Unify	( dataConCannotMatch )
import VarSet
import BasicTypes
import Util
import Outputable
import List( nub )
\end{code}


%************************************************************************
%*									*
		The SimplCont type
%*									*
%************************************************************************

A SimplCont allows the simplifier to traverse the expression in a 
zipper-like fashion.  The SimplCont represents the rest of the expression,
"above" the point of interest.

You can also think of a SimplCont as an "evaluation context", using
that term in the way it is used for operational semantics. This is the
way I usually think of it, For example you'll often see a syntax for
evaluation context looking like
	C ::= []  |  C e   |  case C of alts  |  C `cast` co
That's the kind of thing we are doing here, and I use that syntax in
the comments.


Key points:
  * A SimplCont describes a *strict* context (just like 
    evaluation contexts do).  E.g. Just [] is not a SimplCont

  * A SimplCont describes a context that *does not* bind
    any variables.  E.g. \x. [] is not a SimplCont

\begin{code}
data SimplCont	
  = Stop		-- An empty context, or hole, []     
	OutType		-- Type of the result
	LetRhsFlag
	Bool		-- True <=> There is something interesting about
			--          the context, and hence the inliner
			--	    should be a bit keener (see interestingCallContext)
			-- Two cases:
			-- (a) This is the RHS of a thunk whose type suggests
			--     that update-in-place would be possible
			-- (b) This is an argument of a function that has RULES
			--     Inlining the call might allow the rule to fire

  | CoerceIt 		-- C `cast` co
	OutCoercion		-- The coercion simplified
	SimplCont

  | ApplyTo  		-- C arg
	DupFlag 
	InExpr SimplEnv		-- The argument and its static env
	SimplCont

  | Select   		-- case C of alts
	DupFlag 
	InId [InAlt] SimplEnv	-- The case binder, alts, and subst-env
	SimplCont

  -- The two strict forms have no DupFlag, because we never duplicate them
  | StrictBind 		-- (\x* \xs. e) C
	InId [InBndr]		-- let x* = [] in e 	
	InExpr SimplEnv		--	is a special case 
	SimplCont	

  | StrictArg 		-- e C
	OutExpr OutType		-- e and its type
	(Bool,[Bool])		-- Whether the function at the head of e has rules,
	SimplCont		--     plus strictness flags for further args

data LetRhsFlag = AnArg		-- It's just an argument not a let RHS
		| AnRhs 	-- It's the RHS of a let (so please float lets out of big lambdas)

instance Outputable LetRhsFlag where
  ppr AnArg = ptext SLIT("arg")
  ppr AnRhs = ptext SLIT("rhs")

instance Outputable SimplCont where
  ppr (Stop ty is_rhs _)  	     = ptext SLIT("Stop") <> brackets (ppr is_rhs) <+> ppr ty
  ppr (ApplyTo dup arg se cont)      = ((ptext SLIT("ApplyTo") <+> ppr dup <+> pprParendExpr arg)
					  {-  $$ nest 2 (pprSimplEnv se) -}) $$ ppr cont
  ppr (StrictBind b _ _ _ cont)      = (ptext SLIT("StrictBind") <+> ppr b) $$ ppr cont
  ppr (StrictArg f _ _ cont)         = (ptext SLIT("StrictArg") <+> ppr f) $$ ppr cont
  ppr (Select dup bndr alts se cont) = (ptext SLIT("Select") <+> ppr dup <+> ppr bndr) $$ 
				       (nest 4 (ppr alts)) $$ ppr cont 
  ppr (CoerceIt co cont)	     = (ptext SLIT("CoerceIt") <+> ppr co) $$ ppr cont

data DupFlag = OkToDup | NoDup

instance Outputable DupFlag where
  ppr OkToDup = ptext SLIT("ok")
  ppr NoDup   = ptext SLIT("nodup")



-------------------
mkBoringStop :: OutType -> SimplCont
mkBoringStop ty = Stop ty AnArg False

mkLazyArgStop :: OutType -> Bool -> SimplCont
mkLazyArgStop ty has_rules = Stop ty AnArg (canUpdateInPlace ty || has_rules)

mkRhsStop :: OutType -> SimplCont
mkRhsStop ty = Stop ty AnRhs (canUpdateInPlace ty)

-------------------
contIsRhsOrArg (Stop {})       	 = True
contIsRhsOrArg (StrictBind {}) 	 = True
contIsRhsOrArg (StrictArg {})  	 = True
contIsRhsOrArg other	         = False

-------------------
contIsDupable :: SimplCont -> Bool
contIsDupable (Stop {})     		 = True
contIsDupable (ApplyTo  OkToDup _ _ _)   = True
contIsDupable (Select   OkToDup _ _ _ _) = True
contIsDupable (CoerceIt _ cont)          = contIsDupable cont
contIsDupable other	  	         = False

-------------------
contIsTrivial :: SimplCont -> Bool
contIsTrivial (Stop {})			  = True
contIsTrivial (ApplyTo _ (Type _) _ cont) = contIsTrivial cont
contIsTrivial (CoerceIt _ cont) 	  = contIsTrivial cont
contIsTrivial other			  = False

-------------------
contResultType :: SimplCont -> OutType
contResultType (Stop to_ty _ _)	      	 = to_ty
contResultType (StrictArg _ _ _ cont) 	 = contResultType cont
contResultType (StrictBind _ _ _ _ cont) = contResultType cont
contResultType (ApplyTo _ _ _ cont)   	 = contResultType cont
contResultType (CoerceIt _ cont)      	 = contResultType cont
contResultType (Select _ _ _ _ cont)  	 = contResultType cont

-------------------
countValArgs :: SimplCont -> Int
countValArgs (ApplyTo _ (Type ty) se cont) = countValArgs cont
countValArgs (ApplyTo _ val_arg   se cont) = 1 + countValArgs cont
countValArgs other			   = 0

countArgs :: SimplCont -> Int
countArgs (ApplyTo _ arg se cont) = 1 + countArgs cont
countArgs other			  = 0

contArgs :: SimplCont -> ([OutExpr], SimplCont)
-- Uses substitution to turn each arg into an OutExpr
contArgs cont = go [] cont
  where
    go args (ApplyTo _ arg se cont) = go (substExpr se arg : args) cont
    go args cont		    = (reverse args, cont)

dropArgs :: Int -> SimplCont -> SimplCont
dropArgs 0 cont = cont
dropArgs n (ApplyTo _ _ _ cont) = dropArgs (n-1) cont
dropArgs n other		= pprPanic "dropArgs" (ppr n <+> ppr other)

--------------------
splitInlineCont :: SimplCont -> Maybe (SimplCont, SimplCont)
-- Returns Nothing if the continuation should dissolve an InlineMe Note
-- Return Just (c1,c2) otherwise, 
--	where c1 is the continuation to put inside the InlineMe 
--	and   c2 outside

-- Example: (__inline_me__ (/\a. e)) ty
--	Here we want to do the beta-redex without dissolving the InlineMe
-- See test simpl017 (and Trac #1627) for a good example of why this is important

splitInlineCont (ApplyTo dup (Type ty) se c)
  | Just (c1, c2) <- splitInlineCont c 	        = Just (ApplyTo dup (Type ty) se c1, c2)
splitInlineCont cont@(Stop ty _ _)       	= Just (mkBoringStop ty, cont)
splitInlineCont cont@(StrictBind bndr _ _ se _) = Just (mkBoringStop (substTy se (idType bndr)), cont)
splitInlineCont cont@(StrictArg _ fun_ty _ _)   = Just (mkBoringStop (funArgTy fun_ty), cont)
splitInlineCont other				= Nothing
	-- NB: the calculation of the type for mkBoringStop is an annoying
	--     duplication of the same calucation in mkDupableCont
\end{code}


\begin{code}
interestingArg :: OutExpr -> Bool
	-- An argument is interesting if it has *some* structure
	-- We are here trying to avoid unfolding a function that
	-- is applied only to variables that have no unfolding
	-- (i.e. they are probably lambda bound): f x y z
	-- There is little point in inlining f here.
interestingArg (Var v)	         = hasSomeUnfolding (idUnfolding v)
					-- Was: isValueUnfolding (idUnfolding v')
					-- But that seems over-pessimistic
				 || isDataConWorkId v
					-- This accounts for an argument like
					-- () or [], which is definitely interesting
interestingArg (Type _)	         = False
interestingArg (App fn (Type _)) = interestingArg fn
interestingArg (Note _ a)	 = interestingArg a

-- Idea (from Sam B); I'm not sure if it's a good idea, so commented out for now
-- interestingArg expr | isUnLiftedType (exprType expr)
--        -- Unlifted args are only ever interesting if we know what they are
--  =                  case expr of
--                        Lit lit -> True
--                        _       -> False

interestingArg other	         = True
	-- Consider 	let x = 3 in f x
	-- The substitution will contain (x -> ContEx 3), and we want to
	-- to say that x is an interesting argument.
	-- But consider also (\x. f x y) y
	-- The substitution will contain (x -> ContEx y), and we want to say
	-- that x is not interesting (assuming y has no unfolding)
\end{code}


Comment about interestingCallContext
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to avoid inlining an expression where there can't possibly be
any gain, such as in an argument position.  Hence, if the continuation
is interesting (eg. a case scrutinee, application etc.) then we
inline, otherwise we don't.  

Previously some_benefit used to return True only if the variable was
applied to some value arguments.  This didn't work:

	let x = _coerce_ (T Int) Int (I# 3) in
	case _coerce_ Int (T Int) x of
		I# y -> ....

we want to inline x, but can't see that it's a constructor in a case
scrutinee position, and some_benefit is False.

Another example:

dMonadST = _/\_ t -> :Monad (g1 _@_ t, g2 _@_ t, g3 _@_ t)

....  case dMonadST _@_ x0 of (a,b,c) -> ....

we'd really like to inline dMonadST here, but we *don't* want to
inline if the case expression is just

	case x of y { DEFAULT -> ... }

since we can just eliminate this case instead (x is in WHNF).  Similar
applies when x is bound to a lambda expression.  Hence
contIsInteresting looks for case expressions with just a single
default case.

\begin{code}
interestingCallContext :: Bool 		-- False <=> no args at all
		       -> Bool		-- False <=> no value args
		       -> SimplCont -> Bool
	-- The "lone-variable" case is important.  I spent ages
	-- messing about with unsatisfactory varaints, but this is nice.
	-- The idea is that if a variable appear all alone
	--	as an arg of lazy fn, or rhs	Stop
	-- 	as scrutinee of a case		Select
	--	as arg of a strict fn		ArgOf
	-- then we should not inline it (unless there is some other reason,
	-- e.g. is is the sole occurrence).  We achieve this by making
	-- interestingCallContext return False for a lone variable.
	--
	-- Why?  At least in the case-scrutinee situation, turning
	--	let x = (a,b) in case x of y -> ...
	-- into
	--	let x = (a,b) in case (a,b) of y -> ...
	-- and thence to 
	--	let x = (a,b) in let y = (a,b) in ...
	-- is bad if the binding for x will remain.
	--
	-- Another example: I discovered that strings
	-- were getting inlined straight back into applications of 'error'
	-- because the latter is strict.
	--	s = "foo"
	--	f = \x -> ...(error s)...

	-- Fundamentally such contexts should not ecourage inlining because
	-- the context can ``see'' the unfolding of the variable (e.g. case or a RULE)
	-- so there's no gain.
	--
	-- However, even a type application or coercion isn't a lone variable.
	-- Consider
	--	case $fMonadST @ RealWorld of { :DMonad a b c -> c }
	-- We had better inline that sucker!  The case won't see through it.
	--
	-- For now, I'm treating treating a variable applied to types 
	-- in a *lazy* context "lone". The motivating example was
	--	f = /\a. \x. BIG
	--	g = /\a. \y.  h (f a)
	-- There's no advantage in inlining f here, and perhaps
	-- a significant disadvantage.  Hence some_val_args in the Stop case

interestingCallContext some_args some_val_args cont
  = interesting cont
  where
    interesting (Select {})              = some_args
    interesting (ApplyTo {})             = True	-- Can happen if we have (coerce t (f x)) y
						-- Perhaps True is a bit over-keen, but I've
						-- seen (coerce f) x, where f has an INLINE prag,
						-- So we have to give some motivaiton for inlining it
    interesting (StrictArg {})	         = some_val_args
    interesting (StrictBind {})	         = some_val_args	-- ??
    interesting (Stop ty _ interesting)  = some_val_args && interesting
    interesting (CoerceIt _ cont)        = interesting cont
	-- If this call is the arg of a strict function, the context
	-- is a bit interesting.  If we inline here, we may get useful
	-- evaluation information to avoid repeated evals: e.g.
	--	x + (y * z)
	-- Here the contIsInteresting makes the '*' keener to inline,
	-- which in turn exposes a constructor which makes the '+' inline.
	-- Assuming that +,* aren't small enough to inline regardless.
	--
	-- It's also very important to inline in a strict context for things
	-- like
	--		foldr k z (f x)
	-- Here, the context of (f x) is strict, and if f's unfolding is
	-- a build it's *great* to inline it here.  So we must ensure that
	-- the context for (f x) is not totally uninteresting.


-------------------
mkArgInfo :: Id
	  -> Int	-- Number of value args
	  -> SimplCont	-- Context of the cal
	  -> (Bool, [Bool])	-- Arg info
-- The arg info consists of
--  * A Bool indicating if the function has rules (recursively)
--  * A [Bool] indicating strictness for each arg
-- The [Bool] is usually infinite, but if it is finite it 
-- guarantees that the function diverges after being given
-- that number of args

mkArgInfo fun n_val_args call_cont
  = (interestingArgContext fun call_cont, fun_stricts)
  where
    vanilla_stricts, fun_stricts :: [Bool]
    vanilla_stricts  = repeat False

    fun_stricts
      = case splitStrictSig (idNewStrictness fun) of
	  (demands, result_info)
		| not (demands `lengthExceeds` n_val_args)
		-> 	-- Enough args, use the strictness given.
			-- For bottoming functions we used to pretend that the arg
			-- is lazy, so that we don't treat the arg as an
			-- interesting context.  This avoids substituting
			-- top-level bindings for (say) strings into 
			-- calls to error.  But now we are more careful about
			-- inlining lone variables, so its ok (see SimplUtils.analyseCont)
		   if isBotRes result_info then
			map isStrictDmd demands		-- Finite => result is bottom
		   else
			map isStrictDmd demands ++ vanilla_stricts

	  other -> vanilla_stricts	-- Not enough args, or no strictness

interestingArgContext :: Id -> SimplCont -> Bool
-- If the argument has form (f x y), where x,y are boring,
-- and f is marked INLINE, then we don't want to inline f.
-- But if the context of the argument is
--	g (f x y) 
-- where g has rules, then we *do* want to inline f, in case it
-- exposes a rule that might fire.  Similarly, if the context is
--	h (g (f x x))
-- where h has rules, then we do want to inline f.
-- The interesting_arg_ctxt flag makes this happen; if it's
-- set, the inliner gets just enough keener to inline f 
-- regardless of how boring f's arguments are, if it's marked INLINE
--
-- The alternative would be to *always* inline an INLINE function,
-- regardless of how boring its context is; but that seems overkill
-- For example, it'd mean that wrapper functions were always inlined
interestingArgContext fn cont
  = idHasRules fn || go cont
  where
    go (Select {})	      = False
    go (ApplyTo {})	      = False
    go (StrictArg {})	      = True
    go (StrictBind {})	      = False	-- ??
    go (CoerceIt _ c)	      = go c
    go (Stop _ _ interesting) = interesting

-------------------
canUpdateInPlace :: Type -> Bool
-- Consider   let x = <wurble> in ...
-- If <wurble> returns an explicit constructor, we might be able
-- to do update in place.  So we treat even a thunk RHS context
-- as interesting if update in place is possible.  We approximate
-- this by seeing if the type has a single constructor with a
-- small arity.  But arity zero isn't good -- we share the single copy
-- for that case, so no point in sharing.

canUpdateInPlace ty 
  | not opt_UF_UpdateInPlace = False
  | otherwise
  = case splitTyConApp_maybe ty of 
	Nothing		-> False 
	Just (tycon, _) -> case tyConDataCons_maybe tycon of
				Just [dc]  -> arity == 1 || arity == 2
				           where
					      arity = dataConRepArity dc
				other -> False
\end{code}



%************************************************************************
%*									*
\subsection{Decisions about inlining}
%*									*
%************************************************************************

Inlining is controlled partly by the SimplifierMode switch.  This has two
settings:

	SimplGently	(a) Simplifying before specialiser/full laziness
			(b) Simplifiying inside INLINE pragma
			(c) Simplifying the LHS of a rule
			(d) Simplifying a GHCi expression or Template 
				Haskell splice

	SimplPhase n	Used at all other times

The key thing about SimplGently is that it does no call-site inlining.
Before full laziness we must be careful not to inline wrappers,
because doing so inhibits floating
    e.g. ...(case f x of ...)...
    ==> ...(case (case x of I# x# -> fw x#) of ...)...
    ==> ...(case x of I# x# -> case fw x# of ...)...
and now the redex (f x) isn't floatable any more.

The no-inlining thing is also important for Template Haskell.  You might be 
compiling in one-shot mode with -O2; but when TH compiles a splice before
running it, we don't want to use -O2.  Indeed, we don't want to inline
anything, because the byte-code interpreter might get confused about 
unboxed tuples and suchlike.

INLINE pragmas
~~~~~~~~~~~~~~
SimplGently is also used as the mode to simplify inside an InlineMe note.

\begin{code}
inlineMode :: SimplifierMode
inlineMode = SimplGently
\end{code}

It really is important to switch off inlinings inside such
expressions.  Consider the following example 

     	let f = \pq -> BIG
     	in
     	let g = \y -> f y y
	    {-# INLINE g #-}
     	in ...g...g...g...g...g...

Now, if that's the ONLY occurrence of f, it will be inlined inside g,
and thence copied multiple times when g is inlined.


This function may be inlinined in other modules, so we
don't want to remove (by inlining) calls to functions that have
specialisations, or that may have transformation rules in an importing
scope.

E.g. 	{-# INLINE f #-}
		f x = ...g...

and suppose that g is strict *and* has specialisations.  If we inline
g's wrapper, we deny f the chance of getting the specialised version
of g when f is inlined at some call site (perhaps in some other
module).

It's also important not to inline a worker back into a wrapper.
A wrapper looks like
	wraper = inline_me (\x -> ...worker... )
Normally, the inline_me prevents the worker getting inlined into
the wrapper (initially, the worker's only call site!).  But,
if the wrapper is sure to be called, the strictness analyser will
mark it 'demanded', so when the RHS is simplified, it'll get an ArgOf
continuation.  That's why the keep_inline predicate returns True for
ArgOf continuations.  It shouldn't do any harm not to dissolve the
inline-me note under these circumstances.

Note that the result is that we do very little simplification
inside an InlineMe.  

	all xs = foldr (&&) True xs
	any p = all . map p  {-# INLINE any #-}

Problem: any won't get deforested, and so if it's exported and the
importer doesn't use the inlining, (eg passes it as an arg) then we
won't get deforestation at all.  We havn't solved this problem yet!


preInlineUnconditionally
~~~~~~~~~~~~~~~~~~~~~~~~
@preInlineUnconditionally@ examines a bndr to see if it is used just
once in a completely safe way, so that it is safe to discard the
binding inline its RHS at the (unique) usage site, REGARDLESS of how
big the RHS might be.  If this is the case we don't simplify the RHS
first, but just inline it un-simplified.

This is much better than first simplifying a perhaps-huge RHS and then
inlining and re-simplifying it.  Indeed, it can be at least quadratically
better.  Consider

	x1 = e1
	x2 = e2[x1]
	x3 = e3[x2]
	...etc...
	xN = eN[xN-1]

We may end up simplifying e1 N times, e2 N-1 times, e3 N-3 times etc.
This can happen with cascades of functions too:

	f1 = \x1.e1
	f2 = \xs.e2[f1]
	f3 = \xs.e3[f3]
	...etc...

THE MAIN INVARIANT is this:

	----  preInlineUnconditionally invariant -----
   IF preInlineUnconditionally chooses to inline x = <rhs>
   THEN doing the inlining should not change the occurrence
	info for the free vars of <rhs>
	----------------------------------------------

For example, it's tempting to look at trivial binding like
	x = y
and inline it unconditionally.  But suppose x is used many times,
but this is the unique occurrence of y.  Then inlining x would change
y's occurrence info, which breaks the invariant.  It matters: y
might have a BIG rhs, which will now be dup'd at every occurrenc of x.


Evne RHSs labelled InlineMe aren't caught here, because there might be
no benefit from inlining at the call site.

[Sept 01] Don't unconditionally inline a top-level thing, because that
can simply make a static thing into something built dynamically.  E.g.
	x = (a,b)
	main = \s -> h x

[Remember that we treat \s as a one-shot lambda.]  No point in
inlining x unless there is something interesting about the call site.

But watch out: if you aren't careful, some useful foldr/build fusion
can be lost (most notably in spectral/hartel/parstof) because the
foldr didn't see the build.  Doing the dynamic allocation isn't a big
deal, in fact, but losing the fusion can be.  But the right thing here
seems to be to do a callSiteInline based on the fact that there is
something interesting about the call site (it's strict).  Hmm.  That
seems a bit fragile.

Conclusion: inline top level things gaily until Phase 0 (the last
phase), at which point don't.

\begin{code}
preInlineUnconditionally :: SimplEnv -> TopLevelFlag -> InId -> InExpr -> Bool
preInlineUnconditionally env top_lvl bndr rhs
  | not active 		   = False
  | opt_SimplNoPreInlining = False
  | otherwise = case idOccInfo bndr of
		  IAmDead	     	     -> True	-- Happens in ((\x.1) v)
	  	  OneOcc in_lam True int_cxt -> try_once in_lam int_cxt
		  other 		     -> False
  where
    phase = getMode env
    active = case phase of
		   SimplGently  -> isAlwaysActive prag
		   SimplPhase n -> isActive n prag
    prag = idInlinePragma bndr

    try_once in_lam int_cxt	-- There's one textual occurrence
	| not in_lam = isNotTopLevel top_lvl || early_phase
	| otherwise  = int_cxt && canInlineInLam rhs

-- Be very careful before inlining inside a lambda, becuase (a) we must not 
-- invalidate occurrence information, and (b) we want to avoid pushing a
-- single allocation (here) into multiple allocations (inside lambda).  
-- Inlining a *function* with a single *saturated* call would be ok, mind you.
--	|| (if is_cheap && not (canInlineInLam rhs) then pprTrace "preinline" (ppr bndr <+> ppr rhs) ok else ok)
--	where 
--	 	is_cheap = exprIsCheap rhs
--		ok = is_cheap && int_cxt

	-- 	int_cxt		The context isn't totally boring
	-- E.g. let f = \ab.BIG in \y. map f xs
	-- 	Don't want to substitute for f, because then we allocate
	--	its closure every time the \y is called
	-- But: let f = \ab.BIG in \y. map (f y) xs
	--	Now we do want to substitute for f, even though it's not 
	--	saturated, because we're going to allocate a closure for 
	--	(f y) every time round the loop anyhow.

	-- canInlineInLam => free vars of rhs are (Once in_lam) or Many,
	-- so substituting rhs inside a lambda doesn't change the occ info.
	-- Sadly, not quite the same as exprIsHNF.
    canInlineInLam (Lit l)		= True
    canInlineInLam (Lam b e)		= isRuntimeVar b || canInlineInLam e
    canInlineInLam (Note _ e)		= canInlineInLam e
    canInlineInLam _			= False

    early_phase = case phase of
			SimplPhase 0 -> False
			other	     -> True
-- If we don't have this early_phase test, consider
--	x = length [1,2,3]
-- The full laziness pass carefully floats all the cons cells to
-- top level, and preInlineUnconditionally floats them all back in.
-- Result is (a) static allocation replaced by dynamic allocation
--	     (b) many simplifier iterations because this tickles
--		 a related problem; only one inlining per pass
-- 
-- On the other hand, I have seen cases where top-level fusion is
-- lost if we don't inline top level thing (e.g. string constants)
-- Hence the test for phase zero (which is the phase for all the final
-- simplifications).  Until phase zero we take no special notice of
-- top level things, but then we become more leery about inlining
-- them.  

\end{code}

postInlineUnconditionally
~~~~~~~~~~~~~~~~~~~~~~~~~
@postInlineUnconditionally@ decides whether to unconditionally inline
a thing based on the form of its RHS; in particular if it has a
trivial RHS.  If so, we can inline and discard the binding altogether.

NB: a loop breaker has must_keep_binding = True and non-loop-breakers
only have *forward* references Hence, it's safe to discard the binding
	
NOTE: This isn't our last opportunity to inline.  We're at the binding
site right now, and we'll get another opportunity when we get to the
ocurrence(s)

Note that we do this unconditional inlining only for trival RHSs.
Don't inline even WHNFs inside lambdas; doing so may simply increase
allocation when the function is called. This isn't the last chance; see
NOTE above.

NB: Even inline pragmas (e.g. IMustBeINLINEd) are ignored here Why?
Because we don't even want to inline them into the RHS of constructor
arguments. See NOTE above

NB: At one time even NOINLINE was ignored here: if the rhs is trivial
it's best to inline it anyway.  We often get a=E; b=a from desugaring,
with both a and b marked NOINLINE.  But that seems incompatible with
our new view that inlining is like a RULE, so I'm sticking to the 'active'
story for now.

\begin{code}
postInlineUnconditionally 
    :: SimplEnv -> TopLevelFlag
    -> InId		-- The binder (an OutId would be fine too)
    -> OccInfo 		-- From the InId
    -> OutExpr
    -> Unfolding
    -> Bool
postInlineUnconditionally env top_lvl bndr occ_info rhs unfolding
  | not active		   = False
  | isLoopBreaker occ_info = False	-- If it's a loop-breaker of any kind, dont' inline
					-- because it might be referred to "earlier"
  | isExportedId bndr      = False
  | exprIsTrivial rhs 	   = True
  | otherwise
  = case occ_info of
	-- The point of examining occ_info here is that for *non-values* 
	-- that occur outside a lambda, the call-site inliner won't have
	-- a chance (becuase it doesn't know that the thing
	-- only occurs once).   The pre-inliner won't have gotten
	-- it either, if the thing occurs in more than one branch
	-- So the main target is things like
	--	let x = f y in
	--	case v of
	--	   True  -> case x of ...
	--	   False -> case x of ...
	-- I'm not sure how important this is in practice
      OneOcc in_lam one_br int_cxt	-- OneOcc => no code-duplication issue
	->     smallEnoughToInline unfolding	-- Small enough to dup
			-- ToDo: consider discount on smallEnoughToInline if int_cxt is true
			--
		 	-- NB: Do NOT inline arbitrarily big things, even if one_br is True
			-- Reason: doing so risks exponential behaviour.  We simplify a big
			--	   expression, inline it, and simplify it again.  But if the
			--	   very same thing happens in the big expression, we get 
			--	   exponential cost!
			-- PRINCIPLE: when we've already simplified an expression once, 
			-- make sure that we only inline it if it's reasonably small.

	   &&  ((isNotTopLevel top_lvl && not in_lam) || 
			-- But outside a lambda, we want to be reasonably aggressive
			-- about inlining into multiple branches of case
			-- e.g. let x = <non-value> 
			--	in case y of { C1 -> ..x..; C2 -> ..x..; C3 -> ... } 
			-- Inlining can be a big win if C3 is the hot-spot, even if
			-- the uses in C1, C2 are not 'interesting'
			-- An example that gets worse if you add int_cxt here is 'clausify'

		(isCheapUnfolding unfolding && int_cxt))
			-- isCheap => acceptable work duplication; in_lam may be true
			-- int_cxt to prevent us inlining inside a lambda without some 
			-- good reason.  See the notes on int_cxt in preInlineUnconditionally

      IAmDead -> True	-- This happens; for example, the case_bndr during case of
			-- known constructor:  case (a,b) of x { (p,q) -> ... }
			-- Here x isn't mentioned in the RHS, so we don't want to
			-- create the (dead) let-binding  let x = (a,b) in ...

      other -> False

-- Here's an example that we don't handle well:
--	let f = if b then Left (\x.BIG) else Right (\y.BIG)
--	in \y. ....case f of {...} ....
-- Here f is used just once, and duplicating the case work is fine (exprIsCheap).
-- But
-- * We can't preInlineUnconditionally because that woud invalidate
--   the occ info for b.  
-- * We can't postInlineUnconditionally because the RHS is big, and
--   that risks exponential behaviour
-- * We can't call-site inline, because the rhs is big
-- Alas!

  where
    active = case getMode env of
		   SimplGently  -> isAlwaysActive prag
		   SimplPhase n -> isActive n prag
    prag = idInlinePragma bndr

activeInline :: SimplEnv -> OutId -> Bool
activeInline env id
  = case getMode env of
      SimplGently -> False
	-- No inlining at all when doing gentle stuff,
	-- except for local things that occur once
	-- The reason is that too little clean-up happens if you 
	-- don't inline use-once things.   Also a bit of inlining is *good* for
	-- full laziness; it can expose constant sub-expressions.
	-- Example in spectral/mandel/Mandel.hs, where the mandelset 
	-- function gets a useful let-float if you inline windowToViewport

	-- NB: we used to have a second exception, for data con wrappers.
	-- On the grounds that we use gentle mode for rule LHSs, and 
	-- they match better when data con wrappers are inlined.
	-- But that only really applies to the trivial wrappers (like (:)),
	-- and they are now constructed as Compulsory unfoldings (in MkId)
	-- so they'll happen anyway.

      SimplPhase n -> isActive n prag
  where
    prag = idInlinePragma id

activeRule :: DynFlags -> SimplEnv -> Maybe (Activation -> Bool)
-- Nothing => No rules at all
activeRule dflags env
  | not (dopt Opt_RewriteRules dflags)
  = Nothing	-- Rewriting is off
  | otherwise
  = case getMode env of
	SimplGently  -> Just isAlwaysActive
			-- Used to be Nothing (no rules in gentle mode)
			-- Main motivation for changing is that I wanted
			-- 	lift String ===> ...
			-- to work in Template Haskell when simplifying
			-- splices, so we get simpler code for literal strings
	SimplPhase n -> Just (isActive n)
\end{code}


%************************************************************************
%*									*
	Rebuilding a lambda
%*									*
%************************************************************************

\begin{code}
mkLam :: [OutBndr] -> OutExpr -> SimplM OutExpr
-- mkLam tries three things
--	a) eta reduction, if that gives a trivial expression
--	b) eta expansion [only if there are some value lambdas]

mkLam [] body 
  = return body
mkLam bndrs body
  = do	{ dflags <- getDOptsSmpl
	; mkLam' dflags bndrs body }
  where
    mkLam' :: DynFlags -> [OutBndr] -> OutExpr -> SimplM OutExpr
    mkLam' dflags bndrs (Cast body co)
      | not (any bad bndrs)
	-- Note [Casts and lambdas]
      = do { lam <- mkLam' dflags bndrs body
	   ; return (mkCoerce (mkPiTypes bndrs co) lam) }
      where
	co_vars  = tyVarsOfType co
	bad bndr = isCoVar bndr && bndr `elemVarSet` co_vars      

    mkLam' dflags bndrs body
      | dopt Opt_DoEtaReduction dflags,
        Just etad_lam <- tryEtaReduce bndrs body
      = do { tick (EtaReduction (head bndrs))
	   ; return etad_lam }

      | dopt Opt_DoLambdaEtaExpansion dflags,
   	any isRuntimeVar bndrs
      = do { body' <- tryEtaExpansion dflags body
 	   ; return (mkLams bndrs body') }
   
      | otherwise 
      = returnSmpl (mkLams bndrs body)
\end{code}

Note [Casts and lambdas]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider 
	(\x. (\y. e) `cast` g1) `cast` g2
There is a danger here that the two lambdas look separated, and the 
full laziness pass might float an expression to between the two.

So this equation in mkLam' floats the g1 out, thus:
	(\x. e `cast` g1)  -->  (\x.e) `cast` (tx -> g1)
where x:tx.

In general, this floats casts outside lambdas, where (I hope) they
might meet and cancel with some other cast:
	\x. e `cast` co   ===>   (\x. e) `cast` (tx -> co)
	/\a. e `cast` co  ===>   (/\a. e) `cast` (/\a. co)
	/\g. e `cast` co  ===>   (/\g. e) `cast` (/\g. co)
  		  	  (if not (g `in` co))

Notice that it works regardless of 'e'.  Originally it worked only
if 'e' was itself a lambda, but in some cases that resulted in 
fruitless iteration in the simplifier.  A good example was when
compiling Text.ParserCombinators.ReadPrec, where we had a definition 
like	(\x. Get `cast` g)
where Get is a constructor with nonzero arity.  Then mkLam eta-expanded
the Get, and the next iteration eta-reduced it, and then eta-expanded 
it again.

Note also the side condition for the case of coercion binders.
It does not make sense to transform
	/\g. e `cast` g  ==>  (/\g.e) `cast` (/\g.g)
because the latter is not well-kinded.

--	c) floating lets out through big lambdas 
--		[only if all tyvar lambdas, and only if this lambda
--		 is the RHS of a let]

{-	Sept 01: I'm experimenting with getting the
	full laziness pass to float out past big lambdsa
 | all isTyVar bndrs,	-- Only for big lambdas
   contIsRhs cont	-- Only try the rhs type-lambda floating
			-- if this is indeed a right-hand side; otherwise
			-- we end up floating the thing out, only for float-in
			-- to float it right back in again!
 = tryRhsTyLam env bndrs body		`thenSmpl` \ (floats, body') ->
   returnSmpl (floats, mkLams bndrs body')
-}


%************************************************************************
%*									*
		Eta reduction
%*									*
%************************************************************************

Note [Eta reduction conditions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We try for eta reduction here, but *only* if we get all the way to an
trivial expression.  We don't want to remove extra lambdas unless we
are going to avoid allocating this thing altogether.

There are some particularly delicate points here:

* Eta reduction is not valid in general:  
	\x. bot  /=  bot
  This matters, partly for old-fashioned correctness reasons but,
  worse, getting it wrong can yield a seg fault. Consider
	f = \x.f x
	h y = case (case y of { True -> f `seq` True; False -> False }) of
		True -> ...; False -> ...

  If we (unsoundly) eta-reduce f to get f=f, the strictness analyser
  says f=bottom, and replaces the (f `seq` True) with just
  (f `cast` unsafe-co).  BUT, as thing stand, 'f' got arity 1, and it
  *keeps* arity 1 (perhaps also wrongly).  So CorePrep eta-expands 
  the definition again, so that it does not termninate after all.
  Result: seg-fault because the boolean case actually gets a function value.
  See Trac #1947.

  So it's important to to the right thing.

* We need to be careful if we just look at f's arity. Currently (Dec07),
  f's arity is visible in its own RHS (see Note [Arity robustness] in 
  SimplEnv) so we must *not* trust the arity when checking that 'f' is
  a value.  Instead, look at the unfolding. 

  However for GlobalIds we can look at the arity; and for primops we
  must, since they have no unfolding.  

* Regardless of whether 'f' is a vlaue, we always want to 
  reduce (/\a -> f a) to f
  This came up in a RULE: foldr (build (/\a -> g a))
  did not match 	   foldr (build (/\b -> ...something complex...))
  The type checker can insert these eta-expanded versions,
  with both type and dictionary lambdas; hence the slightly 
  ad-hoc isDictId

These delicacies are why we don't use exprIsTrivial and exprIsHNF here.
Alas.

\begin{code}
tryEtaReduce :: [OutBndr] -> OutExpr -> Maybe OutExpr
tryEtaReduce bndrs body 
  = go (reverse bndrs) body
  where
    go (b : bs) (App fun arg) | ok_arg b arg = go bs fun	-- Loop round
    go []       fun           | ok_fun fun   = Just fun		-- Success!
    go _        _			     = Nothing		-- Failure!

	-- Note [Eta reduction conditions]
    ok_fun (App fun (Type ty)) 
	| not (any (`elemVarSet` tyVarsOfType ty) bndrs)
	=  ok_fun fun
    ok_fun (Var fun_id)
	=  not (fun_id `elem` bndrs)
	&& (ok_fun_id fun_id || all ok_lam bndrs)
    ok_fun _fun = False

    ok_fun_id fun
	| isLocalId fun       = isEvaldUnfolding (idUnfolding fun)
	| isDataConWorkId fun = True
	| isGlobalId fun      = idArity fun > 0

    ok_lam v = isTyVar v || isDictId v

    ok_arg b arg = varToCoreExpr b `cheapEqExpr` arg
\end{code}


%************************************************************************
%*									*
		Eta expansion
%*									*
%************************************************************************


We go for:
   f = \x1..xn -> N  ==>   f = \x1..xn y1..ym -> N y1..ym
				 (n >= 0)

where (in both cases) 

	* The xi can include type variables

	* The yi are all value variables

	* N is a NORMAL FORM (i.e. no redexes anywhere)
	  wanting a suitable number of extra args.

The biggest reason for doing this is for cases like

	f = \x -> case x of
		    True  -> \y -> e1
		    False -> \y -> e2

Here we want to get the lambdas together.  A good exmaple is the nofib
program fibheaps, which gets 25% more allocation if you don't do this
eta-expansion.

We may have to sandwich some coerces between the lambdas
to make the types work.   exprEtaExpandArity looks through coerces
when computing arity; and etaExpand adds the coerces as necessary when
actually computing the expansion.

\begin{code}
tryEtaExpansion :: DynFlags -> OutExpr -> SimplM OutExpr
-- There is at least one runtime binder in the binders
tryEtaExpansion dflags body
  = getUniquesSmpl 			`thenSmpl` \ us ->
    returnSmpl (etaExpand fun_arity us body (exprType body))
  where
    fun_arity = exprEtaExpandArity dflags body
\end{code}


%************************************************************************
%*									*
\subsection{Floating lets out of big lambdas}
%*									*
%************************************************************************

Note [Floating and type abstraction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
	x = /\a. C e1 e2
We'd like to float this to 
	y1 = /\a. e1
	y2 = /\a. e2
	x = /\a. C (y1 a) (y2 a)
for the usual reasons: we want to inline x rather vigorously.

You may think that this kind of thing is rare.  But in some programs it is
common.  For example, if you do closure conversion you might get:

	data a :-> b = forall e. (e -> a -> b) :$ e

	f_cc :: forall a. a :-> a
	f_cc = /\a. (\e. id a) :$ ()

Now we really want to inline that f_cc thing so that the
construction of the closure goes away. 

So I have elaborated simplLazyBind to understand right-hand sides that look
like
	/\ a1..an. body

and treat them specially. The real work is done in SimplUtils.abstractFloats,
but there is quite a bit of plumbing in simplLazyBind as well.

The same transformation is good when there are lets in the body:

	/\abc -> let(rec) x = e in b
   ==>
	let(rec) x' = /\abc -> let x = x' a b c in e
	in 
	/\abc -> let x = x' a b c in b

This is good because it can turn things like:

	let f = /\a -> letrec g = ... g ... in g
into
	letrec g' = /\a -> ... g' a ...
	in
	let f = /\ a -> g' a

which is better.  In effect, it means that big lambdas don't impede
let-floating.

This optimisation is CRUCIAL in eliminating the junk introduced by
desugaring mutually recursive definitions.  Don't eliminate it lightly!

[May 1999]  If we do this transformation *regardless* then we can
end up with some pretty silly stuff.  For example, 

	let 
	    st = /\ s -> let { x1=r1 ; x2=r2 } in ...
	in ..
becomes
	let y1 = /\s -> r1
	    y2 = /\s -> r2
	    st = /\s -> ...[y1 s/x1, y2 s/x2]
	in ..

Unless the "..." is a WHNF there is really no point in doing this.
Indeed it can make things worse.  Suppose x1 is used strictly,
and is of the form

	x1* = case f y of { (a,b) -> e }

If we abstract this wrt the tyvar we then can't do the case inline
as we would normally do.

That's why the whole transformation is part of the same process that
floats let-bindings and constructor arguments out of RHSs.  In particular,
it is guarded by the doFloatFromRhs call in simplLazyBind.


\begin{code}
abstractFloats :: [OutTyVar] -> SimplEnv -> OutExpr -> SimplM ([OutBind], OutExpr)
abstractFloats main_tvs body_env body
  = ASSERT( notNull body_floats )
    do	{ (subst, float_binds) <- mapAccumLSmpl abstract empty_subst body_floats
	; return (float_binds, CoreSubst.substExpr subst body) }
  where
    main_tv_set = mkVarSet main_tvs
    body_floats = getFloats body_env
    empty_subst = CoreSubst.mkEmptySubst (seInScope body_env)

    abstract :: CoreSubst.Subst -> OutBind -> SimplM (CoreSubst.Subst, OutBind)
    abstract subst (NonRec id rhs)
      = do { (poly_id, poly_app) <- mk_poly tvs_here id
	   ; let poly_rhs = mkLams tvs_here rhs'
		 subst'   = CoreSubst.extendIdSubst subst id poly_app
	   ; return (subst', (NonRec poly_id poly_rhs)) }
      where
	rhs' = CoreSubst.substExpr subst rhs
	tvs_here | any isCoVar main_tvs = main_tvs	-- Note [Abstract over coercions]
		 | otherwise 
		 = varSetElems (main_tv_set `intersectVarSet` exprSomeFreeVars isTyVar rhs')
	
		-- Abstract only over the type variables free in the rhs
		-- wrt which the new binding is abstracted.  But the naive
		-- approach of abstract wrt the tyvars free in the Id's type
		-- fails. Consider:
		--	/\ a b -> let t :: (a,b) = (e1, e2)
		--		      x :: a     = fst t
		--		  in ...
		-- Here, b isn't free in x's type, but we must nevertheless
		-- abstract wrt b as well, because t's type mentions b.
		-- Since t is floated too, we'd end up with the bogus:
		--	poly_t = /\ a b -> (e1, e2)
		--	poly_x = /\ a   -> fst (poly_t a *b*)
		-- So for now we adopt the even more naive approach of
		-- abstracting wrt *all* the tyvars.  We'll see if that
		-- gives rise to problems.   SLPJ June 98

    abstract subst (Rec prs)
       = do { (poly_ids, poly_apps) <- mapAndUnzipSmpl (mk_poly tvs_here) ids
	    ; let subst' = CoreSubst.extendSubstList subst (ids `zip` poly_apps)
		  poly_rhss = [mkLams tvs_here (CoreSubst.substExpr subst' rhs) | rhs <- rhss]
	    ; return (subst', Rec (poly_ids `zip` poly_rhss)) }
       where
	 (ids,rhss) = unzip prs
	 	-- For a recursive group, it's a bit of a pain to work out the minimal
		-- set of tyvars over which to abstract:
		--	/\ a b c.  let x = ...a... in
		--	 	   letrec { p = ...x...q...
		--			    q = .....p...b... } in
		--		   ...
		-- Since 'x' is abstracted over 'a', the {p,q} group must be abstracted
		-- over 'a' (because x is replaced by (poly_x a)) as well as 'b'.  
		-- Since it's a pain, we just use the whole set, which is always safe
		-- 
		-- If you ever want to be more selective, remember this bizarre case too:
		--	x::a = x
		-- Here, we must abstract 'x' over 'a'.
	 tvs_here = main_tvs

    mk_poly tvs_here var
      = do { uniq <- getUniqueSmpl
	   ; let  poly_name = setNameUnique (idName var) uniq		-- Keep same name
		  poly_ty   = mkForAllTys tvs_here (idType var)	-- But new type of course
		  poly_id   = mkLocalId poly_name poly_ty 
	   ; return (poly_id, mkTyApps (Var poly_id) (mkTyVarTys tvs_here)) }
		-- In the olden days, it was crucial to copy the occInfo of the original var, 
		-- because we were looking at occurrence-analysed but as yet unsimplified code!
		-- In particular, we mustn't lose the loop breakers.  BUT NOW we are looking
		-- at already simplified code, so it doesn't matter
		-- 
		-- It's even right to retain single-occurrence or dead-var info:
		-- Suppose we started with  /\a -> let x = E in B
		-- where x occurs once in B. Then we transform to:
		--	let x' = /\a -> E in /\a -> let x* = x' a in B
		-- where x* has an INLINE prag on it.  Now, once x* is inlined,
		-- the occurrences of x' will be just the occurrences originally
		-- pinned on x.
\end{code}

Note [Abstract over coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If a coercion variable (g :: a ~ Int) is free in the RHS, then so is the
type variable a.  Rather than sort this mess out, we simply bale out and abstract
wrt all the type variables if any of them are coercion variables.


Historical note: if you use let-bindings instead of a substitution, beware of this:

		-- Suppose we start with:
		--
		--	x = /\ a -> let g = G in E
		--
		-- Then we'll float to get
		--
		--	x = let poly_g = /\ a -> G
		--	    in /\ a -> let g = poly_g a in E
		--
		-- But now the occurrence analyser will see just one occurrence
		-- of poly_g, not inside a lambda, so the simplifier will
		-- PreInlineUnconditionally poly_g back into g!  Badk to square 1!
		-- (I used to think that the "don't inline lone occurrences" stuff
		--  would stop this happening, but since it's the *only* occurrence,
		--  PreInlineUnconditionally kicks in first!)
		--
		-- Solution: put an INLINE note on g's RHS, so that poly_g seems
		--	     to appear many times.  (NB: mkInlineMe eliminates
		--	     such notes on trivial RHSs, so do it manually.)

%************************************************************************
%*									*
		prepareAlts
%*									*
%************************************************************************

prepareAlts tries these things:

1.  If several alternatives are identical, merge them into
    a single DEFAULT alternative.  I've occasionally seen this 
    making a big difference:

	case e of		=====>     case e of
	  C _ -> f x			     D v -> ....v....
	  D v -> ....v....		     DEFAULT -> f x
	  DEFAULT -> f x

   The point is that we merge common RHSs, at least for the DEFAULT case.
   [One could do something more elaborate but I've never seen it needed.]
   To avoid an expensive test, we just merge branches equal to the *first*
   alternative; this picks up the common cases
	a) all branches equal
	b) some branches equal to the DEFAULT (which occurs first)

2.  Case merging:
       case e of b {             ==>   case e of b {
    	 p1 -> rhs1	                 p1 -> rhs1
    	 ...	                         ...
    	 pm -> rhsm                      pm -> rhsm
    	 _  -> case b of b' {            pn -> let b'=b in rhsn
 		     pn -> rhsn          ...
 		     ...                 po -> let b'=b in rhso
 		     po -> rhso          _  -> let b'=b in rhsd
 		     _  -> rhsd
       }  
    
    which merges two cases in one case when -- the default alternative of
    the outer case scrutises the same variable as the outer case This
    transformation is called Case Merging.  It avoids that the same
    variable is scrutinised multiple times.


The case where transformation (1) showed up was like this (lib/std/PrelCError.lhs):

	x | p `is` 1 -> e1
	  | p `is` 2 -> e2
	...etc...

where @is@ was something like
	
	p `is` n = p /= (-1) && p == n

This gave rise to a horrible sequence of cases

	case p of
	  (-1) -> $j p
	  1    -> e1
	  DEFAULT -> $j p

and similarly in cascade for all the join points!

Note [Dead binders]
~~~~~~~~~~~~~~~~~~~~
We do this *here*, looking at un-simplified alternatives, because we
have to check that r doesn't mention the variables bound by the
pattern in each alternative, so the binder-info is rather useful.

\begin{code}
prepareAlts :: SimplEnv -> OutExpr -> OutId -> [InAlt] -> SimplM ([AltCon], [InAlt])
prepareAlts env scrut case_bndr' alts
  = do	{ dflags <- getDOptsSmpl
	; alts <- combineIdenticalAlts case_bndr' alts

	; let (alts_wo_default, maybe_deflt) = findDefault alts
	      alt_cons = [con | (con,_,_) <- alts_wo_default]
	      imposs_deflt_cons = nub (imposs_cons ++ alt_cons)
		-- "imposs_deflt_cons" are handled 
		--   EITHER by the context, 
		--   OR by a non-DEFAULT branch in this case expression.

	; default_alts <- prepareDefault dflags env case_bndr' mb_tc_app 
					 imposs_deflt_cons maybe_deflt

	; let trimmed_alts = filterOut impossible_alt alts_wo_default
	      merged_alts = mergeAlts trimmed_alts default_alts
		-- We need the mergeAlts in case the new default_alt 
		-- has turned into a constructor alternative.
		-- The merge keeps the inner DEFAULT at the front, if there is one
		-- and interleaves the alternatives in the right order

	; return (imposs_deflt_cons, merged_alts) }
  where
    mb_tc_app = splitTyConApp_maybe (idType case_bndr')
    Just (_, inst_tys) = mb_tc_app 

    imposs_cons = case scrut of
		    Var v -> otherCons (idUnfolding v)
		    other -> []

    impossible_alt :: CoreAlt -> Bool
    impossible_alt (con, _, _) | con `elem` imposs_cons = True
    impossible_alt (DataAlt con, _, _) = dataConCannotMatch inst_tys con
    impossible_alt alt		       = False


--------------------------------------------------
--	1. Merge identical branches
--------------------------------------------------
combineIdenticalAlts :: OutId -> [InAlt] -> SimplM [InAlt]

combineIdenticalAlts case_bndr alts@((con1,bndrs1,rhs1) : con_alts)
  | all isDeadBinder bndrs1,			-- Remember the default 
    length filtered_alts < length con_alts	-- alternative comes first
	-- Also Note [Dead binders]
  = do	{ tick (AltMerge case_bndr)
	; return ((DEFAULT, [], rhs1) : filtered_alts) }
  where
    filtered_alts	 = filter keep con_alts
    keep (con,bndrs,rhs) = not (all isDeadBinder bndrs && rhs `cheapEqExpr` rhs1)

combineIdenticalAlts case_bndr alts = return alts

-------------------------------------------------------------------------
--			Prepare the default alternative
-------------------------------------------------------------------------
prepareDefault :: DynFlags
	       -> SimplEnv
	       -> OutId		-- Case binder; need just for its type. Note that as an
				--   OutId, it has maximum information; this is important.
				--   Test simpl013 is an example
	       -> Maybe (TyCon, [Type])	-- Type of scrutinee, decomposed
	       -> [AltCon]	-- These cons can't happen when matching the default
	       -> Maybe InExpr	-- Rhs
	       -> SimplM [InAlt]	-- Still unsimplified
					-- We use a list because it's what mergeAlts expects,
					-- And becuase case-merging can cause many to show up

-------	Merge nested cases ----------
prepareDefault dflags env outer_bndr bndr_ty imposs_cons (Just deflt_rhs)
  | dopt Opt_CaseMerge dflags
  , Case (Var inner_scrut_var) inner_bndr _ inner_alts <- deflt_rhs
  , DoneId inner_scrut_var' <- substId env inner_scrut_var
	-- Remember, inner_scrut_var is an InId, but outer_bndr is an OutId
  , inner_scrut_var' == outer_bndr
	-- NB: the substId means that if the outer scrutinee was a 
	--     variable, and inner scrutinee is the same variable, 
	--     then inner_scrut_var' will be outer_bndr
	--     via the magic of simplCaseBinder
  = do	{ tick (CaseMerge outer_bndr)

	; let munge_rhs rhs = bindCaseBndr inner_bndr (Var outer_bndr) rhs
	; return [(con, args, munge_rhs rhs) | (con, args, rhs) <- inner_alts,
					       not (con `elem` imposs_cons) ]
		-- NB: filter out any imposs_cons.  Example:
		--	case x of 
		--	  A -> e1
		--	  DEFAULT -> case x of 
		--			A -> e2
		--			B -> e3
		-- When we merge, we must ensure that e1 takes 
		-- precedence over e2 as the value for A!  
	}
    	-- Warning: don't call prepareAlts recursively!
    	-- Firstly, there's no point, because inner alts have already had
    	-- mkCase applied to them, so they won't have a case in their default
    	-- Secondly, if you do, you get an infinite loop, because the bindCaseBndr
    	-- in munge_rhs may put a case into the DEFAULT branch!


--------- Fill in known constructor -----------
prepareDefault dflags env case_bndr (Just (tycon, inst_tys)) imposs_cons (Just deflt_rhs)
  | 	-- This branch handles the case where we are 
	-- scrutinisng an algebraic data type
    isAlgTyCon tycon		-- It's a data type, tuple, or unboxed tuples.  
  , not (isNewTyCon tycon)	-- We can have a newtype, if we are just doing an eval:
				-- 	case x of { DEFAULT -> e }
				-- and we don't want to fill in a default for them!
  , Just all_cons <- tyConDataCons_maybe tycon
  , not (null all_cons)		-- This is a tricky corner case.  If the data type has no constructors,
				-- which GHC allows, then the case expression will have at most a default
				-- alternative.  We don't want to eliminate that alternative, because the
				-- invariant is that there's always one alternative.  It's more convenient
				-- to leave	
				--	case x of { DEFAULT -> e }     
				-- as it is, rather than transform it to
				--	error "case cant match"
				-- which would be quite legitmate.  But it's a really obscure corner, and
				-- not worth wasting code on.
  , let imposs_data_cons = [con | DataAlt con <- imposs_cons]	-- We now know it's a data type 
	impossible con  = con `elem` imposs_data_cons || dataConCannotMatch inst_tys con
  = case filterOut impossible all_cons of
	[]    -> return []	-- Eliminate the default alternative
				-- altogether if it can't match

	[con] -> 	-- It matches exactly one constructor, so fill it in
		 do { tick (FillInCaseDefault case_bndr)
                    ; us <- getUniquesSmpl
                    ; let (ex_tvs, co_tvs, arg_ids) =
                              dataConRepInstPat us con inst_tys
                    ; return [(DataAlt con, ex_tvs ++ co_tvs ++ arg_ids, deflt_rhs)] }

	two_or_more -> return [(DEFAULT, [], deflt_rhs)]

--------- Catch-all cases -----------
prepareDefault dflags env case_bndr bndr_ty imposs_cons (Just deflt_rhs)
  = return [(DEFAULT, [], deflt_rhs)]

prepareDefault dflags env case_bndr bndr_ty imposs_cons Nothing
  = return []	-- No default branch
\end{code}



=================================================================================

mkCase tries these things

1.  Eliminate the case altogether if possible

2.  Case-identity:

	case e of 		===> e
		True  -> True;
		False -> False

    and similar friends.


\begin{code}
mkCase :: OutExpr -> OutId -> OutType
       -> [OutAlt]		-- Increasing order
       -> SimplM OutExpr

--------------------------------------------------
--	1. Check for empty alternatives
--------------------------------------------------

-- This isn't strictly an error.  It's possible that the simplifer might "see"
-- that an inner case has no accessible alternatives before it "sees" that the
-- entire branch of an outer case is inaccessible.  So we simply
-- put an error case here insteadd
mkCase scrut case_bndr ty []
  = pprTrace "mkCase: null alts" (ppr case_bndr <+> ppr scrut) $
    return (mkApps (Var rUNTIME_ERROR_ID)
		   [Type ty, Lit (mkStringLit "Impossible alternative")])


--------------------------------------------------
--	2. Identity case
--------------------------------------------------

mkCase scrut case_bndr ty alts	-- Identity case
  | all identity_alt alts
  = tick (CaseIdentity case_bndr)		`thenSmpl_`
    returnSmpl (re_cast scrut)
  where
    identity_alt (con, args, rhs) = check_eq con args (de_cast rhs)

    check_eq DEFAULT       _    (Var v)   = v == case_bndr
    check_eq (LitAlt lit') _    (Lit lit) = lit == lit'
    check_eq (DataAlt con) args rhs       = rhs `cheapEqExpr` mkConApp con (arg_tys ++ varsToCoreExprs args)
					 || rhs `cheapEqExpr` Var case_bndr
    check_eq con args rhs = False

    arg_tys = map Type (tyConAppArgs (idType case_bndr))

	-- We've seen this:
	--	case e of x { _ -> x `cast` c }
	-- And we definitely want to eliminate this case, to give
	--	e `cast` c
	-- So we throw away the cast from the RHS, and reconstruct
	-- it at the other end.  All the RHS casts must be the same
	-- if (all identity_alt alts) holds.
	-- 
	-- Don't worry about nested casts, because the simplifier combines them
    de_cast (Cast e _) = e
    de_cast e	       = e

    re_cast scrut = case head alts of
			(_,_,Cast _ co) -> Cast scrut co
			other	        -> scrut



--------------------------------------------------
--	Catch-all
--------------------------------------------------
mkCase scrut bndr ty alts = returnSmpl (Case scrut bndr ty alts)
\end{code}


When adding auxiliary bindings for the case binder, it's worth checking if
its dead, because it often is, and occasionally these mkCase transformations
cascade rather nicely.

\begin{code}
bindCaseBndr bndr rhs body
  | isDeadBinder bndr = body
  | otherwise	      = bindNonRec bndr rhs body
\end{code}
