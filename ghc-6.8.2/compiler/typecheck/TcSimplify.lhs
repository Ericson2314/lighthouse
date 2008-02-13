%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

TcSimplify

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module TcSimplify (
	tcSimplifyInfer, tcSimplifyInferCheck,
	tcSimplifyCheck, tcSimplifyRestricted,
	tcSimplifyRuleLhs, tcSimplifyIPs, 
	tcSimplifySuperClasses,
	tcSimplifyTop, tcSimplifyInteractive,
	tcSimplifyBracket, tcSimplifyCheckPat,

	tcSimplifyDeriv, tcSimplifyDefault,
	bindInstsOfLocalFuns, 

        misMatchMsg
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} TcUnify( unifyType )
import HsSyn

import TcRnMonad
import Inst
import TcEnv
import InstEnv
import TcGadt
import TcType
import TcMType
import TcIface
import TcTyFuns
import TypeRep
import Var
import Name
import NameSet
import Class
import FunDeps
import PrelInfo
import PrelNames
import Type
import TysWiredIn
import ErrUtils
import BasicTypes
import VarSet
import VarEnv
import Module
import FiniteMap
import Bag
import Outputable
import Maybes
import ListSetOps
import Util
import UniqSet
import SrcLoc
import DynFlags

import Data.List
\end{code}


%************************************************************************
%*									*
\subsection{NOTES}
%*									*
%************************************************************************

	--------------------------------------
	Notes on functional dependencies (a bug)
	--------------------------------------

Consider this:

	class C a b | a -> b
	class D a b | a -> b

	instance D a b => C a b	-- Undecidable 
				-- (Not sure if it's crucial to this eg)
	f :: C a b => a -> Bool
	f _ = True
	
	g :: C a b => a -> Bool
	g = f

Here f typechecks, but g does not!!  Reason: before doing improvement,
we reduce the (C a b1) constraint from the call of f to (D a b1).

Here is a more complicated example:

| > class Foo a b | a->b
| >
| > class Bar a b | a->b
| >
| > data Obj = Obj
| >
| > instance Bar Obj Obj
| >
| > instance (Bar a b) => Foo a b
| >
| > foo:: (Foo a b) => a -> String
| > foo _ = "works"
| >
| > runFoo:: (forall a b. (Foo a b) => a -> w) -> w
| > runFoo f = f Obj
| 
| *Test> runFoo foo
| 
| <interactive>:1:
|     Could not deduce (Bar a b) from the context (Foo a b)
|       arising from use of `foo' at <interactive>:1
|     Probable fix:
|         Add (Bar a b) to the expected type of an expression
|     In the first argument of `runFoo', namely `foo'
|     In the definition of `it': it = runFoo foo
| 
| Why all of the sudden does GHC need the constraint Bar a b? The
| function foo didn't ask for that... 

The trouble is that to type (runFoo foo), GHC has to solve the problem:

	Given constraint	Foo a b
	Solve constraint	Foo a b'

Notice that b and b' aren't the same.  To solve this, just do
improvement and then they are the same.  But GHC currently does
	simplify constraints
	apply improvement
	and loop

That is usually fine, but it isn't here, because it sees that Foo a b is
not the same as Foo a b', and so instead applies the instance decl for
instance Bar a b => Foo a b.  And that's where the Bar constraint comes
from.

The Right Thing is to improve whenever the constraint set changes at
all.  Not hard in principle, but it'll take a bit of fiddling to do.  

Note [Choosing which variables to quantify]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are about to do a generalisation step.  We have in our hand

	G	the environment
	T	the type of the RHS
	C	the constraints from that RHS

The game is to figure out

	Q	the set of type variables over which to quantify
	Ct	the constraints we will *not* quantify over
	Cq	the constraints we will quantify over

So we're going to infer the type

	forall Q. Cq => T

and float the constraints Ct further outwards.

Here are the things that *must* be true:

 (A)	Q intersect fv(G) = EMPTY			limits how big Q can be
 (B)	Q superset fv(Cq union T) \ oclose(fv(G),C)	limits how small Q can be

 (A) says we can't quantify over a variable that's free in the environment. 
 (B) says we must quantify over all the truly free variables in T, else 
     we won't get a sufficiently general type.  

We do not *need* to quantify over any variable that is fixed by the
free vars of the environment G.

	BETWEEN THESE TWO BOUNDS, ANY Q WILL DO!

Example:	class H x y | x->y where ...

	fv(G) = {a}	C = {H a b, H c d}
			T = c -> b

	(A)  Q intersect {a} is empty
	(B)  Q superset {a,b,c,d} \ oclose({a}, C) = {a,b,c,d} \ {a,b} = {c,d}

	So Q can be {c,d}, {b,c,d}

In particular, it's perfectly OK to quantify over more type variables
than strictly necessary; there is no need to quantify over 'b', since
it is determined by 'a' which is free in the envt, but it's perfectly
OK to do so.  However we must not quantify over 'a' itself.

Other things being equal, however, we'd like to quantify over as few
variables as possible: smaller types, fewer type applications, more
constraints can get into Ct instead of Cq.  Here's a good way to
choose Q:

	Q = grow( fv(T), C ) \ oclose( fv(G), C )

That is, quantify over all variable that that MIGHT be fixed by the
call site (which influences T), but which aren't DEFINITELY fixed by
G.  This choice definitely quantifies over enough type variables,
albeit perhaps too many.

Why grow( fv(T), C ) rather than fv(T)?  Consider

	class H x y | x->y where ...

	T = c->c
	C = (H c d)

  If we used fv(T) = {c} we'd get the type

	forall c. H c d => c -> b

  And then if the fn was called at several different c's, each of
  which fixed d differently, we'd get a unification error, because
  d isn't quantified.  Solution: quantify d.  So we must quantify
  everything that might be influenced by c.

Why not oclose( fv(T), C )?  Because we might not be able to see
all the functional dependencies yet:

	class H x y | x->y where ...
	instance H x y => Eq (T x y) where ...

	T = c->c
	C = (Eq (T c d))

Now oclose(fv(T),C) = {c}, because the functional dependency isn't
apparent yet, and that's wrong.  We must really quantify over d too.

There really isn't any point in quantifying over any more than
grow( fv(T), C ), because the call sites can't possibly influence
any other type variables.



-------------------------------------
	Note [Ambiguity]
-------------------------------------

It's very hard to be certain when a type is ambiguous.  Consider

	class K x
	class H x y | x -> y
	instance H x y => K (x,y)

Is this type ambiguous?
	forall a b. (K (a,b), Eq b) => a -> a

Looks like it!  But if we simplify (K (a,b)) we get (H a b) and
now we see that a fixes b.  So we can't tell about ambiguity for sure
without doing a full simplification.  And even that isn't possible if
the context has some free vars that may get unified.  Urgle!

Here's another example: is this ambiguous?
	forall a b. Eq (T b) => a -> a
Not if there's an insance decl (with no context)
	instance Eq (T b) where ...

You may say of this example that we should use the instance decl right
away, but you can't always do that:

	class J a b where ...
	instance J Int b where ...

	f :: forall a b. J a b => a -> a

(Notice: no functional dependency in J's class decl.)
Here f's type is perfectly fine, provided f is only called at Int.
It's premature to complain when meeting f's signature, or even
when inferring a type for f.



However, we don't *need* to report ambiguity right away.  It'll always
show up at the call site.... and eventually at main, which needs special
treatment.  Nevertheless, reporting ambiguity promptly is an excellent thing.

So here's the plan.  We WARN about probable ambiguity if

	fv(Cq) is not a subset of  oclose(fv(T) union fv(G), C)

(all tested before quantification).
That is, all the type variables in Cq must be fixed by the the variables
in the environment, or by the variables in the type.

Notice that we union before calling oclose.  Here's an example:

	class J a b c | a b -> c
	fv(G) = {a}

Is this ambiguous?
	forall b c. (J a b c) => b -> b

Only if we union {a} from G with {b} from T before using oclose,
do we see that c is fixed.

It's a bit vague exactly which C we should use for this oclose call.  If we
don't fix enough variables we might complain when we shouldn't (see
the above nasty example).  Nothing will be perfect.  That's why we can
only issue a warning.


Can we ever be *certain* about ambiguity?  Yes: if there's a constraint

	c in C such that fv(c) intersect (fv(G) union fv(T)) = EMPTY

then c is a "bubble"; there's no way it can ever improve, and it's
certainly ambiguous.  UNLESS it is a constant (sigh).  And what about
the nasty example?

	class K x
	class H x y | x -> y
	instance H x y => K (x,y)

Is this type ambiguous?
	forall a b. (K (a,b), Eq b) => a -> a

Urk.  The (Eq b) looks "definitely ambiguous" but it isn't.  What we are after
is a "bubble" that's a set of constraints

	Cq = Ca union Cq'  st  fv(Ca) intersect (fv(Cq') union fv(T) union fv(G)) = EMPTY

Hence another idea.  To decide Q start with fv(T) and grow it
by transitive closure in Cq (no functional dependencies involved).
Now partition Cq using Q, leaving the definitely-ambiguous and probably-ok.
The definitely-ambiguous can then float out, and get smashed at top level
(which squashes out the constants, like Eq (T a) above)


	--------------------------------------
		Notes on principal types
	--------------------------------------

    class C a where
      op :: a -> a

    f x = let g y = op (y::Int) in True

Here the principal type of f is (forall a. a->a)
but we'll produce the non-principal type
    f :: forall a. C Int => a -> a


	--------------------------------------
	The need for forall's in constraints
	--------------------------------------

[Exchange on Haskell Cafe 5/6 Dec 2000]

  class C t where op :: t -> Bool
  instance C [t] where op x = True

  p y = (let f :: c -> Bool; f x = op (y >> return x) in f, y ++ [])
  q y = (y ++ [], let f :: c -> Bool; f x = op (y >> return x) in f)

The definitions of p and q differ only in the order of the components in
the pair on their right-hand sides.  And yet:

  ghc and "Typing Haskell in Haskell" reject p, but accept q;
  Hugs rejects q, but accepts p;
  hbc rejects both p and q;
  nhc98 ... (Malcolm, can you fill in the blank for us!).

The type signature for f forces context reduction to take place, and
the results of this depend on whether or not the type of y is known,
which in turn depends on which component of the pair the type checker
analyzes first.  

Solution: if y::m a, float out the constraints
	Monad m, forall c. C (m c)
When m is later unified with [], we can solve both constraints.


	--------------------------------------
		Notes on implicit parameters
	--------------------------------------

Note [Inheriting implicit parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:

	f x = (x::Int) + ?y

where f is *not* a top-level binding.
From the RHS of f we'll get the constraint (?y::Int).
There are two types we might infer for f:

	f :: Int -> Int

(so we get ?y from the context of f's definition), or

	f :: (?y::Int) => Int -> Int

At first you might think the first was better, becuase then
?y behaves like a free variable of the definition, rather than
having to be passed at each call site.  But of course, the WHOLE
IDEA is that ?y should be passed at each call site (that's what
dynamic binding means) so we'd better infer the second.

BOTTOM LINE: when *inferring types* you *must* quantify 
over implicit parameters. See the predicate isFreeWhenInferring.


Note [Implicit parameters and ambiguity] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What type should we infer for this?
	f x = (show ?y, x::Int)
Since we must quantify over the ?y, the most plausible type is
	f :: (Show a, ?y::a) => Int -> (String, Int)
But notice that the type of the RHS is (String,Int), with no type 
varibables mentioned at all!  The type of f looks ambiguous.  But
it isn't, because at a call site we might have
	let ?y = 5::Int in f 7
and all is well.  In effect, implicit parameters are, well, parameters,
so we can take their type variables into account as part of the
"tau-tvs" stuff.  This is done in the function 'FunDeps.grow'.


Question 2: type signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~~
BUT WATCH OUT: When you supply a type signature, we can't force you
to quantify over implicit parameters.  For example:

	(?x + 1) :: Int

This is perfectly reasonable.  We do not want to insist on

	(?x + 1) :: (?x::Int => Int)

That would be silly.  Here, the definition site *is* the occurrence site,
so the above strictures don't apply.  Hence the difference between
tcSimplifyCheck (which *does* allow implicit paramters to be inherited)
and tcSimplifyCheckBind (which does not).

What about when you supply a type signature for a binding?
Is it legal to give the following explicit, user type 
signature to f, thus:

	f :: Int -> Int
	f x = (x::Int) + ?y

At first sight this seems reasonable, but it has the nasty property
that adding a type signature changes the dynamic semantics.
Consider this:

	(let f x = (x::Int) + ?y
 	 in (f 3, f 3 with ?y=5))  with ?y = 6

		returns (3+6, 3+5)
vs
	(let f :: Int -> Int
	     f x = x + ?y
	 in (f 3, f 3 with ?y=5))  with ?y = 6

		returns (3+6, 3+6)

Indeed, simply inlining f (at the Haskell source level) would change the
dynamic semantics.

Nevertheless, as Launchbury says (email Oct 01) we can't really give the
semantics for a Haskell program without knowing its typing, so if you 
change the typing you may change the semantics.

To make things consistent in all cases where we are *checking* against
a supplied signature (as opposed to inferring a type), we adopt the
rule: 

	a signature does not need to quantify over implicit params.

[This represents a (rather marginal) change of policy since GHC 5.02,
which *required* an explicit signature to quantify over all implicit
params for the reasons mentioned above.]

But that raises a new question.  Consider 

	Given (signature)	?x::Int
	Wanted (inferred)	?x::Int, ?y::Bool

Clearly we want to discharge the ?x and float the ?y out.  But
what is the criterion that distinguishes them?  Clearly it isn't
what free type variables they have.  The Right Thing seems to be
to float a constraint that
	neither mentions any of the quantified type variables
	nor any of the quantified implicit parameters

See the predicate isFreeWhenChecking.


Question 3: monomorphism
~~~~~~~~~~~~~~~~~~~~~~~~
There's a nasty corner case when the monomorphism restriction bites:

	z = (x::Int) + ?y

The argument above suggests that we *must* generalise
over the ?y parameter, to get
	z :: (?y::Int) => Int,
but the monomorphism restriction says that we *must not*, giving
	z :: Int.
Why does the momomorphism restriction say this?  Because if you have

	let z = x + ?y in z+z

you might not expect the addition to be done twice --- but it will if
we follow the argument of Question 2 and generalise over ?y.


Question 4: top level
~~~~~~~~~~~~~~~~~~~~~
At the top level, monomorhism makes no sense at all.

    module Main where
	main = let ?x = 5 in print foo

	foo = woggle 3

	woggle :: (?x :: Int) => Int -> Int
	woggle y = ?x + y

We definitely don't want (foo :: Int) with a top-level implicit parameter
(?x::Int) becuase there is no way to bind it.  


Possible choices
~~~~~~~~~~~~~~~~
(A) Always generalise over implicit parameters
    Bindings that fall under the monomorphism restriction can't
	be generalised

    Consequences:
	* Inlining remains valid
	* No unexpected loss of sharing
	* But simple bindings like
		z = ?y + 1
	  will be rejected, unless you add an explicit type signature
	  (to avoid the monomorphism restriction)
		z :: (?y::Int) => Int
		z = ?y + 1
	  This seems unacceptable

(B) Monomorphism restriction "wins"
    Bindings that fall under the monomorphism restriction can't
	be generalised
    Always generalise over implicit parameters *except* for bindings
	that fall under the monomorphism restriction

    Consequences
	* Inlining isn't valid in general
	* No unexpected loss of sharing
	* Simple bindings like
		z = ?y + 1
	  accepted (get value of ?y from binding site)

(C) Always generalise over implicit parameters
    Bindings that fall under the monomorphism restriction can't
	be generalised, EXCEPT for implicit parameters
    Consequences
	* Inlining remains valid
	* Unexpected loss of sharing (from the extra generalisation)
	* Simple bindings like
		z = ?y + 1
	  accepted (get value of ?y from occurrence sites)


Discussion
~~~~~~~~~~
None of these choices seems very satisfactory.  But at least we should
decide which we want to do.

It's really not clear what is the Right Thing To Do.  If you see

	z = (x::Int) + ?y

would you expect the value of ?y to be got from the *occurrence sites*
of 'z', or from the valuue of ?y at the *definition* of 'z'?  In the
case of function definitions, the answer is clearly the former, but
less so in the case of non-fucntion definitions.   On the other hand,
if we say that we get the value of ?y from the definition site of 'z',
then inlining 'z' might change the semantics of the program.

Choice (C) really says "the monomorphism restriction doesn't apply
to implicit parameters".  Which is fine, but remember that every
innocent binding 'x = ...' that mentions an implicit parameter in
the RHS becomes a *function* of that parameter, called at each
use of 'x'.  Now, the chances are that there are no intervening 'with'
clauses that bind ?y, so a decent compiler should common up all
those function calls.  So I think I strongly favour (C).  Indeed,
one could make a similar argument for abolishing the monomorphism
restriction altogether.

BOTTOM LINE: we choose (B) at present.  See tcSimplifyRestricted



%************************************************************************
%*									*
\subsection{tcSimplifyInfer}
%*									*
%************************************************************************

tcSimplify is called when we *inferring* a type.  Here's the overall game plan:

    1. Compute Q = grow( fvs(T), C )

    2. Partition C based on Q into Ct and Cq.  Notice that ambiguous
       predicates will end up in Ct; we deal with them at the top level

    3. Try improvement, using functional dependencies

    4. If Step 3 did any unification, repeat from step 1
       (Unification can change the result of 'grow'.)

Note: we don't reduce dictionaries in step 2.  For example, if we have
Eq (a,b), we don't simplify to (Eq a, Eq b).  So Q won't be different
after step 2.  However note that we may therefore quantify over more
type variables than we absolutely have to.

For the guts, we need a loop, that alternates context reduction and
improvement with unification.  E.g. Suppose we have

	class C x y | x->y where ...

and tcSimplify is called with:
	(C Int a, C Int b)
Then improvement unifies a with b, giving
	(C Int a, C Int a)

If we need to unify anything, we rattle round the whole thing all over
again.


\begin{code}
tcSimplifyInfer
	:: SDoc
	-> TcTyVarSet		-- fv(T); type vars
	-> [Inst]		-- Wanted
	-> TcM ([TcTyVar],	-- Tyvars to quantify (zonked and quantified)
		[Inst],		-- Dict Ids that must be bound here (zonked)
		TcDictBinds)	-- Bindings
	-- Any free (escaping) Insts are tossed into the environment
\end{code}


\begin{code}
tcSimplifyInfer doc tau_tvs wanted
  = do	{ tau_tvs1 <- zonkTcTyVarsAndFV (varSetElems tau_tvs)
	; wanted'  <- mappM zonkInst wanted	-- Zonk before deciding quantified tyvars
	; gbl_tvs  <- tcGetGlobalTyVars
	; let preds1   = fdPredsOfInsts wanted'
	      gbl_tvs1 = oclose preds1 gbl_tvs
	      qtvs     = grow preds1 tau_tvs1 `minusVarSet` gbl_tvs1
			-- See Note [Choosing which variables to quantify]

		-- To maximise sharing, remove from consideration any 
		-- constraints that don't mention qtvs at all
	; let (free, bound) = partition (isFreeWhenInferring qtvs) wanted'
	; extendLIEs free

		-- To make types simple, reduce as much as possible
	; traceTc (text "infer" <+> (ppr preds1 $$ ppr (grow preds1 tau_tvs1) $$ ppr gbl_tvs $$ 
		   ppr gbl_tvs1 $$ ppr free $$ ppr bound))
	; (irreds1, binds1) <- tryHardCheckLoop doc bound

		-- Note [Inference and implication constraints]
	; let want_dict d = tyVarsOfInst d `intersectsVarSet` qtvs
	; (irreds2, binds2) <- approximateImplications doc want_dict irreds1

		-- Now work out all over again which type variables to quantify,
		-- exactly in the same way as before, but starting from irreds2.  Why?
		-- a) By now improvment may have taken place, and we must *not*
		--    quantify over any variable free in the environment
		--    tc137 (function h inside g) is an example
		--
		-- b) Do not quantify over constraints that *now* do not
		--    mention quantified type variables, because they are
		--    simply ambiguous (or might be bound further out).  Example:
		--    	f :: Eq b => a -> (a, b)
		--    	g x = fst (f x)
		--    From the RHS of g we get the MethodInst f77 :: alpha -> (alpha, beta)
		--    We decide to quantify over 'alpha' alone, but free1 does not include f77
		--    because f77 mentions 'alpha'.  Then reducing leaves only the (ambiguous)
		--    constraint (Eq beta), which we dump back into the free set
		--    See test tcfail181
		--
		-- c) irreds may contain type variables not previously mentioned,
		--    e.g.  instance D a x => Foo [a] 
		--	    wanteds = Foo [a]
		--       Then after simplifying we'll get (D a x), and x is fresh
		-- 	 We must quantify over x else it'll be totally unbound
	; tau_tvs2 <- zonkTcTyVarsAndFV (varSetElems tau_tvs1)
	; gbl_tvs2 <- zonkTcTyVarsAndFV (varSetElems gbl_tvs1)
		-- Note that we start from gbl_tvs1
		-- We use tcGetGlobalTyVars, then oclose wrt preds2, because
		-- we've already put some of the original preds1 into frees
		-- E.g. 	wanteds = C a b	  (where a->b)
		--		gbl_tvs = {a}
		--		tau_tvs = {b}
		-- Then b is fixed by gbl_tvs, so (C a b) will be in free, and
		-- irreds2 will be empty.  But we don't want to generalise over b!
	; let preds2 = fdPredsOfInsts irreds2	-- irreds2 is zonked
	      qtvs   = grow preds2 tau_tvs2 `minusVarSet` oclose preds2 gbl_tvs2
	; let (free, irreds3) = partition (isFreeWhenInferring qtvs) irreds2
	; extendLIEs free

		-- Turn the quantified meta-type variables into real type variables
	; qtvs2 <- zonkQuantifiedTyVars (varSetElems qtvs)

		-- We can't abstract over any remaining unsolved 
		-- implications so instead just float them outwards. Ugh.
	; let (q_dicts0, implics) = partition isAbstractableInst irreds3
	; loc <- getInstLoc (ImplicOrigin doc)
	; implic_bind <- bindIrreds loc qtvs2 q_dicts0 implics

		-- Prepare equality instances for quantification
	; let (q_eqs0,q_dicts) = partition isEqInst q_dicts0
	; q_eqs <- mappM finalizeEqInst q_eqs0

	; return (qtvs2, q_eqs ++ q_dicts, binds1 `unionBags` binds2 `unionBags` implic_bind) }
	-- NB: when we are done, we might have some bindings, but
	-- the final qtvs might be empty.  See Note [NO TYVARS] below.

approximateImplications :: SDoc -> (Inst -> Bool) -> [Inst] -> TcM ([Inst], TcDictBinds)
-- Note [Inference and implication constraints]
-- Given a bunch of Dict and ImplicInsts, try to approximate the implications by
--	- fetching any dicts inside them that are free
--	- using those dicts as cruder constraints, to solve the implications
--	- returning the extra ones too

approximateImplications doc want_dict irreds
  | null extra_dicts 
  = return (irreds, emptyBag)
  | otherwise
  = do	{ extra_dicts' <- mapM cloneDict extra_dicts
	; tryHardCheckLoop doc (extra_dicts' ++ irreds) }
		-- By adding extra_dicts', we make them 
		-- available to solve the implication constraints
  where 
    extra_dicts = get_dicts (filter isImplicInst irreds)

    get_dicts :: [Inst] -> [Inst]	-- Returns only Dicts
	-- Find the wanted constraints in implication constraints that satisfy
	-- want_dict, and are not bound by forall's in the constraint itself
    get_dicts ds = concatMap get_dict ds

    get_dict d@(Dict {}) | want_dict d = [d]
		         | otherwise   = []
    get_dict (ImplicInst {tci_tyvars = tvs, tci_wanted = wanteds})
	= [ d | let tv_set = mkVarSet tvs
	      , d <- get_dicts wanteds 
	      , not (tyVarsOfInst d `intersectsVarSet` tv_set)]
    get_dict i@(EqInst {}) | want_dict i = [i]
			   | otherwise   = [] 
    get_dict other = pprPanic "approximateImplications" (ppr other)
\end{code}

Note [Inference and implication constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
Suppose we have a wanted implication constraint (perhaps arising from
a nested pattern match) like
	C a => D [a]
and we are now trying to quantify over 'a' when inferring the type for
a function.  In principle it's possible that there might be an instance
	instance (C a, E a) => D [a]
so the context (E a) would suffice.  The Right Thing is to abstract over
the implication constraint, but we don't do that (a) because it'll be
surprising to programmers and (b) because we don't have the machinery to deal
with 'given' implications.

So our best approximation is to make (D [a]) part of the inferred
context, so we can use that to discharge the implication. Hence
the strange function get_dicts in approximateImplications.

The common cases are more clear-cut, when we have things like
	forall a. C a => C b
Here, abstracting over (C b) is not an approximation at all -- but see
Note [Freeness and implications].
 
See Trac #1430 and test tc228.


\begin{code}
-----------------------------------------------------------
-- tcSimplifyInferCheck is used when we know the constraints we are to simplify
-- against, but we don't know the type variables over which we are going to quantify.
-- This happens when we have a type signature for a mutually recursive group
tcSimplifyInferCheck
	 :: InstLoc
	 -> TcTyVarSet		-- fv(T)
	 -> [Inst]		-- Given
	 -> [Inst]		-- Wanted
	 -> TcM ([TyVar],	-- Fully zonked, and quantified
		 TcDictBinds)	-- Bindings

tcSimplifyInferCheck loc tau_tvs givens wanteds
  = do	{ traceTc (text "tcSimplifyInferCheck <-" <+> ppr wanteds)
      	; (irreds, binds) <- gentleCheckLoop loc givens wanteds

	-- Figure out which type variables to quantify over
	-- You might think it should just be the signature tyvars,
	-- but in bizarre cases you can get extra ones
	-- 	f :: forall a. Num a => a -> a
	--	f x = fst (g (x, head [])) + 1
	--	g a b = (b,a)
	-- Here we infer g :: forall a b. a -> b -> (b,a)
	-- We don't want g to be monomorphic in b just because
	-- f isn't quantified over b.
	; let all_tvs = varSetElems (tau_tvs `unionVarSet` tyVarsOfInsts givens)
	; all_tvs <- zonkTcTyVarsAndFV all_tvs
	; gbl_tvs <- tcGetGlobalTyVars
	; let qtvs = varSetElems (all_tvs `minusVarSet` gbl_tvs)
		-- We could close gbl_tvs, but its not necessary for
		-- soundness, and it'll only affect which tyvars, not which
		-- dictionaries, we quantify over

	; qtvs' <- zonkQuantifiedTyVars qtvs

		-- Now we are back to normal (c.f. tcSimplCheck)
	; implic_bind <- bindIrreds loc qtvs' givens irreds

	; traceTc (text "tcSimplifyInferCheck ->" <+> ppr (implic_bind))
	; return (qtvs', binds `unionBags` implic_bind) }
\end{code}

Note [Squashing methods]
~~~~~~~~~~~~~~~~~~~~~~~~~
Be careful if you want to float methods more:
	truncate :: forall a. RealFrac a => forall b. Integral b => a -> b
From an application (truncate f i) we get
	t1 = truncate at f
	t2 = t1 at i
If we have also have a second occurrence of truncate, we get
	t3 = truncate at f
	t4 = t3 at i
When simplifying with i,f free, we might still notice that
t1=t3; but alas, the binding for t2 (which mentions t1)
may continue to float out!


Note [NO TYVARS]
~~~~~~~~~~~~~~~~~
	class Y a b | a -> b where
	    y :: a -> X b
	
	instance Y [[a]] a where
	    y ((x:_):_) = X x
	
	k :: X a -> X a -> X a

	g :: Num a => [X a] -> [X a]
	g xs = h xs
	    where
	    h ys = ys ++ map (k (y [[0]])) xs

The excitement comes when simplifying the bindings for h.  Initially
try to simplify {y @ [[t1]] t2, 0 @ t1}, with initial qtvs = {t2}.
From this we get t1:=:t2, but also various bindings.  We can't forget
the bindings (because of [LOOP]), but in fact t1 is what g is
polymorphic in.  

The net effect of [NO TYVARS] 

\begin{code}
isFreeWhenInferring :: TyVarSet -> Inst	-> Bool
isFreeWhenInferring qtvs inst
  =  isFreeWrtTyVars qtvs inst	-- Constrains no quantified vars
  && isInheritableInst inst	-- and no implicit parameter involved
				--   see Note [Inheriting implicit parameters]

{-	No longer used (with implication constraints)
isFreeWhenChecking :: TyVarSet	-- Quantified tyvars
	 	   -> NameSet	-- Quantified implicit parameters
		   -> Inst -> Bool
isFreeWhenChecking qtvs ips inst
  =  isFreeWrtTyVars qtvs inst
  && isFreeWrtIPs    ips inst
-}

isFreeWrtTyVars qtvs inst = tyVarsOfInst inst `disjointVarSet` qtvs
isFreeWrtIPs     ips inst = not (any (`elemNameSet` ips) (ipNamesOfInst inst))
\end{code}


%************************************************************************
%*									*
\subsection{tcSimplifyCheck}
%*									*
%************************************************************************

@tcSimplifyCheck@ is used when we know exactly the set of variables
we are going to quantify over.  For example, a class or instance declaration.

\begin{code}
-----------------------------------------------------------
-- tcSimplifyCheck is used when checking expression type signatures,
-- class decls, instance decls etc.
tcSimplifyCheck	:: InstLoc
	 	-> [TcTyVar]		-- Quantify over these
	 	-> [Inst]		-- Given
	 	-> [Inst]		-- Wanted
	 	-> TcM TcDictBinds	-- Bindings
tcSimplifyCheck loc qtvs givens wanteds 
  = ASSERT( all isTcTyVar qtvs && all isSkolemTyVar qtvs )
    do	{ traceTc (text "tcSimplifyCheck")
      	; (irreds, binds) <- gentleCheckLoop loc givens wanteds
	; implic_bind <- bindIrreds loc qtvs givens irreds
	; return (binds `unionBags` implic_bind) }

-----------------------------------------------------------
-- tcSimplifyCheckPat is used for existential pattern match
tcSimplifyCheckPat :: InstLoc
	 	   -> [CoVar] -> Refinement
	 	   -> [TcTyVar]		-- Quantify over these
	 	   -> [Inst]		-- Given
	 	   -> [Inst]		-- Wanted
	 	   -> TcM TcDictBinds	-- Bindings
tcSimplifyCheckPat loc co_vars reft qtvs givens wanteds
  = ASSERT( all isTcTyVar qtvs && all isSkolemTyVar qtvs )
    do	{ traceTc (text "tcSimplifyCheckPat")
      	; (irreds, binds) <- gentleCheckLoop loc givens wanteds
	; implic_bind <- bindIrredsR loc qtvs co_vars reft 
				    givens irreds
	; return (binds `unionBags` implic_bind) }

-----------------------------------------------------------
bindIrreds :: InstLoc -> [TcTyVar]
	   -> [Inst] -> [Inst]
	   -> TcM TcDictBinds
bindIrreds loc qtvs givens irreds 
  = bindIrredsR loc qtvs [] emptyRefinement givens irreds

bindIrredsR :: InstLoc -> [TcTyVar] -> [CoVar]
	    -> Refinement -> [Inst] -> [Inst]
	    -> TcM TcDictBinds	
-- Make a binding that binds 'irreds', by generating an implication
-- constraint for them, *and* throwing the constraint into the LIE
bindIrredsR loc qtvs co_vars reft givens irreds
  | null irreds
  = return emptyBag
  | otherwise
  = do	{ let givens' = filter isAbstractableInst givens
		-- The givens can (redundantly) include methods
		-- We want to retain both EqInsts and Dicts
		-- There should be no implicadtion constraints
		-- See Note [Pruning the givens in an implication constraint]

	   -- If there are no 'givens' *and* the refinement is empty
	   -- (the refinement is like more givens), then it's safe to 
	   -- partition the 'wanteds' by their qtvs, thereby trimming irreds
	   -- See Note [Freeness and implications]
	; irreds' <- if null givens' && isEmptyRefinement reft
	     	     then do
	     	    	{ let qtv_set = mkVarSet qtvs
	     	    	      (frees, real_irreds) = partition (isFreeWrtTyVars qtv_set) irreds
	     	    	; extendLIEs frees
	     	    	; return real_irreds }
	     	     else return irreds
	
	; let all_tvs = qtvs ++ co_vars	-- Abstract over all these
	; (implics, bind) <- makeImplicationBind loc all_tvs reft givens' irreds'
			-- This call does the real work
			-- If irreds' is empty, it does something sensible
	; extendLIEs implics
	; return bind } 


makeImplicationBind :: InstLoc -> [TcTyVar] -> Refinement
		    -> [Inst] -> [Inst]
		    -> TcM ([Inst], TcDictBinds)
-- Make a binding that binds 'irreds', by generating an implication
-- constraint for them, *and* throwing the constraint into the LIE
-- The binding looks like
--	(ir1, .., irn) = f qtvs givens
-- where f is (evidence for) the new implication constraint
--	f :: forall qtvs. {reft} givens => (ir1, .., irn)
-- qtvs includes coercion variables
--
-- This binding must line up the 'rhs' in reduceImplication
makeImplicationBind loc all_tvs reft
		    givens 	-- Guaranteed all Dicts
				-- or EqInsts
		    irreds
 | null irreds			-- If there are no irreds, we are done
 = return ([], emptyBag)
 | otherwise			-- Otherwise we must generate a binding
 = do	{ uniq <- newUnique 
	; span <- getSrcSpanM
	; let (eq_givens, dict_givens) = partition isEqInst givens
	      eq_tyvar_cos = mkTyVarTys (varSetElems $ tyVarsOfTypes $ map eqInstType eq_givens)
		-- Urgh! See line 2187 or thereabouts.  I believe that all these
		-- 'givens' must be a simple CoVar.  This MUST be cleaned up.

	; let name = mkInternalName uniq (mkVarOcc "ic") span
	      implic_inst = ImplicInst { tci_name = name, tci_reft = reft,
					 tci_tyvars = all_tvs, 
					 tci_given = (eq_givens ++ dict_givens),
					 tci_wanted = irreds, tci_loc = loc }
	; let   -- only create binder for dict_irreds
	      (eq_irreds, dict_irreds) = partition isEqInst irreds
              n_dict_irreds = length dict_irreds
	      dict_irred_ids = map instToId dict_irreds
	      tup_ty = mkTupleTy Boxed n_dict_irreds (map idType dict_irred_ids)
	      pat = TuplePat (map nlVarPat dict_irred_ids) Boxed tup_ty
	      rhs = L span (mkHsWrap co (HsVar (instToId implic_inst)))
	      co  = mkWpApps (map instToId dict_givens) <.> mkWpTyApps eq_tyvar_cos <.> mkWpTyApps (mkTyVarTys all_tvs)
	      bind | [dict_irred_id] <- dict_irred_ids  = VarBind dict_irred_id rhs
		   | otherwise        = PatBind { pat_lhs = L span pat, 
				                  pat_rhs = unguardedGRHSs rhs, 
				      	          pat_rhs_ty = tup_ty,
				      	          bind_fvs = placeHolderNames }
	; -- pprTrace "Make implic inst" (ppr (implic_inst,irreds,dict_irreds,tup_ty)) $
	  return ([implic_inst], unitBag (L span bind)) }

-----------------------------------------------------------
tryHardCheckLoop :: SDoc
	     -> [Inst]			-- Wanted
	     -> TcM ([Inst], TcDictBinds)

tryHardCheckLoop doc wanteds
  = do { (irreds,binds) <- checkLoop (mkRedEnv doc try_me []) wanteds
       ; return (irreds,binds)
       }
  where
    try_me inst = ReduceMe AddSCs
	-- Here's the try-hard bit

-----------------------------------------------------------
gentleCheckLoop :: InstLoc
	       -> [Inst]		-- Given
	       -> [Inst]		-- Wanted
	       -> TcM ([Inst], TcDictBinds)

gentleCheckLoop inst_loc givens wanteds
  = do { (irreds,binds) <- checkLoop env wanteds
       ; return (irreds,binds)
       }
  where
    env = mkRedEnv (pprInstLoc inst_loc) try_me givens

    try_me inst | isMethodOrLit inst = ReduceMe AddSCs
		| otherwise	     = Stop
	-- When checking against a given signature 
	-- we MUST be very gentle: Note [Check gently]

gentleInferLoop :: SDoc -> [Inst]
	        -> TcM ([Inst], TcDictBinds)
gentleInferLoop doc wanteds
  = do 	{ (irreds, binds) <- checkLoop env wanteds
	; return (irreds, binds) }
  where
    env = mkRedEnv doc try_me []
    try_me inst | isMethodOrLit inst = ReduceMe AddSCs
		| otherwise	     = Stop
\end{code}

Note [Check gently]
~~~~~~~~~~~~~~~~~~~~
We have to very careful about not simplifying too vigorously
Example:  
  data T a where
    MkT :: a -> T [a]

  f :: Show b => T b -> b
  f (MkT x) = show [x]

Inside the pattern match, which binds (a:*, x:a), we know that
	b ~ [a]
Hence we have a dictionary for Show [a] available; and indeed we 
need it.  We are going to build an implication contraint
	forall a. (b~[a]) => Show [a]
Later, we will solve this constraint using the knowledge (Show b)
	
But we MUST NOT reduce (Show [a]) to (Show a), else the whole
thing becomes insoluble.  So we simplify gently (get rid of literals
and methods only, plus common up equal things), deferring the real
work until top level, when we solve the implication constraint
with tryHardCheckLooop.


\begin{code}
-----------------------------------------------------------
checkLoop :: RedEnv
	  -> [Inst]			-- Wanted
	  -> TcM ([Inst], TcDictBinds)
-- Precondition: givens are completely rigid
-- Postcondition: returned Insts are zonked

checkLoop env wanteds
  = go env wanteds
  where go env wanteds
	  = do {  -- We do need to zonk the givens; cf Note [Zonking RedEnv]
                ; env'     <- zonkRedEnv env
		; wanteds' <- zonkInsts  wanteds
	
		; (improved, binds, irreds) <- reduceContext env' wanteds'

		; if not improved then
	 	     return (irreds, binds)
		  else do
	
		-- If improvement did some unification, we go round again.
		-- We start again with irreds, not wanteds
		-- Using an instance decl might have introduced a fresh type variable
		-- which might have been unified, so we'd get an infinite loop
		-- if we started again with wanteds!  See Note [LOOP]
		{ (irreds1, binds1) <- go env' irreds
		; return (irreds1, binds `unionBags` binds1) } }
\end{code}

Note [Zonking RedEnv]
~~~~~~~~~~~~~~~~~~~~~
It might appear as if the givens in RedEnv are always rigid, but that is not
necessarily the case for programs involving higher-rank types that have class
contexts constraining the higher-rank variables.  An example from tc237 in the
testsuite is

  class Modular s a | s -> a

  wim ::  forall a w. Integral a 
                        => a -> (forall s. Modular s a => M s w) -> w
  wim i k = error "urk"

  test5  ::  (Modular s a, Integral a) => M s a
  test5  =   error "urk"

  test4   =   wim 4 test4'

Notice how the variable 'a' of (Modular s a) in the rank-2 type of wim is
quantified further outside.  When type checking test4, we have to check
whether the signature of test5 is an instance of 

  (forall s. Modular s a => M s w)

Consequently, we will get (Modular s t_a), where t_a is a TauTv into the
givens. 

Given the FD of Modular in this example, class improvement will instantiate
t_a to 'a', where 'a' is the skolem from test5's signatures (due to the
Modular s a predicate in that signature).  If we don't zonk (Modular s t_a) in
the givens, we will get into a loop as improveOne uses the unification engine
TcGadt.tcUnifyTys, which doesn't know about mutable type variables.


Note [LOOP]
~~~~~~~~~~~
	class If b t e r | b t e -> r
	instance If T t e t
	instance If F t e e
	class Lte a b c | a b -> c where lte :: a -> b -> c
	instance Lte Z b T
	instance (Lte a b l,If l b a c) => Max a b c

Wanted:	Max Z (S x) y

Then we'll reduce using the Max instance to:
	(Lte Z (S x) l, If l (S x) Z y)
and improve by binding l->T, after which we can do some reduction
on both the Lte and If constraints.  What we *can't* do is start again
with (Max Z (S x) y)!



%************************************************************************
%*									*
		tcSimplifySuperClasses
%*									*
%************************************************************************

Note [SUPERCLASS-LOOP 1]
~~~~~~~~~~~~~~~~~~~~~~~~
We have to be very, very careful when generating superclasses, lest we
accidentally build a loop. Here's an example:

  class S a

  class S a => C a where { opc :: a -> a }
  class S b => D b where { opd :: b -> b }
  
  instance C Int where
     opc = opd
  
  instance D Int where
     opd = opc

From (instance C Int) we get the constraint set {ds1:S Int, dd:D Int}
Simplifying, we may well get:
	$dfCInt = :C ds1 (opd dd)
	dd  = $dfDInt
	ds1 = $p1 dd
Notice that we spot that we can extract ds1 from dd.  

Alas!  Alack! We can do the same for (instance D Int):

	$dfDInt = :D ds2 (opc dc)
	dc  = $dfCInt
	ds2 = $p1 dc

And now we've defined the superclass in terms of itself.

Solution: never generate a superclass selectors at all when
satisfying the superclass context of an instance declaration.

Two more nasty cases are in
	tcrun021
	tcrun033

\begin{code}
tcSimplifySuperClasses 
	:: InstLoc 
	-> [Inst]	-- Given 
	-> [Inst]	-- Wanted
	-> TcM TcDictBinds
tcSimplifySuperClasses loc givens sc_wanteds
  = do	{ traceTc (text "tcSimplifySuperClasses")
	; (irreds,binds1) <- checkLoop env sc_wanteds
	; let (tidy_env, tidy_irreds) = tidyInsts irreds
	; reportNoInstances tidy_env (Just (loc, givens)) tidy_irreds
	; return binds1 }
  where
    env = mkRedEnv (pprInstLoc loc) try_me givens
    try_me inst = ReduceMe NoSCs
	-- Like tryHardCheckLoop, but with NoSCs
\end{code}


%************************************************************************
%*									*
\subsection{tcSimplifyRestricted}
%*									*
%************************************************************************

tcSimplifyRestricted infers which type variables to quantify for a 
group of restricted bindings.  This isn't trivial.

Eg1:	id = \x -> x
	We want to quantify over a to get id :: forall a. a->a
	
Eg2:	eq = (==)
	We do not want to quantify over a, because there's an Eq a 
	constraint, so we get eq :: a->a->Bool	(notice no forall)

So, assume:
	RHS has type 'tau', whose free tyvars are tau_tvs
	RHS has constraints 'wanteds'

Plan A (simple)
  Quantify over (tau_tvs \ ftvs(wanteds))
  This is bad. The constraints may contain (Monad (ST s))
  where we have 	instance Monad (ST s) where...
  so there's no need to be monomorphic in s!

  Also the constraint might be a method constraint,
  whose type mentions a perfectly innocent tyvar:
	  op :: Num a => a -> b -> a
  Here, b is unconstrained.  A good example would be
	foo = op (3::Int)
  We want to infer the polymorphic type
	foo :: forall b. b -> b


Plan B (cunning, used for a long time up to and including GHC 6.2)
  Step 1: Simplify the constraints as much as possible (to deal 
  with Plan A's problem).  Then set
	qtvs = tau_tvs \ ftvs( simplify( wanteds ) )

  Step 2: Now simplify again, treating the constraint as 'free' if 
  it does not mention qtvs, and trying to reduce it otherwise.
  The reasons for this is to maximise sharing.

  This fails for a very subtle reason.  Suppose that in the Step 2
  a constraint (Foo (Succ Zero) (Succ Zero) b) gets thrown upstairs as 'free'.
  In the Step 1 this constraint might have been simplified, perhaps to
  (Foo Zero Zero b), AND THEN THAT MIGHT BE IMPROVED, to bind 'b' to 'T'.
  This won't happen in Step 2... but that in turn might prevent some other
  constraint (Baz [a] b) being simplified (e.g. via instance Baz [a] T where {..}) 
  and that in turn breaks the invariant that no constraints are quantified over.

  Test typecheck/should_compile/tc177 (which failed in GHC 6.2) demonstrates
  the problem.


Plan C (brutal)
  Step 1: Simplify the constraints as much as possible (to deal 
  with Plan A's problem).  Then set
	qtvs = tau_tvs \ ftvs( simplify( wanteds ) )
  Return the bindings from Step 1.
  

A note about Plan C (arising from "bug" reported by George Russel March 2004)
Consider this:

      instance (HasBinary ty IO) => HasCodedValue ty

      foo :: HasCodedValue a => String -> IO a

      doDecodeIO :: HasCodedValue a => () -> () -> IO a
      doDecodeIO codedValue view  
        = let { act = foo "foo" } in  act

You might think this should work becuase the call to foo gives rise to a constraint
(HasCodedValue t), which can be satisfied by the type sig for doDecodeIO.  But the
restricted binding act = ... calls tcSimplifyRestricted, and PlanC simplifies the
constraint using the (rather bogus) instance declaration, and now we are stuffed.

I claim this is not really a bug -- but it bit Sergey as well as George.  So here's
plan D


Plan D (a variant of plan B)
  Step 1: Simplify the constraints as much as possible (to deal 
  with Plan A's problem), BUT DO NO IMPROVEMENT.  Then set
	qtvs = tau_tvs \ ftvs( simplify( wanteds ) )

  Step 2: Now simplify again, treating the constraint as 'free' if 
  it does not mention qtvs, and trying to reduce it otherwise.

  The point here is that it's generally OK to have too few qtvs; that is,
  to make the thing more monomorphic than it could be.  We don't want to
  do that in the common cases, but in wierd cases it's ok: the programmer
  can always add a signature.  

  Too few qtvs => too many wanteds, which is what happens if you do less
  improvement.


\begin{code}
tcSimplifyRestricted 	-- Used for restricted binding groups
			-- i.e. ones subject to the monomorphism restriction
	:: SDoc
	-> TopLevelFlag
	-> [Name]		-- Things bound in this group
	-> TcTyVarSet		-- Free in the type of the RHSs
	-> [Inst]		-- Free in the RHSs
	-> TcM ([TyVar],	-- Tyvars to quantify (zonked and quantified)
		TcDictBinds)	-- Bindings
	-- tcSimpifyRestricted returns no constraints to
	-- quantify over; by definition there are none.
	-- They are all thrown back in the LIE

tcSimplifyRestricted doc top_lvl bndrs tau_tvs wanteds
	-- Zonk everything in sight
  = do	{ traceTc (text "tcSimplifyRestricted")
	; wanteds' <- zonkInsts wanteds

   	-- 'ReduceMe': Reduce as far as we can.  Don't stop at
	-- dicts; the idea is to get rid of as many type
	-- variables as possible, and we don't want to stop
	-- at (say) Monad (ST s), because that reduces
	-- immediately, with no constraint on s.
	--
	-- BUT do no improvement!  See Plan D above
	-- HOWEVER, some unification may take place, if we instantiate
	-- 	    a method Inst with an equality constraint
	; let env = mkNoImproveRedEnv doc (\i -> ReduceMe AddSCs)
	; (_imp, _binds, constrained_dicts) <- reduceContext env wanteds'

	-- Next, figure out the tyvars we will quantify over
	; tau_tvs' <- zonkTcTyVarsAndFV (varSetElems tau_tvs)
	; gbl_tvs' <- tcGetGlobalTyVars
	; constrained_dicts' <- zonkInsts constrained_dicts

	; let qtvs1 = tau_tvs' `minusVarSet` oclose (fdPredsOfInsts constrained_dicts) gbl_tvs'
				-- As in tcSimplifyInfer

		-- Do not quantify over constrained type variables:
		-- this is the monomorphism restriction
	      constrained_tvs' = tyVarsOfInsts constrained_dicts'
	      qtvs = qtvs1 `minusVarSet` constrained_tvs'
	      pp_bndrs = pprWithCommas (quotes . ppr) bndrs

	-- Warn in the mono
	; warn_mono <- doptM Opt_WarnMonomorphism
	; warnTc (warn_mono && (constrained_tvs' `intersectsVarSet` qtvs1))
		 (vcat[ ptext SLIT("the Monomorphism Restriction applies to the binding")
				<> plural bndrs <+> ptext SLIT("for") <+> pp_bndrs,
			ptext SLIT("Consider giving a type signature for") <+> pp_bndrs])

	; traceTc (text "tcSimplifyRestricted" <+> vcat [
		pprInsts wanteds, pprInsts constrained_dicts',
		ppr _binds,
		ppr constrained_tvs', ppr tau_tvs', ppr qtvs ])

	-- The first step may have squashed more methods than
	-- necessary, so try again, this time more gently, knowing the exact
	-- set of type variables to quantify over.
	--
	-- We quantify only over constraints that are captured by qtvs;
	-- these will just be a subset of non-dicts.  This in contrast
	-- to normal inference (using isFreeWhenInferring) in which we quantify over
	-- all *non-inheritable* constraints too.  This implements choice
	-- (B) under "implicit parameter and monomorphism" above.
	--
	-- Remember that we may need to do *some* simplification, to
	-- (for example) squash {Monad (ST s)} into {}.  It's not enough
	-- just to float all constraints
	--
	-- At top level, we *do* squash methods becuase we want to 
	-- expose implicit parameters to the test that follows
	; let is_nested_group = isNotTopLevel top_lvl
	      try_me inst | isFreeWrtTyVars qtvs inst,
			   (is_nested_group || isDict inst) = Stop
		          | otherwise  	         = ReduceMe AddSCs
	      env = mkNoImproveRedEnv doc try_me
	; (_imp, binds, irreds) <- reduceContext env wanteds'

	-- See "Notes on implicit parameters, Question 4: top level"
	; ASSERT( all (isFreeWrtTyVars qtvs) irreds )	-- None should be captured
	  if is_nested_group then
		extendLIEs irreds
	  else do { let (bad_ips, non_ips) = partition isIPDict irreds
		  ; addTopIPErrs bndrs bad_ips
		  ; extendLIEs non_ips }

	; qtvs' <- zonkQuantifiedTyVars (varSetElems qtvs)
	; return (qtvs', binds) }
\end{code}


%************************************************************************
%*									*
		tcSimplifyRuleLhs
%*									*
%************************************************************************

On the LHS of transformation rules we only simplify methods and constants,
getting dictionaries.  We want to keep all of them unsimplified, to serve
as the available stuff for the RHS of the rule.

Example.  Consider the following left-hand side of a rule
	
	f (x == y) (y > z) = ...

If we typecheck this expression we get constraints

	d1 :: Ord a, d2 :: Eq a

We do NOT want to "simplify" to the LHS

	forall x::a, y::a, z::a, d1::Ord a.
	  f ((==) (eqFromOrd d1) x y) ((>) d1 y z) = ...

Instead we want	

	forall x::a, y::a, z::a, d1::Ord a, d2::Eq a.
	  f ((==) d2 x y) ((>) d1 y z) = ...

Here is another example:

	fromIntegral :: (Integral a, Num b) => a -> b
	{-# RULES "foo"  fromIntegral = id :: Int -> Int #-}

In the rule, a=b=Int, and Num Int is a superclass of Integral Int. But
we *dont* want to get

	forall dIntegralInt.
	   fromIntegral Int Int dIntegralInt (scsel dIntegralInt) = id Int

because the scsel will mess up RULE matching.  Instead we want

	forall dIntegralInt, dNumInt.
	  fromIntegral Int Int dIntegralInt dNumInt = id Int

Even if we have 

	g (x == y) (y == z) = ..

where the two dictionaries are *identical*, we do NOT WANT

	forall x::a, y::a, z::a, d1::Eq a
	  f ((==) d1 x y) ((>) d1 y z) = ...

because that will only match if the dict args are (visibly) equal.
Instead we want to quantify over the dictionaries separately.

In short, tcSimplifyRuleLhs must *only* squash LitInst and MethInts, leaving
all dicts unchanged, with absolutely no sharing.  It's simpler to do this
from scratch, rather than further parameterise simpleReduceLoop etc

\begin{code}
tcSimplifyRuleLhs :: [Inst] -> TcM ([Inst], TcDictBinds)
tcSimplifyRuleLhs wanteds
  = go [] emptyBag wanteds
  where
    go dicts binds []
	= return (dicts, binds)
    go dicts binds (w:ws)
	| isDict w
	= go (w:dicts) binds ws
	| otherwise
	= do { w' <- zonkInst w  -- So that (3::Int) does not generate a call
				 -- to fromInteger; this looks fragile to me
	     ; lookup_result <- lookupSimpleInst w'
	     ; case lookup_result of
		 GenInst ws' rhs -> 
                   go dicts (addInstToDictBind binds w rhs) (ws' ++ ws)
		 NoInstance	 -> pprPanic "tcSimplifyRuleLhs" (ppr w)
	  }
\end{code}

tcSimplifyBracket is used when simplifying the constraints arising from
a Template Haskell bracket [| ... |].  We want to check that there aren't
any constraints that can't be satisfied (e.g. Show Foo, where Foo has no
Show instance), but we aren't otherwise interested in the results.
Nor do we care about ambiguous dictionaries etc.  We will type check
this bracket again at its usage site.

\begin{code}
tcSimplifyBracket :: [Inst] -> TcM ()
tcSimplifyBracket wanteds
  = do	{ tryHardCheckLoop doc wanteds
	; return () }
  where
    doc = text "tcSimplifyBracket"
\end{code}


%************************************************************************
%*									*
\subsection{Filtering at a dynamic binding}
%*									*
%************************************************************************

When we have
	let ?x = R in B

we must discharge all the ?x constraints from B.  We also do an improvement
step; if we have ?x::t1 and ?x::t2 we must unify t1, t2.

Actually, the constraints from B might improve the types in ?x. For example

	f :: (?x::Int) => Char -> Char
	let ?x = 3 in f 'c'

then the constraint (?x::Int) arising from the call to f will
force the binding for ?x to be of type Int.

\begin{code}
tcSimplifyIPs :: [Inst]		-- The implicit parameters bound here
	      -> [Inst]		-- Wanted
	      -> TcM TcDictBinds
	-- We need a loop so that we do improvement, and then
	-- (next time round) generate a binding to connect the two
	-- 	let ?x = e in ?x
	-- Here the two ?x's have different types, and improvement 
	-- makes them the same.

tcSimplifyIPs given_ips wanteds
  = do	{ wanteds'   <- zonkInsts wanteds
	; given_ips' <- zonkInsts given_ips
		-- Unusually for checking, we *must* zonk the given_ips

	; let env = mkRedEnv doc try_me given_ips'
	; (improved, binds, irreds) <- reduceContext env wanteds'

	; if not improved then 
		ASSERT( all is_free irreds )
		do { extendLIEs irreds
		   ; return binds }
	  else
		tcSimplifyIPs given_ips wanteds }
  where
    doc	   = text "tcSimplifyIPs" <+> ppr given_ips
    ip_set = mkNameSet (ipNamesOfInsts given_ips)
    is_free inst = isFreeWrtIPs ip_set inst

	-- Simplify any methods that mention the implicit parameter
    try_me inst | is_free inst = Stop
		| otherwise    = ReduceMe NoSCs
\end{code}


%************************************************************************
%*									*
\subsection[binds-for-local-funs]{@bindInstsOfLocalFuns@}
%*									*
%************************************************************************

When doing a binding group, we may have @Insts@ of local functions.
For example, we might have...
\begin{verbatim}
let f x = x + 1	    -- orig local function (overloaded)
    f.1 = f Int	    -- two instances of f
    f.2 = f Float
 in
    (f.1 5, f.2 6.7)
\end{verbatim}
The point is: we must drop the bindings for @f.1@ and @f.2@ here,
where @f@ is in scope; those @Insts@ must certainly not be passed
upwards towards the top-level.	If the @Insts@ were binding-ified up
there, they would have unresolvable references to @f@.

We pass in an @init_lie@ of @Insts@ and a list of locally-bound @Ids@.
For each method @Inst@ in the @init_lie@ that mentions one of the
@Ids@, we create a binding.  We return the remaining @Insts@ (in an
@LIE@), as well as the @HsBinds@ generated.

\begin{code}
bindInstsOfLocalFuns ::	[Inst] -> [TcId] -> TcM TcDictBinds
-- Simlifies only MethodInsts, and generate only bindings of form 
--	fm = f tys dicts
-- We're careful not to even generate bindings of the form
--	d1 = d2
-- You'd think that'd be fine, but it interacts with what is
-- arguably a bug in Match.tidyEqnInfo (see notes there)

bindInstsOfLocalFuns wanteds local_ids
  | null overloaded_ids
	-- Common case
  = extendLIEs wanteds		`thenM_`
    returnM emptyLHsBinds

  | otherwise
  = do	{ (irreds, binds) <- gentleInferLoop doc for_me
	; extendLIEs not_for_me	
	; extendLIEs irreds
	; return binds }
  where
    doc		     = text "bindInsts" <+> ppr local_ids
    overloaded_ids   = filter is_overloaded local_ids
    is_overloaded id = isOverloadedTy (idType id)
    (for_me, not_for_me) = partition (isMethodFor overloaded_set) wanteds

    overloaded_set = mkVarSet overloaded_ids	-- There can occasionally be a lot of them
						-- so it's worth building a set, so that
						-- lookup (in isMethodFor) is faster
\end{code}


%************************************************************************
%*									*
\subsection{Data types for the reduction mechanism}
%*									*
%************************************************************************

The main control over context reduction is here

\begin{code}
data RedEnv 
  = RedEnv { red_doc	:: SDoc			-- The context
	   , red_try_me :: Inst -> WhatToDo
	   , red_improve :: Bool		-- True <=> do improvement
	   , red_givens :: [Inst]		-- All guaranteed rigid
						-- Always dicts
						-- but see Note [Rigidity]
	   , red_reft :: Refinement		-- The refinement to apply to the 'givens'
						-- You should think of it as 'given equalities'
	   , red_stack  :: (Int, [Inst])	-- Recursion stack (for err msg)
						-- See Note [RedStack]
  }

-- Note [Rigidity]
-- The red_givens are rigid so far as cmpInst is concerned.
-- There is one case where they are not totally rigid, namely in tcSimplifyIPs
--	let ?x = e in ...
-- Here, the given is (?x::a), where 'a' is not necy a rigid type
-- But that doesn't affect the comparison, which is based only on mame.

-- Note [RedStack]
-- The red_stack pair (n,insts) pair is just used for error reporting.
-- 'n' is always the depth of the stack.
-- The 'insts' is the stack of Insts being reduced: to produce X
-- I had to produce Y, to produce Y I had to produce Z, and so on.


mkRedEnv :: SDoc -> (Inst -> WhatToDo) -> [Inst] -> RedEnv
mkRedEnv doc try_me givens
  = RedEnv { red_doc = doc, red_try_me = try_me,
	     red_givens = givens, 
	     red_reft = emptyRefinement,
	     red_stack = (0,[]),
	     red_improve = True }	

mkNoImproveRedEnv :: SDoc -> (Inst -> WhatToDo) -> RedEnv
-- Do not do improvement; no givens
mkNoImproveRedEnv doc try_me
  = RedEnv { red_doc = doc, red_try_me = try_me,
	     red_givens = [], red_reft = emptyRefinement,
	     red_stack = (0,[]),
	     red_improve = True }	

data WhatToDo
 = ReduceMe WantSCs	-- Try to reduce this
			-- If there's no instance, add the inst to the 
			-- irreductible ones, but don't produce an error 
			-- message of any kind.
			-- It might be quite legitimate such as (Eq a)!

 | Stop		-- Return as irreducible unless it can
			-- be reduced to a constant in one step
			-- Do not add superclasses; see 

data WantSCs = NoSCs | AddSCs	-- Tells whether we should add the superclasses
				-- of a predicate when adding it to the avails
	-- The reason for this flag is entirely the super-class loop problem
	-- Note [SUPER-CLASS LOOP 1]

zonkRedEnv :: RedEnv -> TcM RedEnv
zonkRedEnv env 
  = do { givens' <- mappM zonkInst (red_givens env)
       ; return $ env {red_givens = givens'}
       }
\end{code}


%************************************************************************
%*									*
\subsection[reduce]{@reduce@}
%*									*
%************************************************************************

Note [Ancestor Equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~
During context reduction, we add to the wanted equalities also those
equalities that (transitively) occur in superclass contexts of wanted
class constraints.  Consider the following code

  class a ~ Int => C a
  instance C Int

If (C a) is wanted, we want to add (a ~ Int), which will be discharged by
substituting Int for a.  Hence, we ultimately want (C Int), which we
discharge with the explicit instance.

\begin{code}
reduceContext :: RedEnv
	      -> [Inst]			-- Wanted
	      -> TcM (ImprovementDone,
		      TcDictBinds,	-- Dictionary bindings
		      [Inst])		-- Irreducible

reduceContext env wanteds
  = do	{ traceTc (text "reduceContext" <+> (vcat [
	     text "----------------------",
	     red_doc env,
	     text "given" <+> ppr (red_givens env),
	     text "wanted" <+> ppr wanteds,
	     text "----------------------"
	     ]))


	; let givens 		 	   = red_givens env
	      (given_eqs0, given_dicts0)   = partition isEqInst givens
	      (wanted_eqs0, wanted_non_eqs) = partition isEqInst wanteds
	      (wanted_implics0, wanted_dicts0) = partition isImplicInst wanted_non_eqs

          -- We want to add as wanted equalities those that (transitively) 
          -- occur in superclass contexts of wanted class constraints.
          -- See Note [Ancestor Equalities]
	; ancestor_eqs <- ancestorEqualities wanted_dicts0
        ; let wanted_eqs = wanted_eqs0 ++ ancestor_eqs
	; traceTc $ text "reduceContext: ancestor eqs" <+> ppr ancestor_eqs

	  -- 1. Normalise the *given* *equality* constraints
	; (given_eqs, eliminate_skolems) <- normaliseGivenEqs given_eqs0

	  -- 2. Normalise the *given* *dictionary* constraints
	  --    wrt. the toplevel and given equations
	; (given_dicts, given_binds) <- normaliseGivenDicts given_eqs
                                                            given_dicts0

          -- 5. Build the Avail mapping from "given_dicts"
	  --    Add dicts refined by the current type refinement
	; (init_state, extra_givens) <- getLIE $ do 
		{ init_state <- foldlM addGiven emptyAvails given_dicts
		; let reft = red_reft env
		; if isEmptyRefinement reft then return init_state
		  else foldlM (addRefinedGiven reft)
				    init_state given_dicts }

	-- *** ToDo: what to do with the "extra_givens"?  For the
	-- moment I'm simply discarding them, which is probably wrong

	  -- 7. Normalise the *wanted* *dictionary* constraints
	  --    wrt. the toplevel and given equations
	  -- NB: normalisation includes zonking as part of what it does
	  --     so it's important to do it after any unifications
	  --     that happened as a result of the addGivens
	; (wanted_dicts,normalise_binds1) <- normaliseWantedDicts given_eqs wanted_dicts0

          -- 6. Solve the *wanted* *dictionary* constraints (not implications)
	  --    This may expose some further equational constraints...
	; (avails, extra_eqs) <- getLIE (reduceList env wanted_dicts init_state)
	; (dict_binds, bound_dicts, dict_irreds) <- extractResults avails wanted_dicts
	; traceTc $ text "reduceContext extractresults" <+> vcat
		      [ppr avails,ppr wanted_dicts,ppr dict_binds]

	-- *** ToDo: what to do with the "extra_eqs"?  For the
	-- moment I'm simply discarding them, which is probably wrong

     -- Solve the wanted *implications*.  In doing so, we can provide
     -- as "given"   all the dicts that were originally given,
     --          *or* for which we now have bindings,
     --          *or* which are now irreds
   ; let implic_env = env { red_givens = givens ++ bound_dicts ++ dict_irreds }
   ; (implic_binds_s, implic_irreds_s) <- mapAndUnzipM (reduceImplication implic_env) wanted_implics0
   ; let implic_binds  = unionManyBags implic_binds_s
         implic_irreds = concat implic_irreds_s


	  -- 3. Solve the *wanted* *equation* constraints
	; eq_irreds0 <- solveWantedEqs given_eqs wanted_eqs

	  -- 4. Normalise the *wanted* equality constraints with respect to
	  --    each other 
	; eq_irreds <- normaliseWantedEqs eq_irreds0

	  -- 8. Substitute the wanted *equations* in the wanted *dictionaries*
    ; let irreds = dict_irreds ++ implic_irreds
    ; (norm_irreds, normalise_binds2) <- substEqInDictInsts eq_irreds irreds
		
	  -- 9. eliminate the artificial skolem constants introduced in 1.
	; eliminate_skolems	

	  -- Figure out whether we should go round again
  	  -- My current plan is to see if any of the mutable tyvars in
	  -- givens or irreds has been filled in by improvement.  
	  -- If so, there is merit in going around again, because
	  -- we may make further progress
	  -- 
	  -- ToDo: is it only mutable stuff?  We may have exposed new
	  --	   equality constraints and should probably go round again
	  --	   then as well.  But currently we are dropping them on the
	  --	   floor anyway.

	; let all_irreds = norm_irreds ++ eq_irreds
	; improved <- anyM isFilledMetaTyVar $ varSetElems $
		      tyVarsOfInsts (givens ++ all_irreds)

	-- The old plan (fragile)
	-- improveed   = availsImproved avails 
	--		 || (not $ isEmptyBag normalise_binds1)
	--		 || (not $ isEmptyBag normalise_binds2)
	--		 || (any isEqInst irreds)

	; traceTc (text "reduceContext end" <+> (vcat [
	     text "----------------------",
	     red_doc env,
	     text "given" <+> ppr givens,
	     text "given_eqs" <+> ppr given_eqs,
	     text "wanted" <+> ppr wanteds,
	     text "wanted_dicts" <+> ppr wanted_dicts,
	     text "----",
	     text "avails" <+> pprAvails avails,
	     text "improved =" <+> ppr improved,
	     text "irreds = " <+> ppr irreds,
	     text "dict-binds = " <+> ppr dict_binds,
	     text "implic-binds = " <+> ppr implic_binds,
	     text "----------------------"
	     ]))

	; return (improved, 
                  given_binds `unionBags` normalise_binds1 
                              `unionBags` normalise_binds2 
                              `unionBags` dict_binds
                              `unionBags` implic_binds,
                  all_irreds)
        }

tcImproveOne :: Avails -> Inst -> TcM ImprovementDone
tcImproveOne avails inst
  | not (isDict inst) = return False
  | otherwise
  = do	{ inst_envs <- tcGetInstEnvs
	; let eqns = improveOne (classInstances inst_envs)
				(dictPred inst, pprInstArising inst)
				[ (dictPred p, pprInstArising p)
				| p <- availsInsts avails, isDict p ]
		-- Avails has all the superclasses etc (good)
		-- It also has all the intermediates of the deduction (good)
		-- It does not have duplicates (good)
		-- NB that (?x::t1) and (?x::t2) will be held separately in avails
		--    so that improve will see them separate
	; traceTc (text "improveOne" <+> ppr inst)
	; unifyEqns eqns }

unifyEqns :: [(Equation,(PredType,SDoc),(PredType,SDoc))] 
	  -> TcM ImprovementDone
unifyEqns [] = return False
unifyEqns eqns
  = do	{ traceTc (ptext SLIT("Improve:") <+> vcat (map pprEquationDoc eqns))
        ; mappM_ unify eqns
	; return True }
  where
    unify ((qtvs, pairs), what1, what2)
	 = addErrCtxtM (mkEqnMsg what1 what2)	$
	   tcInstTyVars (varSetElems qtvs)	`thenM` \ (_, _, tenv) ->
	   mapM_ (unif_pr tenv) pairs
    unif_pr tenv (ty1,ty2) =  unifyType (substTy tenv ty1) (substTy tenv ty2)

pprEquationDoc (eqn, (p1,w1), (p2,w2)) = vcat [pprEquation eqn, nest 2 (ppr p1), nest 2 (ppr p2)]

mkEqnMsg (pred1,from1) (pred2,from2) tidy_env
  = do	{ pred1' <- zonkTcPredType pred1; pred2' <- zonkTcPredType pred2
	; let { pred1'' = tidyPred tidy_env pred1'; pred2'' = tidyPred tidy_env pred2' }
	; let msg = vcat [ptext SLIT("When using functional dependencies to combine"),
			  nest 2 (sep [ppr pred1'' <> comma, nest 2 from1]), 
			  nest 2 (sep [ppr pred2'' <> comma, nest 2 from2])]
	; return (tidy_env, msg) }
\end{code}

The main context-reduction function is @reduce@.  Here's its game plan.

\begin{code}
reduceList :: RedEnv -> [Inst] -> Avails -> TcM Avails
reduceList env@(RedEnv {red_stack = (n,stk)}) wanteds state
  = do 	{ traceTc (text "reduceList " <+> (ppr wanteds $$ ppr state))
	; dopts <- getDOpts
#ifdef DEBUG
	; if n > 8 then
		dumpTcRn (hang (ptext SLIT("Interesting! Context reduction stack depth") <+> int n) 
			     2 (ifPprDebug (nest 2 (pprStack stk))))
	  else return ()
#endif
	; if n >= ctxtStkDepth dopts then
	    failWithTc (reduceDepthErr n stk)
	  else
	    go wanteds state }
  where
    go []     state = return state
    go (w:ws) state = do { state' <- reduce (env {red_stack = (n+1, w:stk)}) w state
			 ; go ws state' }

    -- Base case: we're done!
reduce env wanted avails
    -- It's the same as an existing inst, or a superclass thereof
  | Just avail <- findAvail avails wanted
  = do { traceTc (text "reduce: found " <+> ppr wanted)
       ; returnM avails	
       }

  | otherwise
  = do	{ traceTc (text "reduce" <+> ppr wanted $$ ppr avails)
	; case red_try_me env wanted of {
	    Stop -> try_simple (addIrred NoSCs);
			-- See Note [No superclasses for Stop]

	    ReduceMe want_scs -> do	-- It should be reduced
		{ (avails, lookup_result) <- reduceInst env avails wanted
		; case lookup_result of
		    NoInstance -> addIrred want_scs avails wanted
			     -- Add it and its superclasses
		    	     
		    GenInst [] rhs -> addWanted want_scs avails wanted rhs []

		    GenInst wanteds' rhs
			  -> do	{ avails1 <- addIrred NoSCs avails wanted
				; avails2 <- reduceList env wanteds' avails1
				; addWanted want_scs avails2 wanted rhs wanteds' } }
		-- Temporarily do addIrred *before* the reduceList, 
		-- which has the effect of adding the thing we are trying
		-- to prove to the database before trying to prove the things it
		-- needs.  See note [RECURSIVE DICTIONARIES]
		-- NB: we must not do an addWanted before, because that adds the
		--     superclasses too, and that can lead to a spurious loop; see
		--     the examples in [SUPERCLASS-LOOP]
		-- So we do an addIrred before, and then overwrite it afterwards with addWanted
    } }
  where
  	-- First, see if the inst can be reduced to a constant in one step
	-- Works well for literals (1::Int) and constant dictionaries (d::Num Int)
	-- Don't bother for implication constraints, which take real work
    try_simple do_this_otherwise
      = do { res <- lookupSimpleInst wanted
	   ; case res of
	        GenInst [] rhs -> addWanted AddSCs avails wanted rhs []
	        other	       -> do_this_otherwise avails wanted }
\end{code}


Note [SUPERCLASS-LOOP 2]
~~~~~~~~~~~~~~~~~~~~~~~~
But the above isn't enough.  Suppose we are *given* d1:Ord a,
and want to deduce (d2:C [a]) where

	class Ord a => C a where
	instance Ord [a] => C [a] where ...

Then we'll use the instance decl to deduce C [a] from Ord [a], and then add the
superclasses of C [a] to avails.  But we must not overwrite the binding
for Ord [a] (which is obtained from Ord a) with a superclass selection or we'll just
build a loop! 

Here's another variant, immortalised in tcrun020
	class Monad m => C1 m
	class C1 m => C2 m x
	instance C2 Maybe Bool
For the instance decl we need to build (C1 Maybe), and it's no good if
we run around and add (C2 Maybe Bool) and its superclasses to the avails 
before we search for C1 Maybe.

Here's another example 
 	class Eq b => Foo a b
	instance Eq a => Foo [a] a
If we are reducing
	(Foo [t] t)

we'll first deduce that it holds (via the instance decl).  We must not
then overwrite the Eq t constraint with a superclass selection!

At first I had a gross hack, whereby I simply did not add superclass constraints
in addWanted, though I did for addGiven and addIrred.  This was sub-optimal,
becuase it lost legitimate superclass sharing, and it still didn't do the job:
I found a very obscure program (now tcrun021) in which improvement meant the
simplifier got two bites a the cherry... so something seemed to be an Stop
first time, but reducible next time.

Now we implement the Right Solution, which is to check for loops directly 
when adding superclasses.  It's a bit like the occurs check in unification.


Note [RECURSIVE DICTIONARIES]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider 
    data D r = ZeroD | SuccD (r (D r));
    
    instance (Eq (r (D r))) => Eq (D r) where
        ZeroD     == ZeroD     = True
        (SuccD a) == (SuccD b) = a == b
        _         == _         = False;
    
    equalDC :: D [] -> D [] -> Bool;
    equalDC = (==);

We need to prove (Eq (D [])).  Here's how we go:

	d1 : Eq (D [])

by instance decl, holds if
	d2 : Eq [D []]
	where 	d1 = dfEqD d2

by instance decl of Eq, holds if
	d3 : D []
	where	d2 = dfEqList d3
		d1 = dfEqD d2

But now we can "tie the knot" to give

	d3 = d1
	d2 = dfEqList d3
	d1 = dfEqD d2

and it'll even run!  The trick is to put the thing we are trying to prove
(in this case Eq (D []) into the database before trying to prove its
contributing clauses.
	

%************************************************************************
%*									*
		Reducing a single constraint
%*									*
%************************************************************************

\begin{code}
---------------------------------------------
reduceInst :: RedEnv -> Avails -> Inst -> TcM (Avails, LookupInstResult)
reduceInst env avails other_inst
  = do	{ result <- lookupSimpleInst other_inst
	; return (avails, result) }
\end{code}

Note [Equational Constraints in Implication Constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An implication constraint is of the form 
	Given => Wanted 
where Given and Wanted may contain both equational and dictionary
constraints. The delay and reduction of these two kinds of constraints
is distinct:

-) In the generated code, wanted Dictionary constraints are wrapped up in an
   implication constraint that is created at the code site where the wanted
   dictionaries can be reduced via a let-binding. This let-bound implication
   constraint is deconstructed at the use-site of the wanted dictionaries.

-) While the reduction of equational constraints is also delayed, the delay
   is not manifest in the generated code. The required evidence is generated
   in the code directly at the use-site. There is no let-binding and deconstruction
   necessary. The main disadvantage is that we cannot exploit sharing as the
   same evidence may be generated at multiple use-sites. However, this disadvantage
   is limited because it only concerns coercions which are erased.

The different treatment is motivated by the different in representation. Dictionary
constraints require manifest runtime dictionaries, while equations require coercions
which are types.

\begin{code}
---------------------------------------------
reduceImplication :: RedEnv -> Inst -> TcM (TcDictBinds, [Inst])
\end{code}

Suppose we are simplifying the constraint
	forall bs. extras => wanted
in the context of an overall simplification problem with givens 'givens',
and refinment 'reft'.

Note that
  * The refinement is often empty

  * The 'extra givens' need not mention any of the quantified type variables
	e.g. 	forall {}. Eq a => Eq [a]
		forall {}. C Int => D (Tree Int)

    This happens when you have something like
	data T a where
	  T1 :: Eq a => a -> T a

	f :: T a -> Int
	f x = ...(case x of { T1 v -> v==v })...

\begin{code}
	-- ToDo: should we instantiate tvs?  I think it's not necessary
	--
	-- Note on coercion variables:
	--
	--	The extra given coercion variables are bound at two different sites:
	--	-) in the creation context of the implication constraint	
	--		the solved equational constraints use these binders
	--
	--	-) at the solving site of the implication constraint
	--		the solved dictionaries use these binders		
	--		these binders are generated by reduceImplication
	--
reduceImplication env
   orig_implic@(ImplicInst { tci_name = name, tci_loc = inst_loc,
                     tci_tyvars = tvs, tci_reft = reft,
                     tci_given = extra_givens, tci_wanted = wanteds })
  = do	{ 	-- Add refined givens, and the extra givens
		-- Todo fix this 
--	  (refined_red_givens,refined_avails)
--		<- if isEmptyRefinement reft then return (red_givens env,orig_avails)
--		   else foldlM (addRefinedGiven reft) ([],orig_avails) (red_givens env)
--	Commented out SLPJ Sept 07; see comment with extractLocalResults below
	  let refined_red_givens = []

		-- Solve the sub-problem
	; let try_me inst = ReduceMe AddSCs	-- Note [Freeness and implications]
	      env' = env { red_givens = extra_givens ++ red_givens env
			 , red_reft = reft
			 , red_doc = sep [ptext SLIT("reduceImplication for") <+> ppr name,
					  nest 2 (parens $ ptext SLIT("within") <+> red_doc env)]
			 , red_try_me = try_me }

	; traceTc (text "reduceImplication" <+> vcat
			[ ppr (red_givens env), ppr extra_givens,
			  ppr reft, ppr wanteds])
	; (irreds,binds) <- checkLoop env' wanteds
	; let   (extra_eq_givens, extra_dict_givens) = partition isEqInst extra_givens
			-- SLPJ Sept 07: I think this is bogus; currently
			-- there are no Eqinsts in extra_givens
		dict_ids = map instToId extra_dict_givens 

		-- Note [Reducing implication constraints]
		-- Tom -- update note, put somewhere!

	; traceTc (text "reduceImplication result" <+> vcat
			[ppr irreds, ppr binds])

	; -- extract superclass binds
	  --  (sc_binds,_) <- extractResults avails []
-- 	; traceTc (text "reduceImplication sc_binds" <+> vcat
--			[ppr sc_binds, ppr avails])
--  

	-- SLPJ Sept 07: what if improvement happened inside the checkLoop?
	-- Then we must iterate the outer loop too!

	; traceTc (text "reduceImplication condition" <+> ppr ((isEmptyLHsBinds binds) || (null irreds)))

--	Progress is no longer measered by the number of bindings
	; if (isEmptyLHsBinds binds) && (not $ null irreds) then 	-- No progress
		-- If there are any irreds, we back off and do nothing
		return (emptyBag, [orig_implic])
	  else do
	{ (simpler_implic_insts, bind) <- makeImplicationBind inst_loc tvs reft extra_givens irreds
			-- This binding is useless if the recursive simplification
			-- made no progress; but currently we don't try to optimise that
			-- case.  After all, we only try hard to reduce at top level, or
			-- when inferring types.

	; let	dict_wanteds = filter (not . isEqInst) wanteds
		-- TOMDO: given equational constraints bug!
		--  we need a different evidence for given
		--  equations depending on whether we solve
		--  dictionary constraints or equational constraints

		eq_tyvars = varSetElems $ tyVarsOfTypes $ map eqInstType extra_eq_givens
			-- SLPJ Sept07: this looks Utterly Wrong to me, but I think
			--	        that current extra_givens has no EqInsts, so
			--		it makes no difference
		-- dict_ids = map instToId extra_givens
		co  = mkWpTyLams tvs <.> mkWpTyLams eq_tyvars <.> mkWpLams dict_ids <.> WpLet (binds `unionBags` bind)
		rhs = mkHsWrap co payload
	        loc = instLocSpan inst_loc
		payload | [dict_wanted] <- dict_wanteds = HsVar (instToId dict_wanted)
			| otherwise = ExplicitTuple (map (L loc . HsVar . instToId) dict_wanteds) Boxed

	
	; traceTc (vcat [text "reduceImplication" <+> ppr name,
			 ppr simpler_implic_insts,
			 text "->" <+> ppr rhs])
	; return (unitBag (L loc (VarBind (instToId orig_implic) (L loc rhs))),
              simpler_implic_insts)
  	} 
    }
\end{code}

Note [Freeness and implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's hard to say when an implication constraint can be floated out.  Consider
	forall {} Eq a => Foo [a]
The (Foo [a]) doesn't mention any of the quantified variables, but it
still might be partially satisfied by the (Eq a). 

There is a useful special case when it *is* easy to partition the 
constraints, namely when there are no 'givens'.  Consider
	forall {a}. () => Bar b
There are no 'givens', and so there is no reason to capture (Bar b).
We can let it float out.  But if there is even one constraint we
must be much more careful:
	forall {a}. C a b => Bar (m b)
because (C a b) might have a superclass (D b), from which we might 
deduce (Bar [b]) when m later gets instantiated to [].  Ha!

Here is an even more exotic example
	class C a => D a b
Now consider the constraint
	forall b. D Int b => C Int
We can satisfy the (C Int) from the superclass of D, so we don't want
to float the (C Int) out, even though it mentions no type variable in
the constraints!

Note [Pruning the givens in an implication constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are about to form the implication constraint
	forall tvs.  Eq a => Ord b
The (Eq a) cannot contribute to the (Ord b), because it has no access to
the type variable 'b'.  So we could filter out the (Eq a) from the givens.

Doing so would be a bit tidier, but all the implication constraints get
simplified away by the optimiser, so it's no great win.   So I don't take
advantage of that at the moment.

If you do, BE CAREFUL of wobbly type variables.


%************************************************************************
%*									*
		Avails and AvailHow: the pool of evidence
%*									*
%************************************************************************


\begin{code}
data Avails = Avails !ImprovementDone !AvailEnv

type ImprovementDone = Bool	-- True <=> some unification has happened
				-- so some Irreds might now be reducible
				-- keys that are now 

type AvailEnv = FiniteMap Inst AvailHow
data AvailHow
  = IsIrred 		-- Used for irreducible dictionaries,
			-- which are going to be lambda bound

  | Given Inst 		-- Used for dictionaries for which we have a binding
			-- e.g. those "given" in a signature

  | Rhs 		-- Used when there is a RHS
	(LHsExpr TcId) 	-- The RHS
	[Inst]		-- Insts free in the RHS; we need these too

instance Outputable Avails where
  ppr = pprAvails

pprAvails (Avails imp avails)
  = vcat [ ptext SLIT("Avails") <> (if imp then ptext SLIT("[improved]") else empty)
	 , nest 2 $ braces $ 
	   vcat [ sep [ppr inst, nest 2 (equals <+> ppr avail)]
		| (inst,avail) <- fmToList avails ]]

instance Outputable AvailHow where
    ppr = pprAvail

-------------------------
pprAvail :: AvailHow -> SDoc
pprAvail IsIrred	= text "Irred"
pprAvail (Given x)   	= text "Given" <+> ppr x
pprAvail (Rhs rhs bs)   = sep [text "Rhs" <+> ppr bs,
			       nest 2 (ppr rhs)]

-------------------------
extendAvailEnv :: AvailEnv -> Inst -> AvailHow -> AvailEnv
extendAvailEnv env inst avail = addToFM env inst avail

findAvailEnv :: AvailEnv -> Inst -> Maybe AvailHow
findAvailEnv env wanted = lookupFM env wanted
	-- NB 1: the Ord instance of Inst compares by the class/type info
	--  *not* by unique.  So
	--	d1::C Int ==  d2::C Int

emptyAvails :: Avails
emptyAvails = Avails False emptyFM

findAvail :: Avails -> Inst -> Maybe AvailHow
findAvail (Avails _ avails) wanted = findAvailEnv avails wanted

elemAvails :: Inst -> Avails -> Bool
elemAvails wanted (Avails _ avails) = wanted `elemFM` avails

extendAvails :: Avails -> Inst -> AvailHow -> TcM Avails
-- Does improvement
extendAvails avails@(Avails imp env) inst avail 
  = do	{ imp1 <- tcImproveOne avails inst	-- Do any improvement
	; return (Avails (imp || imp1) (extendAvailEnv env inst avail)) }

availsInsts :: Avails -> [Inst]
availsInsts (Avails _ avails) = keysFM avails

availsImproved (Avails imp _) = imp

updateImprovement :: Avails -> Avails -> Avails
-- (updateImprovement a1 a2) sets a1's improvement flag from a2
updateImprovement (Avails _ avails1) (Avails imp2 _) = Avails imp2 avails1
\end{code}

Extracting the bindings from a bunch of Avails.
The bindings do *not* come back sorted in dependency order.
We assume that they'll be wrapped in a big Rec, so that the
dependency analyser can sort them out later

\begin{code}
type DoneEnv = FiniteMap Inst [Id]
-- Tracks which things we have evidence for

extractResults :: Avails
	       -> [Inst]		-- Wanted
            -> TcM (TcDictBinds,     -- Bindings
                [Inst],      -- The insts bound by the bindings
                [Inst])      -- Irreducible ones
			-- Note [Reducing implication constraints]

extractResults (Avails _ avails) wanteds
  = go emptyBag [] [] emptyFM wanteds
  where
    go	:: TcDictBinds 	-- Bindings for dicts
	-> [Inst]   -- Bound by the bindings
	-> [Inst]	-- Irreds
	-> DoneEnv	-- Has an entry for each inst in the above three sets
	-> [Inst]	-- Wanted
	-> TcM (TcDictBinds, [Inst], [Inst])
    go binds bound_dicts irreds done []
      = return (binds, bound_dicts, irreds)

    go binds bound_dicts irreds done (w:ws)
      | Just done_ids@(done_id : rest_done_ids) <- lookupFM done w
      = if w_id `elem` done_ids then
	   go binds bound_dicts irreds done ws
	else
	   go (add_bind (nlHsVar done_id)) bound_dicts irreds
	      (addToFM done w (done_id : w_id : rest_done_ids)) ws

      | otherwise	-- Not yet done
      = case findAvailEnv avails w of
	  Nothing -> pprTrace "Urk: extractResults" (ppr w) $
		     go binds bound_dicts irreds done ws

	  Just IsIrred -> go binds bound_dicts (w:irreds) done' ws

	  Just (Rhs rhs ws') -> go (add_bind rhs) (w:bound_dicts) irreds done' (ws' ++ ws)

	  Just (Given g) -> go binds' bound_dicts irreds (addToFM done w [g_id]) ws 
		where
		  g_id = instToId g
		  binds' | w_id == g_id = binds
			 | otherwise    = add_bind (nlHsVar g_id)
      where
	w_id  = instToId w	
	done' = addToFM done w [w_id]
	add_bind rhs = addInstToDictBind binds w rhs
\end{code}


Note [No superclasses for Stop]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we decide not to reduce an Inst -- the 'WhatToDo' --- we still
add it to avails, so that any other equal Insts will be commoned up
right here.  However, we do *not* add superclasses.  If we have
	df::Floating a
	dn::Num a
but a is not bound here, then we *don't* want to derive dn from df
here lest we lose sharing.

\begin{code}
addWanted :: WantSCs -> Avails -> Inst -> LHsExpr TcId -> [Inst] -> TcM Avails
addWanted want_scs avails wanted rhs_expr wanteds
  = addAvailAndSCs want_scs avails wanted avail
  where
    avail = Rhs rhs_expr wanteds

addGiven :: Avails -> Inst -> TcM Avails
addGiven avails given = addAvailAndSCs AddSCs avails given (Given given)
	-- Always add superclasses for 'givens'
	--
	-- No ASSERT( not (given `elemAvails` avails) ) because in an instance
	-- decl for Ord t we can add both Ord t and Eq t as 'givens', 
	-- so the assert isn't true

addRefinedGiven :: Refinement -> Avails -> Inst -> TcM Avails
addRefinedGiven reft avails given
  | isDict given	-- We sometimes have 'given' methods, but they
			-- are always optional, so we can drop them
  , let pred = dictPred given
  , isRefineablePred pred	-- See Note [ImplicInst rigidity]
  , Just (co, pred) <- refinePred reft pred
  = do 	{ new_given <- newDictBndr (instLoc given) pred
	; let rhs = L (instSpan given) $
		    HsWrap (WpCo co) (HsVar (instToId given))
	; addAvailAndSCs AddSCs avails new_given (Rhs rhs [given]) }
	    -- ToDo: the superclasses of the original given all exist in Avails 
	    -- so we could really just cast them, but it's more awkward to do,
	    -- and hopefully the optimiser will spot the duplicated work
  | otherwise
  = return avails
\end{code}

Note [ImplicInst rigidity]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
	C :: forall ab. (Eq a, Ord b) => b -> T a
	
	...(case x of C v -> <body>)...

From the case (where x::T ty) we'll get an implication constraint
	forall b. (Eq ty, Ord b) => <body-constraints>
Now suppose <body-constraints> itself has an implication constraint 
of form
	forall c. <reft> => <payload>
Then, we can certainly apply the refinement <reft> to the Ord b, becuase it is
existential, but we probably should not apply it to the (Eq ty) because it may
be wobbly. Hence the isRigidInst

@Insts@ are ordered by their class/type info, rather than by their
unique.  This allows the context-reduction mechanism to use standard finite
maps to do their stuff.  It's horrible that this code is here, rather
than with the Avails handling stuff in TcSimplify

\begin{code}
addIrred :: WantSCs -> Avails -> Inst -> TcM Avails
addIrred want_scs avails irred = ASSERT2( not (irred `elemAvails` avails), ppr irred $$ ppr avails )
    	      		         addAvailAndSCs want_scs avails irred IsIrred

addAvailAndSCs :: WantSCs -> Avails -> Inst -> AvailHow -> TcM Avails
addAvailAndSCs want_scs avails inst avail
  | not (isClassDict inst) = extendAvails avails inst avail
  | NoSCs <- want_scs	   = extendAvails avails inst avail
  | otherwise		   = do { traceTc (text "addAvailAndSCs" <+> vcat [ppr inst, ppr deps])
				; avails' <- extendAvails avails inst avail
				; addSCs is_loop avails' inst }
  where
    is_loop pred = any (`tcEqType` mkPredTy pred) dep_tys
			-- Note: this compares by *type*, not by Unique
    deps         = findAllDeps (unitVarSet (instToVar inst)) avail
    dep_tys	 = map idType (varSetElems deps)

    findAllDeps :: IdSet -> AvailHow -> IdSet
    -- Find all the Insts that this one depends on
    -- See Note [SUPERCLASS-LOOP 2]
    -- Watch out, though.  Since the avails may contain loops 
    -- (see Note [RECURSIVE DICTIONARIES]), so we need to track the ones we've seen so far
    findAllDeps so_far (Rhs _ kids) = foldl find_all so_far kids
    findAllDeps so_far other	    = so_far

    find_all :: IdSet -> Inst -> IdSet
    find_all so_far kid
      | isEqInst kid                       = so_far
      | kid_id `elemVarSet` so_far	   = so_far
      | Just avail <- findAvail avails kid = findAllDeps so_far' avail
      | otherwise			   = so_far'
      where
	so_far' = extendVarSet so_far kid_id	-- Add the new kid to so_far
	kid_id = instToId kid

addSCs :: (TcPredType -> Bool) -> Avails -> Inst -> TcM Avails
	-- Add all the superclasses of the Inst to Avails
	-- The first param says "don't do this because the original thing
	--	depends on this one, so you'd build a loop"
	-- Invariant: the Inst is already in Avails.

addSCs is_loop avails dict
  = ASSERT( isDict dict )
    do	{ sc_dicts <- newDictBndrs (instLoc dict) sc_theta'
	; foldlM add_sc avails (zipEqual "add_scs" sc_dicts sc_sels) }
  where
    (clas, tys) = getDictClassTys dict
    (tyvars, sc_theta, sc_sels, _) = classBigSig clas
    sc_theta' = filter (not . isEqPred) $
                  substTheta (zipTopTvSubst tyvars tys) sc_theta

    add_sc avails (sc_dict, sc_sel)
      | is_loop (dictPred sc_dict) = return avails 	-- See Note [SUPERCLASS-LOOP 2]
      | is_given sc_dict 	   = return avails
      | otherwise		   = do { avails' <- extendAvails avails sc_dict (Rhs sc_sel_rhs [dict])
					; addSCs is_loop avails' sc_dict }
      where
	sc_sel_rhs = L (instSpan dict) (HsWrap co_fn (HsVar sc_sel))
	co_fn	   = WpApp (instToVar dict) <.> mkWpTyApps tys

    is_given :: Inst -> Bool
    is_given sc_dict = case findAvail avails sc_dict of
			  Just (Given _) -> True	-- Given is cheaper than superclass selection
			  other		 -> False	

-- From the a set of insts obtain all equalities that (transitively) occur in
-- superclass contexts of class constraints (aka the ancestor equalities). 
--
ancestorEqualities :: [Inst] -> TcM [Inst]
ancestorEqualities
  =   mapM mkWantedEqInst               -- turn only equality predicates..
    . filter isEqPred                   -- ..into wanted equality insts
    . bagToList 
    . addAEsToBag emptyBag              -- collect the superclass constraints..
    . map dictPred                      -- ..of all predicates in a bag
    . filter isClassDict
  where
    addAEsToBag :: Bag PredType -> [PredType] -> Bag PredType
    addAEsToBag bag []           = bag
    addAEsToBag bag (pred:preds)
      | pred `elemBag` bag = addAEsToBag bag         preds
      | isEqPred pred      = addAEsToBag bagWithPred preds
      | isClassPred pred   = addAEsToBag bagWithPred predsWithSCs
      | otherwise          = addAEsToBag bag         preds
      where
        bagWithPred  = bag `snocBag` pred
        predsWithSCs = preds ++ substTheta (zipTopTvSubst tyvars tys) sc_theta
        --
        (tyvars, sc_theta, _, _) = classBigSig clas
        (clas, tys) 		 = getClassPredTys pred 
\end{code}


%************************************************************************
%*									*
\section{tcSimplifyTop: defaulting}
%*									*
%************************************************************************


@tcSimplifyTop@ is called once per module to simplify all the constant
and ambiguous Insts.

We need to be careful of one case.  Suppose we have

	instance Num a => Num (Foo a b) where ...

and @tcSimplifyTop@ is given a constraint (Num (Foo x y)).  Then it'll simplify
to (Num x), and default x to Int.  But what about y??

It's OK: the final zonking stage should zap y to (), which is fine.


\begin{code}
tcSimplifyTop, tcSimplifyInteractive :: [Inst] -> TcM TcDictBinds
tcSimplifyTop wanteds
  = tc_simplify_top doc False wanteds
  where 
    doc = text "tcSimplifyTop"

tcSimplifyInteractive wanteds
  = tc_simplify_top doc True wanteds
  where 
    doc = text "tcSimplifyInteractive"

-- The TcLclEnv should be valid here, solely to improve
-- error message generation for the monomorphism restriction
tc_simplify_top doc interactive wanteds
  = do	{ dflags <- getDOpts
  	; wanteds <- zonkInsts wanteds
	; mapM_ zonkTopTyVar (varSetElems (tyVarsOfInsts wanteds))

	; traceTc (text "tc_simplify_top 0: " <+> ppr wanteds)
	; (irreds1, binds1) <- tryHardCheckLoop doc1 wanteds
--	; (irreds1, binds1) <- gentleInferLoop doc1 wanteds
	; traceTc (text "tc_simplify_top 1: " <+> ppr irreds1)
	; (irreds2, binds2) <- approximateImplications doc2 (\d -> True) irreds1
	; traceTc (text "tc_simplify_top 2: " <+> ppr irreds2)

		-- Use the defaulting rules to do extra unification
		-- NB: irreds2 are already zonked
	; (irreds3, binds3) <- disambiguate doc3 interactive dflags irreds2

		-- Deal with implicit parameters
	; let (bad_ips, non_ips) = partition isIPDict irreds3
	      (ambigs, others)   = partition isTyVarDict non_ips

	; topIPErrs bad_ips	-- Can arise from   f :: Int -> Int
				--		    f x = x + ?y
	; addNoInstanceErrs others
	; addTopAmbigErrs ambigs	

	; return (binds1 `unionBags` binds2 `unionBags` binds3) }
  where
    doc1 = doc <+> ptext SLIT("(first round)")
    doc2 = doc <+> ptext SLIT("(approximate)")
    doc3 = doc <+> ptext SLIT("(disambiguate)")
\end{code}

If a dictionary constrains a type variable which is
	* not mentioned in the environment
	* and not mentioned in the type of the expression
then it is ambiguous. No further information will arise to instantiate
the type variable; nor will it be generalised and turned into an extra
parameter to a function.

It is an error for this to occur, except that Haskell provided for
certain rules to be applied in the special case of numeric types.
Specifically, if
	* at least one of its classes is a numeric class, and
	* all of its classes are numeric or standard
then the type variable can be defaulted to the first type in the
default-type list which is an instance of all the offending classes.

So here is the function which does the work.  It takes the ambiguous
dictionaries and either resolves them (producing bindings) or
complains.  It works by splitting the dictionary list by type
variable, and using @disambigOne@ to do the real business.

@disambigOne@ assumes that its arguments dictionaries constrain all
the same type variable.

ADR Comment 20/6/94: I've changed the @CReturnable@ case to default to
@()@ instead of @Int@.  I reckon this is the Right Thing to do since
the most common use of defaulting is code like:
\begin{verbatim}
	_ccall_ foo	`seqPrimIO` bar
\end{verbatim}
Since we're not using the result of @foo@, the result if (presumably)
@void@.

\begin{code}
disambiguate :: SDoc -> Bool -> DynFlags -> [Inst] -> TcM ([Inst], TcDictBinds)
	-- Just does unification to fix the default types
	-- The Insts are assumed to be pre-zonked
disambiguate doc interactive dflags insts
  | null insts
  = return (insts, emptyBag)

  | null defaultable_groups
  = do	{ traceTc (text "disambigutate, no defaultable groups" <+> vcat [ppr unaries, ppr insts, ppr bad_tvs, ppr defaultable_groups])
	; return (insts, emptyBag) }

  | otherwise
  = do 	{  	-- Figure out what default types to use
	  default_tys <- getDefaultTys extended_defaulting ovl_strings

	; traceTc (text "disambiguate1" <+> vcat [ppr insts, ppr unaries, ppr bad_tvs, ppr defaultable_groups])
	; mapM_ (disambigGroup default_tys) defaultable_groups

	-- disambigGroup does unification, hence try again
	; tryHardCheckLoop doc insts }

  where
   extended_defaulting = interactive || dopt Opt_ExtendedDefaultRules dflags
   ovl_strings = dopt Opt_OverloadedStrings dflags

   unaries :: [(Inst, Class, TcTyVar)]  -- (C tv) constraints
   bad_tvs :: TcTyVarSet  -- Tyvars mentioned by *other* constraints
   (unaries, bad_tvs_s) = partitionWith find_unary insts 
   bad_tvs 		= unionVarSets bad_tvs_s

	-- Finds unary type-class constraints
   find_unary d@(Dict {tci_pred = ClassP cls [ty]})
	| Just tv <- tcGetTyVar_maybe ty = Left (d,cls,tv)
   find_unary inst			 = Right (tyVarsOfInst inst)

		-- Group by type variable
   defaultable_groups :: [[(Inst,Class,TcTyVar)]]
   defaultable_groups = filter defaultable_group (equivClasses cmp_tv unaries)
   cmp_tv (_,_,tv1) (_,_,tv2) = tv1 `compare` tv2

   defaultable_group :: [(Inst,Class,TcTyVar)] -> Bool
   defaultable_group ds@((_,_,tv):_)
	=  isTyConableTyVar tv	-- Note [Avoiding spurious errors]
	&& not (tv `elemVarSet` bad_tvs)
	&& defaultable_classes [c | (_,c,_) <- ds]
   defaultable_group [] = panic "defaultable_group"

   defaultable_classes clss 
	| extended_defaulting = any isInteractiveClass clss
	| otherwise 	      = all is_std_class clss && (any is_num_class clss)

 	-- In interactive mode, or with -fextended-default-rules,
	-- we default Show a to Show () to avoid graututious errors on "show []"
   isInteractiveClass cls 
	= is_num_class cls || (classKey cls `elem` [showClassKey, eqClassKey, ordClassKey])

   is_num_class cls = isNumericClass cls || (ovl_strings && (cls `hasKey` isStringClassKey))
	-- is_num_class adds IsString to the standard numeric classes, 
	-- when -foverloaded-strings is enabled

   is_std_class cls = isStandardClass cls || (ovl_strings && (cls `hasKey` isStringClassKey))
	-- Similarly is_std_class

-----------------------
disambigGroup :: [Type]			-- The default types
	      -> [(Inst,Class,TcTyVar)]	-- All standard classes of form (C a)
	      -> TcM ()	-- Just does unification, to fix the default types

disambigGroup default_tys dicts
  = try_default default_tys
  where
    (_,_,tyvar) = ASSERT(not (null dicts)) head dicts	-- Should be non-empty
    classes = [c | (_,c,_) <- dicts]

    try_default [] = return ()
    try_default (default_ty : default_tys)
      = tryTcLIE_ (try_default default_tys) $
    	do { tcSimplifyDefault [mkClassPred clas [default_ty] | clas <- classes]
		-- This may fail; then the tryTcLIE_ kicks in
		-- Failure here is caused by there being no type in the
		-- default list which can satisfy all the ambiguous classes.
		-- For example, if Real a is reqd, but the only type in the
		-- default list is Int.

		-- After this we can't fail
	   ; warnDefault dicts default_ty
	   ; unifyType default_ty (mkTyVarTy tyvar) 
	   ; return () -- TOMDO: do something with the coercion
	   }


-----------------------
getDefaultTys :: Bool -> Bool -> TcM [Type]
getDefaultTys extended_deflts ovl_strings
  = do	{ mb_defaults <- getDeclaredDefaultTys
	; case mb_defaults of {
	   Just tys -> return tys ;	-- User-supplied defaults
	   Nothing  -> do

	-- No use-supplied default
	-- Use [Integer, Double], plus modifications
	{ integer_ty <- tcMetaTy integerTyConName
	; checkWiredInTyCon doubleTyCon
	; string_ty <- tcMetaTy stringTyConName
	; return (opt_deflt extended_deflts unitTy
			-- Note [Default unitTy]
			++
		  [integer_ty,doubleTy]
			++
		  opt_deflt ovl_strings string_ty) } } }
  where
    opt_deflt True  ty = [ty]
    opt_deflt False ty = []
\end{code}

Note [Default unitTy]
~~~~~~~~~~~~~~~~~~~~~
In interative mode (or with -fextended-default-rules) we add () as the first type we
try when defaulting.  This has very little real impact, except in the following case.
Consider: 
	Text.Printf.printf "hello"
This has type (forall a. IO a); it prints "hello", and returns 'undefined'.  We don't
want the GHCi repl loop to try to print that 'undefined'.  The neatest thing is to
default the 'a' to (), rather than to Integer (which is what would otherwise happen;
and then GHCi doesn't attempt to print the ().  So in interactive mode, we add
() to the list of defaulting types.  See Trac #1200.

Note [Avoiding spurious errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When doing the unification for defaulting, we check for skolem
type variables, and simply don't default them.  For example:
   f = (*)	-- Monomorphic
   g :: Num a => a -> a
   g x = f x x
Here, we get a complaint when checking the type signature for g,
that g isn't polymorphic enough; but then we get another one when
dealing with the (Num a) context arising from f's definition;
we try to unify a with Int (to default it), but find that it's
already been unified with the rigid variable from g's type sig


%************************************************************************
%*									*
\subsection[simple]{@Simple@ versions}
%*									*
%************************************************************************

Much simpler versions when there are no bindings to make!

@tcSimplifyThetas@ simplifies class-type constraints formed by
@deriving@ declarations and when specialising instances.  We are
only interested in the simplified bunch of class/type constraints.

It simplifies to constraints of the form (C a b c) where
a,b,c are type variables.  This is required for the context of
instance declarations.

\begin{code}
tcSimplifyDeriv :: InstOrigin
		-> [TyVar]	
		-> ThetaType		-- Wanted
	        -> TcM ThetaType	-- Needed
-- Given  instance (wanted) => C inst_ty 
-- Simplify 'wanted' as much as possible

tcSimplifyDeriv orig tyvars theta
  = do	{ (tvs, _, tenv) <- tcInstTyVars tyvars
	-- The main loop may do unification, and that may crash if 
	-- it doesn't see a TcTyVar, so we have to instantiate. Sigh
	-- ToDo: what if two of them do get unified?
	; wanteds <- newDictBndrsO orig (substTheta tenv theta)
	; (irreds, _) <- tryHardCheckLoop doc wanteds

	; let (tv_dicts, others) = partition ok irreds
	; addNoInstanceErrs others
	-- See Note [Exotic derived instance contexts] in TcMType

	; let rev_env = zipTopTvSubst tvs (mkTyVarTys tyvars)
	      simpl_theta = substTheta rev_env (map dictPred tv_dicts)
		-- This reverse-mapping is a pain, but the result
		-- should mention the original TyVars not TcTyVars

	; return simpl_theta }
  where
    doc = ptext SLIT("deriving classes for a data type")

    ok dict | isDict dict = validDerivPred (dictPred dict)
	    | otherwise	  = False
\end{code}


@tcSimplifyDefault@ just checks class-type constraints, essentially;
used with \tr{default} declarations.  We are only interested in
whether it worked or not.

\begin{code}
tcSimplifyDefault :: ThetaType	-- Wanted; has no type variables in it
		  -> TcM ()

tcSimplifyDefault theta
  = newDictBndrsO DefaultOrigin theta	`thenM` \ wanteds ->
    tryHardCheckLoop doc wanteds	`thenM` \ (irreds, _) ->
    addNoInstanceErrs  irreds	 	`thenM_`
    if null irreds then
	returnM ()
    else
	failM
  where
    doc = ptext SLIT("default declaration")
\end{code}


%************************************************************************
%*									*
\section{Errors and contexts}
%*									*
%************************************************************************

ToDo: for these error messages, should we note the location as coming
from the insts, or just whatever seems to be around in the monad just
now?

\begin{code}
groupErrs :: ([Inst] -> TcM ())	-- Deal with one group
	  -> [Inst]		-- The offending Insts
          -> TcM ()
-- Group together insts with the same origin
-- We want to report them together in error messages

groupErrs report_err [] 
  = return ()
groupErrs report_err (inst:insts) 
  = do	{ do_one (inst:friends)
	; groupErrs report_err others }
  where
	-- (It may seem a bit crude to compare the error messages,
	--  but it makes sure that we combine just what the user sees,
	--  and it avoids need equality on InstLocs.)
   (friends, others) = partition is_friend insts
   loc_msg	     = showSDoc (pprInstLoc (instLoc inst))
   is_friend friend  = showSDoc (pprInstLoc (instLoc friend)) == loc_msg
   do_one insts = addInstCtxt (instLoc (head insts)) (report_err insts)
		-- Add location and context information derived from the Insts

-- Add the "arising from..." part to a message about bunch of dicts
addInstLoc :: [Inst] -> Message -> Message
addInstLoc insts msg = msg $$ nest 2 (pprInstArising (head insts))

addTopIPErrs :: [Name] -> [Inst] -> TcM ()
addTopIPErrs bndrs [] 
  = return ()
addTopIPErrs bndrs ips
  = do	{ dflags <- getDOpts
	; addErrTcM (tidy_env, mk_msg dflags tidy_ips) }
  where
    (tidy_env, tidy_ips) = tidyInsts ips
    mk_msg dflags ips 
	= vcat [sep [ptext SLIT("Implicit parameters escape from"),
	        nest 2 (ptext SLIT("the monomorphic top-level binding") 
					    <> plural bndrs <+> ptext SLIT("of")
					    <+> pprBinders bndrs <> colon)],
		nest 2 (vcat (map ppr_ip ips)),
		monomorphism_fix dflags]
    ppr_ip ip = pprPred (dictPred ip) <+> pprInstArising ip

topIPErrs :: [Inst] -> TcM ()
topIPErrs dicts
  = groupErrs report tidy_dicts
  where
    (tidy_env, tidy_dicts) = tidyInsts dicts
    report dicts = addErrTcM (tidy_env, mk_msg dicts)
    mk_msg dicts = addInstLoc dicts (ptext SLIT("Unbound implicit parameter") <> 
				     plural tidy_dicts <+> pprDictsTheta tidy_dicts)

addNoInstanceErrs :: [Inst]	-- Wanted (can include implications)
		  -> TcM ()	
addNoInstanceErrs insts
  = do	{ let (tidy_env, tidy_insts) = tidyInsts insts
	; reportNoInstances tidy_env Nothing tidy_insts }

reportNoInstances 
	:: TidyEnv
	-> Maybe (InstLoc, [Inst])	-- Context
			-- Nothing => top level
			-- Just (d,g) => d describes the construct
			--		 with givens g
	-> [Inst]	-- What is wanted (can include implications)
	-> TcM ()	

reportNoInstances tidy_env mb_what insts 
  = groupErrs (report_no_instances tidy_env mb_what) insts

report_no_instances tidy_env mb_what insts
  = do { inst_envs <- tcGetInstEnvs
       ; let (implics, insts1)  = partition isImplicInst insts
	     (insts2, overlaps) = partitionWith (check_overlap inst_envs) insts1
             (eqInsts, insts3)  = partition isEqInst insts2
       ; traceTc (text "reportNoInstances" <+> vcat 
                       [ppr insts, ppr implics, ppr insts1, ppr insts2])
       ; mapM_ complain_implic implics
       ; mapM_ (\doc -> addErrTcM (tidy_env, doc)) overlaps
       ; groupErrs complain_no_inst insts3 
       ; mapM_ (addErrTcM . mk_eq_err) eqInsts
       }
  where
    complain_no_inst insts = addErrTcM (tidy_env, mk_no_inst_err insts)

    complain_implic inst	-- Recurse!
      = reportNoInstances tidy_env 
			  (Just (tci_loc inst, tci_given inst)) 
			  (tci_wanted inst)

    check_overlap :: (InstEnv,InstEnv) -> Inst -> Either Inst SDoc
	-- Right msg  => overlap message
	-- Left  inst => no instance
    check_overlap inst_envs wanted
	| not (isClassDict wanted) = Left wanted
	| otherwise
	= case lookupInstEnv inst_envs clas tys of
		-- The case of exactly one match and no unifiers means a
		-- successful lookup.  That can't happen here, because dicts
		-- only end up here if they didn't match in Inst.lookupInst
#ifdef DEBUG
		([m],[]) -> pprPanic "reportNoInstance" (ppr wanted)
#endif
		([], _)  -> Left wanted		-- No match
		res	 -> Right (mk_overlap_msg wanted res)
	  where
	    (clas,tys) = getDictClassTys wanted

    mk_overlap_msg dict (matches, unifiers)
      = ASSERT( not (null matches) )
        vcat [	addInstLoc [dict] ((ptext SLIT("Overlapping instances for") 
					<+> pprPred (dictPred dict))),
    		sep [ptext SLIT("Matching instances") <> colon,
    		     nest 2 (vcat [pprInstances ispecs, pprInstances unifiers])],
		if not (isSingleton matches)
    		then 	-- Two or more matches
		     empty
    		else 	-- One match, plus some unifiers
		ASSERT( not (null unifiers) )
		parens (vcat [ptext SLIT("The choice depends on the instantiation of") <+>
	    		         quotes (pprWithCommas ppr (varSetElems (tyVarsOfInst dict))),
			      ptext SLIT("To pick the first instance above, use -fallow-incoherent-instances"),
			      ptext SLIT("when compiling the other instance declarations")])]
      where
    	ispecs = [ispec | (ispec, _) <- matches]

    mk_eq_err :: Inst -> (TidyEnv, SDoc)
    mk_eq_err inst = misMatchMsg tidy_env (eqInstTys inst)

    mk_no_inst_err insts
      | null insts = empty

      | Just (loc, givens) <- mb_what,   -- Nested (type signatures, instance decls)
	not (isEmptyVarSet (tyVarsOfInsts insts))
      = vcat [ addInstLoc insts $
	       sep [ ptext SLIT("Could not deduce") <+> pprDictsTheta insts
	           , nest 2 $ ptext SLIT("from the context") <+> pprDictsTheta givens]
	     , show_fixes (fix1 loc : fixes2) ]

      | otherwise	-- Top level 
      = vcat [ addInstLoc insts $
	       ptext SLIT("No instance") <> plural insts
		    <+> ptext SLIT("for") <+> pprDictsTheta insts
	     , show_fixes fixes2 ]

      where
    	fix1 loc = sep [ ptext SLIT("add") <+> pprDictsTheta insts
				 <+> ptext SLIT("to the context of"),
			 nest 2 (ppr (instLocOrigin loc)) ]
			 -- I'm not sure it helps to add the location
			 -- nest 2 (ptext SLIT("at") <+> ppr (instLocSpan loc)) ]

    	fixes2 | null instance_dicts = []
	       | otherwise	     = [sep [ptext SLIT("add an instance declaration for"),
				        pprDictsTheta instance_dicts]]
	instance_dicts = [d | d <- insts, isClassDict d, not (isTyVarDict d)]
		-- Insts for which it is worth suggesting an adding an instance declaration
		-- Exclude implicit parameters, and tyvar dicts

	show_fixes :: [SDoc] -> SDoc
	show_fixes []     = empty
	show_fixes (f:fs) = sep [ptext SLIT("Possible fix:"), 
				 nest 2 (vcat (f : map (ptext SLIT("or") <+>) fs))]

addTopAmbigErrs dicts
-- Divide into groups that share a common set of ambiguous tyvars
  = ifErrsM (return ()) $	-- Only report ambiguity if no other errors happened
				-- See Note [Avoiding spurious errors]
    mapM_ report (equivClasses cmp [(d, tvs_of d) | d <- tidy_dicts])
  where
    (tidy_env, tidy_dicts) = tidyInsts dicts

    tvs_of :: Inst -> [TcTyVar]
    tvs_of d = varSetElems (tyVarsOfInst d)
    cmp (_,tvs1) (_,tvs2) = tvs1 `compare` tvs2
    
    report :: [(Inst,[TcTyVar])] -> TcM ()
    report pairs@((inst,tvs) : _)	-- The pairs share a common set of ambiguous tyvars
	= mkMonomorphismMsg tidy_env tvs	`thenM` \ (tidy_env, mono_msg) ->
	  setSrcSpan (instSpan inst) $
		-- the location of the first one will do for the err message
	  addErrTcM (tidy_env, msg $$ mono_msg)
	where
	  dicts = map fst pairs
	  msg = sep [text "Ambiguous type variable" <> plural tvs <+> 
			  pprQuotedList tvs <+> in_msg,
		     nest 2 (pprDictsInFull dicts)]
	  in_msg = text "in the constraint" <> plural dicts <> colon
    report [] = panic "addTopAmbigErrs"


mkMonomorphismMsg :: TidyEnv -> [TcTyVar] -> TcM (TidyEnv, Message)
-- There's an error with these Insts; if they have free type variables
-- it's probably caused by the monomorphism restriction. 
-- Try to identify the offending variable
-- ASSUMPTION: the Insts are fully zonked
mkMonomorphismMsg tidy_env inst_tvs
  = do	{ dflags <- getDOpts
	; (tidy_env, docs) <- findGlobals (mkVarSet inst_tvs) tidy_env
	; return (tidy_env, mk_msg dflags docs) }
  where
    mk_msg _ _ | any isRuntimeUnk inst_tvs
        =  vcat [ptext SLIT("Cannot resolve unknown runtime types:") <+>
                   (pprWithCommas ppr inst_tvs),
                ptext SLIT("Use :print or :force to determine these types")]
    mk_msg _ []   = ptext SLIT("Probable fix: add a type signature that fixes these type variable(s)")
			-- This happens in things like
			--	f x = show (read "foo")
			-- where monomorphism doesn't play any role
    mk_msg dflags docs 
	= vcat [ptext SLIT("Possible cause: the monomorphism restriction applied to the following:"),
		nest 2 (vcat docs),
		monomorphism_fix dflags]

monomorphism_fix :: DynFlags -> SDoc
monomorphism_fix dflags
  = ptext SLIT("Probable fix:") <+> vcat
	[ptext SLIT("give these definition(s) an explicit type signature"),
	 if dopt Opt_MonomorphismRestriction dflags
           then ptext SLIT("or use -fno-monomorphism-restriction")
           else empty]	-- Only suggest adding "-fno-monomorphism-restriction"
			-- if it is not already set!
    
warnDefault ups default_ty
  = doptM Opt_WarnTypeDefaults  `thenM` \ warn_flag ->
    addInstCtxt (instLoc (head (dicts))) (warnTc warn_flag warn_msg)
  where
    dicts = [d | (d,_,_) <- ups]

	-- Tidy them first
    (_, tidy_dicts) = tidyInsts dicts
    warn_msg  = vcat [ptext SLIT("Defaulting the following constraint(s) to type") <+>
				quotes (ppr default_ty),
		      pprDictsInFull tidy_dicts]

reduceDepthErr n stack
  = vcat [ptext SLIT("Context reduction stack overflow; size =") <+> int n,
	  ptext SLIT("Use -fcontext-stack=N to increase stack size to N"),
	  nest 4 (pprStack stack)]

pprStack stack = vcat (map pprInstInFull stack)
\end{code}
