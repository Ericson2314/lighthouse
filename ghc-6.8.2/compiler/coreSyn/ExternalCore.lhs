%
% (c) The University of Glasgow 2001-2006
%
\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module ExternalCore where


data Module 
 = Module Mname [Tdef] [Vdefg]

data Tdef 
  = Data (Qual Tcon) [Tbind] [Cdef]
  | Newtype (Qual Tcon) [Tbind] (Maybe Ty)

data Cdef 
  = Constr Dcon [Tbind] [Ty]
  | GadtConstr Dcon Ty

data Vdefg 
  = Rec [Vdef]
  | Nonrec Vdef

type Vdef = (Bool,Var,Ty,Exp) 	-- Top level bindings are unqualified now

data Exp 
  = Var (Qual Var)
  | Dcon (Qual Dcon)
  | Lit Lit
  | App Exp Exp
  | Appt Exp Ty
  | Lam Bind Exp 	  
  | Let Vdefg Exp
  | Case Exp Vbind Ty [Alt] {- non-empty list -}
  | Cast Exp Ty
  | Note String Exp
  | External String String Ty {- target name, convention, and type -} 
  | DynExternal String Ty {- convention and type (incl. Addr# of target as first arg) -} 
  | Label String

data Bind 
  = Vb Vbind
  | Tb Tbind

data Alt 
  = Acon (Qual Dcon) [Tbind] [Vbind] Exp
  | Alit Lit Exp
  | Adefault Exp

type Vbind = (Var,Ty)
type Tbind = (Tvar,Kind)

data Ty 
  = Tvar Tvar
  | Tcon (Qual Tcon)
  | Tapp Ty Ty
  | Tforall Tbind Ty 

data Kind 
  = Klifted
  | Kunlifted
  | Kunboxed
  | Kopen
  | Karrow Kind Kind
  | Keq Ty Ty

data Lit 
  = Lint Integer Ty
  | Lrational Rational Ty
  | Lchar Char Ty
  | Lstring String Ty
  

type Mname = Id
type Var = Id
type Tvar = Id
type Tcon = Id
type Dcon = Id

type Qual t = (Mname,t)

type Id = String

primMname = "base:GHC.Prim"

tcArrow :: Qual Tcon
tcArrow = (primMname, "(->)")

\end{code}




