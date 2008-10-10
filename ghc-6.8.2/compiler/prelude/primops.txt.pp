-----------------------------------------------------------------------
-- $Id: primops.txt.pp,v 1.37 2005/11/25 09:46:19 simonmar Exp $
--
-- Primitive Operations and Types
--
-----------------------------------------------------------------------

-- This file is processed by the utility program genprimopcode to produce
-- a number of include files within the compiler and optionally to produce
-- human-readable documentation.
--
-- It should first be preprocessed.
--
-- To add a new primop, you currently need to update the following files:
--
--	- this file (ghc/compiler/prelude/primops.txt.pp), which includes
--	  the type of the primop, and various other properties (its
--	  strictness attributes, whether it is defined as a macro
--	  or as out-of-line code, etc.)
--
--	- if the primop is inline (i.e. a macro), then:
--	  	ghc/compiler/AbsCUtils.lhs (dscCOpStmt)
--		  defines the translation of the primop into simpler
--		  abstract C operations.
--		
--	- or, for an out-of-line primop:
--	        ghc/includes/StgMiscClosures.h (just add the declaration)
--		ghc/rts/PrimOps.cmm     (define it here)
--		ghc/rts/Linker.c       (declare the symbol for GHCi)
--
--	- the User's Guide 
--

-- This file is divided into named sections, each containing or more
-- primop entries. Section headers have the format:
--
--	section "section-name" {description}
--
-- This information is used solely when producing documentation; it is
-- otherwise ignored.  The description is optional.
--
-- The format of each primop entry is as follows:
--
--	primop internal-name "name-in-program-text" type category {description} attributes

-- The default attribute values which apply if you don't specify
-- other ones.  Attribute values can be True, False, or arbitrary
-- text between curly brackets.  This is a kludge to enable 
-- processors of this file to easily get hold of simple info
-- (eg, out_of_line), whilst avoiding parsing complex expressions
-- needed for strictness info.

defaults
   has_side_effects = False
   out_of_line      = False
   commutable       = False
   needs_wrapper    = False
   can_fail         = False
   strictness       = { \ arity -> mkStrictSig (mkTopDmdType (replicate arity lazyDmd) TopRes) }

-- Currently, documentation is produced using latex, so contents of
-- description fields should be legal latex. Descriptions can contain
-- matched pairs of embedded curly brackets.

#include "MachDeps.h"

-- We need platform defines (tests for mingw32 below).  However, we only
-- test the TARGET platform, which doesn't vary between stages, so the
-- stage1 platform defines are fine:
#include "../stage1/ghc_boot_platform.h"

section "The word size story."
	{Haskell98 specifies that signed integers (type {\tt Int})
	 must contain at least 30 bits. GHC always implements {\tt
	 Int} using the primitive type {\tt Int\#}, whose size equals
	 the {\tt MachDeps.h} constant {\tt WORD\_SIZE\_IN\_BITS}.
	 This is normally set based on the {\tt config.h} parameter
	 {\tt SIZEOF\_HSWORD}, i.e., 32 bits on 32-bit machines, 64
	 bits on 64-bit machines.  However, it can also be explicitly
	 set to a smaller number, e.g., 31 bits, to allow the
	 possibility of using tag bits. Currently GHC itself has only
	 32-bit and 64-bit variants, but 30 or 31-bit code can be
	 exported as an external core file for use in other back ends.

	 GHC also implements a primitive unsigned integer type {\tt
	 Word\#} which always has the same number of bits as {\tt
	 Int\#}.
	
	 In addition, GHC supports families of explicit-sized integers
	 and words at 8, 16, 32, and 64 bits, with the usual
	 arithmetic operations, comparisons, and a range of
	 conversions.  The 8-bit and 16-bit sizes are always
	 represented as {\tt Int\#} and {\tt Word\#}, and the
	 operations implemented in terms of the the primops on these
	 types, with suitable range restrictions on the results (using
	 the {\tt narrow$n$Int\#} and {\tt narrow$n$Word\#} families
	 of primops.  The 32-bit sizes are represented using {\tt
	 Int\#} and {\tt Word\#} when {\tt WORD\_SIZE\_IN\_BITS}
	 $\geq$ 32; otherwise, these are represented using distinct
	 primitive types {\tt Int32\#} and {\tt Word32\#}. These (when
	 needed) have a complete set of corresponding operations;
	 however, nearly all of these are implemented as external C
	 functions rather than as primops.  Exactly the same story
	 applies to the 64-bit sizes.  All of these details are hidden
	 under the {\tt PrelInt} and {\tt PrelWord} modules, which use
	 {\tt \#if}-defs to invoke the appropriate types and
	 operators.

	 Word size also matters for the families of primops for
	 indexing/reading/writing fixed-size quantities at offsets
	 from an array base, address, or foreign pointer.  Here, a
	 slightly different approach is taken.  The names of these
	 primops are fixed, but their {\it types} vary according to
	 the value of {\tt WORD\_SIZE\_IN\_BITS}. For example, if word
	 size is at least 32 bits then an operator like
	 \texttt{indexInt32Array\#} has type {\tt ByteArr\# -> Int\#
	 -> Int\#}; otherwise it has type {\tt ByteArr\# -> Int\# ->
	 Int32\#}.  This approach confines the necessary {\tt
	 \#if}-defs to this file; no conditional compilation is needed
	 in the files that expose these primops.

	 Finally, there are strongly deprecated primops for coercing
         between {\tt Addr\#}, the primitive type of machine
         addresses, and {\tt Int\#}.  These are pretty bogus anyway,
         but will work on existing 32-bit and 64-bit GHC targets; they
         are completely bogus when tag bits are used in {\tt Int\#},
         so are not available in this case.  }
	
-- Define synonyms for indexing ops. 

#if WORD_SIZE_IN_BITS < 32 
#define INT32 Int32#
#define WORD32 Word32#
#else
#define INT32 Int#
#define WORD32 Word#
#endif

#if WORD_SIZE_IN_BITS < 64
#define INT64 Int64#
#define WORD64 Word64#
#else
#define INT64 Int#
#define WORD64 Word#
#endif

------------------------------------------------------------------------
section "Char#" 
	{Operations on 31-bit characters.}
------------------------------------------------------------------------

primtype Char#

primop   CharGtOp  "gtChar#"   Compare   Char# -> Char# -> Bool
primop   CharGeOp  "geChar#"   Compare   Char# -> Char# -> Bool

primop   CharEqOp  "eqChar#"   Compare
   Char# -> Char# -> Bool
   with commutable = True

primop   CharNeOp  "neChar#"   Compare
   Char# -> Char# -> Bool
   with commutable = True

primop   CharLtOp  "ltChar#"   Compare   Char# -> Char# -> Bool
primop   CharLeOp  "leChar#"   Compare   Char# -> Char# -> Bool

primop   OrdOp   "ord#"  GenPrimOp   Char# -> Int#

------------------------------------------------------------------------
section "Int#"
	{Operations on native-size integers (30+ bits).}
------------------------------------------------------------------------

primtype Int#

primop   IntAddOp    "+#"    Dyadic
   Int# -> Int# -> Int#
   with commutable = True

primop   IntSubOp    "-#"    Dyadic   Int# -> Int# -> Int#

primop   IntMulOp    "*#" 
   Dyadic   Int# -> Int# -> Int#
   {Low word of signed integer multiply.}
   with commutable = True

primop   IntMulMayOfloOp  "mulIntMayOflo#" 
   Dyadic   Int# -> Int# -> Int#
   {Return non-zero if there is any possibility that the upper word of a
    signed integer multiply might contain useful information.  Return
    zero only if you are completely sure that no overflow can occur.
    On a 32-bit platform, the recommmended implementation is to do a 
    32 x 32 -> 64 signed multiply, and subtract result[63:32] from
    (result[31] >>signed 31).  If this is zero, meaning that the 
    upper word is merely a sign extension of the lower one, no
    overflow can occur.

    On a 64-bit platform it is not always possible to 
    acquire the top 64 bits of the result.  Therefore, a recommended 
    implementation is to take the absolute value of both operands, and 
    return 0 iff bits[63:31] of them are zero, since that means that their 
    magnitudes fit within 31 bits, so the magnitude of the product must fit 
    into 62 bits.

    If in doubt, return non-zero, but do make an effort to create the
    correct answer for small args, since otherwise the performance of
    \texttt{(*) :: Integer -> Integer -> Integer} will be poor.
   }
   with commutable = True

primop   IntQuotOp    "quotInt#"    Dyadic
   Int# -> Int# -> Int#
   {Rounds towards zero.}
   with can_fail = True

primop   IntRemOp    "remInt#"    Dyadic
   Int# -> Int# -> Int#
   {Satisfies \texttt{(quotInt\# x y) *\# y +\# (remInt\# x y) == x}.}
   with can_fail = True

primop   IntGcdOp    "gcdInt#"    Dyadic   Int# -> Int# -> Int#
   with out_of_line = True

primop   IntNegOp    "negateInt#"    Monadic   Int# -> Int#
primop   IntAddCOp   "addIntC#"    GenPrimOp   Int# -> Int# -> (# Int#, Int# #)
	 {Add with carry.  First member of result is (wrapped) sum; 
          second member is 0 iff no overflow occured.}
primop   IntSubCOp   "subIntC#"    GenPrimOp   Int# -> Int# -> (# Int#, Int# #)
	 {Subtract with carry.  First member of result is (wrapped) difference; 
          second member is 0 iff no overflow occured.}

primop   IntGtOp  ">#"   Compare   Int# -> Int# -> Bool
primop   IntGeOp  ">=#"   Compare   Int# -> Int# -> Bool

primop   IntEqOp  "==#"   Compare
   Int# -> Int# -> Bool
   with commutable = True

primop   IntNeOp  "/=#"   Compare
   Int# -> Int# -> Bool
   with commutable = True

primop   IntLtOp  "<#"   Compare   Int# -> Int# -> Bool
primop   IntLeOp  "<=#"   Compare   Int# -> Int# -> Bool

primop   ChrOp   "chr#"   GenPrimOp   Int# -> Char#

primop   Int2WordOp "int2Word#" GenPrimOp Int# -> Word#
primop   Int2FloatOp   "int2Float#"      GenPrimOp  Int# -> Float#
primop   Int2DoubleOp   "int2Double#"          GenPrimOp  Int# -> Double#

primop   Int2IntegerOp    "int2Integer#"
   GenPrimOp Int# -> (# Int#, ByteArr# #)
   with out_of_line = True

primop   ISllOp   "uncheckedIShiftL#" GenPrimOp  Int# -> Int# -> Int#
	 {Shift left.  Result undefined if shift amount is not
          in the range 0 to word size - 1 inclusive.}
primop   ISraOp   "uncheckedIShiftRA#" GenPrimOp Int# -> Int# -> Int#
	 {Shift right arithmetic.  Result undefined if shift amount is not
          in the range 0 to word size - 1 inclusive.}
primop   ISrlOp   "uncheckedIShiftRL#" GenPrimOp Int# -> Int# -> Int#
	 {Shift right logical.  Result undefined if shift amount is not
          in the range 0 to word size - 1 inclusive.}

------------------------------------------------------------------------
section "Word#"
	{Operations on native-sized unsigned words (30+ bits).}
------------------------------------------------------------------------

primtype Word#

primop   WordAddOp   "plusWord#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

primop   WordSubOp   "minusWord#"   Dyadic   Word# -> Word# -> Word#

primop   WordMulOp   "timesWord#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

primop   WordQuotOp   "quotWord#"   Dyadic   Word# -> Word# -> Word#
   with can_fail = True

primop   WordRemOp   "remWord#"   Dyadic   Word# -> Word# -> Word#
   with can_fail = True

primop   AndOp   "and#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

primop   OrOp   "or#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

primop   XorOp   "xor#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

primop   NotOp   "not#"   Monadic   Word# -> Word#

primop   SllOp   "uncheckedShiftL#"   GenPrimOp   Word# -> Int# -> Word#
	 {Shift left logical.   Result undefined if shift amount is not
          in the range 0 to word size - 1 inclusive.}
primop   SrlOp   "uncheckedShiftRL#"   GenPrimOp   Word# -> Int# -> Word#
	 {Shift right logical.   Result undefined if shift  amount is not
          in the range 0 to word size - 1 inclusive.}

primop   Word2IntOp   "word2Int#"   GenPrimOp   Word# -> Int#

primop   Word2IntegerOp   "word2Integer#"   GenPrimOp 
   Word# -> (# Int#, ByteArr# #)
   with out_of_line = True

primop   WordGtOp   "gtWord#"   Compare   Word# -> Word# -> Bool
primop   WordGeOp   "geWord#"   Compare   Word# -> Word# -> Bool
primop   WordEqOp   "eqWord#"   Compare   Word# -> Word# -> Bool
primop   WordNeOp   "neWord#"   Compare   Word# -> Word# -> Bool
primop   WordLtOp   "ltWord#"   Compare   Word# -> Word# -> Bool
primop   WordLeOp   "leWord#"   Compare   Word# -> Word# -> Bool

------------------------------------------------------------------------
section "Narrowings" 
	{Explicit narrowing of native-sized ints or words.}
------------------------------------------------------------------------

primop   Narrow8IntOp      "narrow8Int#"      Monadic   Int# -> Int#
primop   Narrow16IntOp     "narrow16Int#"     Monadic   Int# -> Int#
primop   Narrow32IntOp     "narrow32Int#"     Monadic   Int# -> Int#
primop   Narrow8WordOp     "narrow8Word#"     Monadic   Word# -> Word#
primop   Narrow16WordOp    "narrow16Word#"    Monadic   Word# -> Word#
primop   Narrow32WordOp    "narrow32Word#"    Monadic   Word# -> Word#


#if WORD_SIZE_IN_BITS < 32
------------------------------------------------------------------------
section "Int32#"
	{Operations on 32-bit integers ({\tt Int32\#}).  This type is only used
         if plain {\tt Int\#} has less than 32 bits.  In any case, the operations
	 are not primops; they are implemented (if needed) as ccalls instead.}
------------------------------------------------------------------------

primtype Int32#

primop   Int32ToIntegerOp   "int32ToInteger#" GenPrimOp 
   Int32# -> (# Int#, ByteArr# #)
   with out_of_line = True


------------------------------------------------------------------------
section "Word32#"
	{Operations on 32-bit unsigned words. This type is only used 
	 if plain {\tt Word\#} has less than 32 bits. In any case, the operations
	 are not primops; they are implemented (if needed) as ccalls instead.}
------------------------------------------------------------------------

primtype Word32#

primop   Word32ToIntegerOp   "word32ToInteger#" GenPrimOp
   Word32# -> (# Int#, ByteArr# #)
   with out_of_line = True


#endif 


#if WORD_SIZE_IN_BITS < 64
------------------------------------------------------------------------
section "Int64#"
	{Operations on 64-bit unsigned words. This type is only used 
	 if plain {\tt Int\#} has less than 64 bits. In any case, the operations
	 are not primops; they are implemented (if needed) as ccalls instead.}
------------------------------------------------------------------------

primtype Int64#

primop   Int64ToIntegerOp   "int64ToInteger#" GenPrimOp 
   Int64# -> (# Int#, ByteArr# #)
   with out_of_line = True

------------------------------------------------------------------------
section "Word64#"
	{Operations on 64-bit unsigned words. This type is only used 
	 if plain {\tt Word\#} has less than 64 bits. In any case, the operations
	 are not primops; they are implemented (if needed) as ccalls instead.}
------------------------------------------------------------------------

primtype Word64#

primop   Word64ToIntegerOp   "word64ToInteger#" GenPrimOp
   Word64# -> (# Int#, ByteArr# #)
   with out_of_line = True

#endif

------------------------------------------------------------------------
section "Integer#"
	{Operations on arbitrary-precision integers. These operations are 
implemented via the GMP package. An integer is represented as a pair
consisting of an {\tt Int\#} representing the number of 'limbs' in use and
the sign, and a {\tt ByteArr\#} containing the 'limbs' themselves.  Such pairs
are returned as unboxed pairs, but must be passed as separate
components.

For .NET these operations are implemented by foreign imports, so the
primops are omitted.}
------------------------------------------------------------------------

#ifndef ILX

primop   IntegerAddOp   "plusInteger#" GenPrimOp   
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   with commutable = True
        out_of_line = True

primop   IntegerSubOp   "minusInteger#" GenPrimOp  
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   with out_of_line = True

primop   IntegerMulOp   "timesInteger#" GenPrimOp   
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   with commutable = True
        out_of_line = True

primop   IntegerGcdOp   "gcdInteger#" GenPrimOp    
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   {Greatest common divisor.}
   with commutable = True
        out_of_line = True

primop   IntegerIntGcdOp   "gcdIntegerInt#" GenPrimOp
   Int# -> ByteArr# -> Int# -> Int#
   {Greatest common divisor, where second argument is an ordinary {\tt Int\#}.}
   with out_of_line = True

primop   IntegerDivExactOp   "divExactInteger#" GenPrimOp
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   {Divisor is guaranteed to be a factor of dividend.}
   with out_of_line = True

primop   IntegerQuotOp   "quotInteger#" GenPrimOp
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   {Rounds towards zero.}
   with out_of_line = True

primop   IntegerRemOp   "remInteger#" GenPrimOp
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   {Satisfies \texttt{plusInteger\# (timesInteger\# (quotInteger\# x y) y) (remInteger\# x y) == x}.}
   with out_of_line = True

primop   IntegerCmpOp   "cmpInteger#"   GenPrimOp  
   Int# -> ByteArr# -> Int# -> ByteArr# -> Int#
   {Returns -1,0,1 according as first argument is less than, equal to, or greater than second argument.}
   with needs_wrapper = True
        out_of_line = True

primop   IntegerCmpIntOp   "cmpIntegerInt#" GenPrimOp
   Int# -> ByteArr# -> Int# -> Int#
   {Returns -1,0,1 according as first argument is less than, equal to, or greater than second argument, which
   is an ordinary Int\#.}
   with needs_wrapper = True
        out_of_line = True

primop   IntegerQuotRemOp   "quotRemInteger#" GenPrimOp
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr#, Int#, ByteArr# #)
   {Compute quot and rem simulaneously.}
   with can_fail = True
        out_of_line = True

primop   IntegerDivModOp    "divModInteger#"  GenPrimOp
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr#, Int#, ByteArr# #)
   {Compute div and mod simultaneously, where div rounds towards negative infinity
    and\texttt{(q,r) = divModInteger\#(x,y)} implies \texttt{plusInteger\# (timesInteger\# q y) r = x}.}
   with can_fail = True
        out_of_line = True

primop   Integer2IntOp   "integer2Int#"    GenPrimOp
   Int# -> ByteArr# -> Int#
   with needs_wrapper = True
        out_of_line = True

primop   Integer2WordOp   "integer2Word#"   GenPrimOp
   Int# -> ByteArr# -> Word#
   with needs_wrapper = True
        out_of_line = True

#if WORD_SIZE_IN_BITS < 32
primop   IntegerToInt32Op   "integerToInt32#" GenPrimOp
   Int# -> ByteArr# -> Int32#

primop   IntegerToWord32Op   "integerToWord32#" GenPrimOp
   Int# -> ByteArr# -> Word32#
#endif

primop   IntegerAndOp  "andInteger#" GenPrimOp
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   with out_of_line = True

primop   IntegerOrOp  "orInteger#" GenPrimOp
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   with out_of_line = True

primop   IntegerXorOp  "xorInteger#" GenPrimOp
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   with out_of_line = True

primop   IntegerComplementOp  "complementInteger#" GenPrimOp
   Int# -> ByteArr# -> (# Int#, ByteArr# #)
   with out_of_line = True

#endif /* ndef ILX */

------------------------------------------------------------------------
section "Double#"
	{Operations on double-precision (64 bit) floating-point numbers.}
------------------------------------------------------------------------

primtype Double#

primop   DoubleGtOp ">##"   Compare   Double# -> Double# -> Bool
primop   DoubleGeOp ">=##"   Compare   Double# -> Double# -> Bool

primop DoubleEqOp "==##"   Compare
   Double# -> Double# -> Bool
   with commutable = True

primop DoubleNeOp "/=##"   Compare
   Double# -> Double# -> Bool
   with commutable = True

primop   DoubleLtOp "<##"   Compare   Double# -> Double# -> Bool
primop   DoubleLeOp "<=##"   Compare   Double# -> Double# -> Bool

primop   DoubleAddOp   "+##"   Dyadic
   Double# -> Double# -> Double#
   with commutable = True

primop   DoubleSubOp   "-##"   Dyadic   Double# -> Double# -> Double#

primop   DoubleMulOp   "*##"   Dyadic
   Double# -> Double# -> Double#
   with commutable = True

primop   DoubleDivOp   "/##"   Dyadic
   Double# -> Double# -> Double#
   with can_fail = True

primop   DoubleNegOp   "negateDouble#"  Monadic   Double# -> Double#

primop   Double2IntOp   "double2Int#"          GenPrimOp  Double# -> Int#
primop   Double2FloatOp   "double2Float#" GenPrimOp Double# -> Float#

primop   DoubleExpOp   "expDouble#"      Monadic
   Double# -> Double#
   with needs_wrapper = True

primop   DoubleLogOp   "logDouble#"      Monadic         
   Double# -> Double#
   with
   needs_wrapper = True
   can_fail = True

primop   DoubleSqrtOp   "sqrtDouble#"      Monadic  
   Double# -> Double#
   with needs_wrapper = True

primop   DoubleSinOp   "sinDouble#"      Monadic          
   Double# -> Double#
   with needs_wrapper = True

primop   DoubleCosOp   "cosDouble#"      Monadic          
   Double# -> Double#
   with needs_wrapper = True

primop   DoubleTanOp   "tanDouble#"      Monadic          
   Double# -> Double#
   with needs_wrapper = True

primop   DoubleAsinOp   "asinDouble#"      Monadic 
   Double# -> Double#
   with
   needs_wrapper = True
   can_fail = True

primop   DoubleAcosOp   "acosDouble#"      Monadic  
   Double# -> Double#
   with
   needs_wrapper = True
   can_fail = True

primop   DoubleAtanOp   "atanDouble#"      Monadic  
   Double# -> Double#
   with
   needs_wrapper = True

primop   DoubleSinhOp   "sinhDouble#"      Monadic  
   Double# -> Double#
   with needs_wrapper = True

primop   DoubleCoshOp   "coshDouble#"      Monadic  
   Double# -> Double#
   with needs_wrapper = True

primop   DoubleTanhOp   "tanhDouble#"      Monadic  
   Double# -> Double#
   with needs_wrapper = True

primop   DoublePowerOp   "**##" Dyadic  
   Double# -> Double# -> Double#
   {Exponentiation.}
   with needs_wrapper = True

primop   DoubleDecodeOp   "decodeDouble#" GenPrimOp    
   Double# -> (# Int#, Int#, ByteArr# #)
   {Convert to arbitrary-precision integer.
    First {\tt Int\#} in result is the exponent; second {\tt Int\#} and {\tt ByteArr\#}
    represent an {\tt Integer\#} holding the mantissa.}
   with out_of_line = True

------------------------------------------------------------------------
section "Float#" 
	{Operations on single-precision (32-bit) floating-point numbers.}
------------------------------------------------------------------------

primtype Float#

primop   FloatGtOp  "gtFloat#"   Compare   Float# -> Float# -> Bool
primop   FloatGeOp  "geFloat#"   Compare   Float# -> Float# -> Bool

primop   FloatEqOp  "eqFloat#"   Compare
   Float# -> Float# -> Bool
   with commutable = True

primop   FloatNeOp  "neFloat#"   Compare
   Float# -> Float# -> Bool
   with commutable = True

primop   FloatLtOp  "ltFloat#"   Compare   Float# -> Float# -> Bool
primop   FloatLeOp  "leFloat#"   Compare   Float# -> Float# -> Bool

primop   FloatAddOp   "plusFloat#"      Dyadic            
   Float# -> Float# -> Float#
   with commutable = True

primop   FloatSubOp   "minusFloat#"      Dyadic      Float# -> Float# -> Float#

primop   FloatMulOp   "timesFloat#"      Dyadic    
   Float# -> Float# -> Float#
   with commutable = True

primop   FloatDivOp   "divideFloat#"      Dyadic  
   Float# -> Float# -> Float#
   with can_fail = True

primop   FloatNegOp   "negateFloat#"      Monadic    Float# -> Float#

primop   Float2IntOp   "float2Int#"      GenPrimOp  Float# -> Int#

primop   FloatExpOp   "expFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatLogOp   "logFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True
        can_fail = True

primop   FloatSqrtOp   "sqrtFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatSinOp   "sinFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatCosOp   "cosFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatTanOp   "tanFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatAsinOp   "asinFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True
        can_fail = True

primop   FloatAcosOp   "acosFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True
        can_fail = True

primop   FloatAtanOp   "atanFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatSinhOp   "sinhFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatCoshOp   "coshFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatTanhOp   "tanhFloat#"      Monadic          
   Float# -> Float#
   with needs_wrapper = True

primop   FloatPowerOp   "powerFloat#"      Dyadic   
   Float# -> Float# -> Float#
   with needs_wrapper = True

primop   Float2DoubleOp   "float2Double#" GenPrimOp  Float# -> Double#

primop   FloatDecodeOp   "decodeFloat#" GenPrimOp
   Float# -> (# Int#, Int#, ByteArr# #)
   {Convert to arbitrary-precision integer.
    First {\tt Int\#} in result is the exponent; second {\tt Int\#} and {\tt ByteArr\#}
    represent an {\tt Integer\#} holding the mantissa.}
   with out_of_line = True

------------------------------------------------------------------------
section "Arrays"
	{Operations on {\tt Array\#}.}
------------------------------------------------------------------------

primtype Array# a

primtype MutArr# s a

primop  NewArrayOp "newArray#" GenPrimOp
   Int# -> a -> State# s -> (# State# s, MutArr# s a #)
   {Create a new mutable array of specified size (in bytes),
    in the specified state thread,
    with each element containing the specified initial value.}
   with
   out_of_line = True

primop  SameMutableArrayOp "sameMutableArray#" GenPrimOp
   MutArr# s a -> MutArr# s a -> Bool

primop  ReadArrayOp "readArray#" GenPrimOp
   MutArr# s a -> Int# -> State# s -> (# State# s, a #)
   {Read from specified index of mutable array. Result is not yet evaluated.}

primop  WriteArrayOp "writeArray#" GenPrimOp
   MutArr# s a -> Int# -> a -> State# s -> State# s
   {Write to specified index of mutable array.}
   with
   has_side_effects = True

primop  IndexArrayOp "indexArray#" GenPrimOp
   Array# a -> Int# -> (# a #)
   {Read from specified index of immutable array. Result is packaged into
    an unboxed singleton; the result itself is not yet evaluated.}

primop  UnsafeFreezeArrayOp "unsafeFreezeArray#" GenPrimOp
   MutArr# s a -> State# s -> (# State# s, Array# a #)
   {Make a mutable array immutable, without copying.}
   with
   has_side_effects = True

primop  UnsafeThawArrayOp  "unsafeThawArray#" GenPrimOp
   Array# a -> State# s -> (# State# s, MutArr# s a #)
   {Make an immutable array mutable, without copying.}
   with
   out_of_line = True

------------------------------------------------------------------------
section "Byte Arrays"
	{Operations on {\tt ByteArray\#}. A {\tt ByteArray\#} is a just a region of
         raw memory in the garbage-collected heap, which is not scanned
         for pointers. It carries its own size (in bytes). There are
	 three sets of operations for accessing byte array contents:
	 index for reading from immutable byte arrays, and read/write
	 for mutable byte arrays.  Each set contains operations for 
	 a range of useful primitive data types.  Each operation takes	
	 an offset measured in terms of the size fo the primitive type
	 being read or written.}

------------------------------------------------------------------------

primtype ByteArr#

primtype MutByteArr# s

primop  NewByteArrayOp_Char "newByteArray#" GenPrimOp
   Int# -> State# s -> (# State# s, MutByteArr# s #)
   {Create a new mutable byte array of specified size (in bytes), in
    the specified state thread.}
   with out_of_line = True

primop  NewPinnedByteArrayOp_Char "newPinnedByteArray#" GenPrimOp
   Int# -> State# s -> (# State# s, MutByteArr# s #)
   {Create a mutable byte array that the GC guarantees not to move.}
   with out_of_line = True

primop  ByteArrayContents_Char "byteArrayContents#" GenPrimOp
   ByteArr# -> Addr#
   {Intended for use with pinned arrays; otherwise very unsafe!}

primop  SameMutableByteArrayOp "sameMutableByteArray#" GenPrimOp
   MutByteArr# s -> MutByteArr# s -> Bool

primop  UnsafeFreezeByteArrayOp "unsafeFreezeByteArray#" GenPrimOp
   MutByteArr# s -> State# s -> (# State# s, ByteArr# #)
   {Make a mutable byte array immutable, without copying.}
   with
   has_side_effects = True

primop  SizeofByteArrayOp "sizeofByteArray#" GenPrimOp  
   ByteArr# -> Int#

primop  SizeofMutableByteArrayOp "sizeofMutableByteArray#" GenPrimOp
   MutByteArr# s -> Int#


primop IndexByteArrayOp_Char "indexCharArray#" GenPrimOp
   ByteArr# -> Int# -> Char#
   {Read 8-bit character; offset in bytes.}

primop IndexByteArrayOp_WideChar "indexWideCharArray#" GenPrimOp
   ByteArr# -> Int# -> Char#
   {Read 31-bit character; offset in 4-byte words.}

primop IndexByteArrayOp_Int "indexIntArray#" GenPrimOp
   ByteArr# -> Int# -> Int#

primop IndexByteArrayOp_Word "indexWordArray#" GenPrimOp
   ByteArr# -> Int# -> Word#

primop IndexByteArrayOp_Addr "indexAddrArray#" GenPrimOp
   ByteArr# -> Int# -> Addr#

primop IndexByteArrayOp_Float "indexFloatArray#" GenPrimOp
   ByteArr# -> Int# -> Float#

primop IndexByteArrayOp_Double "indexDoubleArray#" GenPrimOp
   ByteArr# -> Int# -> Double#

primop IndexByteArrayOp_StablePtr "indexStablePtrArray#" GenPrimOp
   ByteArr# -> Int# -> StablePtr# a

primop IndexByteArrayOp_Int8 "indexInt8Array#" GenPrimOp
   ByteArr# -> Int# -> Int#

primop IndexByteArrayOp_Int16 "indexInt16Array#" GenPrimOp
   ByteArr# -> Int# -> Int#

primop IndexByteArrayOp_Int32 "indexInt32Array#" GenPrimOp
   ByteArr# -> Int# -> INT32

primop IndexByteArrayOp_Int64 "indexInt64Array#" GenPrimOp
   ByteArr# -> Int# -> INT64

primop IndexByteArrayOp_Word8 "indexWord8Array#" GenPrimOp
   ByteArr# -> Int# -> Word#

primop IndexByteArrayOp_Word16 "indexWord16Array#" GenPrimOp
   ByteArr# -> Int# -> Word#

primop IndexByteArrayOp_Word32 "indexWord32Array#" GenPrimOp
   ByteArr# -> Int# -> WORD32

primop IndexByteArrayOp_Word64 "indexWord64Array#" GenPrimOp
   ByteArr# -> Int# -> WORD64

primop  ReadByteArrayOp_Char "readCharArray#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Char# #)
   {Read 8-bit character; offset in bytes.}

primop  ReadByteArrayOp_WideChar "readWideCharArray#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Char# #)
   {Read 31-bit character; offset in 4-byte words.}

primop  ReadByteArrayOp_Int "readIntArray#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Int# #)

primop  ReadByteArrayOp_Word "readWordArray#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Word# #)

primop  ReadByteArrayOp_Addr "readAddrArray#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Addr# #)

primop  ReadByteArrayOp_Float "readFloatArray#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Float# #)

primop  ReadByteArrayOp_Double "readDoubleArray#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Double# #)

primop  ReadByteArrayOp_StablePtr "readStablePtrArray#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, StablePtr# a #)

primop  ReadByteArrayOp_Int8 "readInt8Array#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Int# #)

primop  ReadByteArrayOp_Int16 "readInt16Array#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Int# #)

primop  ReadByteArrayOp_Int32 "readInt32Array#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, INT32 #)

primop  ReadByteArrayOp_Int64 "readInt64Array#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, INT64 #)

primop  ReadByteArrayOp_Word8 "readWord8Array#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Word# #)

primop  ReadByteArrayOp_Word16 "readWord16Array#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, Word# #)

primop  ReadByteArrayOp_Word32 "readWord32Array#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, WORD32 #)

primop  ReadByteArrayOp_Word64 "readWord64Array#" GenPrimOp
   MutByteArr# s -> Int# -> State# s -> (# State# s, WORD64 #)

primop  WriteByteArrayOp_Char "writeCharArray#" GenPrimOp
   MutByteArr# s -> Int# -> Char# -> State# s -> State# s
   {Write 8-bit character; offset in bytes.}
   with has_side_effects = True

primop  WriteByteArrayOp_WideChar "writeWideCharArray#" GenPrimOp
   MutByteArr# s -> Int# -> Char# -> State# s -> State# s
   {Write 31-bit character; offset in 4-byte words.}
   with has_side_effects = True

primop  WriteByteArrayOp_Int "writeIntArray#" GenPrimOp
   MutByteArr# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Word "writeWordArray#" GenPrimOp
   MutByteArr# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Addr "writeAddrArray#" GenPrimOp
   MutByteArr# s -> Int# -> Addr# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Float "writeFloatArray#" GenPrimOp
   MutByteArr# s -> Int# -> Float# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Double "writeDoubleArray#" GenPrimOp
   MutByteArr# s -> Int# -> Double# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_StablePtr "writeStablePtrArray#" GenPrimOp
   MutByteArr# s -> Int# -> StablePtr# a -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Int8 "writeInt8Array#" GenPrimOp
   MutByteArr# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Int16 "writeInt16Array#" GenPrimOp
   MutByteArr# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Int32 "writeInt32Array#" GenPrimOp
   MutByteArr# s -> Int# -> INT32 -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Int64 "writeInt64Array#" GenPrimOp
   MutByteArr# s -> Int# -> INT64 -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Word8 "writeWord8Array#" GenPrimOp
   MutByteArr# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Word16 "writeWord16Array#" GenPrimOp
   MutByteArr# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Word32 "writeWord32Array#" GenPrimOp
   MutByteArr# s -> Int# -> WORD32 -> State# s -> State# s
   with has_side_effects = True

primop  WriteByteArrayOp_Word64 "writeWord64Array#" GenPrimOp
   MutByteArr# s -> Int# -> WORD64 -> State# s -> State# s
   with has_side_effects = True

------------------------------------------------------------------------
section "Addr#"
------------------------------------------------------------------------

primtype Addr#
	{ An arbitrary machine address assumed to point outside
	 the garbage-collected heap. }

pseudoop "nullAddr#" Addr#
	{ The null address. }

primop	 AddrAddOp "plusAddr#" GenPrimOp Addr# -> Int# -> Addr#
primop	 AddrSubOp "minusAddr#" GenPrimOp Addr# -> Addr# -> Int#
	 {Result is meaningless if two {\tt Addr\#}s are so far apart that their
	 difference doesn't fit in an {\tt Int\#}.}
primop	 AddrRemOp "remAddr#" GenPrimOp Addr# -> Int# -> Int#
	 {Return the remainder when the {\tt Addr\#} arg, treated like an {\tt Int\#},
	  is divided by the {\tt Int\#} arg.}
#if (WORD_SIZE_IN_BITS == 32 || WORD_SIZE_IN_BITS == 64)
primop   Addr2IntOp  "addr2Int#"     GenPrimOp   Addr# -> Int#
	{Coerce directly from address to int. Strongly deprecated.}
primop   Int2AddrOp   "int2Addr#"    GenPrimOp  Int# -> Addr#
	{Coerce directly from int to address. Strongly deprecated.}
#endif

primop   AddrGtOp  "gtAddr#"   Compare   Addr# -> Addr# -> Bool
primop   AddrGeOp  "geAddr#"   Compare   Addr# -> Addr# -> Bool
primop   AddrEqOp  "eqAddr#"   Compare   Addr# -> Addr# -> Bool
primop   AddrNeOp  "neAddr#"   Compare   Addr# -> Addr# -> Bool
primop   AddrLtOp  "ltAddr#"   Compare   Addr# -> Addr# -> Bool
primop   AddrLeOp  "leAddr#"   Compare   Addr# -> Addr# -> Bool

primop IndexOffAddrOp_Char "indexCharOffAddr#" GenPrimOp
   Addr# -> Int# -> Char#
   {Reads 8-bit character; offset in bytes.}

primop IndexOffAddrOp_WideChar "indexWideCharOffAddr#" GenPrimOp
   Addr# -> Int# -> Char#
   {Reads 31-bit character; offset in 4-byte words.}

primop IndexOffAddrOp_Int "indexIntOffAddr#" GenPrimOp
   Addr# -> Int# -> Int#

primop IndexOffAddrOp_Word "indexWordOffAddr#" GenPrimOp
   Addr# -> Int# -> Word#

primop IndexOffAddrOp_Addr "indexAddrOffAddr#" GenPrimOp
   Addr# -> Int# -> Addr#

primop IndexOffAddrOp_Float "indexFloatOffAddr#" GenPrimOp
   Addr# -> Int# -> Float#

primop IndexOffAddrOp_Double "indexDoubleOffAddr#" GenPrimOp
   Addr# -> Int# -> Double#

primop IndexOffAddrOp_StablePtr "indexStablePtrOffAddr#" GenPrimOp
   Addr# -> Int# -> StablePtr# a

primop IndexOffAddrOp_Int8 "indexInt8OffAddr#" GenPrimOp
   Addr# -> Int# -> Int#

primop IndexOffAddrOp_Int16 "indexInt16OffAddr#" GenPrimOp
   Addr# -> Int# -> Int#

primop IndexOffAddrOp_Int32 "indexInt32OffAddr#" GenPrimOp
   Addr# -> Int# -> INT32

primop IndexOffAddrOp_Int64 "indexInt64OffAddr#" GenPrimOp
   Addr# -> Int# -> INT64

primop IndexOffAddrOp_Word8 "indexWord8OffAddr#" GenPrimOp
   Addr# -> Int# -> Word#

primop IndexOffAddrOp_Word16 "indexWord16OffAddr#" GenPrimOp
   Addr# -> Int# -> Word#

primop IndexOffAddrOp_Word32 "indexWord32OffAddr#" GenPrimOp
   Addr# -> Int# -> WORD32

primop IndexOffAddrOp_Word64 "indexWord64OffAddr#" GenPrimOp
   Addr# -> Int# -> WORD64

primop ReadOffAddrOp_Char "readCharOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Char# #)
   {Reads 8-bit character; offset in bytes.}

primop ReadOffAddrOp_WideChar "readWideCharOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Char# #)
   {Reads 31-bit character; offset in 4-byte words.}

primop ReadOffAddrOp_Int "readIntOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Int# #)

primop ReadOffAddrOp_Word "readWordOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Word# #)

primop ReadOffAddrOp_Addr "readAddrOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Addr# #)

primop ReadOffAddrOp_Float "readFloatOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Float# #)

primop ReadOffAddrOp_Double "readDoubleOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Double# #)

primop ReadOffAddrOp_StablePtr "readStablePtrOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, StablePtr# a #)

primop ReadOffAddrOp_Int8 "readInt8OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Int# #)

primop ReadOffAddrOp_Int16 "readInt16OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Int# #)

primop ReadOffAddrOp_Int32 "readInt32OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, INT32 #)

primop ReadOffAddrOp_Int64 "readInt64OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, INT64 #)

primop ReadOffAddrOp_Word8 "readWord8OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Word# #)

primop ReadOffAddrOp_Word16 "readWord16OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Word# #)

primop ReadOffAddrOp_Word32 "readWord32OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, WORD32 #)

primop ReadOffAddrOp_Word64 "readWord64OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, WORD64 #)


primop  WriteOffAddrOp_Char "writeCharOffAddr#" GenPrimOp
   Addr# -> Int# -> Char# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_WideChar "writeWideCharOffAddr#" GenPrimOp
   Addr# -> Int# -> Char# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Int "writeIntOffAddr#" GenPrimOp
   Addr# -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Word "writeWordOffAddr#" GenPrimOp
   Addr# -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Addr "writeAddrOffAddr#" GenPrimOp
   Addr# -> Int# -> Addr# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Float "writeFloatOffAddr#" GenPrimOp
   Addr# -> Int# -> Float# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Double "writeDoubleOffAddr#" GenPrimOp
   Addr# -> Int# -> Double# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_StablePtr "writeStablePtrOffAddr#" GenPrimOp
   Addr# -> Int# -> StablePtr# a -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Int8 "writeInt8OffAddr#" GenPrimOp
   Addr# -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Int16 "writeInt16OffAddr#" GenPrimOp
   Addr# -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Int32 "writeInt32OffAddr#" GenPrimOp
   Addr# -> Int# -> INT32 -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Int64 "writeInt64OffAddr#" GenPrimOp
   Addr# -> Int# -> INT64 -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Word8 "writeWord8OffAddr#" GenPrimOp
   Addr# -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Word16 "writeWord16OffAddr#" GenPrimOp
   Addr# -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Word32 "writeWord32OffAddr#" GenPrimOp
   Addr# -> Int# -> WORD32 -> State# s -> State# s
   with has_side_effects = True

primop  WriteOffAddrOp_Word64 "writeWord64OffAddr#" GenPrimOp
   Addr# -> Int# -> WORD64 -> State# s -> State# s
   with has_side_effects = True

------------------------------------------------------------------------
section "Mutable variables"
	{Operations on MutVar\#s.}
------------------------------------------------------------------------

primtype MutVar# s a
	{A {\tt MutVar\#} behaves like a single-element mutable array.}

primop  NewMutVarOp "newMutVar#" GenPrimOp
   a -> State# s -> (# State# s, MutVar# s a #)
   {Create {\tt MutVar\#} with specified initial value in specified state thread.}
   with
   out_of_line = True

primop  ReadMutVarOp "readMutVar#" GenPrimOp
   MutVar# s a -> State# s -> (# State# s, a #)
   {Read contents of {\tt MutVar\#}. Result is not yet evaluated.}

primop  WriteMutVarOp "writeMutVar#"  GenPrimOp
   MutVar# s a -> a -> State# s -> State# s
   {Write contents of {\tt MutVar\#}.}
   with
   has_side_effects = True

primop  SameMutVarOp "sameMutVar#" GenPrimOp
   MutVar# s a -> MutVar# s a -> Bool

-- not really the right type, but we don't know about pairs here.  The
-- correct type is
--
--   MutVar# s a -> (a -> (a,b)) -> State# s -> (# State# s, b #)
--
primop  AtomicModifyMutVarOp "atomicModifyMutVar#" GenPrimOp
   MutVar# s a -> (a -> b) -> State# s -> (# State# s, c #)
   with
   has_side_effects = True
   out_of_line = True

------------------------------------------------------------------------
section "Exceptions"
------------------------------------------------------------------------

primop  CatchOp "catch#" GenPrimOp
          (State# RealWorld -> (# State# RealWorld, a #) )
       -> (b -> State# RealWorld -> (# State# RealWorld, a #) ) 
       -> State# RealWorld
       -> (# State# RealWorld, a #)
   with
	-- Catch is actually strict in its first argument
	-- but we don't want to tell the strictness
	-- analyser about that!
        -- might use caught action multiply
   out_of_line = True

primop  RaiseOp "raise#" GenPrimOp
   a -> b
   with
   strictness  = { \ arity -> mkStrictSig (mkTopDmdType [lazyDmd] BotRes) }
      -- NB: result is bottom
   out_of_line = True

-- raiseIO# needs to be a primop, because exceptions in the IO monad
-- must be *precise* - we don't want the strictness analyser turning
-- one kind of bottom into another, as it is allowed to do in pure code.

primop  RaiseIOOp "raiseIO#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, b #)
   with
   out_of_line = True

primop  BlockAsyncExceptionsOp "blockAsyncExceptions#" GenPrimOp
        (State# RealWorld -> (# State# RealWorld, a #))
     -> (State# RealWorld -> (# State# RealWorld, a #))
   with
   out_of_line = True

primop  UnblockAsyncExceptionsOp "unblockAsyncExceptions#" GenPrimOp
        (State# RealWorld -> (# State# RealWorld, a #))
     -> (State# RealWorld -> (# State# RealWorld, a #))
   with
   out_of_line = True

------------------------------------------------------------------------
section "Delay/wait operations"
------------------------------------------------------------------------

primop  DelayOp "delay#" GenPrimOp
   Int# -> State# s -> State# s
   {Sleep specified number of microseconds.}
   with
   needs_wrapper    = True
   has_side_effects = True
   out_of_line      = True

primop  WaitReadOp "waitRead#" GenPrimOp
   Int# -> State# s -> State# s
   {Block until input is available on specified file descriptor.}
   with
   needs_wrapper    = True
   has_side_effects = True
   out_of_line      = True

primop  WaitWriteOp "waitWrite#" GenPrimOp
   Int# -> State# s -> State# s
   {Block until output is possible on specified file descriptor.}
   with
   needs_wrapper    = True
   has_side_effects = True
   out_of_line      = True

#ifdef mingw32_TARGET_OS
primop  AsyncReadOp "asyncRead#" GenPrimOp
   Int# -> Int# -> Int# -> Addr# -> State# RealWorld-> (# State# RealWorld, Int#, Int# #)
   {Asynchronously read bytes from specified file descriptor.}
   with
   needs_wrapper    = True
   has_side_effects = True
   out_of_line      = True

primop  AsyncWriteOp "asyncWrite#" GenPrimOp
   Int# -> Int# -> Int# -> Addr# -> State# RealWorld-> (# State# RealWorld, Int#, Int# #)
   {Asynchronously write bytes from specified file descriptor.}
   with
   needs_wrapper    = True
   has_side_effects = True
   out_of_line      = True

primop  AsyncDoProcOp "asyncDoProc#" GenPrimOp
   Addr# -> Addr# -> State# RealWorld-> (# State# RealWorld, Int#, Int# #)
   {Asynchronously perform procedure (first arg), passing it 2nd arg.}
   with
   needs_wrapper    = True
   has_side_effects = True
   out_of_line      = True

#endif

------------------------------------------------------------------------
section "Concurrency primitives"
------------------------------------------------------------------------

primtype State# s
	{ {\tt State\#} is the primitive, unlifted type of states.  It has
	one type parameter, thus {\tt State\# RealWorld}, or {\tt State\# s},
	where s is a type variable. The only purpose of the type parameter
	is to keep different state threads separate.  It is represented by
	nothing at all. }

primtype RealWorld
	{ {\tt RealWorld} is deeply magical.  It is {\it primitive}, but it is not
	{\it unlifted} (hence {\tt ptrArg}).  We never manipulate values of type
	{\tt RealWorld}; it's only used in the type system, to parameterise {\tt State\#}. }

primop  NoDuplicateOp "noDuplicate#" GenPrimOp
   State# RealWorld -> State# RealWorld
   with
   out_of_line = True

------------------------------------------------------------------------
section "Lightweight concurrency"
------------------------------------------------------------------------
primtype TSO#

primop  SwitchOp "switch#" GenPrimOp
   TSO# -> State# RealWorld -> State# RealWorld
   with
   has_side_effects = True
   out_of_line = True

primop  GetTSOOp "getTSO#" GenPrimOp
   State# RealWorld -> (# State# RealWorld, TSO# #)

primop  NewSContOp "newSCont#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, TSO# #)
   with
   has_side_effects = True
   out_of_line      = True

primop  NewTLSKeyOp "newTLSKey#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, Int# #)
   with
   has_side_effects = True
   out_of_line      = True

primop  GetTLSOp "getTLS#" GenPrimOp
   Int# -> a
   with out_of_line = True

primop  SetTLSOp "setTLS#" GenPrimOp
   Int# -> a -> State# RealWorld -> State# RealWorld
   with
   has_side_effects = True
   out_of_line      = True

primop  RestoreHsIRQsOp "restoreHsIRQs#" GenPrimOp
   Int# -> State# RealWorld -> State# RealWorld
   with has_side_effects = True

primop  DisableHsIRQsOp "disableHsIRQs#" GenPrimOp
   State# RealWorld -> (# State# RealWorld, Int# #)
   with has_side_effects = True

------------------------------------------------------------------------
section "Weak pointers"
------------------------------------------------------------------------

primtype Weak# b

-- note that tyvar "o" denotes openAlphaTyVar

primop  MkWeakOp "mkWeak#" GenPrimOp
   o -> b -> c -> State# RealWorld -> (# State# RealWorld, Weak# b #)
   with
   has_side_effects = True
   out_of_line      = True

primop  DeRefWeakOp "deRefWeak#" GenPrimOp
   Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, a #)
   with
   has_side_effects = True
   out_of_line      = True

primop  FinalizeWeakOp "finalizeWeak#" GenPrimOp
   Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, 
              (State# RealWorld -> (# State# RealWorld, () #)) #)
   with
   has_side_effects = True
   out_of_line      = True

primop TouchOp "touch#" GenPrimOp
   o -> State# RealWorld -> State# RealWorld
   with
   has_side_effects = True

------------------------------------------------------------------------
section "Stable pointers and names"
------------------------------------------------------------------------

primtype StablePtr# a

primtype StableName# a

primop  MakeStablePtrOp "makeStablePtr#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)
   with
   has_side_effects = True
   out_of_line      = True

primop  DeRefStablePtrOp "deRefStablePtr#" GenPrimOp
   StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #)
   with
   needs_wrapper    = True
   has_side_effects = True
   out_of_line      = True

primop  EqStablePtrOp "eqStablePtr#" GenPrimOp
   StablePtr# a -> StablePtr# a -> Int#
   with
   has_side_effects = True

primop  MakeStableNameOp "makeStableName#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, StableName# a #)
   with
   needs_wrapper    = True
   has_side_effects = True
   out_of_line      = True

primop  EqStableNameOp "eqStableName#" GenPrimOp
   StableName# a -> StableName# a -> Int#

primop  StableNameToIntOp "stableNameToInt#" GenPrimOp
   StableName# a -> Int#

------------------------------------------------------------------------
section "Unsafe pointer equality"
--  (#1 Bad Guy: Alistair Reid :)   
------------------------------------------------------------------------

primop  ReallyUnsafePtrEqualityOp "reallyUnsafePtrEquality#" GenPrimOp
   a -> a -> Int#

------------------------------------------------------------------------
section "Parallelism"
------------------------------------------------------------------------

primop  ParOp "par#" GenPrimOp
   a -> Int#
   with
      -- Note that Par is lazy to avoid that the sparked thing
      -- gets evaluted strictly, which it should *not* be
   has_side_effects = True

-- HWL: The first 4 Int# in all par... annotations denote:
--   name, granularity info, size of result, degree of parallelism
--      Same  structure as _seq_ i.e. returns Int#
-- KSW: v, the second arg in parAt# and parAtForNow#, is used only to determine
--   `the processor containing the expression v'; it is not evaluated

primop  ParGlobalOp  "parGlobal#"  GenPrimOp
   a -> Int# -> Int# -> Int# -> Int# -> b -> Int#
   with
   has_side_effects = True

primop  ParLocalOp  "parLocal#"  GenPrimOp
   a -> Int# -> Int# -> Int# -> Int# -> b -> Int#
   with
   has_side_effects = True

primop  ParAtOp  "parAt#"  GenPrimOp
   b -> a -> Int# -> Int# -> Int# -> Int# -> c -> Int#
   with
   has_side_effects = True

primop  ParAtAbsOp  "parAtAbs#"  GenPrimOp
   a -> Int# -> Int# -> Int# -> Int# -> Int# -> b -> Int#
   with
   has_side_effects = True

primop  ParAtRelOp  "parAtRel#" GenPrimOp
   a -> Int# -> Int# -> Int# -> Int# -> Int# -> b -> Int#
   with
   has_side_effects = True

primop  ParAtForNowOp  "parAtForNow#" GenPrimOp
   b -> a -> Int# -> Int# -> Int# -> Int# -> c -> Int#
   with
   has_side_effects = True

-- copyable# and noFollow# are yet to be implemented (for GpH)
--
--primop  CopyableOp  "copyable#" GenPrimOp
--   a -> Int#
--   with
--   has_side_effects = True
--
--primop  NoFollowOp "noFollow#" GenPrimOp
--   a -> Int#
--   with
--   has_side_effects = True


------------------------------------------------------------------------
section "Tag to enum stuff"
	{Convert back and forth between values of enumerated types
	and small integers.}
------------------------------------------------------------------------

primop  DataToTagOp "dataToTag#" GenPrimOp
   a -> Int#
   with
   strictness  = { \ arity -> mkStrictSig (mkTopDmdType [seqDmd] TopRes) }
	-- dataToTag# must have an evaluated argument

primop  TagToEnumOp "tagToEnum#" GenPrimOp     
   Int# -> a

------------------------------------------------------------------------
section "Bytecode operations" 
	{Support for the bytecode interpreter and linker.}
------------------------------------------------------------------------
#ifdef ALLOW_INTERPRETER
primtype BCO#
   {Primitive bytecode type.}
#endif
primop   AddrToHValueOp "addrToHValue#" GenPrimOp
   Addr# -> (# a #)
   {Convert an {\tt Addr\#} to a followable type.}

#ifdef ALLOW_INTERPRETER
primop   MkApUpd0_Op "mkApUpd0#" GenPrimOp
   BCO# -> (# a #)
   with
   out_of_line = True

primop  NewBCOOp "newBCO#" GenPrimOp
   ByteArr# -> ByteArr# -> Array# a -> Int# -> ByteArr# -> State# s -> (# State# s, BCO# #)
   with
   has_side_effects = True
   out_of_line      = True
#endif

primop  UnpackClosureOp "unpackClosure#" GenPrimOp
   a -> (# Addr#, Array# b, ByteArr# #)
   with
   out_of_line = True

primop  GetApStackValOp "getApStackVal#" GenPrimOp
   a -> Int# -> (# Int#, b #)
   with
   out_of_line = True

------------------------------------------------------------------------
section "Etc" 
	{Miscellaneous built-ins}
------------------------------------------------------------------------

pseudoop   "seq"
   a -> b -> b
   { Evaluates its first argument to head normal form, and then returns its second
	argument as the result. }

pseudoop   "inline"
   a -> a
   { The call {\tt (inline f)} arranges that f is inlined, regardless of its size.
	More precisely, the call {\tt (inline f)} rewrites to the right-hand side of
	{\tt f}'s definition. This allows the programmer to control inlining from a
	particular call site rather than the definition site of the function (c.f.
	{\tt INLINE} pragmas in User's Guide, Section 7.10.3, "INLINE and NOINLINE
	pragmas").

	This inlining occurs regardless of the argument to the call or the size of
	{\tt f}'s definition; it is unconditional. The main caveat is that {\tt f}'s
	definition must be visible to the compiler. That is, {\tt f} must be
	{\tt let}-bound in the current scope. If no inlining takes place, the
	{\tt inline} function expands to the identity function in Phase zero; so its
	use imposes no overhead.

	If the function is defined in another module, GHC only exposes its inlining
	in the interface file if the function is sufficiently small that it might be
	inlined by the automatic mechanism. There is currently no way to tell GHC to
	expose arbitrarily-large functions in the interface file. (This shortcoming
	is something that could be fixed, with some kind of pragma.) }

pseudoop   "lazy"
   a -> a
   { The {\tt lazy} function restrains strictness analysis a little. The call
	{\tt (lazy e)} means the same as {\tt e}, but {\tt lazy} has a magical
	property so far as strictness analysis is concerned: it is lazy in its first
	argument, even though its semantics is strict. After strictness analysis has
	run, calls to {\tt lazy} are inlined to be the identity function.

	This behaviour is occasionally useful when controlling evaluation order.
	Notably, {\tt lazy} is used in the library definition of {\tt Control.Parallel.par}:

	{\tt par :: a -> b -> b}

	{\tt par x y = case (par\# x) of \_ -> lazy y}

	If {\tt lazy} were not lazy, {\tt par} would look strict in {\tt y} which
	would defeat the whole purpose of {\tt par}.

	Like {\tt seq}, the argument of {\tt lazy} can have an unboxed type. }

primtype Any a
	{ The type constructor {\tt Any} is type to which you can unsafely coerce any
	lifted type, and back. 

	  * It is lifted, and hence represented by a pointer

	  * It does not claim to be a {\it data} type, and that's important for
	    the code generator, because the code gen may {\it enter} a data value
	    but never enters a function value.  

	It's also used to instantiate un-constrained type variables after type
	checking.  For example

	{\tt length Any []}

	Annoyingly, we sometimes need {\tt Any}s of other kinds, such as {\tt (* -> *)} etc.
	This is a bit like tuples.   We define a couple of useful ones here,
	and make others up on the fly.  If any of these others end up being exported
	into interface files, we'll get a crash; at least until we add interface-file
	syntax to support them. }

pseudoop   "unsafeCoerce#"
   a -> b
   { The function {\tt unsafeCoerce\#} allows you to side-step the typechecker entirely. That
	is, it allows you to coerce any type into any other type. If you use this function,
	you had better get it right, otherwise segmentation faults await. It is generally
	used when you want to write a program that you know is well-typed, but where Haskell's
	type system is not expressive enough to prove that it is well typed.

        The following uses of {\tt unsafeCoerce\#} are supposed to work (i.e. not lead to
        spurious compile-time or run-time crashes):

         * Casting any lifted type to {\tt Any}

         * Casting {\tt Any} back to the real type

         * Casting an unboxed type to another unboxed type of the same size

         * Casting between two types that have the same runtime representation.  One case is when
           the two types differ only in "phantom" type parameters, for example
           {\tt Ptr Int} to {\tt Ptr Float}, or {\tt [Int]} to {\tt [Float]} when the list is 
           known to be empty.  Also, a {\tt newtype} of a type {\tt T} has the same representation
           at runtime as {\tt T}.

        Other uses of {\tt unsafeCoerce\#} are undefined.
        }

-- NB. It is tempting to think that casting a value to a type that it doesn't have is safe
-- as long as you don't "do anything" with the value in its cast form, such as seq on it.  This
-- isn't the case: the compiler can insert seqs itself, and if these happen at the wrong type,
-- Bad Things Might Happen.  See bug #1616: in this case we cast a function of type (a,b) -> (a,b)
-- to () -> () and back again.  The strictness analyser saw that the function was strict, but
-- the wrapper had type () -> (), and hence the wrapper de-constructed the (), the worker re-constructed
-- a new (), with the result that the code ended up with "case () of (a,b) -> ...".

------------------------------------------------------------------------
---                                                                  ---
------------------------------------------------------------------------

thats_all_folks



