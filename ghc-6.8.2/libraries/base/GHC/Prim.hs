-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Arr
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- GHC's primitive types and operations.
--
-----------------------------------------------------------------------------
module GHC.Prim (
	
-- * The word size story.
-- |Haskell98 specifies that signed integers (type @Int@)
-- 	 must contain at least 30 bits. GHC always implements @Int@ using the primitive type @Int\#@, whose size equals
-- 	 the @MachDeps.h@ constant @WORD\_SIZE\_IN\_BITS@.
-- 	 This is normally set based on the @config.h@ parameter
-- 	 @SIZEOF\_HSWORD@, i.e., 32 bits on 32-bit machines, 64
-- 	 bits on 64-bit machines.  However, it can also be explicitly
-- 	 set to a smaller number, e.g., 31 bits, to allow the
-- 	 possibility of using tag bits. Currently GHC itself has only
-- 	 32-bit and 64-bit variants, but 30 or 31-bit code can be
-- 	 exported as an external core file for use in other back ends.
-- 
-- 	 GHC also implements a primitive unsigned integer type @Word\#@ which always has the same number of bits as @Int\#@.
-- 	
-- 	 In addition, GHC supports families of explicit-sized integers
-- 	 and words at 8, 16, 32, and 64 bits, with the usual
-- 	 arithmetic operations, comparisons, and a range of
-- 	 conversions.  The 8-bit and 16-bit sizes are always
-- 	 represented as @Int\#@ and @Word\#@, and the
-- 	 operations implemented in terms of the the primops on these
-- 	 types, with suitable range restrictions on the results (using
-- 	 the @narrow$n$Int\#@ and @narrow$n$Word\#@ families
-- 	 of primops.  The 32-bit sizes are represented using @Int\#@ and @Word\#@ when @WORD\_SIZE\_IN\_BITS@
-- 	 $\geq$ 32; otherwise, these are represented using distinct
-- 	 primitive types @Int32\#@ and @Word32\#@. These (when
-- 	 needed) have a complete set of corresponding operations;
-- 	 however, nearly all of these are implemented as external C
-- 	 functions rather than as primops.  Exactly the same story
-- 	 applies to the 64-bit sizes.  All of these details are hidden
-- 	 under the @PrelInt@ and @PrelWord@ modules, which use
-- 	 @\#if@-defs to invoke the appropriate types and
-- 	 operators.
-- 
-- 	 Word size also matters for the families of primops for
-- 	 indexing\/reading\/writing fixed-size quantities at offsets
-- 	 from an array base, address, or foreign pointer.  Here, a
-- 	 slightly different approach is taken.  The names of these
-- 	 primops are fixed, but their /types/ vary according to
-- 	 the value of @WORD\_SIZE\_IN\_BITS@. For example, if word
-- 	 size is at least 32 bits then an operator like
-- 	 @indexInt32Array\#@ has type @ByteArr\# -> Int\# 	 -> Int\#@; otherwise it has type @ByteArr\# -> Int\# -> 	 Int32\#@.  This approach confines the necessary @\#if@-defs to this file; no conditional compilation is needed
-- 	 in the files that expose these primops.
-- 
-- 	 Finally, there are strongly deprecated primops for coercing
--          between @Addr\#@, the primitive type of machine
--          addresses, and @Int\#@.  These are pretty bogus anyway,
--          but will work on existing 32-bit and 64-bit GHC targets; they
--          are completely bogus when tag bits are used in @Int\#@,
--          so are not available in this case.  


	
-- * Char#
-- |Operations on 31-bit characters.


	Char#,
	gtChar#,
	geChar#,
	eqChar#,
	neChar#,
	ltChar#,
	leChar#,
	ord#,
	
-- * Int#
-- |Operations on native-size integers (30+ bits).


	Int#,
	(+#),
	(-#),
	(*#),
	mulIntMayOflo#,
	quotInt#,
	remInt#,
	gcdInt#,
	negateInt#,
	addIntC#,
	subIntC#,
	(>#),
	(>=#),
	(==#),
	(/=#),
	(<#),
	(<=#),
	chr#,
	int2Word#,
	int2Float#,
	int2Double#,
	int2Integer#,
	uncheckedIShiftL#,
	uncheckedIShiftRA#,
	uncheckedIShiftRL#,
	
-- * Word#
-- |Operations on native-sized unsigned words (30+ bits).


	Word#,
	plusWord#,
	minusWord#,
	timesWord#,
	quotWord#,
	remWord#,
	and#,
	or#,
	xor#,
	not#,
	uncheckedShiftL#,
	uncheckedShiftRL#,
	word2Int#,
	word2Integer#,
	gtWord#,
	geWord#,
	eqWord#,
	neWord#,
	ltWord#,
	leWord#,
	
-- * Narrowings
-- |Explicit narrowing of native-sized ints or words.


	narrow8Int#,
	narrow16Int#,
	narrow32Int#,
	narrow8Word#,
	narrow16Word#,
	narrow32Word#,
	
-- * Integer#
-- |Operations on arbitrary-precision integers. These operations are 
-- implemented via the GMP package. An integer is represented as a pair
-- consisting of an @Int\#@ representing the number of \'limbs\' in use and
-- the sign, and a @ByteArr\#@ containing the \'limbs\' themselves.  Such pairs
-- are returned as unboxed pairs, but must be passed as separate
-- components.
-- 
-- For .NET these operations are implemented by foreign imports, so the
-- primops are omitted.


	plusInteger#,
	minusInteger#,
	timesInteger#,
	gcdInteger#,
	gcdIntegerInt#,
	divExactInteger#,
	quotInteger#,
	remInteger#,
	cmpInteger#,
	cmpIntegerInt#,
	quotRemInteger#,
	divModInteger#,
	integer2Int#,
	integer2Word#,
	andInteger#,
	orInteger#,
	xorInteger#,
	complementInteger#,
	
-- * Double#
-- |Operations on double-precision (64 bit) floating-point numbers.


	Double#,
	(>##),
	(>=##),
	(==##),
	(/=##),
	(<##),
	(<=##),
	(+##),
	(-##),
	(*##),
	(/##),
	negateDouble#,
	double2Int#,
	double2Float#,
	expDouble#,
	logDouble#,
	sqrtDouble#,
	sinDouble#,
	cosDouble#,
	tanDouble#,
	asinDouble#,
	acosDouble#,
	atanDouble#,
	sinhDouble#,
	coshDouble#,
	tanhDouble#,
	(**##),
	decodeDouble#,
	
-- * Float#
-- |Operations on single-precision (32-bit) floating-point numbers.


	Float#,
	gtFloat#,
	geFloat#,
	eqFloat#,
	neFloat#,
	ltFloat#,
	leFloat#,
	plusFloat#,
	minusFloat#,
	timesFloat#,
	divideFloat#,
	negateFloat#,
	float2Int#,
	expFloat#,
	logFloat#,
	sqrtFloat#,
	sinFloat#,
	cosFloat#,
	tanFloat#,
	asinFloat#,
	acosFloat#,
	atanFloat#,
	sinhFloat#,
	coshFloat#,
	tanhFloat#,
	powerFloat#,
	float2Double#,
	decodeFloat#,
	
-- * Arrays
-- |Operations on @Array\#@.


	Array#,
	MutArr#,
	newArray#,
	sameMutableArray#,
	readArray#,
	writeArray#,
	indexArray#,
	unsafeFreezeArray#,
	unsafeThawArray#,
	
-- * Byte Arrays
-- |Operations on @ByteArray\#@. A @ByteArray\#@ is a just a region of
--          raw memory in the garbage-collected heap, which is not scanned
--          for pointers. It carries its own size (in bytes). There are
-- 	 three sets of operations for accessing byte array contents:
-- 	 index for reading from immutable byte arrays, and read\/write
-- 	 for mutable byte arrays.  Each set contains operations for 
-- 	 a range of useful primitive data types.  Each operation takes	
-- 	 an offset measured in terms of the size fo the primitive type
-- 	 being read or written.


	ByteArr#,
	MutByteArr#,
	newByteArray#,
	newPinnedByteArray#,
	byteArrayContents#,
	sameMutableByteArray#,
	unsafeFreezeByteArray#,
	sizeofByteArray#,
	sizeofMutableByteArray#,
	indexCharArray#,
	indexWideCharArray#,
	indexIntArray#,
	indexWordArray#,
	indexAddrArray#,
	indexFloatArray#,
	indexDoubleArray#,
	indexStablePtrArray#,
	indexInt8Array#,
	indexInt16Array#,
	indexInt32Array#,
	indexInt64Array#,
	indexWord8Array#,
	indexWord16Array#,
	indexWord32Array#,
	indexWord64Array#,
	readCharArray#,
	readWideCharArray#,
	readIntArray#,
	readWordArray#,
	readAddrArray#,
	readFloatArray#,
	readDoubleArray#,
	readStablePtrArray#,
	readInt8Array#,
	readInt16Array#,
	readInt32Array#,
	readInt64Array#,
	readWord8Array#,
	readWord16Array#,
	readWord32Array#,
	readWord64Array#,
	writeCharArray#,
	writeWideCharArray#,
	writeIntArray#,
	writeWordArray#,
	writeAddrArray#,
	writeFloatArray#,
	writeDoubleArray#,
	writeStablePtrArray#,
	writeInt8Array#,
	writeInt16Array#,
	writeInt32Array#,
	writeInt64Array#,
	writeWord8Array#,
	writeWord16Array#,
	writeWord32Array#,
	writeWord64Array#,
	
-- * Addr#
-- |


	Addr#,
	nullAddr#,
	plusAddr#,
	minusAddr#,
	remAddr#,
	addr2Int#,
	int2Addr#,
	gtAddr#,
	geAddr#,
	eqAddr#,
	neAddr#,
	ltAddr#,
	leAddr#,
	indexCharOffAddr#,
	indexWideCharOffAddr#,
	indexIntOffAddr#,
	indexWordOffAddr#,
	indexAddrOffAddr#,
	indexFloatOffAddr#,
	indexDoubleOffAddr#,
	indexStablePtrOffAddr#,
	indexInt8OffAddr#,
	indexInt16OffAddr#,
	indexInt32OffAddr#,
	indexInt64OffAddr#,
	indexWord8OffAddr#,
	indexWord16OffAddr#,
	indexWord32OffAddr#,
	indexWord64OffAddr#,
	readCharOffAddr#,
	readWideCharOffAddr#,
	readIntOffAddr#,
	readWordOffAddr#,
	readAddrOffAddr#,
	readFloatOffAddr#,
	readDoubleOffAddr#,
	readStablePtrOffAddr#,
	readInt8OffAddr#,
	readInt16OffAddr#,
	readInt32OffAddr#,
	readInt64OffAddr#,
	readWord8OffAddr#,
	readWord16OffAddr#,
	readWord32OffAddr#,
	readWord64OffAddr#,
	writeCharOffAddr#,
	writeWideCharOffAddr#,
	writeIntOffAddr#,
	writeWordOffAddr#,
	writeAddrOffAddr#,
	writeFloatOffAddr#,
	writeDoubleOffAddr#,
	writeStablePtrOffAddr#,
	writeInt8OffAddr#,
	writeInt16OffAddr#,
	writeInt32OffAddr#,
	writeInt64OffAddr#,
	writeWord8OffAddr#,
	writeWord16OffAddr#,
	writeWord32OffAddr#,
	writeWord64OffAddr#,
	
-- * Mutable variables
-- |Operations on MutVar\#s.


	MutVar#,
	newMutVar#,
	readMutVar#,
	writeMutVar#,
	sameMutVar#,
	atomicModifyMutVar#,
	
-- * Exceptions
-- |


	catch#,
	raise#,
	raiseIO#,
	blockAsyncExceptions#,
	unblockAsyncExceptions#,
	
-- * STM-accessible Mutable Variables
-- |


	TVar#,
	atomically#,
	retry#,
	catchRetry#,
	catchSTM#,
	check#,
	newTVar#,
	readTVar#,
	writeTVar#,
	sameTVar#,
	
-- * Synchronized Mutable Variables
-- |Operations on @MVar\#@s. 


	MVar#,
	newMVar#,
	takeMVar#,
	tryTakeMVar#,
	putMVar#,
	tryPutMVar#,
	sameMVar#,
	isEmptyMVar#,
	
-- * Delay\/wait operations
-- |


	delay#,
	waitRead#,
	waitWrite#,
	
-- * Concurrency primitives
-- |


	State#,
	RealWorld,
	ThreadId#,
	fork#,
	forkOn#,
	killThread#,
	yield#,
	myThreadId#,
	labelThread#,
	isCurrentThreadBound#,
	noDuplicate#,
	
-- * Weak pointers
-- |


	Weak#,
	mkWeak#,
	deRefWeak#,
	finalizeWeak#,
	touch#,
	
-- * Stable pointers and names
-- |


	StablePtr#,
	StableName#,
	makeStablePtr#,
	deRefStablePtr#,
	eqStablePtr#,
	makeStableName#,
	eqStableName#,
	stableNameToInt#,
	
-- * Unsafe pointer equality
-- |


	reallyUnsafePtrEquality#,
	
-- * Parallelism
-- |


	par#,
	parGlobal#,
	parLocal#,
	parAt#,
	parAtAbs#,
	parAtRel#,
	parAtForNow#,
	
-- * Tag to enum stuff
-- |Convert back and forth between values of enumerated types
-- 	and small integers.


	dataToTag#,
	tagToEnum#,
	
-- * Bytecode operations
-- |Support for the bytecode interpreter and linker.


	BCO#,
	addrToHValue#,
	mkApUpd0#,
	newBCO#,
	unpackClosure#,
	getApStackVal#,
	
-- * Etc
-- |Miscellaneous built-ins


	seq,
	inline,
	lazy,
	Any,
	unsafeCoerce#,
) where

{-
has_side_effects = False
out_of_line = False
commutable = False
needs_wrapper = False
can_fail = False
strictness = {  \ arity -> mkStrictSig (mkTopDmdType (replicate arity lazyDmd) TopRes) }
-}


data Char#
gtChar# :: Char# -> Char# -> Bool
geChar# :: Char# -> Char# -> Bool
eqChar# :: Char# -> Char# -> Bool
neChar# :: Char# -> Char# -> Bool
ltChar# :: Char# -> Char# -> Bool
leChar# :: Char# -> Char# -> Bool
ord# :: Char# -> Int#

data Int#
(+#) :: Int# -> Int# -> Int#
(-#) :: Int# -> Int# -> Int#

-- |Low word of signed integer multiply.
(*#) :: Int# -> Int# -> Int#

-- |Return non-zero if there is any possibility that the upper word of a
--     signed integer multiply might contain useful information.  Return
--     zero only if you are completely sure that no overflow can occur.
--     On a 32-bit platform, the recommmended implementation is to do a 
--     32 x 32 -> 64 signed multiply, and subtract result[63:32] from
--     (result[31] >>signed 31).  If this is zero, meaning that the 
--     upper word is merely a sign extension of the lower one, no
--     overflow can occur.
-- 
--     On a 64-bit platform it is not always possible to 
--     acquire the top 64 bits of the result.  Therefore, a recommended 
--     implementation is to take the absolute value of both operands, and 
--     return 0 iff bits[63:31] of them are zero, since that means that their 
--     magnitudes fit within 31 bits, so the magnitude of the product must fit 
--     into 62 bits.
-- 
--     If in doubt, return non-zero, but do make an effort to create the
--     correct answer for small args, since otherwise the performance of
--     @(*) :: Integer -> Integer -> Integer@ will be poor.
--    
mulIntMayOflo# :: Int# -> Int# -> Int#

-- |Rounds towards zero.
quotInt# :: Int# -> Int# -> Int#

-- |Satisfies @(quotInt\# x y) *\# y +\# (remInt\# x y) == x@.
remInt# :: Int# -> Int# -> Int#
gcdInt# :: Int# -> Int# -> Int#
negateInt# :: Int# -> Int#

-- |Add with carry.  First member of result is (wrapped) sum; 
--           second member is 0 iff no overflow occured.
addIntC# :: Int# -> Int# -> (# Int#,Int# #)

-- |Subtract with carry.  First member of result is (wrapped) difference; 
--           second member is 0 iff no overflow occured.
subIntC# :: Int# -> Int# -> (# Int#,Int# #)
(>#) :: Int# -> Int# -> Bool
(>=#) :: Int# -> Int# -> Bool
(==#) :: Int# -> Int# -> Bool
(/=#) :: Int# -> Int# -> Bool
(<#) :: Int# -> Int# -> Bool
(<=#) :: Int# -> Int# -> Bool
chr# :: Int# -> Char#
int2Word# :: Int# -> Word#
int2Float# :: Int# -> Float#
int2Double# :: Int# -> Double#
int2Integer# :: Int# -> (# Int#,ByteArr# #)

-- |Shift left.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
uncheckedIShiftL# :: Int# -> Int# -> Int#

-- |Shift right arithmetic.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
uncheckedIShiftRA# :: Int# -> Int# -> Int#

-- |Shift right logical.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
uncheckedIShiftRL# :: Int# -> Int# -> Int#

data Word#
plusWord# :: Word# -> Word# -> Word#
minusWord# :: Word# -> Word# -> Word#
timesWord# :: Word# -> Word# -> Word#
quotWord# :: Word# -> Word# -> Word#
remWord# :: Word# -> Word# -> Word#
and# :: Word# -> Word# -> Word#
or# :: Word# -> Word# -> Word#
xor# :: Word# -> Word# -> Word#
not# :: Word# -> Word#

-- |Shift left logical.   Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
uncheckedShiftL# :: Word# -> Int# -> Word#

-- |Shift right logical.   Result undefined if shift  amount is not
--           in the range 0 to word size - 1 inclusive.
uncheckedShiftRL# :: Word# -> Int# -> Word#
word2Int# :: Word# -> Int#
word2Integer# :: Word# -> (# Int#,ByteArr# #)
gtWord# :: Word# -> Word# -> Bool
geWord# :: Word# -> Word# -> Bool
eqWord# :: Word# -> Word# -> Bool
neWord# :: Word# -> Word# -> Bool
ltWord# :: Word# -> Word# -> Bool
leWord# :: Word# -> Word# -> Bool

narrow8Int# :: Int# -> Int#
narrow16Int# :: Int# -> Int#
narrow32Int# :: Int# -> Int#
narrow8Word# :: Word# -> Word#
narrow16Word# :: Word# -> Word#
narrow32Word# :: Word# -> Word#

plusInteger# :: Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#,ByteArr# #)
minusInteger# :: Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#,ByteArr# #)
timesInteger# :: Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#,ByteArr# #)

-- |Greatest common divisor.
gcdInteger# :: Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#,ByteArr# #)

-- |Greatest common divisor, where second argument is an ordinary @Int\#@.
gcdIntegerInt# :: Int# -> ByteArr# -> Int# -> Int#

-- |Divisor is guaranteed to be a factor of dividend.
divExactInteger# :: Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#,ByteArr# #)

-- |Rounds towards zero.
quotInteger# :: Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#,ByteArr# #)

-- |Satisfies @plusInteger\# (timesInteger\# (quotInteger\# x y) y) (remInteger\# x y) == x@.
remInteger# :: Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#,ByteArr# #)

-- |Returns -1,0,1 according as first argument is less than, equal to, or greater than second argument.
cmpInteger# :: Int# -> ByteArr# -> Int# -> ByteArr# -> Int#

-- |Returns -1,0,1 according as first argument is less than, equal to, or greater than second argument, which
--    is an ordinary Int\#.
cmpIntegerInt# :: Int# -> ByteArr# -> Int# -> Int#

-- |Compute quot and rem simulaneously.
quotRemInteger# :: Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#,ByteArr#,Int#,ByteArr# #)

-- |Compute div and mod simultaneously, where div rounds towards negative infinity
--     and@(q,r) = divModInteger\#(x,y)@ implies @plusInteger\# (timesInteger\# q y) r = x@.
divModInteger# :: Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#,ByteArr#,Int#,ByteArr# #)
integer2Int# :: Int# -> ByteArr# -> Int#
integer2Word# :: Int# -> ByteArr# -> Word#
andInteger# :: Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#,ByteArr# #)
orInteger# :: Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#,ByteArr# #)
xorInteger# :: Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#,ByteArr# #)
complementInteger# :: Int# -> ByteArr# -> (# Int#,ByteArr# #)

data Double#
(>##) :: Double# -> Double# -> Bool
(>=##) :: Double# -> Double# -> Bool
(==##) :: Double# -> Double# -> Bool
(/=##) :: Double# -> Double# -> Bool
(<##) :: Double# -> Double# -> Bool
(<=##) :: Double# -> Double# -> Bool
(+##) :: Double# -> Double# -> Double#
(-##) :: Double# -> Double# -> Double#
(*##) :: Double# -> Double# -> Double#
(/##) :: Double# -> Double# -> Double#
negateDouble# :: Double# -> Double#
double2Int# :: Double# -> Int#
double2Float# :: Double# -> Float#
expDouble# :: Double# -> Double#
logDouble# :: Double# -> Double#
sqrtDouble# :: Double# -> Double#
sinDouble# :: Double# -> Double#
cosDouble# :: Double# -> Double#
tanDouble# :: Double# -> Double#
asinDouble# :: Double# -> Double#
acosDouble# :: Double# -> Double#
atanDouble# :: Double# -> Double#
sinhDouble# :: Double# -> Double#
coshDouble# :: Double# -> Double#
tanhDouble# :: Double# -> Double#

-- |Exponentiation.
(**##) :: Double# -> Double# -> Double#

-- |Convert to arbitrary-precision integer.
--     First @Int\#@ in result is the exponent; second @Int\#@ and @ByteArr\#@
--     represent an @Integer\#@ holding the mantissa.
decodeDouble# :: Double# -> (# Int#,Int#,ByteArr# #)

data Float#
gtFloat# :: Float# -> Float# -> Bool
geFloat# :: Float# -> Float# -> Bool
eqFloat# :: Float# -> Float# -> Bool
neFloat# :: Float# -> Float# -> Bool
ltFloat# :: Float# -> Float# -> Bool
leFloat# :: Float# -> Float# -> Bool
plusFloat# :: Float# -> Float# -> Float#
minusFloat# :: Float# -> Float# -> Float#
timesFloat# :: Float# -> Float# -> Float#
divideFloat# :: Float# -> Float# -> Float#
negateFloat# :: Float# -> Float#
float2Int# :: Float# -> Int#
expFloat# :: Float# -> Float#
logFloat# :: Float# -> Float#
sqrtFloat# :: Float# -> Float#
sinFloat# :: Float# -> Float#
cosFloat# :: Float# -> Float#
tanFloat# :: Float# -> Float#
asinFloat# :: Float# -> Float#
acosFloat# :: Float# -> Float#
atanFloat# :: Float# -> Float#
sinhFloat# :: Float# -> Float#
coshFloat# :: Float# -> Float#
tanhFloat# :: Float# -> Float#
powerFloat# :: Float# -> Float# -> Float#
float2Double# :: Float# -> Double#

-- |Convert to arbitrary-precision integer.
--     First @Int\#@ in result is the exponent; second @Int\#@ and @ByteArr\#@
--     represent an @Integer\#@ holding the mantissa.
decodeFloat# :: Float# -> (# Int#,Int#,ByteArr# #)

data Array# a
data MutArr# s a

-- |Create a new mutable array of specified size (in bytes),
--     in the specified state thread,
--     with each element containing the specified initial value.
newArray# :: Int# -> a -> State# s -> (# State# s,MutArr# s a #)
sameMutableArray# :: MutArr# s a -> MutArr# s a -> Bool

-- |Read from specified index of mutable array. Result is not yet evaluated.
readArray# :: MutArr# s a -> Int# -> State# s -> (# State# s,a #)

-- |Write to specified index of mutable array.
writeArray# :: MutArr# s a -> Int# -> a -> State# s -> State# s

-- |Read from specified index of immutable array. Result is packaged into
--     an unboxed singleton; the result itself is not yet evaluated.
indexArray# :: Array# a -> Int# -> (# a #)

-- |Make a mutable array immutable, without copying.
unsafeFreezeArray# :: MutArr# s a -> State# s -> (# State# s,Array# a #)

-- |Make an immutable array mutable, without copying.
unsafeThawArray# :: Array# a -> State# s -> (# State# s,MutArr# s a #)

data ByteArr#
data MutByteArr# s

-- |Create a new mutable byte array of specified size (in bytes), in
--     the specified state thread.
newByteArray# :: Int# -> State# s -> (# State# s,MutByteArr# s #)

-- |Create a mutable byte array that the GC guarantees not to move.
newPinnedByteArray# :: Int# -> State# s -> (# State# s,MutByteArr# s #)

-- |Intended for use with pinned arrays; otherwise very unsafe!
byteArrayContents# :: ByteArr# -> Addr#
sameMutableByteArray# :: MutByteArr# s -> MutByteArr# s -> Bool

-- |Make a mutable byte array immutable, without copying.
unsafeFreezeByteArray# :: MutByteArr# s -> State# s -> (# State# s,ByteArr# #)
sizeofByteArray# :: ByteArr# -> Int#
sizeofMutableByteArray# :: MutByteArr# s -> Int#

-- |Read 8-bit character; offset in bytes.
indexCharArray# :: ByteArr# -> Int# -> Char#

-- |Read 31-bit character; offset in 4-byte words.
indexWideCharArray# :: ByteArr# -> Int# -> Char#
indexIntArray# :: ByteArr# -> Int# -> Int#
indexWordArray# :: ByteArr# -> Int# -> Word#
indexAddrArray# :: ByteArr# -> Int# -> Addr#
indexFloatArray# :: ByteArr# -> Int# -> Float#
indexDoubleArray# :: ByteArr# -> Int# -> Double#
indexStablePtrArray# :: ByteArr# -> Int# -> StablePtr# a
indexInt8Array# :: ByteArr# -> Int# -> Int#
indexInt16Array# :: ByteArr# -> Int# -> Int#
indexInt32Array# :: ByteArr# -> Int# -> Int#
indexInt64Array# :: ByteArr# -> Int# -> Int#
indexWord8Array# :: ByteArr# -> Int# -> Word#
indexWord16Array# :: ByteArr# -> Int# -> Word#
indexWord32Array# :: ByteArr# -> Int# -> Word#
indexWord64Array# :: ByteArr# -> Int# -> Word#

-- |Read 8-bit character; offset in bytes.
readCharArray# :: MutByteArr# s -> Int# -> State# s -> (# State# s,Char# #)

-- |Read 31-bit character; offset in 4-byte words.
readWideCharArray# :: MutByteArr# s -> Int# -> State# s -> (# State# s,Char# #)
readIntArray# :: MutByteArr# s -> Int# -> State# s -> (# State# s,Int# #)
readWordArray# :: MutByteArr# s -> Int# -> State# s -> (# State# s,Word# #)
readAddrArray# :: MutByteArr# s -> Int# -> State# s -> (# State# s,Addr# #)
readFloatArray# :: MutByteArr# s -> Int# -> State# s -> (# State# s,Float# #)
readDoubleArray# :: MutByteArr# s -> Int# -> State# s -> (# State# s,Double# #)
readStablePtrArray# :: MutByteArr# s -> Int# -> State# s -> (# State# s,StablePtr# a #)
readInt8Array# :: MutByteArr# s -> Int# -> State# s -> (# State# s,Int# #)
readInt16Array# :: MutByteArr# s -> Int# -> State# s -> (# State# s,Int# #)
readInt32Array# :: MutByteArr# s -> Int# -> State# s -> (# State# s,Int# #)
readInt64Array# :: MutByteArr# s -> Int# -> State# s -> (# State# s,Int# #)
readWord8Array# :: MutByteArr# s -> Int# -> State# s -> (# State# s,Word# #)
readWord16Array# :: MutByteArr# s -> Int# -> State# s -> (# State# s,Word# #)
readWord32Array# :: MutByteArr# s -> Int# -> State# s -> (# State# s,Word# #)
readWord64Array# :: MutByteArr# s -> Int# -> State# s -> (# State# s,Word# #)

-- |Write 8-bit character; offset in bytes.
writeCharArray# :: MutByteArr# s -> Int# -> Char# -> State# s -> State# s

-- |Write 31-bit character; offset in 4-byte words.
writeWideCharArray# :: MutByteArr# s -> Int# -> Char# -> State# s -> State# s
writeIntArray# :: MutByteArr# s -> Int# -> Int# -> State# s -> State# s
writeWordArray# :: MutByteArr# s -> Int# -> Word# -> State# s -> State# s
writeAddrArray# :: MutByteArr# s -> Int# -> Addr# -> State# s -> State# s
writeFloatArray# :: MutByteArr# s -> Int# -> Float# -> State# s -> State# s
writeDoubleArray# :: MutByteArr# s -> Int# -> Double# -> State# s -> State# s
writeStablePtrArray# :: MutByteArr# s -> Int# -> StablePtr# a -> State# s -> State# s
writeInt8Array# :: MutByteArr# s -> Int# -> Int# -> State# s -> State# s
writeInt16Array# :: MutByteArr# s -> Int# -> Int# -> State# s -> State# s
writeInt32Array# :: MutByteArr# s -> Int# -> Int# -> State# s -> State# s
writeInt64Array# :: MutByteArr# s -> Int# -> Int# -> State# s -> State# s
writeWord8Array# :: MutByteArr# s -> Int# -> Word# -> State# s -> State# s
writeWord16Array# :: MutByteArr# s -> Int# -> Word# -> State# s -> State# s
writeWord32Array# :: MutByteArr# s -> Int# -> Word# -> State# s -> State# s
writeWord64Array# :: MutByteArr# s -> Int# -> Word# -> State# s -> State# s


-- | An arbitrary machine address assumed to point outside
-- 	 the garbage-collected heap. 
data Addr#

-- | The null address. 
nullAddr# :: Addr#
plusAddr# :: Addr# -> Int# -> Addr#

-- |Result is meaningless if two @Addr\#@s are so far apart that their
-- 	 difference doesn\'t fit in an @Int\#@.
minusAddr# :: Addr# -> Addr# -> Int#

-- |Return the remainder when the @Addr\#@ arg, treated like an @Int\#@,
-- 	  is divided by the @Int\#@ arg.
remAddr# :: Addr# -> Int# -> Int#

-- |Coerce directly from address to int. Strongly deprecated.
addr2Int# :: Addr# -> Int#

-- |Coerce directly from int to address. Strongly deprecated.
int2Addr# :: Int# -> Addr#
gtAddr# :: Addr# -> Addr# -> Bool
geAddr# :: Addr# -> Addr# -> Bool
eqAddr# :: Addr# -> Addr# -> Bool
neAddr# :: Addr# -> Addr# -> Bool
ltAddr# :: Addr# -> Addr# -> Bool
leAddr# :: Addr# -> Addr# -> Bool

-- |Reads 8-bit character; offset in bytes.
indexCharOffAddr# :: Addr# -> Int# -> Char#

-- |Reads 31-bit character; offset in 4-byte words.
indexWideCharOffAddr# :: Addr# -> Int# -> Char#
indexIntOffAddr# :: Addr# -> Int# -> Int#
indexWordOffAddr# :: Addr# -> Int# -> Word#
indexAddrOffAddr# :: Addr# -> Int# -> Addr#
indexFloatOffAddr# :: Addr# -> Int# -> Float#
indexDoubleOffAddr# :: Addr# -> Int# -> Double#
indexStablePtrOffAddr# :: Addr# -> Int# -> StablePtr# a
indexInt8OffAddr# :: Addr# -> Int# -> Int#
indexInt16OffAddr# :: Addr# -> Int# -> Int#
indexInt32OffAddr# :: Addr# -> Int# -> Int#
indexInt64OffAddr# :: Addr# -> Int# -> Int#
indexWord8OffAddr# :: Addr# -> Int# -> Word#
indexWord16OffAddr# :: Addr# -> Int# -> Word#
indexWord32OffAddr# :: Addr# -> Int# -> Word#
indexWord64OffAddr# :: Addr# -> Int# -> Word#

-- |Reads 8-bit character; offset in bytes.
readCharOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Char# #)

-- |Reads 31-bit character; offset in 4-byte words.
readWideCharOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Char# #)
readIntOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Int# #)
readWordOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Word# #)
readAddrOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Addr# #)
readFloatOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Float# #)
readDoubleOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Double# #)
readStablePtrOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,StablePtr# a #)
readInt8OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Int# #)
readInt16OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Int# #)
readInt32OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Int# #)
readInt64OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Int# #)
readWord8OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Word# #)
readWord16OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Word# #)
readWord32OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Word# #)
readWord64OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Word# #)
writeCharOffAddr# :: Addr# -> Int# -> Char# -> State# s -> State# s
writeWideCharOffAddr# :: Addr# -> Int# -> Char# -> State# s -> State# s
writeIntOffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
writeWordOffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
writeAddrOffAddr# :: Addr# -> Int# -> Addr# -> State# s -> State# s
writeFloatOffAddr# :: Addr# -> Int# -> Float# -> State# s -> State# s
writeDoubleOffAddr# :: Addr# -> Int# -> Double# -> State# s -> State# s
writeStablePtrOffAddr# :: Addr# -> Int# -> StablePtr# a -> State# s -> State# s
writeInt8OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
writeInt16OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
writeInt32OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
writeInt64OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
writeWord8OffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
writeWord16OffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
writeWord32OffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
writeWord64OffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s


-- |A @MutVar\#@ behaves like a single-element mutable array.
data MutVar# s a

-- |Create @MutVar\#@ with specified initial value in specified state thread.
newMutVar# :: a -> State# s -> (# State# s,MutVar# s a #)

-- |Read contents of @MutVar\#@. Result is not yet evaluated.
readMutVar# :: MutVar# s a -> State# s -> (# State# s,a #)

-- |Write contents of @MutVar\#@.
writeMutVar# :: MutVar# s a -> a -> State# s -> State# s
sameMutVar# :: MutVar# s a -> MutVar# s a -> Bool
atomicModifyMutVar# :: MutVar# s a -> (a -> b) -> State# s -> (# State# s,c #)

catch# :: (State# (RealWorld) -> (# State# (RealWorld),a #)) -> (b -> State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),a #)
raise# :: a -> b
raiseIO# :: a -> State# (RealWorld) -> (# State# (RealWorld),b #)
blockAsyncExceptions# :: (State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),a #)
unblockAsyncExceptions# :: (State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),a #)

data TVar# s a
atomically# :: (State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),a #)
retry# :: State# (RealWorld) -> (# State# (RealWorld),a #)
catchRetry# :: (State# (RealWorld) -> (# State# (RealWorld),a #)) -> (State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),a #)
catchSTM# :: (State# (RealWorld) -> (# State# (RealWorld),a #)) -> (b -> State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),a #)
check# :: (State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),() #)

-- |Create a new @TVar\#@ holding a specified initial value.
newTVar# :: a -> State# s -> (# State# s,TVar# s a #)

-- |Read contents of @TVar\#@.  Result is not yet evaluated.
readTVar# :: TVar# s a -> State# s -> (# State# s,a #)

-- |Write contents of @TVar\#@.
writeTVar# :: TVar# s a -> a -> State# s -> State# s
sameTVar# :: TVar# s a -> TVar# s a -> Bool


-- | A shared mutable variable (/not/ the same as a @MutVar\#@!).
-- 	(Note: in a non-concurrent implementation, @(MVar\# a)@ can be
-- 	represented by @(MutVar\# (Maybe a))@.) 
data MVar# s a

-- |Create new @MVar\#@; initially empty.
newMVar# :: State# s -> (# State# s,MVar# s a #)

-- |If @MVar\#@ is empty, block until it becomes full.
--    Then remove and return its contents, and set it empty.
takeMVar# :: MVar# s a -> State# s -> (# State# s,a #)

-- |If @MVar\#@ is empty, immediately return with integer 0 and value undefined.
--    Otherwise, return with integer 1 and contents of @MVar\#@, and set @MVar\#@ empty.
tryTakeMVar# :: MVar# s a -> State# s -> (# State# s,Int#,a #)

-- |If @MVar\#@ is full, block until it becomes empty.
--    Then store value arg as its new contents.
putMVar# :: MVar# s a -> a -> State# s -> State# s

-- |If @MVar\#@ is full, immediately return with integer 0.
--     Otherwise, store value arg as @MVar\#@\'s new contents, and return with integer 1.
tryPutMVar# :: MVar# s a -> a -> State# s -> (# State# s,Int# #)
sameMVar# :: MVar# s a -> MVar# s a -> Bool

-- |Return 1 if @MVar\#@ is empty; 0 otherwise.
isEmptyMVar# :: MVar# s a -> State# s -> (# State# s,Int# #)


-- |Sleep specified number of microseconds.
delay# :: Int# -> State# s -> State# s

-- |Block until input is available on specified file descriptor.
waitRead# :: Int# -> State# s -> State# s

-- |Block until output is possible on specified file descriptor.
waitWrite# :: Int# -> State# s -> State# s


-- | @State\#@ is the primitive, unlifted type of states.  It has
-- 	one type parameter, thus @State\# RealWorld@, or @State\# s@,
-- 	where s is a type variable. The only purpose of the type parameter
-- 	is to keep different state threads separate.  It is represented by
-- 	nothing at all. 
data State# s

-- | @RealWorld@ is deeply magical.  It is /primitive/, but it is not
-- 	/unlifted/ (hence @ptrArg@).  We never manipulate values of type
-- 	@RealWorld@; it\'s only used in the type system, to parameterise @State\#@. 
data RealWorld

-- |(In a non-concurrent implementation, this can be a singleton
-- 	type, whose (unique) value is returned by @myThreadId\#@.  The 
-- 	other operations can be omitted.)
data ThreadId#
fork# :: a -> State# (RealWorld) -> (# State# (RealWorld),ThreadId# #)
forkOn# :: Int# -> a -> State# (RealWorld) -> (# State# (RealWorld),ThreadId# #)
killThread# :: ThreadId# -> a -> State# (RealWorld) -> State# (RealWorld)
yield# :: State# (RealWorld) -> State# (RealWorld)
myThreadId# :: State# (RealWorld) -> (# State# (RealWorld),ThreadId# #)
labelThread# :: ThreadId# -> Addr# -> State# (RealWorld) -> State# (RealWorld)
isCurrentThreadBound# :: State# (RealWorld) -> (# State# (RealWorld),Int# #)
noDuplicate# :: State# (RealWorld) -> State# (RealWorld)

data Weak# b
mkWeak# :: o -> b -> c -> State# (RealWorld) -> (# State# (RealWorld),Weak# b #)
deRefWeak# :: Weak# a -> State# (RealWorld) -> (# State# (RealWorld),Int#,a #)
finalizeWeak# :: Weak# a -> State# (RealWorld) -> (# State# (RealWorld),Int#,State# (RealWorld) -> (# State# (RealWorld),() #) #)
touch# :: o -> State# (RealWorld) -> State# (RealWorld)

data StablePtr# a
data StableName# a
makeStablePtr# :: a -> State# (RealWorld) -> (# State# (RealWorld),StablePtr# a #)
deRefStablePtr# :: StablePtr# a -> State# (RealWorld) -> (# State# (RealWorld),a #)
eqStablePtr# :: StablePtr# a -> StablePtr# a -> Int#
makeStableName# :: a -> State# (RealWorld) -> (# State# (RealWorld),StableName# a #)
eqStableName# :: StableName# a -> StableName# a -> Int#
stableNameToInt# :: StableName# a -> Int#

reallyUnsafePtrEquality# :: a -> a -> Int#

par# :: a -> Int#
parGlobal# :: a -> Int# -> Int# -> Int# -> Int# -> b -> Int#
parLocal# :: a -> Int# -> Int# -> Int# -> Int# -> b -> Int#
parAt# :: b -> a -> Int# -> Int# -> Int# -> Int# -> c -> Int#
parAtAbs# :: a -> Int# -> Int# -> Int# -> Int# -> Int# -> b -> Int#
parAtRel# :: a -> Int# -> Int# -> Int# -> Int# -> Int# -> b -> Int#
parAtForNow# :: b -> a -> Int# -> Int# -> Int# -> Int# -> c -> Int#

dataToTag# :: a -> Int#
tagToEnum# :: Int# -> a


-- |Primitive bytecode type.
data BCO#

-- |Convert an @Addr\#@ to a followable type.
addrToHValue# :: Addr# -> (# a #)
mkApUpd0# :: BCO# -> (# a #)
newBCO# :: ByteArr# -> ByteArr# -> Array# a -> Int# -> ByteArr# -> State# s -> (# State# s,BCO# #)
unpackClosure# :: a -> (# Addr#,Array# b,ByteArr# #)
getApStackVal# :: a -> Int# -> (# Int#,b #)


-- | Evaluates its first argument to head normal form, and then returns its second
-- 	argument as the result. 
seq :: a -> b -> b

-- | The call @(inline f)@ arranges that f is inlined, regardless of its size.
-- 	More precisely, the call @(inline f)@ rewrites to the right-hand side of
-- 	@f@\'s definition. This allows the programmer to control inlining from a
-- 	particular call site rather than the definition site of the function (c.f.
-- 	@INLINE@ pragmas in User\'s Guide, Section 7.10.3, \"INLINE and NOINLINE
-- 	pragmas\").
-- 
-- 	This inlining occurs regardless of the argument to the call or the size of
-- 	@f@\'s definition; it is unconditional. The main caveat is that @f@\'s
-- 	definition must be visible to the compiler. That is, @f@ must be
-- 	@let@-bound in the current scope. If no inlining takes place, the
-- 	@inline@ function expands to the identity function in Phase zero; so its
-- 	use imposes no overhead.
-- 
-- 	If the function is defined in another module, GHC only exposes its inlining
-- 	in the interface file if the function is sufficiently small that it might be
-- 	inlined by the automatic mechanism. There is currently no way to tell GHC to
-- 	expose arbitrarily-large functions in the interface file. (This shortcoming
-- 	is something that could be fixed, with some kind of pragma.) 
inline :: a -> a

-- | The @lazy@ function restrains strictness analysis a little. The call
-- 	@(lazy e)@ means the same as @e@, but @lazy@ has a magical
-- 	property so far as strictness analysis is concerned: it is lazy in its first
-- 	argument, even though its semantics is strict. After strictness analysis has
-- 	run, calls to @lazy@ are inlined to be the identity function.
-- 
-- 	This behaviour is occasionally useful when controlling evaluation order.
-- 	Notably, @lazy@ is used in the library definition of @Control.Parallel.par@:
-- 
-- 	@par :: a -> b -> b@
-- 
-- 	@par x y = case (par\# x) of \_ -> lazy y@
-- 
-- 	If @lazy@ were not lazy, @par@ would look strict in @y@ which
-- 	would defeat the whole purpose of @par@.
-- 
-- 	Like @seq@, the argument of @lazy@ can have an unboxed type. 
lazy :: a -> a

-- | The type constructor @Any@ is type to which you can unsafely coerce any
-- 	lifted type, and back. 
-- 
-- 	  * It is lifted, and hence represented by a pointer
-- 
-- 	  * It does not claim to be a /data/ type, and that\'s important for
-- 	    the code generator, because the code gen may /enter/ a data value
-- 	    but never enters a function value.  
-- 
-- 	It\'s also used to instantiate un-constrained type variables after type
-- 	checking.  For example
-- 
-- 	@length Any []@
-- 
-- 	Annoyingly, we sometimes need @Any@s of other kinds, such as @(* -> *)@ etc.
-- 	This is a bit like tuples.   We define a couple of useful ones here,
-- 	and make others up on the fly.  If any of these others end up being exported
-- 	into interface files, we\'ll get a crash; at least until we add interface-file
-- 	syntax to support them. 
data Any a

-- | The function @unsafeCoerce\#@ allows you to side-step the typechecker entirely. That
-- 	is, it allows you to coerce any type into any other type. If you use this function,
-- 	you had better get it right, otherwise segmentation faults await. It is generally
-- 	used when you want to write a program that you know is well-typed, but where Haskell\'s
-- 	type system is not expressive enough to prove that it is well typed.
-- 
--         The following uses of @unsafeCoerce\#@ are supposed to work (i.e. not lead to
--         spurious compile-time or run-time crashes):
-- 
--          * Casting any lifted type to @Any@
-- 
--          * Casting @Any@ back to the real type
-- 
--          * Casting an unboxed type to another unboxed type of the same size
-- 
--          * Casting between two types that have the same runtime representation.  One case is when
--            the two types differ only in \"phantom\" type parameters, for example
--            @Ptr Int@ to @Ptr Float@, or @[Int]@ to @[Float]@ when the list is 
--            known to be empty.  Also, a @newtype@ of a type @T@ has the same representation
--            at runtime as @T@.
-- 
--         Other uses of @unsafeCoerce\#@ are undefined.
--         
unsafeCoerce# :: a -> b



