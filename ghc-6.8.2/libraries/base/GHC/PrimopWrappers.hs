{-# OPTIONS -fno-implicit-prelude #-}
module GHC.PrimopWrappers where
import qualified GHC.Prim
{-# NOINLINE gtChar# #-}
gtChar# a1 a2 = (GHC.Prim.gtChar#) a1 a2
{-# NOINLINE geChar# #-}
geChar# a1 a2 = (GHC.Prim.geChar#) a1 a2
{-# NOINLINE eqChar# #-}
eqChar# a1 a2 = (GHC.Prim.eqChar#) a1 a2
{-# NOINLINE neChar# #-}
neChar# a1 a2 = (GHC.Prim.neChar#) a1 a2
{-# NOINLINE ltChar# #-}
ltChar# a1 a2 = (GHC.Prim.ltChar#) a1 a2
{-# NOINLINE leChar# #-}
leChar# a1 a2 = (GHC.Prim.leChar#) a1 a2
{-# NOINLINE ord# #-}
ord# a1 = (GHC.Prim.ord#) a1
{-# NOINLINE (+#) #-}
(+#) a1 a2 = (GHC.Prim.+#) a1 a2
{-# NOINLINE (-#) #-}
(-#) a1 a2 = (GHC.Prim.-#) a1 a2
{-# NOINLINE (*#) #-}
(*#) a1 a2 = (GHC.Prim.*#) a1 a2
{-# NOINLINE mulIntMayOflo# #-}
mulIntMayOflo# a1 a2 = (GHC.Prim.mulIntMayOflo#) a1 a2
{-# NOINLINE quotInt# #-}
quotInt# a1 a2 = (GHC.Prim.quotInt#) a1 a2
{-# NOINLINE remInt# #-}
remInt# a1 a2 = (GHC.Prim.remInt#) a1 a2
{-# NOINLINE gcdInt# #-}
gcdInt# a1 a2 = (GHC.Prim.gcdInt#) a1 a2
{-# NOINLINE negateInt# #-}
negateInt# a1 = (GHC.Prim.negateInt#) a1
{-# NOINLINE addIntC# #-}
addIntC# a1 a2 = (GHC.Prim.addIntC#) a1 a2
{-# NOINLINE subIntC# #-}
subIntC# a1 a2 = (GHC.Prim.subIntC#) a1 a2
{-# NOINLINE (>#) #-}
(>#) a1 a2 = (GHC.Prim.>#) a1 a2
{-# NOINLINE (>=#) #-}
(>=#) a1 a2 = (GHC.Prim.>=#) a1 a2
{-# NOINLINE (==#) #-}
(==#) a1 a2 = (GHC.Prim.==#) a1 a2
{-# NOINLINE (/=#) #-}
(/=#) a1 a2 = (GHC.Prim./=#) a1 a2
{-# NOINLINE (<#) #-}
(<#) a1 a2 = (GHC.Prim.<#) a1 a2
{-# NOINLINE (<=#) #-}
(<=#) a1 a2 = (GHC.Prim.<=#) a1 a2
{-# NOINLINE chr# #-}
chr# a1 = (GHC.Prim.chr#) a1
{-# NOINLINE int2Word# #-}
int2Word# a1 = (GHC.Prim.int2Word#) a1
{-# NOINLINE int2Float# #-}
int2Float# a1 = (GHC.Prim.int2Float#) a1
{-# NOINLINE int2Double# #-}
int2Double# a1 = (GHC.Prim.int2Double#) a1
{-# NOINLINE int2Integer# #-}
int2Integer# a1 = (GHC.Prim.int2Integer#) a1
{-# NOINLINE uncheckedIShiftL# #-}
uncheckedIShiftL# a1 a2 = (GHC.Prim.uncheckedIShiftL#) a1 a2
{-# NOINLINE uncheckedIShiftRA# #-}
uncheckedIShiftRA# a1 a2 = (GHC.Prim.uncheckedIShiftRA#) a1 a2
{-# NOINLINE uncheckedIShiftRL# #-}
uncheckedIShiftRL# a1 a2 = (GHC.Prim.uncheckedIShiftRL#) a1 a2
{-# NOINLINE plusWord# #-}
plusWord# a1 a2 = (GHC.Prim.plusWord#) a1 a2
{-# NOINLINE minusWord# #-}
minusWord# a1 a2 = (GHC.Prim.minusWord#) a1 a2
{-# NOINLINE timesWord# #-}
timesWord# a1 a2 = (GHC.Prim.timesWord#) a1 a2
{-# NOINLINE quotWord# #-}
quotWord# a1 a2 = (GHC.Prim.quotWord#) a1 a2
{-# NOINLINE remWord# #-}
remWord# a1 a2 = (GHC.Prim.remWord#) a1 a2
{-# NOINLINE and# #-}
and# a1 a2 = (GHC.Prim.and#) a1 a2
{-# NOINLINE or# #-}
or# a1 a2 = (GHC.Prim.or#) a1 a2
{-# NOINLINE xor# #-}
xor# a1 a2 = (GHC.Prim.xor#) a1 a2
{-# NOINLINE not# #-}
not# a1 = (GHC.Prim.not#) a1
{-# NOINLINE uncheckedShiftL# #-}
uncheckedShiftL# a1 a2 = (GHC.Prim.uncheckedShiftL#) a1 a2
{-# NOINLINE uncheckedShiftRL# #-}
uncheckedShiftRL# a1 a2 = (GHC.Prim.uncheckedShiftRL#) a1 a2
{-# NOINLINE word2Int# #-}
word2Int# a1 = (GHC.Prim.word2Int#) a1
{-# NOINLINE word2Integer# #-}
word2Integer# a1 = (GHC.Prim.word2Integer#) a1
{-# NOINLINE gtWord# #-}
gtWord# a1 a2 = (GHC.Prim.gtWord#) a1 a2
{-# NOINLINE geWord# #-}
geWord# a1 a2 = (GHC.Prim.geWord#) a1 a2
{-# NOINLINE eqWord# #-}
eqWord# a1 a2 = (GHC.Prim.eqWord#) a1 a2
{-# NOINLINE neWord# #-}
neWord# a1 a2 = (GHC.Prim.neWord#) a1 a2
{-# NOINLINE ltWord# #-}
ltWord# a1 a2 = (GHC.Prim.ltWord#) a1 a2
{-# NOINLINE leWord# #-}
leWord# a1 a2 = (GHC.Prim.leWord#) a1 a2
{-# NOINLINE narrow8Int# #-}
narrow8Int# a1 = (GHC.Prim.narrow8Int#) a1
{-# NOINLINE narrow16Int# #-}
narrow16Int# a1 = (GHC.Prim.narrow16Int#) a1
{-# NOINLINE narrow32Int# #-}
narrow32Int# a1 = (GHC.Prim.narrow32Int#) a1
{-# NOINLINE narrow8Word# #-}
narrow8Word# a1 = (GHC.Prim.narrow8Word#) a1
{-# NOINLINE narrow16Word# #-}
narrow16Word# a1 = (GHC.Prim.narrow16Word#) a1
{-# NOINLINE narrow32Word# #-}
narrow32Word# a1 = (GHC.Prim.narrow32Word#) a1
{-# NOINLINE plusInteger# #-}
plusInteger# a1 a2 a3 a4 = (GHC.Prim.plusInteger#) a1 a2 a3 a4
{-# NOINLINE minusInteger# #-}
minusInteger# a1 a2 a3 a4 = (GHC.Prim.minusInteger#) a1 a2 a3 a4
{-# NOINLINE timesInteger# #-}
timesInteger# a1 a2 a3 a4 = (GHC.Prim.timesInteger#) a1 a2 a3 a4
{-# NOINLINE gcdInteger# #-}
gcdInteger# a1 a2 a3 a4 = (GHC.Prim.gcdInteger#) a1 a2 a3 a4
{-# NOINLINE gcdIntegerInt# #-}
gcdIntegerInt# a1 a2 a3 = (GHC.Prim.gcdIntegerInt#) a1 a2 a3
{-# NOINLINE divExactInteger# #-}
divExactInteger# a1 a2 a3 a4 = (GHC.Prim.divExactInteger#) a1 a2 a3 a4
{-# NOINLINE quotInteger# #-}
quotInteger# a1 a2 a3 a4 = (GHC.Prim.quotInteger#) a1 a2 a3 a4
{-# NOINLINE remInteger# #-}
remInteger# a1 a2 a3 a4 = (GHC.Prim.remInteger#) a1 a2 a3 a4
{-# NOINLINE cmpInteger# #-}
cmpInteger# a1 a2 a3 a4 = (GHC.Prim.cmpInteger#) a1 a2 a3 a4
{-# NOINLINE cmpIntegerInt# #-}
cmpIntegerInt# a1 a2 a3 = (GHC.Prim.cmpIntegerInt#) a1 a2 a3
{-# NOINLINE quotRemInteger# #-}
quotRemInteger# a1 a2 a3 a4 = (GHC.Prim.quotRemInteger#) a1 a2 a3 a4
{-# NOINLINE divModInteger# #-}
divModInteger# a1 a2 a3 a4 = (GHC.Prim.divModInteger#) a1 a2 a3 a4
{-# NOINLINE integer2Int# #-}
integer2Int# a1 a2 = (GHC.Prim.integer2Int#) a1 a2
{-# NOINLINE integer2Word# #-}
integer2Word# a1 a2 = (GHC.Prim.integer2Word#) a1 a2
{-# NOINLINE andInteger# #-}
andInteger# a1 a2 a3 a4 = (GHC.Prim.andInteger#) a1 a2 a3 a4
{-# NOINLINE orInteger# #-}
orInteger# a1 a2 a3 a4 = (GHC.Prim.orInteger#) a1 a2 a3 a4
{-# NOINLINE xorInteger# #-}
xorInteger# a1 a2 a3 a4 = (GHC.Prim.xorInteger#) a1 a2 a3 a4
{-# NOINLINE complementInteger# #-}
complementInteger# a1 a2 = (GHC.Prim.complementInteger#) a1 a2
{-# NOINLINE (>##) #-}
(>##) a1 a2 = (GHC.Prim.>##) a1 a2
{-# NOINLINE (>=##) #-}
(>=##) a1 a2 = (GHC.Prim.>=##) a1 a2
{-# NOINLINE (==##) #-}
(==##) a1 a2 = (GHC.Prim.==##) a1 a2
{-# NOINLINE (/=##) #-}
(/=##) a1 a2 = (GHC.Prim./=##) a1 a2
{-# NOINLINE (<##) #-}
(<##) a1 a2 = (GHC.Prim.<##) a1 a2
{-# NOINLINE (<=##) #-}
(<=##) a1 a2 = (GHC.Prim.<=##) a1 a2
{-# NOINLINE (+##) #-}
(+##) a1 a2 = (GHC.Prim.+##) a1 a2
{-# NOINLINE (-##) #-}
(-##) a1 a2 = (GHC.Prim.-##) a1 a2
{-# NOINLINE (*##) #-}
(*##) a1 a2 = (GHC.Prim.*##) a1 a2
{-# NOINLINE (/##) #-}
(/##) a1 a2 = (GHC.Prim./##) a1 a2
{-# NOINLINE negateDouble# #-}
negateDouble# a1 = (GHC.Prim.negateDouble#) a1
{-# NOINLINE double2Int# #-}
double2Int# a1 = (GHC.Prim.double2Int#) a1
{-# NOINLINE double2Float# #-}
double2Float# a1 = (GHC.Prim.double2Float#) a1
{-# NOINLINE expDouble# #-}
expDouble# a1 = (GHC.Prim.expDouble#) a1
{-# NOINLINE logDouble# #-}
logDouble# a1 = (GHC.Prim.logDouble#) a1
{-# NOINLINE sqrtDouble# #-}
sqrtDouble# a1 = (GHC.Prim.sqrtDouble#) a1
{-# NOINLINE sinDouble# #-}
sinDouble# a1 = (GHC.Prim.sinDouble#) a1
{-# NOINLINE cosDouble# #-}
cosDouble# a1 = (GHC.Prim.cosDouble#) a1
{-# NOINLINE tanDouble# #-}
tanDouble# a1 = (GHC.Prim.tanDouble#) a1
{-# NOINLINE asinDouble# #-}
asinDouble# a1 = (GHC.Prim.asinDouble#) a1
{-# NOINLINE acosDouble# #-}
acosDouble# a1 = (GHC.Prim.acosDouble#) a1
{-# NOINLINE atanDouble# #-}
atanDouble# a1 = (GHC.Prim.atanDouble#) a1
{-# NOINLINE sinhDouble# #-}
sinhDouble# a1 = (GHC.Prim.sinhDouble#) a1
{-# NOINLINE coshDouble# #-}
coshDouble# a1 = (GHC.Prim.coshDouble#) a1
{-# NOINLINE tanhDouble# #-}
tanhDouble# a1 = (GHC.Prim.tanhDouble#) a1
{-# NOINLINE (**##) #-}
(**##) a1 a2 = (GHC.Prim.**##) a1 a2
{-# NOINLINE decodeDouble# #-}
decodeDouble# a1 = (GHC.Prim.decodeDouble#) a1
{-# NOINLINE gtFloat# #-}
gtFloat# a1 a2 = (GHC.Prim.gtFloat#) a1 a2
{-# NOINLINE geFloat# #-}
geFloat# a1 a2 = (GHC.Prim.geFloat#) a1 a2
{-# NOINLINE eqFloat# #-}
eqFloat# a1 a2 = (GHC.Prim.eqFloat#) a1 a2
{-# NOINLINE neFloat# #-}
neFloat# a1 a2 = (GHC.Prim.neFloat#) a1 a2
{-# NOINLINE ltFloat# #-}
ltFloat# a1 a2 = (GHC.Prim.ltFloat#) a1 a2
{-# NOINLINE leFloat# #-}
leFloat# a1 a2 = (GHC.Prim.leFloat#) a1 a2
{-# NOINLINE plusFloat# #-}
plusFloat# a1 a2 = (GHC.Prim.plusFloat#) a1 a2
{-# NOINLINE minusFloat# #-}
minusFloat# a1 a2 = (GHC.Prim.minusFloat#) a1 a2
{-# NOINLINE timesFloat# #-}
timesFloat# a1 a2 = (GHC.Prim.timesFloat#) a1 a2
{-# NOINLINE divideFloat# #-}
divideFloat# a1 a2 = (GHC.Prim.divideFloat#) a1 a2
{-# NOINLINE negateFloat# #-}
negateFloat# a1 = (GHC.Prim.negateFloat#) a1
{-# NOINLINE float2Int# #-}
float2Int# a1 = (GHC.Prim.float2Int#) a1
{-# NOINLINE expFloat# #-}
expFloat# a1 = (GHC.Prim.expFloat#) a1
{-# NOINLINE logFloat# #-}
logFloat# a1 = (GHC.Prim.logFloat#) a1
{-# NOINLINE sqrtFloat# #-}
sqrtFloat# a1 = (GHC.Prim.sqrtFloat#) a1
{-# NOINLINE sinFloat# #-}
sinFloat# a1 = (GHC.Prim.sinFloat#) a1
{-# NOINLINE cosFloat# #-}
cosFloat# a1 = (GHC.Prim.cosFloat#) a1
{-# NOINLINE tanFloat# #-}
tanFloat# a1 = (GHC.Prim.tanFloat#) a1
{-# NOINLINE asinFloat# #-}
asinFloat# a1 = (GHC.Prim.asinFloat#) a1
{-# NOINLINE acosFloat# #-}
acosFloat# a1 = (GHC.Prim.acosFloat#) a1
{-# NOINLINE atanFloat# #-}
atanFloat# a1 = (GHC.Prim.atanFloat#) a1
{-# NOINLINE sinhFloat# #-}
sinhFloat# a1 = (GHC.Prim.sinhFloat#) a1
{-# NOINLINE coshFloat# #-}
coshFloat# a1 = (GHC.Prim.coshFloat#) a1
{-# NOINLINE tanhFloat# #-}
tanhFloat# a1 = (GHC.Prim.tanhFloat#) a1
{-# NOINLINE powerFloat# #-}
powerFloat# a1 a2 = (GHC.Prim.powerFloat#) a1 a2
{-# NOINLINE float2Double# #-}
float2Double# a1 = (GHC.Prim.float2Double#) a1
{-# NOINLINE decodeFloat# #-}
decodeFloat# a1 = (GHC.Prim.decodeFloat#) a1
{-# NOINLINE newArray# #-}
newArray# a1 a2 a3 = (GHC.Prim.newArray#) a1 a2 a3
{-# NOINLINE sameMutableArray# #-}
sameMutableArray# a1 a2 = (GHC.Prim.sameMutableArray#) a1 a2
{-# NOINLINE readArray# #-}
readArray# a1 a2 a3 = (GHC.Prim.readArray#) a1 a2 a3
{-# NOINLINE writeArray# #-}
writeArray# a1 a2 a3 a4 = (GHC.Prim.writeArray#) a1 a2 a3 a4
{-# NOINLINE indexArray# #-}
indexArray# a1 a2 = (GHC.Prim.indexArray#) a1 a2
{-# NOINLINE unsafeFreezeArray# #-}
unsafeFreezeArray# a1 a2 = (GHC.Prim.unsafeFreezeArray#) a1 a2
{-# NOINLINE unsafeThawArray# #-}
unsafeThawArray# a1 a2 = (GHC.Prim.unsafeThawArray#) a1 a2
{-# NOINLINE newByteArray# #-}
newByteArray# a1 a2 = (GHC.Prim.newByteArray#) a1 a2
{-# NOINLINE newPinnedByteArray# #-}
newPinnedByteArray# a1 a2 = (GHC.Prim.newPinnedByteArray#) a1 a2
{-# NOINLINE byteArrayContents# #-}
byteArrayContents# a1 = (GHC.Prim.byteArrayContents#) a1
{-# NOINLINE sameMutableByteArray# #-}
sameMutableByteArray# a1 a2 = (GHC.Prim.sameMutableByteArray#) a1 a2
{-# NOINLINE unsafeFreezeByteArray# #-}
unsafeFreezeByteArray# a1 a2 = (GHC.Prim.unsafeFreezeByteArray#) a1 a2
{-# NOINLINE sizeofByteArray# #-}
sizeofByteArray# a1 = (GHC.Prim.sizeofByteArray#) a1
{-# NOINLINE sizeofMutableByteArray# #-}
sizeofMutableByteArray# a1 = (GHC.Prim.sizeofMutableByteArray#) a1
{-# NOINLINE indexCharArray# #-}
indexCharArray# a1 a2 = (GHC.Prim.indexCharArray#) a1 a2
{-# NOINLINE indexWideCharArray# #-}
indexWideCharArray# a1 a2 = (GHC.Prim.indexWideCharArray#) a1 a2
{-# NOINLINE indexIntArray# #-}
indexIntArray# a1 a2 = (GHC.Prim.indexIntArray#) a1 a2
{-# NOINLINE indexWordArray# #-}
indexWordArray# a1 a2 = (GHC.Prim.indexWordArray#) a1 a2
{-# NOINLINE indexAddrArray# #-}
indexAddrArray# a1 a2 = (GHC.Prim.indexAddrArray#) a1 a2
{-# NOINLINE indexFloatArray# #-}
indexFloatArray# a1 a2 = (GHC.Prim.indexFloatArray#) a1 a2
{-# NOINLINE indexDoubleArray# #-}
indexDoubleArray# a1 a2 = (GHC.Prim.indexDoubleArray#) a1 a2
{-# NOINLINE indexStablePtrArray# #-}
indexStablePtrArray# a1 a2 = (GHC.Prim.indexStablePtrArray#) a1 a2
{-# NOINLINE indexInt8Array# #-}
indexInt8Array# a1 a2 = (GHC.Prim.indexInt8Array#) a1 a2
{-# NOINLINE indexInt16Array# #-}
indexInt16Array# a1 a2 = (GHC.Prim.indexInt16Array#) a1 a2
{-# NOINLINE indexInt32Array# #-}
indexInt32Array# a1 a2 = (GHC.Prim.indexInt32Array#) a1 a2
{-# NOINLINE indexInt64Array# #-}
indexInt64Array# a1 a2 = (GHC.Prim.indexInt64Array#) a1 a2
{-# NOINLINE indexWord8Array# #-}
indexWord8Array# a1 a2 = (GHC.Prim.indexWord8Array#) a1 a2
{-# NOINLINE indexWord16Array# #-}
indexWord16Array# a1 a2 = (GHC.Prim.indexWord16Array#) a1 a2
{-# NOINLINE indexWord32Array# #-}
indexWord32Array# a1 a2 = (GHC.Prim.indexWord32Array#) a1 a2
{-# NOINLINE indexWord64Array# #-}
indexWord64Array# a1 a2 = (GHC.Prim.indexWord64Array#) a1 a2
{-# NOINLINE readCharArray# #-}
readCharArray# a1 a2 a3 = (GHC.Prim.readCharArray#) a1 a2 a3
{-# NOINLINE readWideCharArray# #-}
readWideCharArray# a1 a2 a3 = (GHC.Prim.readWideCharArray#) a1 a2 a3
{-# NOINLINE readIntArray# #-}
readIntArray# a1 a2 a3 = (GHC.Prim.readIntArray#) a1 a2 a3
{-# NOINLINE readWordArray# #-}
readWordArray# a1 a2 a3 = (GHC.Prim.readWordArray#) a1 a2 a3
{-# NOINLINE readAddrArray# #-}
readAddrArray# a1 a2 a3 = (GHC.Prim.readAddrArray#) a1 a2 a3
{-# NOINLINE readFloatArray# #-}
readFloatArray# a1 a2 a3 = (GHC.Prim.readFloatArray#) a1 a2 a3
{-# NOINLINE readDoubleArray# #-}
readDoubleArray# a1 a2 a3 = (GHC.Prim.readDoubleArray#) a1 a2 a3
{-# NOINLINE readStablePtrArray# #-}
readStablePtrArray# a1 a2 a3 = (GHC.Prim.readStablePtrArray#) a1 a2 a3
{-# NOINLINE readInt8Array# #-}
readInt8Array# a1 a2 a3 = (GHC.Prim.readInt8Array#) a1 a2 a3
{-# NOINLINE readInt16Array# #-}
readInt16Array# a1 a2 a3 = (GHC.Prim.readInt16Array#) a1 a2 a3
{-# NOINLINE readInt32Array# #-}
readInt32Array# a1 a2 a3 = (GHC.Prim.readInt32Array#) a1 a2 a3
{-# NOINLINE readInt64Array# #-}
readInt64Array# a1 a2 a3 = (GHC.Prim.readInt64Array#) a1 a2 a3
{-# NOINLINE readWord8Array# #-}
readWord8Array# a1 a2 a3 = (GHC.Prim.readWord8Array#) a1 a2 a3
{-# NOINLINE readWord16Array# #-}
readWord16Array# a1 a2 a3 = (GHC.Prim.readWord16Array#) a1 a2 a3
{-# NOINLINE readWord32Array# #-}
readWord32Array# a1 a2 a3 = (GHC.Prim.readWord32Array#) a1 a2 a3
{-# NOINLINE readWord64Array# #-}
readWord64Array# a1 a2 a3 = (GHC.Prim.readWord64Array#) a1 a2 a3
{-# NOINLINE writeCharArray# #-}
writeCharArray# a1 a2 a3 a4 = (GHC.Prim.writeCharArray#) a1 a2 a3 a4
{-# NOINLINE writeWideCharArray# #-}
writeWideCharArray# a1 a2 a3 a4 = (GHC.Prim.writeWideCharArray#) a1 a2 a3 a4
{-# NOINLINE writeIntArray# #-}
writeIntArray# a1 a2 a3 a4 = (GHC.Prim.writeIntArray#) a1 a2 a3 a4
{-# NOINLINE writeWordArray# #-}
writeWordArray# a1 a2 a3 a4 = (GHC.Prim.writeWordArray#) a1 a2 a3 a4
{-# NOINLINE writeAddrArray# #-}
writeAddrArray# a1 a2 a3 a4 = (GHC.Prim.writeAddrArray#) a1 a2 a3 a4
{-# NOINLINE writeFloatArray# #-}
writeFloatArray# a1 a2 a3 a4 = (GHC.Prim.writeFloatArray#) a1 a2 a3 a4
{-# NOINLINE writeDoubleArray# #-}
writeDoubleArray# a1 a2 a3 a4 = (GHC.Prim.writeDoubleArray#) a1 a2 a3 a4
{-# NOINLINE writeStablePtrArray# #-}
writeStablePtrArray# a1 a2 a3 a4 = (GHC.Prim.writeStablePtrArray#) a1 a2 a3 a4
{-# NOINLINE writeInt8Array# #-}
writeInt8Array# a1 a2 a3 a4 = (GHC.Prim.writeInt8Array#) a1 a2 a3 a4
{-# NOINLINE writeInt16Array# #-}
writeInt16Array# a1 a2 a3 a4 = (GHC.Prim.writeInt16Array#) a1 a2 a3 a4
{-# NOINLINE writeInt32Array# #-}
writeInt32Array# a1 a2 a3 a4 = (GHC.Prim.writeInt32Array#) a1 a2 a3 a4
{-# NOINLINE writeInt64Array# #-}
writeInt64Array# a1 a2 a3 a4 = (GHC.Prim.writeInt64Array#) a1 a2 a3 a4
{-# NOINLINE writeWord8Array# #-}
writeWord8Array# a1 a2 a3 a4 = (GHC.Prim.writeWord8Array#) a1 a2 a3 a4
{-# NOINLINE writeWord16Array# #-}
writeWord16Array# a1 a2 a3 a4 = (GHC.Prim.writeWord16Array#) a1 a2 a3 a4
{-# NOINLINE writeWord32Array# #-}
writeWord32Array# a1 a2 a3 a4 = (GHC.Prim.writeWord32Array#) a1 a2 a3 a4
{-# NOINLINE writeWord64Array# #-}
writeWord64Array# a1 a2 a3 a4 = (GHC.Prim.writeWord64Array#) a1 a2 a3 a4
{-# NOINLINE plusAddr# #-}
plusAddr# a1 a2 = (GHC.Prim.plusAddr#) a1 a2
{-# NOINLINE minusAddr# #-}
minusAddr# a1 a2 = (GHC.Prim.minusAddr#) a1 a2
{-# NOINLINE remAddr# #-}
remAddr# a1 a2 = (GHC.Prim.remAddr#) a1 a2
{-# NOINLINE addr2Int# #-}
addr2Int# a1 = (GHC.Prim.addr2Int#) a1
{-# NOINLINE int2Addr# #-}
int2Addr# a1 = (GHC.Prim.int2Addr#) a1
{-# NOINLINE gtAddr# #-}
gtAddr# a1 a2 = (GHC.Prim.gtAddr#) a1 a2
{-# NOINLINE geAddr# #-}
geAddr# a1 a2 = (GHC.Prim.geAddr#) a1 a2
{-# NOINLINE eqAddr# #-}
eqAddr# a1 a2 = (GHC.Prim.eqAddr#) a1 a2
{-# NOINLINE neAddr# #-}
neAddr# a1 a2 = (GHC.Prim.neAddr#) a1 a2
{-# NOINLINE ltAddr# #-}
ltAddr# a1 a2 = (GHC.Prim.ltAddr#) a1 a2
{-# NOINLINE leAddr# #-}
leAddr# a1 a2 = (GHC.Prim.leAddr#) a1 a2
{-# NOINLINE indexCharOffAddr# #-}
indexCharOffAddr# a1 a2 = (GHC.Prim.indexCharOffAddr#) a1 a2
{-# NOINLINE indexWideCharOffAddr# #-}
indexWideCharOffAddr# a1 a2 = (GHC.Prim.indexWideCharOffAddr#) a1 a2
{-# NOINLINE indexIntOffAddr# #-}
indexIntOffAddr# a1 a2 = (GHC.Prim.indexIntOffAddr#) a1 a2
{-# NOINLINE indexWordOffAddr# #-}
indexWordOffAddr# a1 a2 = (GHC.Prim.indexWordOffAddr#) a1 a2
{-# NOINLINE indexAddrOffAddr# #-}
indexAddrOffAddr# a1 a2 = (GHC.Prim.indexAddrOffAddr#) a1 a2
{-# NOINLINE indexFloatOffAddr# #-}
indexFloatOffAddr# a1 a2 = (GHC.Prim.indexFloatOffAddr#) a1 a2
{-# NOINLINE indexDoubleOffAddr# #-}
indexDoubleOffAddr# a1 a2 = (GHC.Prim.indexDoubleOffAddr#) a1 a2
{-# NOINLINE indexStablePtrOffAddr# #-}
indexStablePtrOffAddr# a1 a2 = (GHC.Prim.indexStablePtrOffAddr#) a1 a2
{-# NOINLINE indexInt8OffAddr# #-}
indexInt8OffAddr# a1 a2 = (GHC.Prim.indexInt8OffAddr#) a1 a2
{-# NOINLINE indexInt16OffAddr# #-}
indexInt16OffAddr# a1 a2 = (GHC.Prim.indexInt16OffAddr#) a1 a2
{-# NOINLINE indexInt32OffAddr# #-}
indexInt32OffAddr# a1 a2 = (GHC.Prim.indexInt32OffAddr#) a1 a2
{-# NOINLINE indexInt64OffAddr# #-}
indexInt64OffAddr# a1 a2 = (GHC.Prim.indexInt64OffAddr#) a1 a2
{-# NOINLINE indexWord8OffAddr# #-}
indexWord8OffAddr# a1 a2 = (GHC.Prim.indexWord8OffAddr#) a1 a2
{-# NOINLINE indexWord16OffAddr# #-}
indexWord16OffAddr# a1 a2 = (GHC.Prim.indexWord16OffAddr#) a1 a2
{-# NOINLINE indexWord32OffAddr# #-}
indexWord32OffAddr# a1 a2 = (GHC.Prim.indexWord32OffAddr#) a1 a2
{-# NOINLINE indexWord64OffAddr# #-}
indexWord64OffAddr# a1 a2 = (GHC.Prim.indexWord64OffAddr#) a1 a2
{-# NOINLINE readCharOffAddr# #-}
readCharOffAddr# a1 a2 a3 = (GHC.Prim.readCharOffAddr#) a1 a2 a3
{-# NOINLINE readWideCharOffAddr# #-}
readWideCharOffAddr# a1 a2 a3 = (GHC.Prim.readWideCharOffAddr#) a1 a2 a3
{-# NOINLINE readIntOffAddr# #-}
readIntOffAddr# a1 a2 a3 = (GHC.Prim.readIntOffAddr#) a1 a2 a3
{-# NOINLINE readWordOffAddr# #-}
readWordOffAddr# a1 a2 a3 = (GHC.Prim.readWordOffAddr#) a1 a2 a3
{-# NOINLINE readAddrOffAddr# #-}
readAddrOffAddr# a1 a2 a3 = (GHC.Prim.readAddrOffAddr#) a1 a2 a3
{-# NOINLINE readFloatOffAddr# #-}
readFloatOffAddr# a1 a2 a3 = (GHC.Prim.readFloatOffAddr#) a1 a2 a3
{-# NOINLINE readDoubleOffAddr# #-}
readDoubleOffAddr# a1 a2 a3 = (GHC.Prim.readDoubleOffAddr#) a1 a2 a3
{-# NOINLINE readStablePtrOffAddr# #-}
readStablePtrOffAddr# a1 a2 a3 = (GHC.Prim.readStablePtrOffAddr#) a1 a2 a3
{-# NOINLINE readInt8OffAddr# #-}
readInt8OffAddr# a1 a2 a3 = (GHC.Prim.readInt8OffAddr#) a1 a2 a3
{-# NOINLINE readInt16OffAddr# #-}
readInt16OffAddr# a1 a2 a3 = (GHC.Prim.readInt16OffAddr#) a1 a2 a3
{-# NOINLINE readInt32OffAddr# #-}
readInt32OffAddr# a1 a2 a3 = (GHC.Prim.readInt32OffAddr#) a1 a2 a3
{-# NOINLINE readInt64OffAddr# #-}
readInt64OffAddr# a1 a2 a3 = (GHC.Prim.readInt64OffAddr#) a1 a2 a3
{-# NOINLINE readWord8OffAddr# #-}
readWord8OffAddr# a1 a2 a3 = (GHC.Prim.readWord8OffAddr#) a1 a2 a3
{-# NOINLINE readWord16OffAddr# #-}
readWord16OffAddr# a1 a2 a3 = (GHC.Prim.readWord16OffAddr#) a1 a2 a3
{-# NOINLINE readWord32OffAddr# #-}
readWord32OffAddr# a1 a2 a3 = (GHC.Prim.readWord32OffAddr#) a1 a2 a3
{-# NOINLINE readWord64OffAddr# #-}
readWord64OffAddr# a1 a2 a3 = (GHC.Prim.readWord64OffAddr#) a1 a2 a3
{-# NOINLINE writeCharOffAddr# #-}
writeCharOffAddr# a1 a2 a3 a4 = (GHC.Prim.writeCharOffAddr#) a1 a2 a3 a4
{-# NOINLINE writeWideCharOffAddr# #-}
writeWideCharOffAddr# a1 a2 a3 a4 = (GHC.Prim.writeWideCharOffAddr#) a1 a2 a3 a4
{-# NOINLINE writeIntOffAddr# #-}
writeIntOffAddr# a1 a2 a3 a4 = (GHC.Prim.writeIntOffAddr#) a1 a2 a3 a4
{-# NOINLINE writeWordOffAddr# #-}
writeWordOffAddr# a1 a2 a3 a4 = (GHC.Prim.writeWordOffAddr#) a1 a2 a3 a4
{-# NOINLINE writeAddrOffAddr# #-}
writeAddrOffAddr# a1 a2 a3 a4 = (GHC.Prim.writeAddrOffAddr#) a1 a2 a3 a4
{-# NOINLINE writeFloatOffAddr# #-}
writeFloatOffAddr# a1 a2 a3 a4 = (GHC.Prim.writeFloatOffAddr#) a1 a2 a3 a4
{-# NOINLINE writeDoubleOffAddr# #-}
writeDoubleOffAddr# a1 a2 a3 a4 = (GHC.Prim.writeDoubleOffAddr#) a1 a2 a3 a4
{-# NOINLINE writeStablePtrOffAddr# #-}
writeStablePtrOffAddr# a1 a2 a3 a4 = (GHC.Prim.writeStablePtrOffAddr#) a1 a2 a3 a4
{-# NOINLINE writeInt8OffAddr# #-}
writeInt8OffAddr# a1 a2 a3 a4 = (GHC.Prim.writeInt8OffAddr#) a1 a2 a3 a4
{-# NOINLINE writeInt16OffAddr# #-}
writeInt16OffAddr# a1 a2 a3 a4 = (GHC.Prim.writeInt16OffAddr#) a1 a2 a3 a4
{-# NOINLINE writeInt32OffAddr# #-}
writeInt32OffAddr# a1 a2 a3 a4 = (GHC.Prim.writeInt32OffAddr#) a1 a2 a3 a4
{-# NOINLINE writeInt64OffAddr# #-}
writeInt64OffAddr# a1 a2 a3 a4 = (GHC.Prim.writeInt64OffAddr#) a1 a2 a3 a4
{-# NOINLINE writeWord8OffAddr# #-}
writeWord8OffAddr# a1 a2 a3 a4 = (GHC.Prim.writeWord8OffAddr#) a1 a2 a3 a4
{-# NOINLINE writeWord16OffAddr# #-}
writeWord16OffAddr# a1 a2 a3 a4 = (GHC.Prim.writeWord16OffAddr#) a1 a2 a3 a4
{-# NOINLINE writeWord32OffAddr# #-}
writeWord32OffAddr# a1 a2 a3 a4 = (GHC.Prim.writeWord32OffAddr#) a1 a2 a3 a4
{-# NOINLINE writeWord64OffAddr# #-}
writeWord64OffAddr# a1 a2 a3 a4 = (GHC.Prim.writeWord64OffAddr#) a1 a2 a3 a4
{-# NOINLINE newMutVar# #-}
newMutVar# a1 a2 = (GHC.Prim.newMutVar#) a1 a2
{-# NOINLINE readMutVar# #-}
readMutVar# a1 a2 = (GHC.Prim.readMutVar#) a1 a2
{-# NOINLINE writeMutVar# #-}
writeMutVar# a1 a2 a3 = (GHC.Prim.writeMutVar#) a1 a2 a3
{-# NOINLINE sameMutVar# #-}
sameMutVar# a1 a2 = (GHC.Prim.sameMutVar#) a1 a2
{-# NOINLINE atomicModifyMutVar# #-}
atomicModifyMutVar# a1 a2 a3 = (GHC.Prim.atomicModifyMutVar#) a1 a2 a3
{-# NOINLINE catch# #-}
catch# a1 a2 a3 = (GHC.Prim.catch#) a1 a2 a3
{-# NOINLINE raise# #-}
raise# a1 = (GHC.Prim.raise#) a1
{-# NOINLINE raiseIO# #-}
raiseIO# a1 a2 = (GHC.Prim.raiseIO#) a1 a2
{-# NOINLINE blockAsyncExceptions# #-}
blockAsyncExceptions# a1 a2 = (GHC.Prim.blockAsyncExceptions#) a1 a2
{-# NOINLINE unblockAsyncExceptions# #-}
unblockAsyncExceptions# a1 a2 = (GHC.Prim.unblockAsyncExceptions#) a1 a2
{-# NOINLINE atomically# #-}
atomically# a1 a2 = (GHC.Prim.atomically#) a1 a2
{-# NOINLINE retry# #-}
retry# a1 = (GHC.Prim.retry#) a1
{-# NOINLINE catchRetry# #-}
catchRetry# a1 a2 a3 = (GHC.Prim.catchRetry#) a1 a2 a3
{-# NOINLINE catchSTM# #-}
catchSTM# a1 a2 a3 = (GHC.Prim.catchSTM#) a1 a2 a3
{-# NOINLINE check# #-}
check# a1 a2 = (GHC.Prim.check#) a1 a2
{-# NOINLINE newTVar# #-}
newTVar# a1 a2 = (GHC.Prim.newTVar#) a1 a2
{-# NOINLINE readTVar# #-}
readTVar# a1 a2 = (GHC.Prim.readTVar#) a1 a2
{-# NOINLINE writeTVar# #-}
writeTVar# a1 a2 a3 = (GHC.Prim.writeTVar#) a1 a2 a3
{-# NOINLINE sameTVar# #-}
sameTVar# a1 a2 = (GHC.Prim.sameTVar#) a1 a2
{-# NOINLINE newMVar# #-}
newMVar# a1 = (GHC.Prim.newMVar#) a1
{-# NOINLINE takeMVar# #-}
takeMVar# a1 a2 = (GHC.Prim.takeMVar#) a1 a2
{-# NOINLINE tryTakeMVar# #-}
tryTakeMVar# a1 a2 = (GHC.Prim.tryTakeMVar#) a1 a2
{-# NOINLINE putMVar# #-}
putMVar# a1 a2 a3 = (GHC.Prim.putMVar#) a1 a2 a3
{-# NOINLINE tryPutMVar# #-}
tryPutMVar# a1 a2 a3 = (GHC.Prim.tryPutMVar#) a1 a2 a3
{-# NOINLINE sameMVar# #-}
sameMVar# a1 a2 = (GHC.Prim.sameMVar#) a1 a2
{-# NOINLINE isEmptyMVar# #-}
isEmptyMVar# a1 a2 = (GHC.Prim.isEmptyMVar#) a1 a2
{-# NOINLINE delay# #-}
delay# a1 a2 = (GHC.Prim.delay#) a1 a2
{-# NOINLINE waitRead# #-}
waitRead# a1 a2 = (GHC.Prim.waitRead#) a1 a2
{-# NOINLINE waitWrite# #-}
waitWrite# a1 a2 = (GHC.Prim.waitWrite#) a1 a2
{-# NOINLINE fork# #-}
fork# a1 a2 = (GHC.Prim.fork#) a1 a2
{-# NOINLINE forkOn# #-}
forkOn# a1 a2 a3 = (GHC.Prim.forkOn#) a1 a2 a3
{-# NOINLINE killThread# #-}
killThread# a1 a2 a3 = (GHC.Prim.killThread#) a1 a2 a3
{-# NOINLINE yield# #-}
yield# a1 = (GHC.Prim.yield#) a1
{-# NOINLINE myThreadId# #-}
myThreadId# a1 = (GHC.Prim.myThreadId#) a1
{-# NOINLINE labelThread# #-}
labelThread# a1 a2 a3 = (GHC.Prim.labelThread#) a1 a2 a3
{-# NOINLINE isCurrentThreadBound# #-}
isCurrentThreadBound# a1 = (GHC.Prim.isCurrentThreadBound#) a1
{-# NOINLINE noDuplicate# #-}
noDuplicate# a1 = (GHC.Prim.noDuplicate#) a1
{-# NOINLINE mkWeak# #-}
mkWeak# a1 a2 a3 a4 = (GHC.Prim.mkWeak#) a1 a2 a3 a4
{-# NOINLINE deRefWeak# #-}
deRefWeak# a1 a2 = (GHC.Prim.deRefWeak#) a1 a2
{-# NOINLINE finalizeWeak# #-}
finalizeWeak# a1 a2 = (GHC.Prim.finalizeWeak#) a1 a2
{-# NOINLINE touch# #-}
touch# a1 a2 = (GHC.Prim.touch#) a1 a2
{-# NOINLINE makeStablePtr# #-}
makeStablePtr# a1 a2 = (GHC.Prim.makeStablePtr#) a1 a2
{-# NOINLINE deRefStablePtr# #-}
deRefStablePtr# a1 a2 = (GHC.Prim.deRefStablePtr#) a1 a2
{-# NOINLINE eqStablePtr# #-}
eqStablePtr# a1 a2 = (GHC.Prim.eqStablePtr#) a1 a2
{-# NOINLINE makeStableName# #-}
makeStableName# a1 a2 = (GHC.Prim.makeStableName#) a1 a2
{-# NOINLINE eqStableName# #-}
eqStableName# a1 a2 = (GHC.Prim.eqStableName#) a1 a2
{-# NOINLINE stableNameToInt# #-}
stableNameToInt# a1 = (GHC.Prim.stableNameToInt#) a1
{-# NOINLINE reallyUnsafePtrEquality# #-}
reallyUnsafePtrEquality# a1 a2 = (GHC.Prim.reallyUnsafePtrEquality#) a1 a2
{-# NOINLINE dataToTag# #-}
dataToTag# a1 = (GHC.Prim.dataToTag#) a1
{-# NOINLINE addrToHValue# #-}
addrToHValue# a1 = (GHC.Prim.addrToHValue#) a1
{-# NOINLINE mkApUpd0# #-}
mkApUpd0# a1 = (GHC.Prim.mkApUpd0#) a1
{-# NOINLINE newBCO# #-}
newBCO# a1 a2 a3 a4 a5 a6 = (GHC.Prim.newBCO#) a1 a2 a3 a4 a5 a6
{-# NOINLINE unpackClosure# #-}
unpackClosure# a1 = (GHC.Prim.unpackClosure#) a1
{-# NOINLINE getApStackVal# #-}
getApStackVal# a1 a2 = (GHC.Prim.getApStackVal#) a1 a2
