# -----------------------------------------------------------------------------
# Main compiler Makefile

# Targets:
#
#	all	builds stage1 compiler
#
#	boot stage=N   generate build dirs and dependencies for stage N.
#		       NB. Must be done before 'make stageN'.
#		       NB. Cannot 'make boot stage=2' until stage1 has
#		           been built (similarly for stage3).
#
#	stage1  (or stage=1) builds stage1 compiler
#	stage2  (or stage=2) builds stage2 compiler
#	stage3  (or stage=3) builds stage3 compiler
#

TOP = ..

# Use GHC for compiling C bits (NB. must be before boilerplate include)
#
UseGhcForCc = YES

include $(TOP)/mk/boilerplate.mk

ifeq "$(GhcThreaded)$(GhcProfiled)" "YESYES"
$(error Cannot make GHC both threaded and profiled)
endif


#-----------------------------------------------------------------------------
# Counting source code lines

USER_SRCS = $(filter-out $(DERIVED_SRCS),$(SRCS))
count :
	./count_lines $(USER_SRCS)

# -----------------------------------------------------------------------------
# Bootstrapping

# The stage1/stage2/stage3 business is quite delicate.  Here's how it works:
# 
#  - the variable $(stage) holds the current stage number.  To build a 
#    particular stage, you say 'make stage=N' where N is 1, 2, or 3.
#    N defaults to 1.
#
#  - for stage N, object files and .hi files are placed inside 
#    the directory stageN, in subdirectories as per the sources.
#
#  - .hi-boot files are *linked* into the stageN tree, because in GHC 5.05+
#    the .hi-boot file must reside in the same place as the .hi file.
#
#  - we use explicit -o and -ohi options to direct the output from C & 
#    Haskell compilations.
#
#  - we generate a different .depend file for each build.  They need to be
#    different, because each stage might include different files: stage1
#    might not include GHCi, for example.  For each stage, a normal .depend
#    file is generated, and then post-processed to add the correct stageN/
#    prefix to each object and .hi filename.  The resulting .depend file
#    is named .depend-$(stage).  See the end of this Makefile for details.
#
#  - normal implicit rules don't work any more, because they're of the form
#
#        %.o : %.hs 
#
#    whereas we really need 
#
#	 stageN/%.o : %.hs
#
#    so suffix.mk now defines the appropriate suffix rules when
#    $(odir) is set to a non-empty value.  Here we set $(odir) to
#    stage1, stage2, or stage3.
#
#  There are other plausible designs that might work, but each has different
#  problems:
#
#  - using -odir and -hidir: GHC <= 4.08 doesn't support -hidir, and
#    anyway -odir puts all the objects in one directory (strips off the
#    subdirectory part), which eventually forces us to use VPATH to find
#    the sources.  I have a really bad feeling about VPATH.
#
#  - invoke make in the stageN subdirectory.  This probably requires VPATH
#    too.
#
#  - create a link tree.  The problem with requiring link trees is that 
#    Windows doesn't support symbolic links.

ifeq "$(stage)" ""
stage=1
endif

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
ifeq "$(stage)" "1"
DQ = \"
else
DQ = \\\"
endif
else
DQ = \"
endif

.DUMMY: stage_dir
stage_dirs :
	$(MKDIRHIER) stage$(stage)
	for i in $(ALL_DIRS); do \
	    $(MKDIRHIER) stage$(stage)/$$i; \
	done

ifeq "$(stage) $(ghc_ge_603)" "1 YES"
UsingHsBoot = YES
else
ifneq "$(findstring $(stage), 2 3)" ""
UsingHsBoot = YES
else
UsingHsBoot = NO
endif
endif

boot :: stage_dirs
# On Windows, we can't use symbolic links for the -hi-boot files
# because GHC itself is a Mingw program and does not understand
# symbolic links.  So we have to copy the files instead of link them.
# That means that if you modify a .hi-boot file in Windows, you
# have to to say 'make boot' again.
#
# PS: 'ln -s foo baz' takes 'foo' relative to the path to 'baz'
#     whereas 'cp foo baz' treats the two paths independently.
#     Hence the "../.." in the ln command line
ifeq "$(UsingHsBoot)" "NO"
ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
	for i in */*hi-boot*; do \
	    cp -u -f $$i stage$(stage)/$$i; \
	done
else
	for i in */*hi-boot*; do \
	    ($(RM) -f stage$(stage)/$$i \
	       && $(LN_S) ../../$$i stage$(stage)/$$i) || true ; \
	done
endif
endif

ifeq "$(stage)" "1"
HC=$(GHC)
endif

ifeq "$(stage)" "2"
HC=$(GHC_STAGE1)
endif

ifeq "$(stage)" "3"
HC=$(GHC_STAGE2)
endif

stage1 ::
	$(MAKE) stage=1

stage2 ::
	$(MAKE) stage=2

stage3 ::
	$(MAKE) stage=3

odir=stage$(stage)

SRC_HC_OPTS += $(patsubst %, -i$(odir)/%, $(ALL_DIRS))

SRC_HC_OPTS += -Wall -fno-warn-name-shadowing
# Turn off orphan warnings, but only if the flag exists (i.e. not if we
# are building stage 1 and using GHC < 6.3).
ifneq "$(stage) $(ghc_ge_603)" "1 NO"
SRC_HC_OPTS += -fno-warn-orphans
endif

HS_OBJS = $(patsubst %, $(odir)/%, $(addsuffix .$(way_)o,$(basename $(HS_SRCS))))
C_OBJS = $(patsubst %, $(odir)/%, $(addsuffix .$(way_)o,$(basename $(C_SRCS))))

# Our standard cleaning rules don't know that we're doing our output
# into $(odir), so we have to augment CLEAN_FILES appropriateliy.

CLEAN_FILES += $(odir)/*/*.hi $(odir)/*/*.hi-boot $(odir)/*/*.o-boot

ifeq "$(UsingHsBoot)" "YES"
CLEAN_FILES += $(odir)/*/*.hi-boot $(odir)/*/*.o-boot
endif

ifeq "$(stage)" "1"
mostlyclean clean distclean maintainer-clean ::
	$(MAKE) $@ stage=2
	$(MAKE) $@ stage=3
endif

# -----------------------------------------------------------------------------
#		Set HS_PROG

# Note: there have been reports of people running up against the ARG_MAX limit
# when linking ghc with all its constituent object files. The likely source of 
# the problem is that the environment is a bit too big, so a workaround could
# be to do `env PATH=$(PATH) make ghc' to minimise the environment. (or the
# equivalent of `env' if it doesn't exist locally).
#
ifneq "$(way)" "dll"
ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
GHC_PROG=$(odir)/ghc$(_way)-$(ProjectVersion)
else
GHC_PROG=$(odir)/ghc$(_way)
endif
else
GHC_PROG=$(odir)/ghc-$(ProjectVersion)
endif

ifeq "$(stage)" "1"
HS_PROG = $(GHC_PROG)
endif

# -----------------------------------------------------------------------------
# Create compiler configuration
#
# The 'echo' commands simply spit the values of various make variables
# into Config.hs, whence they can be compiled and used by GHC itself

CONFIG_HS 	= main/Config.hs
boot :: $(CONFIG_HS)

$(CONFIG_HS) : $(FPTOOLS_TOP)/mk/config.mk
	@$(RM) -f $(CONFIG_HS)
	@echo "Creating $(CONFIG_HS) ... "
	@echo "{-# OPTIONS -w #-}" >>$(CONFIG_HS)
	@echo "module Config where" >>$(CONFIG_HS)
	@echo "cProjectName          = \"$(ProjectName)\"" >> $(CONFIG_HS)
	@echo "cProjectVersion       = \"$(ProjectVersion)\"" >> $(CONFIG_HS)
	@echo "cProjectVersionInt    = \"$(ProjectVersionInt)\"" >> $(CONFIG_HS)
	@echo "cProjectPatchLevel    = \"$(ProjectPatchLevel)\"" >> $(CONFIG_HS)
	@echo "cBooterVersion        = \"$(GhcVersion)\"" >> $(CONFIG_HS)
	@echo "cStage                = STAGE" >> $(CONFIG_HS)
	@echo "cHscIfaceFileVersion  = \"$(HscIfaceFileVersion)\"" >> $(CONFIG_HS)
	@echo "cSplitObjs            = \"$(SplitObjs)\"" >> $(CONFIG_HS)
	@echo "cGhcWithInterpreter   = \"$(GhcWithInterpreter)\"" >> $(CONFIG_HS)
	@echo "cGhcWithNativeCodeGen = \"$(GhcWithNativeCodeGen)\"" >> $(CONFIG_HS)
	@echo "cGhcWithSMP           = \"$(GhcWithSMP)\"" >> $(CONFIG_HS)
	@echo "cGhcRTSWays           = \"$(GhcRTSWays)\"" >> $(CONFIG_HS)
	@echo "cGhcUnregisterised    = \"$(GhcUnregisterised)\"" >> $(CONFIG_HS)
	@echo "cGhcEnableTablesNextToCode = \"$(GhcEnableTablesNextToCode)\"" >> $(CONFIG_HS)
	@echo "cLeadingUnderscore    = \"$(LeadingUnderscore)\"" >> $(CONFIG_HS)
	@echo "cRAWCPP_FLAGS         = \"$(RAWCPP_FLAGS)\"" >> $(CONFIG_HS)
	@echo "cGCC                  = \"$(WhatGccIsCalled)\"" >> $(CONFIG_HS)
	@echo "cMKDLL                = \"$(BLD_DLL)\"" >> $(CONFIG_HS)
	@echo "cLdIsGNULd            = \"$(LdIsGNULd)\"" >> $(CONFIG_HS)
	@echo "cLD_X		     = \"$(LD_X)\"" >> $(CONFIG_HS)
	@echo "cPROJECT_DIR          = \"$(PROJECT_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_DRIVER_DIR_REL   = \"$(GHC_DRIVER_DIR_REL)\"" >> $(CONFIG_HS)
	@echo "cGHC_TOUCHY_PGM       = \"$(GHC_TOUCHY_PGM)\"" >> $(CONFIG_HS)
	@echo "cGHC_TOUCHY_DIR_REL   = \"$(GHC_TOUCHY_DIR_REL)\"" >> $(CONFIG_HS)
	@echo "cGHC_UNLIT_PGM        = \"$(GHC_UNLIT_PGM)\"" >> $(CONFIG_HS)
	@echo "cGHC_UNLIT_DIR_REL    = \"$(GHC_UNLIT_DIR_REL)\"" >> $(CONFIG_HS)
	@echo "cGHC_MANGLER_PGM      = \"$(GHC_MANGLER_PGM)\"" >> $(CONFIG_HS)
	@echo "cGHC_MANGLER_DIR_REL  = \"$(GHC_MANGLER_DIR_REL)\"" >> $(CONFIG_HS)
	@echo "cGHC_SPLIT_PGM        = \"$(GHC_SPLIT_PGM)\"" >> $(CONFIG_HS)
	@echo "cGHC_SPLIT_DIR_REL    = \"$(GHC_SPLIT_DIR_REL)\"" >> $(CONFIG_HS)
	@echo "cGHC_SYSMAN_PGM       = \"$(GHC_SYSMAN)\"" >> $(CONFIG_HS)
	@echo "cGHC_SYSMAN_DIR_REL   = \"$(GHC_SYSMAN_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_CP               = \"$(GHC_CP)\"" >> $(CONFIG_HS)
	@echo "cGHC_PERL             = \"$(GHC_PERL)\"" >> $(CONFIG_HS)
ifeq ($(GhcWithIlx),YES)
	@echo "cILX2IL               = \"$(ILX2IL)\"" >> $(CONFIG_HS)
	@echo "cILASM                = \"$(ILASM)\"" >> $(CONFIG_HS)
endif
	@echo "cEnableWin32DLLs      = \"$(EnableWin32DLLs)\"" >> $(CONFIG_HS)
	@echo "cCONTEXT_DIFF         = \"$(CONTEXT_DIFF)\"" >> $(CONFIG_HS)
	@echo "cUSER_WAY_NAMES       = \"$(USER_WAY_NAMES)\"" >> $(CONFIG_HS)
	@echo "cUSER_WAY_OPTS        = \"$(USER_WAY_OPTS)\"" >> $(CONFIG_HS)
	@echo "cDEFAULT_TMPDIR       = \"$(DEFAULT_TMPDIR)\"" >> $(CONFIG_HS)
ifeq "$(RelocatableBuild)" "YES"
	@echo "cRelocatableBuild     = True"                  >> $(CONFIG_HS)
else
	@echo "cRelocatableBuild     = False"                 >> $(CONFIG_HS)
endif
	@echo done.

CLEAN_FILES += $(CONFIG_HS)

# -----------------------------------------------------------------------------
# Create platform includes

# Here we generate a little header file containing CPP symbols that GHC
# uses to determine which platform it is building on/for.  The platforms
# can differ between stage1 and stage2 if we're cross-compiling, so we
# need one of these header files per stage.

PLATFORM_H = ghc_boot_platform.h

stage1/$(PLATFORM_H) : stage_dirs $(FPTOOLS_TOP)/mk/config.mk
	@echo "Creating $@..."
	@$(RM) $@
	@echo "#ifndef __PLATFORM_H__"  >$@
	@echo "#define __PLATFORM_H__" >>$@
	@echo >> $@
	@echo "#define BuildPlatform_NAME  \"$(BUILDPLATFORM)\"" >> $@
	@echo "#define HostPlatform_NAME   \"$(HOSTPLATFORM)\"" >> $@
	@echo "#define TargetPlatform_NAME \"$(TARGETPLATFORM)\"" >> $@
	@echo >> $@
	@echo "#define $(BuildPlatform_CPP)_BUILD  	1" >> $@
	@echo "#define $(HostPlatform_CPP)_HOST		1" >> $@
	@echo "#define $(TargetPlatform_CPP)_TARGET	1" >> $@
	@echo >> $@
	@echo "#define $(BuildArch_CPP)_BUILD_ARCH  	1" >> $@
	@echo "#define $(HostArch_CPP)_HOST_ARCH	1" >> $@
	@echo "#define $(TargetArch_CPP)_TARGET_ARCH	1" >> $@
	@echo "#define BUILD_ARCH \"$(BuildArch_CPP)\"" >> $@
	@echo "#define HOST_ARCH \"$(HostArch_CPP)\"" >> $@
	@echo "#define TARGET_ARCH \"$(TargetArch_CPP)\"" >> $@
	@echo >> $@
	@echo "#define $(BuildOS_CPP)_BUILD_OS 		1" >> $@
	@echo "#define $(HostOS_CPP)_HOST_OS		1" >> $@
	@echo "#define $(TargetOS_CPP)_TARGET_OS	1" >> $@  
	@echo "#define BUILD_OS \"$(BuildOS_CPP)\"" >> $@
	@echo "#define HOST_OS \"$(HostOS_CPP)\"" >> $@
	@echo "#define TARGET_OS \"$(TargetOS_CPP)\"" >> $@
ifeq "$(HostOS_CPP)" "irix"
	@echo "#ifndef $(IRIX_MAJOR)_TARGET_OS		 " >> $@  
	@echo "#define $(IRIX_MAJOR)_TARGET_OS		1" >> $@  
	@echo "#endif					 " >> $@  
endif
	@echo >> $@
	@echo "#define $(BuildVendor_CPP)_BUILD_VENDOR 	1" >> $@
	@echo "#define $(HostVendor_CPP)_HOST_VENDOR	1" >> $@
	@echo "#define $(TargetVendor_CPP)_TARGET_VENDOR  1" >> $@
	@echo "#define BUILD_VENDOR \"$(BuildVendor_CPP)\"" >> $@
	@echo "#define HOST_VENDOR \"$(HostVendor_CPP)\"" >> $@
	@echo "#define TARGET_VENDOR \"$(TargetVendor_CPP)\"" >> $@
	@echo >> $@
	@echo "#endif /* __PLATFORM_H__ */"          >> $@
	@echo "Done."

# For stage2 and above, the BUILD platform is the HOST of stage1, and
# the HOST platform is the TARGET of stage1.  The TARGET remains the same
# (stage1 is the cross-compiler, not stage2).
stage2/$(PLATFORM_H) : stage_dirs $(FPTOOLS_TOP)/mk/config.mk
	@echo "Creating $@..."
	@$(RM) $@
	@echo "#ifndef __PLATFORM_H__"  >$@
	@echo "#define __PLATFORM_H__" >>$@
	@echo >> $@
	@echo "#define BuildPlatform_NAME  \"$(HOSTPLATFORM)\"" >> $@
	@echo "#define HostPlatform_NAME   \"$(TARGETPLATFORM)\"" >> $@
	@echo "#define TargetPlatform_NAME \"$(TARGETPLATFORM)\"" >> $@
	@echo >> $@
	@echo "#define $(HostPlatform_CPP)_BUILD  	1" >> $@
	@echo "#define $(TargetPlatform_CPP)_HOST		1" >> $@
	@echo "#define $(TargetPlatform_CPP)_TARGET	1" >> $@
	@echo >> $@
	@echo "#define $(HostArch_CPP)_BUILD_ARCH  	1" >> $@
	@echo "#define $(TargetArch_CPP)_HOST_ARCH	1" >> $@
	@echo "#define $(TargetArch_CPP)_TARGET_ARCH	1" >> $@
	@echo "#define BUILD_ARCH \"$(HostArch_CPP)\"" >> $@
	@echo "#define HOST_ARCH \"$(TargetArch_CPP)\"" >> $@
	@echo "#define TARGET_ARCH \"$(TargetArch_CPP)\"" >> $@
	@echo >> $@
	@echo "#define $(HostOS_CPP)_BUILD_OS 		1" >> $@
	@echo "#define $(TargetOS_CPP)_HOST_OS		1" >> $@
	@echo "#define $(TargetOS_CPP)_TARGET_OS	1" >> $@  
	@echo "#define BUILD_OS \"$(HostOS_CPP)\"" >> $@
	@echo "#define HOST_OS \"$(TargetOS_CPP)\"" >> $@
	@echo "#define TARGET_OS \"$(TargetOS_CPP)\"" >> $@
ifeq "$(HostOS_CPP)" "irix"
	@echo "#ifndef $(IRIX_MAJOR)_TARGET_OS		 " >> $@  
	@echo "#define $(IRIX_MAJOR)_TARGET_OS		1" >> $@  
	@echo "#endif					 " >> $@  
endif
	@echo >> $@
	@echo "#define $(HostVendor_CPP)_BUILD_VENDOR 	1" >> $@
	@echo "#define $(TargetVendor_CPP)_HOST_VENDOR	1" >> $@
	@echo "#define $(TargetVendor_CPP)_TARGET_VENDOR  1" >> $@
	@echo "#define BUILD_VENDOR \"$(HostVendor_CPP)\"" >> $@
	@echo "#define HOST_VENDOR \"$(TargetVendor_CPP)\"" >> $@
	@echo "#define TARGET_VENDOR \"$(TargetVendor_CPP)\"" >> $@
	@echo >> $@
	@echo "#endif /* __PLATFORM_H__ */"          >> $@
	@echo "Done."

stage3/$(PLATFORM_H) : stage_dirs stage2/$(PLATFORM_H)
	$(CP) stage2/$(PLATFORM_H) stage3/$(PLATFORM_H)

STAGE_PLATFORM_H = stage$(stage)/$(PLATFORM_H)

boot :: $(STAGE_PLATFORM_H)

SRC_HC_OPTS += -Istage$(stage)

# -----------------------------------------------------------------------------
# Set SRCS etc.
#
# First figure out ALL_DIRS, the source sub-directories

ALL_DIRS = \
  utils basicTypes types hsSyn prelude rename typecheck deSugar coreSyn \
  vectorise specialise simplCore stranal stgSyn simplStg codeGen main \
  profiling parser cprAnalysis ndpFlatten iface cmm

# Make sure we include Config.hs even if it doesn't exist yet...
ALL_SRCS += $(CONFIG_HS)

# HsGeneric.hs is not used just now
EXCLUDED_SRCS += hsSyn/HsGeneric.hs

ifeq ($(GhcWithNativeCodeGen),YES)
ALL_DIRS += nativeGen
else
SRC_HC_OPTS += -DOMIT_NATIVE_CODEGEN
endif

ifeq ($(GhcWithIlx),YES)
ALL_DIRS += ilxGen
SRC_HC_OPTS += -DILX
endif

ifeq ($(GhcWithJavaGen),YES)
ALL_DIRS += javaGen
SRC_HC_OPTS += -DJAVA
endif

ifeq "$(BootingFromHc)" "YES"
# HC files are always from a self-booted compiler
bootstrapped = YES
else
ifneq "$(findstring $(stage), 2 3)" ""
bootstrapped = YES
else
bootstrapped = NO
endif
endif

# -----------------------------------------------------------------------------
# Building a compiler with interpreter support
#
# The interpreter, GHCi interface, Template Haskell and Hpc are only
# enabled when we are bootstrapping with the same version of GHC, and
# the interpreter is supported on this platform.

ifeq "$(bootstrapped)" "YES"
SRC_HC_OPTS += -package hpc -package bytestring
PKG_DEPENDS += hpc bytestring
endif

ifeq "$(GhcWithInterpreter) $(bootstrapped)" "YES YES"

# Yes, include the interepreter and Template Haskell extensions
SRC_HC_OPTS += -DGHCI -package template-haskell
PKG_DEPENDS += template-haskell

# Should GHCI be building info tables in the TABLES_NEXT_TO_CODE style
# or not?
ifeq "$(GhcEnableTablesNextToCode) $(GhcUnregisterised)" "YES NO"
SRC_HC_OPTS += -DGHCI_TABLES_NEXT_TO_CODE
endif

ifeq "$(GhcThreaded)" "YES"
# Use threaded RTS with GHCi, so threads don't get blocked at the prompt.
SRC_LD_OPTS += -threaded
endif
ifeq "$(GhcProfiled)" "YES"
SRC_LD_OPTS += -prof
endif
ifeq "$(GhcDebugged)" "YES"
SRC_LD_OPTS += -debug
endif

ALL_DIRS += ghci

# If we are going to use dynamic libraries instead of .o files for ghci,
# we will need to always retain CAFs in the compiler.
# ghci/keepCAFsForGHCi contains a GNU C __attribute__((constructor))
# function which sets the keepCAFs flag for the RTS before any Haskell
# code is run.
ifeq "$(GhcBuildDylibs)" "YES"
else
EXCLUDED_SRCS += ghci/keepCAFsForGHCi.c
endif

# Enable readline if either:
#   - we're building stage 1 and $(GhcHasReadline)="YES"
#   - we're building stage 2/3, and we have built the readline package
#
# But we don't enable readline on Windows, because readline is fairly
# broken there.
#
ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
ifeq "$(stage)" "1"
ifeq "$(GhcHasReadline)" "YES"
SRC_HC_OPTS += -package readline -DUSE_READLINE
PKG_DEPENDS += readline
endif
else
ifeq "$(wildcard $(FPTOOLS_TOP_ABS)/libraries/readline/unbuildable)" ""
SRC_HC_OPTS += -package readline -DUSE_READLINE
PKG_DEPENDS += readline
endif
endif # stage=1
endif # not windows

else

# No interpreter, so exclude Template Haskell modules
EXCLUDED_SRCS += deSugar/DsMeta.hs typecheck/TcSplice.lhs hsSyn/Convert.lhs

endif # bootstrapped with interpreter

# -----------------------------------------------
# mkdependC stuff
#
# Big Fudge to get around inherent problem that Makefile setup
# has got with 'mkdependC'.
# 
SRC_MKDEPENDC_OPTS += -D__GLASGOW_HASKELL__=$(ProjectVersionInt)

# XXX not really correct, hschooks.c actually gets include files like
# RtsFlags.c from the installed GHC, but we can't tell mkdependC about that.
SRC_MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)

# -----------------------------------------------------------------------------
# 		Haskell compilations

SRC_HC_OPTS += \
  -cpp -fglasgow-exts -fno-generics -Rghc-timing \
  -I. -Iparser

# Omitted: 	-I$(GHC_INCLUDE_DIR)
# We should have -I$(GHC_INCLUDE_DIR) in SRC_HC_OPTS, 
# to avoid the use of an explicit path in GHC source files
#	(include "../includes/config.h"
# But alas GHC 4.08 (and others for all I know) uses this very
# same include path when compiling the .hc files it generates.
# Disaster!  Then the hc file sees the GHC 5.02 (or whatever)
# include files.   For the moment we've reverted to using
# an explicit path in the .hs sources
#
# For the benefit of <5.00 compilers, do include GHC_INCLUDE_DIR
# when generating dependencies. (=> it gets passed onto mkdependHS,
# which needs it).
SRC_MKDEPENDHS_OPTS += -I$(GHC_INCLUDE_DIR)

# We need System.Posix (or Posix when ghc < 6.2)
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
ifeq "$(bootstrapped)" "YES"
SRC_HC_OPTS += -package Win32
PKG_DEPENDS += Win32
endif
else
ifeq "$(bootstrapped) $(ghc_ge_601)" "NO NO"
SRC_HC_OPTS += -package posix
else
SRC_HC_OPTS += -package unix
PKG_DEPENDS += unix
endif
endif

# We use the Cabal package in stages 2/3 only; in stage 1 we're using
# the libcompat library which provides the Cabal modules.
ifneq "$(stage)" "1"
SRC_HC_OPTS += -package Cabal
PKG_DEPENDS += Cabal
endif

ifeq "$(ghc_ge_603)" "YES"
# Ignore lang, to avoid potential clash with the Generics module if
# lang happens to be a dependency of some exposed package in the local
# GHC installation (eg. wxHaskell did this around 6.4).
SRC_HC_OPTS += -ignore-package lang
endif

SRC_CC_OPTS += -Iparser -I. -O
SRC_HC_OPTS += -recomp $(GhcHcOpts) $(GhcStage$(stage)HcOpts)
SRC_HC_OPTS += -H16M

ifeq "$(BootingFromHc)" "YES"
SRC_CC_OPTS += -D__GLASGOW_HASKELL__=$(ProjectVersionInt)
endif

# 	Special flags for particular modules
#	The standard suffix rule for compiling a Haskell file
#	adds these flags to the command line

# There used to be a -no-recomp flag on PrimOp, but why?
# It's an expensive module to recompile!
prelude/PrimOp_HC_OPTS 		= -H80m


main/ParsePkgConf_HC_OPTS	+= -fno-warn-incomplete-patterns
parser/Parser_HC_OPTS 		+= -fno-warn-incomplete-patterns

ifeq "$(ghc_ge_603)" "NO"
# Use -fvia-C since the NCG can't handle the narrow16Int# (and intToInt16#?)
# primops on all platforms.
parser/Parser_HC_OPTS		+= -fvia-C
# because the NCG can't handle the 64-bit math in here
prelude/PrelRules_HC_OPTS	+= -fvia-C
# ByteCodeItbls uses primops that the NCG doesn't support.
ghci/ByteCodeItbls_HC_OPTS	+= -fvia-C
ghci/ByteCodeLink_HC_OPTS	+= -fvia-C -monly-3-regs
endif

# Careful optimisation of the parser: we don't want to throw everything
# at it, because that takes too long and doesn't buy much, but we do want
# to inline certain key external functions, so we instruct GHC not to
# throw away inlinings as it would normally do in -Onot mode:
parser/Parser_HC_OPTS		+= -Onot -fno-ignore-interface-pragmas

ifeq "$(HOSTPLATFORM)" "hppa1.1-hp-hpux9"
rename/RnMonad_HC_OPTS 		=  -O2 -O2-for-C
endif

utils/Digraph_HC_OPTS		= -fglasgow-exts 

basicTypes/SrcLoc_HC_OPTS	= -funbox-strict-fields

# We always optimise some low-level modules, otherwise performance of
# a non-optimised compiler is severely affected.
main/BinIface_HC_OPTS		+= -O
utils/Binary_HC_OPTS		+= -O -funbox-strict-fields
utils/FastMutInt_HC_OPTS	+= -O
utils/Encoding_HC_OPTS		+= -O
utils/StringBuffer_HC_OPTS	+= -O -funbox-strict-fields
utils/FastString_HC_OPTS	+= -O -funbox-strict-fields

# ---- Profiling ----
#simplCore/Simplify_HC_OPTS = -auto-all
#simplCore/SimplEnv_HC_OPTS = -auto-all
#simplCore/SimplUtils_HC_OPTS = -auto-all

# CSE interacts badly with top-level IORefs (reportedly in DriverState and
# DriverMkDepend), causing some of them to be commoned up.  We have a fix for
# this in 5.00+, but earlier versions of the compiler will need CSE turned off.
# To be on the safe side, we disable CSE in *all* modules with top-level IORefs.
ghci/InteractiveUI_HC_OPTS	= -fno-cse
main/CmdLineOpts_HC_OPTS	= -fno-cse
main/DriverMkDepend_HC_OPTS	= -fno-cse
main/DriverPipeline_HC_OPTS	= -fno-cse
main/Finder_HC_OPTS		= -fno-cse
main/SysTools_HC_OPTS		= -fno-cse
main/StaticFlags_HC_OPTS	= -fno-cse

# The #include is vital for the via-C route, else the C
# compiler doesn't realise that the stcall foreign imports are indeed
# stdcall, and doesn't generate the Foo@8 name for them
ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
main/SysTools_HC_OPTS		+= '-\#include <windows.h>' '-\#include <process.h>'
endif

parser/Lexer_HC_OPTS += -funbox-strict-fields

# ghc_strlen percolates through so many modules that it is easier to get its
# prototype via a global option instead of a myriad of per-file OPTIONS
SRC_HC_OPTS += '-\#include "cutils.h"'

# ----------------------------------------------------------------------------
#		Generate supporting stuff for prelude/PrimOp.lhs 
#		from prelude/primops.txt

PRIMOP_BITS=primop-data-decl.hs-incl \
            primop-tag.hs-incl  \
            primop-list.hs-incl  \
            primop-has-side-effects.hs-incl  \
            primop-out-of-line.hs-incl  \
            primop-commutable.hs-incl  \
            primop-needs-wrapper.hs-incl  \
            primop-can-fail.hs-incl  \
            primop-strictness.hs-incl  \
            primop-primop-info.hs-incl

CLEAN_FILES += prelude/primops.txt
CLEAN_FILES += $(PRIMOP_BITS)

SRC_CPP_OPTS += -I$(GHC_INCLUDE_DIR)
SRC_CPP_OPTS += ${GhcCppOpts}

ifneq "$(BootingFromHc)" "YES"
prelude/PrimOp.lhs $(odir)/prelude/PrimOp.o: $(PRIMOP_BITS)
endif

ifneq "$(BootingFromHc)" "YES"
depend :: $(PRIMOP_BITS)
endif

# This is an ugly hack: we need stage1/$(PLATFORM_H) built before we
# preprocess primops.txt.pp, but we don't want to just add that
# dependency because we don't want $(PLATFORM_H) built during normal
# operations, because we don't have have dependencies from the .hs
# sources on it, and we don't want those dependencies because that
# would cause everything to be rebuilt every time the Makefile
# changed.  So here we add the required dependency only when making
# boot or depend:
ifneq "$(findstring boot, $(MAKECMDGOALS))$(findstring depend, $(MAKECMDGOALS))" ""
prelude/primops.txt.pp : stage1/$(PLATFORM_H)
endif

primop-data-decl.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --data-decl          < $< > $@
primop-tag.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --primop-tag         < $< > $@
primop-list.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --primop-list        < $< > $@
primop-has-side-effects.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --has-side-effects   < $< > $@
primop-out-of-line.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --out-of-line        < $< > $@
primop-commutable.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --commutable         < $< > $@
primop-needs-wrapper.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --needs-wrapper      < $< > $@
primop-can-fail.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --can-fail           < $< > $@
primop-strictness.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --strictness         < $< > $@
primop-primop-info.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --primop-primop-info < $< > $@

# Usages aren't used any more; but the generator 
# can still generate them if we want them back
primop-usage.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --usage              < $< > $@


#-----------------------------------------------------------------------------
# 		Linking

# Include libghccompat in stage1 only.  In stage2 onwards, all these
# libraries will be available from the main libraries.

ifeq "$(stage)" "1"
include $(GHC_COMPAT_DIR)/compat.mk
endif

SRC_LD_OPTS += -no-link-chk

# -----------------------------------------------------------------------------
# create ghc-inplace, a convenient way to run ghc from the build tree...
# See comments in $(FPTOOLS_TOP)/utils/ghc-pkg/Makefile for why we use
# a real binary here rather than a shell script.

INPLACE_SRC  = $(odir)/ghc-inplace.c
INPLACE_PROG = $(odir)/ghc-inplace$(exeext)
EXCLUDED_C_SRCS += ghc-inplace.c

CLEAN_FILES += $(INPLACE_SRC)

GHC_PATH=$(FPTOOLS_TOP_ABS)/$(GHC_COMPILER_DIR_REL)/$(GHC_PROG)$(exeext)

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
INPLACE_EXTRA_FLAGS = -optc-DWINDOWS
endif

$(INPLACE_SRC): ghc-inplace.c
	$(SED) -e "s@GHC_PATH@$(GHC_PATH)@g" -e "s@TOP_ABS@$(FPTOOLS_TOP_ABS)@g" < $< > $@

$(INPLACE_PROG): $(INPLACE_SRC)
	$(HC) -cpp $(INPLACE_EXTRA_FLAGS) $< -o $@

all :: $(INPLACE_PROG)

CLEAN_FILES += $(INPLACE_PROG)

ifeq "$(stage)" "1"
ghc-inplace : $(INPLACE_PROG)
	$(RM) -f $@ && $(LN_S) $< $@

all :: ghc-inplace

CLEAN_FILES += ghc-inplace
endif

#-----------------------------------------------------------------------------
# 		install

# We don't want ghc treated as an ordinary executable,
# but put it together with the libraries.
# Also don't want any interface files installed

DESTDIR = $(INSTALL_LIBRARY_DIR_GHC)

ifneq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
INSTALL_LIBEXECS += $(GHC_PROG)
else
INSTALL_PROGS += $(GHC_PROG)
endif

# ----------------------------------------------------------------------------
# profiling.

# rename/RnBinds_HC_OPTS += -auto-all
# rename/RnEnv_HC_OPTS += -auto-all
# rename/RnExpr_HC_OPTS += -auto-all
# rename/RnHiFiles_HC_OPTS += -auto-all
# rename/RnHsSyn_HC_OPTS += -auto-all
# rename/Rename_HC_OPTS += -auto-all
# rename/RnIfaces_HC_OPTS += -auto-all
# rename/RnNames_HC_OPTS += -auto-all
# rename/RnSource_HC_OPTS += -auto-all
# rename/RnTypes_HC_OPTS += -auto-all
# 
# typecheck/Inst_HC_OPTS += -auto-all
# typecheck/TcBinds_HC_OPTS += -auto-all
# typecheck/TcClassDcl_HC_OPTS += -auto-all
# typecheck/TcDefaults_HC_OPTS += -auto-all
# typecheck/TcDeriv_HC_OPTS += -auto-all
# typecheck/TcEnv_HC_OPTS += -auto-all
# typecheck/TcExpr_HC_OPTS += -auto-all
# typecheck/TcForeign_HC_OPTS += -auto-all
# typecheck/TcGenDeriv_HC_OPTS += -auto-all
# typecheck/TcHsSyn_HC_OPTS += -auto-all
# typecheck/TcIfaceSig_HC_OPTS += -auto-all
# typecheck/TcInstDcls_HC_OPTS += -auto-all
# typecheck/TcMatches_HC_OPTS += -auto-all
# typecheck/TcMonoType_HC_OPTS += -auto-all
# typecheck/TcMType_HC_OPTS += -auto-all
# typecheck/TcPat_HC_OPTS += -auto-all
# typecheck/TcRnDriver_HC_OPTS += -auto-all
# #typecheck/TcRnMonad_HC_OPTS += -auto-all
# #typecheck/TcRnTypes_HC_OPTS += -auto-all
# typecheck/TcRules_HC_OPTS += -auto-all
# typecheck/TcSimplify_HC_OPTS += -auto-all
# typecheck/TcSplice_HC_OPTS += -auto-all
# typecheck/TcTyClsDecls_HC_OPTS += -auto-all
# typecheck/TcTyDecls_HC_OPTS += -auto-all
# typecheck/TcType_HC_OPTS += -auto-all
# typecheck/TcUnify_HC_OPTS += -auto-all

# coreSyn/CorePrep_HC_OPTS += -auto-all
# parser/Parser_HC_OPTS += -fasm

#-----------------------------------------------------------------------------
# Building the GHC package

# The GHC package is made from the stage 2 build and later.
# Fortunately the package build system framework more or less does the
# right thing for us here.

ifneq "$(findstring $(stage), 2 3)" ""
BUILD_GHC_PACKAGE=YES
endif

ifeq "$(BUILD_GHC_PACKAGE)" "YES"
PACKAGE = ghc
HIERARCHICAL_LIB = NO
VERSION = $(ProjectVersion)
PKG_DEPENDS += base haskell98
PACKAGE_CPP_OPTS += -DPKG_DEPENDS='$(PKG_DEPENDS)'
PACKAGE_CPP_OPTS += -DSTAGE='"$(stage)"'

# Omit Main from the library, the client will want to plug their own Main in
LIBOBJS = $(filter-out $(odir)/main/Main.o   $(odir)/parser/hschooks.o     \
                       $(odir)/main/Main.p_o $(odir)/parser/hschooks.p_o,  \
                       $(OBJS))

# disable splitting: it won't really help with GHC, and the specialised
# build system for compiler/ isn't set up to handle it.
SplitObjs = NO

# the package build system likes to set WAYS=$(GhcLibWays), but we don't 
# really want to build the whole of GHC multiple ways... if you do,
# set GhcCompilerWays instead.
GhcLibWays = $(GhcCompilerWays)

# override $(GhcLibHcOpts): we want GhcStage2HcOpts to take precedence
GhcLibHcOpts =

ifeq "$(DOING_BIN_DIST)" "YES"
# This is derived from the sources when we are in a source tree, but we
# don't have any sources in a bindist, so we have to shortcut it
HS_IFACES := $(wildcard stage$(stage)/*/*.hi)
else
# override default definition of HS_IFACES so we can add $(odir)
HS_IFACES   = $(addsuffix .$(way_)hi,$(basename $(HS_OBJS)))
endif

# Haddock can't handle recursive modules currently, so we disable it for now.
override HADDOCK_DOCS = NO

# Tell package.mk not to set $(HC)
NO_SET_HC = YES

# The stage 2 GHC binary itself is built by  compiling main/Main.hs 
# (the same as used in stage 1) against the GHC package.
#
# This is done by compiling Main.hs separately and linking it with 
# -package ghc.  This is done using a separate Makefile, Makefile.ghcbin
# Why? See comments in Makefile.ghcbin

# The stage 2 and stage 3 package.conf.in files are different, because they
# point to either the stage2/ or stage3/ dirs in import-dirs.  Hence before
# linking the ghc binary we must install the correct version of the package
# configuration.  Yeuch... maybe one day this will all be done more cleanly.
STAMP_PKG_CONF = $(GHC_DRIVER_DIR)/stamp-pkg-conf-$(PACKAGE)

ifeq "$(GhcBuildDylibs)" "YES"
ifeq "$(darwin_TARGET_OS)" "1"
GhcLibraryName=libHS$(PACKAGE)$(_way)_dyn.dylib
else
GhcLibraryName=libHS$(PACKAGE)$(_way)_dyn.so
endif
else
GhcLibraryName=libHS$(PACKAGE)$(_way).a
endif

ifneq "$(DOING_BIN_DIST)" "YES"
$(GHC_PROG) : $(GhcLibraryName) main/Main.hs
	$(RM) package.conf.inplace
	$(RM) $(STAMP_PKG_CONF)
	$(MAKE) $(STAMP_PKG_CONF)
	$(MAKE) -f Makefile.ghcbin $(MFLAGS) HS_PROG=$(GHC_PROG) $@
endif

# Propagate standard targets to Makefile.ghcbin
docs runtests $(BOOT_TARGET) TAGS clean distclean mostlyclean maintainer-clean $(INSTALL_TARGET) $(INSTALL_DOCS_TARGET) html chm HxS ps dvi txt::
	$(MAKE) -f Makefile.ghcbin $(MFLAGS) $@
endif

#-----------------------------------------------------------------------------
# 		clean

MAINTAINER_CLEAN_FILES += parser/Parser.info main/ParsePkgConf.info

#-----------------------------------------------------------------------------
# 		Include target-rule boilerplate

# Don't use the default MKDEPENDHS stuff... we'll do our own, below
MKDEPENDHS_SRCS =
MKDEPENDC_SRCS =

# Make doesn't work this out for itself, it seems
parser/Parser.y : parser/Parser.y.pp
EXTRA_SRCS += parser/Parser.y


#-----------------------------------------------------------------------------
# 		Source files for tags file generation
#
# We want to excluded derived sources, because they won't be in the source
# tree, which is where we are going to move the TAGS file to.a

TAGS_HS_SRCS = parser/Parser.y.pp $(filter-out $(DERIVED_SRCS) main/Config.hs parser/Parser.y, $(sort $(SRCS)))

include $(TOP)/mk/target.mk

ifeq "$(BUILD_GHC_PACKAGE)" "YES"
ifeq "$(way)" ""
all :: $(GHC_PROG)
endif
endif

$(odir)/main/Config.$(way_)o: SRC_HC_OPTS+=-DSTAGE=$(DQ)$(stage)$(DQ)

#-----------------------------------------------------------------------------
# binary-dist

binary-dist:
	$(INSTALL_DIR)                         $(BIN_DIST_DIR)/compiler
	$(INSTALL_DIR)                         $(BIN_DIST_DIR)/compiler/stage$(stage)
	echo "stage=$(stage)"                > $(BIN_DIST_DIR)/compiler/Makefile
	cat Makefile                        >> $(BIN_DIST_DIR)/compiler/Makefile
	$(INSTALL_DATA)    package.conf.in     $(BIN_DIST_DIR)/compiler/
	set -e; for d in stage$(stage)/*/; do $(INSTALL_DIR) $(BIN_DIST_DIR)/compiler/$$d; done
	set -e; for f in $(HS_IFACES); do $(INSTALL_DATA) $$f $(BIN_DIST_DIR)/compiler/$$f; done
ifneq "$(INSTALL_LIBS)" ""
	set -e; for f in $(INSTALL_LIBS); do $(INSTALL_DATA) $$f $(BIN_DIST_DIR)/compiler/$$f; done
endif
ifneq "$(INSTALL_PROGS)" ""
	set -e; for f in $(INSTALL_PROGS); do $(INSTALL_PROGRAM) $$f $(BIN_DIST_DIR)/compiler/$$f; done
endif
ifneq "$(INSTALL_LIBEXECS)" ""
	set -e; for f in $(INSTALL_LIBEXECS); do $(INSTALL_PROGRAM) $$f $(BIN_DIST_DIR)/compiler/$$f; done
endif

# -----------------------------------------------------------------------------
# Explicit dependencies

# Some .hs files #include other source files, but since ghc -M doesn't spit out
# these dependencies we have to include them manually.

# We don't add dependencies on HsVersions.h, ghcautoconf.h, or ghc_boot_platform.h,
# because then modifying one of these files would force recompilation of everything,
# which is probably not what you want.  However, it does mean you have to be
# careful to recompile stuff you need if you reconfigure or change HsVersions.h.

# Aargh, these don't work properly anyway, because GHC's recompilation checker
# just reports "compilation NOT required".  Do we have to add -no-recomp for each
# of these .hs files?  I haven't done anything about this yet.

$(odir)/codeGen/Bitmap.$(way_)o		:  ../includes/MachDeps.h
$(odir)/codeGen/CgCallConv.$(way_)o	:  ../includes/StgFun.h
$(odir)/codeGen/CgProf.$(way_)o		:  ../includes/MachDeps.h
$(odir)/codeGen/CgProf.$(way_)o		:  ../includes/Constants.h
$(odir)/codeGen/CgProf.$(way_)o		:  ../includes/DerivedConstants.h
$(odir)/codeGen/CgTicky.$(way_)o	:  ../includes/DerivedConstants.h
$(odir)/codeGen/ClosureInfo.$(way_)o	:  ../includes/MachDeps.h
$(odir)/codeGen/SMRep.$(way_)o		:  ../includes/MachDeps.h
$(odir)/codeGen/SMRep.$(way_)o		:  ../includes/ClosureTypes.h
$(odir)/ghci/ByteCodeAsm.$(way_)o	:  ../includes/Bytecodes.h
$(odir)/ghci/ByteCodeFFI.$(way_)o	:  nativeGen/NCG.h
$(odir)/ghci/ByteCodeInstr.$(way_)o	:  ../includes/MachDeps.h
$(odir)/ghci/ByteCodeItbls.$(way_)o	:  ../includes/ClosureTypes.h
$(odir)/ghci/ByteCodeItbls.$(way_)o	:  nativeGen/NCG.h
$(odir)/main/Constants.$(way_)o		:  ../includes/MachRegs.h
$(odir)/main/Constants.$(way_)o		:  ../includes/Constants.h
$(odir)/main/Constants.$(way_)o		:  ../includes/MachDeps.h
$(odir)/main/Constants.$(way_)o		:  ../includes/DerivedConstants.h
$(odir)/main/Constants.$(way_)o		:  ../includes/GHCConstants.h
$(odir)/nativeGen/AsmCodeGen.$(way_)o 	:  nativeGen/NCG.h
$(odir)/nativeGen/MachCodeGen.$(way_)o 	:  nativeGen/NCG.h
$(odir)/nativeGen/MachCodeGen.$(way_)o 	: ../includes/MachDeps.h
$(odir)/nativeGen/MachInstrs.$(way_)o	:  nativeGen/NCG.h
$(odir)/nativeGen/MachRegs.$(way_)o	:  nativeGen/NCG.h
$(odir)/nativeGen/MachRegs.$(way_)o	:  ../includes/MachRegs.h
$(odir)/nativeGen/PositionIndependentCode.$(way_)o :  nativeGen/NCG.h
$(odir)/nativeGen/PprMach.$(way_)o	:  nativeGen/NCG.h
$(odir)/nativeGen/RegAllocInfo.$(way_)o :  nativeGen/NCG.h
$(odir)/typecheck/TcForeign.$(way_)o	:  nativeGen/NCG.h
$(odir)/utils/Binary.$(way_)o		:  ../includes/MachDeps.h
$(odir)/utils/FastMutInt.$(way_)o	:  ../includes/MachDeps.h

# -----------------------------------------------------------------------------
# Dependencies

MKDEPENDHS_HC_OPTS = $(patsubst -i$(odir)/%, -i%, $(HC_OPTS))

MKDEPENDHS=$(HC)

# Must do this *after* including target.mk, because $(HS_SRCS) isn't set yet.
depend :: $(STAGE_PLATFORM_H) $(HS_SRCS) $(C_SRCS)
	touch .depend-BASE
ifneq "$(BootingFromHc)" "YES"
	$(MKDEPENDHS) -M -optdep-f -optdep.depend-BASE $(foreach way,$(WAYS),-optdep-s -optdep$(way)) $(foreach obj,$(MKDEPENDHS_OBJ_SUFFICES),-osuf $(obj)) $(MKDEPENDHS_OPTS) $(filter-out -split-objs, $(MKDEPENDHS_HC_OPTS)) $(HS_SRCS)
endif
	$(MKDEPENDC) -f .depend-BASE $(MKDEPENDC_OPTS) $(foreach way,$(WAYS),-s $(way)) -- $(CC_OPTS) -- $(C_SRCS) 
	$(PERL) -pe 'binmode(stdin); binmode(stdout); s@(\S*[._]o)@stage$(stage)/$$1@g; s@(\S*[._]hi)@stage$(stage)/$$1@g; s@^.*/compat.*$$@@g;' <.depend-BASE >.depend-$(stage)
# The binmode stuff tells perl not to add stupid ^M's to the output
#
# The /lib/compat replacement is to workaround a bug in the
# -optdep--exclude-module flag in GHC 6.4.  It is not required for any
# other version of GHC, but doesn't do any harm.

ifeq "$(MakefileDeps)" "YES"
$(CONFIG_HS) : Makefile
stage1/$(PLATFORM_H) : Makefile
stage2/$(PLATFORM_H) : Makefile
endif

-include .depend-$(stage)
