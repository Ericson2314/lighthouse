AC_INIT([Haskell regex-posix package], [0.71], [TextRegexLazy@personal.mightyreason.com], [regex-posix])

AC_CONFIG_SRCDIR([regex-posix.cabal])
AC_CONFIG_HEADERS([include/HsRegexPosixConfig.h])

AC_ARG_WITH([cc],
            [C compiler],
            [CC=$withval])
AC_PROG_CC()

dnl ** check for POSIX regex
HavePosixRegex=NO
AC_CHECK_HEADERS([regex.h], [AC_CHECK_FUNCS(regcomp, 
   [HavePosixRegex=YES;
    AC_DEFINE(HAVE_POSIX_REGEX, [1], [Define to 1 if you have a POSIX regex library installed])])])
AC_SUBST(HavePosixRegex)

if test "$HavePosixRegex" = YES
then
  C_SOURCES=
else
  C_SOURCES="cbits/reallocf.c cbits/regerror.c cbits/regexec.c cbits/regcomp.c cbits/regfree.c"
fi
AC_SUBST(C_SOURCES)

AC_CONFIG_FILES([config.mk regex-posix.buildinfo])

AC_OUTPUT
