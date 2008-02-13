# FP_ARG_READLINE
# -------------
AC_DEFUN([FP_ARG_READLINE],
[AC_ARG_ENABLE([readline],
  [AC_HELP_STRING([--enable-readline],
    [build a Haskell binding for readline.
     (default=autodetect)])],
  [enable_readline=$enableval],
  [enable_readline=yes])

AC_ARG_WITH([readline-includes],
  [AC_HELP_STRING([--with-readline-includes],
    [directory containing readline/readline.h])],
    [readline_includes=$withval],
    [readline_includes=NONE])

AC_ARG_WITH([readline-libraries],
  [AC_HELP_STRING([--with-readline-libraries],
    [directory containing readline library])],
    [readline_libraries=$withval],
    [readline_libraries=NONE])
])# FP_ARG_READLINE

AC_DEFUN([CHECK_READLINE],
[AC_REQUIRE([AC_PROG_CPP])
AC_REQUIRE([AC_PROG_CC])

])
