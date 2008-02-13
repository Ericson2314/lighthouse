AC_INIT([Haskell readline package], [1.0], [libraries@haskell.org], [readline])


FP_ARG_READLINE

if test "$enable_readline" = no; then
  BUILD_PACKAGE_BOOL=False
else

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/HsReadline.h])
 
AC_CONFIG_HEADERS([include/HsReadlineConfig.h])

READLINE_INCLUDE_DIRS=
READLINE_LIB_DIRS=
if test "x$readline_libraries" != "xNONE"; then
  LDFLAGS="-L$readline_libraries $LDFLAGS"
  READLINE_LDFLAGS="-L$readline_libraries $READLINE_LDFLAGS"
  READLINE_LIB_DIRS=$readline_libraries
fi
if test "x$readline_includes" != "xNONE"; then
  CPPFLAGS="-I$readline_includes $CPPFLAGS"
  READLINE_CFLAGS="-I$readline_includes $READLINE_CFLAGS"
  READLINE_INCLUDE_DIRS=$readline_includes
fi
AC_SUBST(READLINE_INCLUDE_DIRS)
AC_SUBST(READLINE_LIB_DIRS)

AC_ARG_WITH([cc],
            [C compiler],
            [CC=$withval])
AC_PROG_CC()

# Mac OS X only: check for GNUreadline.framework (should be harmless on other systems)
# If it's found, we'll prefer it over linking via "-lreadline".

if test "x$readline_libraries" = "xNONE" && test "x$readline_includes" = "xNONE" ; then
  AC_MSG_CHECKING([for GNUreadline.framework])
  save_libs="$LIBS"
  LIBS="-framework GNUreadline"
  AC_CHECK_FUNC(readline, HaveFrameworkReadline=YES, HaveFrameworkReadline=NO)
  LIBS="$save_libs"
fi

if test "x$HaveFrameworkReadline" = xYES ; then
  LIBS="-framework GNUreadline $LIBS"
  READLINE_FRAMEWORKS="GNUreadline"
  AC_DEFINE([HAVE_FRAMEWORK_READLINE], [1], [Define to 1 if we're using GNUreadline.framework.])
else
  dnl ** check for readline, for Hugs and hslibs' Readline
  dnl ncurses supersedes termcap and curses, but for compatibility,
  dnl we have to check for all...
  AC_CHECK_LIB(ncurses, tputs, HaveLibTermcap=YES; LibTermcap=ncurses,
    AC_CHECK_LIB(termcap, tputs, HaveLibTermcap=YES; LibTermcap=termcap,
      AC_CHECK_LIB(curses, tputs, HaveLibTermcap=YES; LibTermcap=curses,
        HaveLibTermcap=NO; LibTermcap=not-installed)))

  if test $HaveLibTermcap = YES ; then
    LIBS="-l$LibTermcap $LIBS"
    AC_CHECK_LIB(readline, readline, HaveLibReadline=YES)
    READLINE_LIBS="readline $LibTermcap"
    LIBS="$LIBS -lreadline"
  fi
fi

dnl ** At this point, $LIB should be completely set, so we can use AC_CHECK_FUNC from now on.

dnl ** Is our readline really readline, or is it BSD's libedit?
dnl ** Second test is necessary on Mac OS X 10.5.
AC_CHECK_FUNC(rl_readline_version, ReadlineIsLibedit=NO, ReadlineIsLibedit=YES)
if test "x$ReadlineIsLibedit" = xNO ; then
  AC_CHECK_FUNC(rl_begin_undo_group, , ReadlineIsLibedit=YES)
fi

if (test "x$HaveLibReadline" = xYES || test "x$HaveFrameworkReadline" = xYES) && test "x$ReadlineIsLibedit" = xNO  ; then
  HAVE_READLINE=YES
else
  AC_MSG_FAILURE([readline not found, so this package cannot be built])
  HAVE_READLINE=NO
fi

if test "x$HAVE_READLINE" = xYES ; then
  AC_CHECK_FUNC(rl_erase_empty_line,
    [AC_DEFINE([HAVE_READLINE_4], [1], [Define to 1 if readline has version >= 4.0.])],
    [AC_DEFINE([HAVE_READLINE_4], [0], [Define to 1 if readline has version >= 4.0.])])
  AC_CHECK_FUNC(rl_free_undo_list,
    [AC_DEFINE([HAVE_READLINE_4_2], [1], [Define to 1 if readline has version >= 4.2.])],
    [AC_DEFINE([HAVE_READLINE_4_2], [0], [Define to 1 if readline has version >= 4.2.])])
  AC_CHECK_LIB(readline, rl_completion_word_break_hook,
    [AC_DEFINE([HAVE_READLINE_5], [1], [Define to 1 if readline has version >= 5.0.])],
    [AC_DEFINE([HAVE_READLINE_5], [0], [Define to 1 if readline has version >= 5.0.])])
else
  AC_DEFINE([HAVE_READLINE_4], [0], [Define to 1 if readline has version >= 4.0.])
  AC_DEFINE([HAVE_READLINE_4_2], [0], [Define to 1 if readline has version >= 4.2.])
  AC_DEFINE([HAVE_READLINE_5], [0], [Define to 1 if readline has version >= 5.0.])
fi

if test "x$HAVE_READLINE" = xYES; then
  BUILD_PACKAGE_BOOL=True
else
  BUILD_PACKAGE_BOOL=False
fi

fi

AC_SUBST([BUILD_PACKAGE_BOOL])
AC_SUBST([READLINE_CFLAGS])
AC_SUBST([READLINE_LDFLAGS])
AC_SUBST([READLINE_LIBS])
AC_SUBST([READLINE_FRAMEWORKS])

AC_CONFIG_FILES([readline.buildinfo])
AC_OUTPUT
