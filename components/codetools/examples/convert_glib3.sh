#!/bin/bash

# do not include gerror.h - it only contains c macros for debugging
# do not include gslice.h, gmem.h - it only contains c macros for fast mem allocation

dir=~/cpp/gtk3/glib/glib
./h2pastest -uG_BEGIN_DECLS -uG_END_DECLS -uG_GNUC_CONST -dG_CONST_RETURN=const \
            -uGLIB_VAR -uG_INLINE_FUNC -uG_GNUC_MAY_ALIAS -uG_GNUC_MALLOC \
            -uG_GNUC_WARN_UNUSED_RESULT -uG_GNUC_NULL_TERMINATED \
            -uG_GNUC_PURE \
  $dir/glib.h \
  $dir/galloca.h \
  $dir/garray.h \
  $dir/gasyncqueue.h \
  $dir/gatomic.h \
  $dir/gbacktrace.h \
  $dir/gbase64.h \
  $dir/gbitlock.h \
  $dir/gbookmarkfile.h \
  $dir/gbsearcharray.h \
  $dir/gbuffer.h \
  $dir/gcache.h \
  $dir/gchecksum.h \
  $dir/gcompletion.h \
  $dir/gconvert.h \
  $dir/gdataset.h \
  $dir/gdatasetprivate.h \
  $dir/gdate.h \
  $dir/gdatetime.h \
  $dir/gdebug.h \
  $dir/gdir.h \
  $dir/gfileutils.h \
  $dir/ghash.h \
  $dir/ghook.h \
  $dir/ghostutils.h \
  $dir/gi18n.h \
  $dir/gi18n-lib.h \
  $dir/giochannel.h \
  $dir/gkeyfile.h \
  $dir/glib.h \
  $dir/glibintl.h \
  $dir/glib-object.h \
  $dir/glib_trace.h \
  $dir/glist.h \
  $dir/gmacros.h \
  $dir/gmain.h \
  $dir/gmappedfile.h \
  $dir/gmarkup.h \
  $dir/gmessages.h \
  $dir/gmirroringtable.h \
  $dir/gnode.h \
  $dir/goption.h \
  $dir/gpattern.h \
  $dir/gpoll.h \
  $dir/gprimes.h \
  $dir/gprintf.h \
  $dir/gprintfint.h \
  $dir/gqsort.h \
  $dir/gquark.h \
  $dir/gqueue.h \
  $dir/grand.h \
  $dir/gregex.h \
  $dir/grel.h \
  $dir/gscanner.h \
  $dir/gscripttable.h \
  $dir/gsequence.h \
  $dir/gshell.h \
  $dir/gslist.h \
  $dir/gspawn.h \
  $dir/gstdio.h \
  $dir/gstrfuncs.h \
  $dir/gstring.h \
  $dir/gtestutils.h \
  $dir/gthread.h \
  $dir/gthreadpool.h \
  $dir/gthreadprivate.h \
  $dir/gtimer.h \
  $dir/gtimezone.h \
  $dir/gtree.h \
  $dir/gtypes.h \
  $dir/gunibreak.h \
  $dir/gunichartables.h \
  $dir/gunicode.h \
  $dir/gunicodeprivate.h \
  $dir/gunicomp.h \
  $dir/gunidecomp.h \
  $dir/gurifuncs.h \
  $dir/gutils.h \
  $dir/gvariant-core.h \
  $dir/gvariant.h \
  $dir/gvariant-internal.h \
  $dir/gvariant-serialiser.h \
  $dir/gvarianttype.h \
  $dir/gvarianttypeinfo.h \
  $dir/gwin32.h


