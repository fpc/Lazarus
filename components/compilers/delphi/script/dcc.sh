#!/bin/bash
#
# Defaults
#
#LIBROOT=/opt/delphi/linuxmint19.1.sdk
LIBROOT=
#opt/delphi/linuxmint19.1.sdk
DELPHIUNITS=~/.delphi_units.txt
DELPHILIBPATH=~/.delphi_libpath.txt
TARGET=linux64
DELPHIDIR=/opt/delphi/delphi-1101
#LINKARGS="-export-dynamic -z now -z relro -z noexecstack"
LINKARGS="-z now -z relro -z noexecstack"
BUILDCONFIG=debug
export WINEDEBUG="-all"
#
# Usage
#
function usage {
  if [ ! -z $1 ]; then
    echo "Error: $1"
  fi  
  echo "Usage: $0 [options] project.dpr"
  echo "Where options is one or more of: "
  echo "-h help          This help message"
  echo "-c build-config  One of debug or release (default: $BUILDCONFIG)"
  echo "-dd DIR          Delphi installation dir (default: $DELPHIDIR)"
  echo "-la ARGS         Extra linker args. (added to default args: $LINKARGS)"
  echo "-lp FILE         Set file with library paths (one per line, default: $DELPHILIBPATH)"
  echo "-n               only echo commands."
  echo "-np              do not set permissions on executable"
  echo "-sr PATH         Set sysroot (default: $LIBROOT)"
  echo "-up FILE         Set file with unit paths (one per line, default: $DELPHIUNITS)"
  echo "-t TARGET        Set OS target (default: $TARGET)"
  echo "-v               Be more verbose"
} 
function info {
  if [ "$VERBOSE" = YES ]; then
    echo $*
  fi  
}
#
# Process command-line options
#
ARGS=
while test $# != 0 
do  
  f=$1
  case $f in
  -c) shift
      BUILDCONFIG=$1;;
  -up) shift
       DELPHIUNITS=$1;;
  -dd) shift
       DELPHIDIR=$1;;     
  -la) shift
       EXTRALINKARGS=$1;;     
  -lp) shift
       DELPHILIBPATH=$1;;
  -sr) shift
       LIBROOT=$1;;
  -t) shift
      TARGET=$1;;
  -n) ECHO=YES;;
  -np) NOPERMISSIONS=YES;;
  -h) usage
     exit 0;;
  -v) VERBOSE=YES;;   
  -*)
     ARGS="$ARGS $f";;
  @*) 
     ARGS="$ARGS $f";;   
  *) PROJECT=$f
  esac
  shift
done
#
# Compiler & target os
#
case $TARGET in
  win32)
    DCC=dcc32
    TARGETOSDIR=win
    TARGETLIBDIR=win32;;
  win64) 
    DCC=dcc64
    TARGETOSDIR=win
    TARGETLIBDIR=win64;;
  linux|linux64) 
    DCC=dcclinux64
    TARGETOSDIR="linux posix"
    TARGETLIBDIR=linux64
    TARGETSDKLIBPATHS="usr/lib/x86_64-linux-gnu/ lib/ lib/x86_64-linux-gnu/ usr/lib/"
    NEEDPERMISSIONS=YES;;
  osx|osx64) 
    DCC=dccosx64
    TARGETOSDIR="osx posix"
    TARGETLIBDIR=osx64
    NEEDPERMISSIONS=YES;;
  osxarm|osxarm64) 
    DCC=dccosxarm64
    TARGETOSDIR="osx posix"
    TARGETLIBDIR=osxarm64
    NEEDPERMISSIONS=YES;;
  ios|ios64)
    DCC=dcciosarm64
    TARGETOSDIR="ios posix"
    TARGETLIBDIR="iosDevice64";;
  android) 
    DCC=dccaarm
    TARGETOSDIR="android posix"
    TARGETLIBDIR="android";;
  android64) 
    DCC=dccaarm64
    TARGETOSDIR="android posix"
    TARGETLIBDIR="android64";;
    
  *) Usage "Unknown target: $TARGET"
     exit 1;;
esac  
info "Building for target: $TARGET, for config: $BUILDCONFIG"
#
# Check echo only
#
if [ "$ECHO" = "YES" ]; then 
  CMD=echo
fi
#
# Unit paths
#
UNITPATHS=.
if [ ! -f "$DELPHIUNITS" ]; then
  info "Creating default unit paths based on $DELPHIDIR installation."
  for d in lib lib/$TARGETLIBDIR/$BUILDCONFIG Imports include 
  do
    UNITPATHS="$UNITPATHS;$DELPHIDIR/$d"
  done
  for d in fmx DUnit DUnit/src 
  do
    UNITPATHS="$UNITPATHS;$DELPHIDIR/source/$d"
  done
  for d in $TARGETOSDIR
  do
    UNITPATHS="$UNITPATHS;$DELPHIDIR/source/rtl/$d"
  done
else  
  info "Creating default unit paths based on $DELPHIUNITS file."
  for U in $(cat $DELPHIUNITS)
  do
    eval "UD=$U" 
    UNITPATHS="$UNITPATHS;$UD"
  done
fi  
info "Unit paths: $UNITPATHS"
info ""
#
# Library paths
#
LIBPATHS=.
if [ ! -f "$DELPHILIBPATH" ]; then
  info "Creating default delphi library paths based on $DELPHIDIR installation."
  LIBPATHS=$DELPHIDIR/lib/$TARGETLIBDIR/$BUILDCONFIG
  if [ ! -z "$TARGETSDKLIBPATHS" ]; then
    for lp in $TARGETSDKLIBPATHS
    do
      LIBPATHS="$LIBPATHS;$LIBROOT/$lp"
    done
  fi  
else  
  info "Creating default delphi library paths based on $DELPHILIBPATH file."
  for L in $(cat $DELPHILIBPATH)
  do
    eval "LD=$L" 
    LIBPATHS="$LIBPATHS;$LD"
  done
fi  
info "Library paths: $LIBPATHS"
info ""
#
# Various linker args
#
case $TARGET in 
  linux|linux64)
  LIBPATHARG=--libpath:"$LIBPATHS"
  SYSROOTARG=--syslibroot:"$LIBROOT"
  LINKEROPTIONSARG="--linker-option:$LINKARGS $EXTRALINKARGS"
esac
#
# Correct project file
#
UNIXPROJECT=$PROJECT
echo "$PROJECT" | grep \\\\ &> /dev/null
if [ $? != 0 ]; then
  PROJECT=$(winepath -w "$PROJECT")
  info "Converted project file to windows notation: $PROJECT";
fi  
#
# Call compiler
#
info "Calling compiler ${DELPHIDIR}/bin/${DCC}.exe:"
#
# The ${var+"$var"} is needed to omit empty arguments, which will be interoreted by delphi as empty filenames :/
#
$CMD wine "${DELPHIDIR}/bin/${DCC}.exe" ${LIBPATHARG+"$LIBPATHARG"} ${SYSROOTARG+"$SYSROOTARG"} ${LINKEROPTIONSARG+"$LINKEROPTIONSARG"} "-U$UNITPATHS" $ARGS "$PROJECT"
E=$?
if [ "$E" = 0 ]; then
  PROJEXE=$(dirname "$UNIXPROJECT")/$(basename "$UNIXPROJECT" .lpr)
  if [ "$NEEDPERMISSIONS" = YES -a ! "$NOPERMISSIONS" = YES ]; then
    if [ -f "$PROJEXE" ]; then
      info "Setting permissions on $PROJEXE"
      $CMD chmod 755 "$PROJEXE"
    else
      info "No output executable found: $PROJEXE"   
    fi  
  fi  
fi  
#
# Output a nice goodbye
#
info ""
info "All done!"
#
# That's all, folks!
#
