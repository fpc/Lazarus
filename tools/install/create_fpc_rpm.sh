#!/bin/bash

#set -x
set -e

#------------------------------------------------------------------------------
# parse parameters
#------------------------------------------------------------------------------
Usage="Usage: $0 [nodocs] [notemp] [deb] <FPCSrcDir> [release]"

WithDOCS=yes
if [ "x$1" = "xnodocs" ]; then
  WithDOCS=no
  shift
fi

WithTempDir=yes
if [ "x$1" = "xnotemp" ]; then
  WithTempDir=no
  shift
fi

PkgType=rpm
if [ "x$1" = "xdeb" ]; then
  PkgType=deb
  shift
fi

FPCSrcDir=$1
if [ "x$FPCSrcDir" = "x" ]; then
  echo $Usage
  exit -1
fi
FPCSrcDir=$(echo $FPCSrcDir)
shift

FPCRelease=$1
if [ "x$FPCRelease" = "x" ]; then
  FPCRelease=$(date +%y%m%d)
else
  shift
fi

if [ ! -d $FPCSrcDir/compiler ]; then
  echo "The directory $FPCSrcDir does not look like a fpc source directory (fpc/)"
  exit -1
fi

#------------------------------------------------------------------------------
# patching
#------------------------------------------------------------------------------

# create a temporary copy of the fpc sources to patch it
TmpDir=~/tmp/fpc_patchdir
if [ "$WithTempDir" = "yes" ]; then
  if [ -d $TmpDir ]; then
    rm -rf $TmpDir
  fi
  mkdir -p $TmpDir

  echo "extracting FPC from local svn ..."
  svn export $FPCSrcDir $TmpDir/fpc
else
  TmpDir=$FPCSrcDir
fi

# retrieve the version information
echo -n "getting FPC version from local svn ..."
VersionFile="$TmpDir/fpc/compiler/version.pas"
CompilerVersion=`cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerRelease=`cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerPatch=`cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerVersionStr="$CompilerVersion.$CompilerRelease.$CompilerPatch"
FPCVersion="$CompilerVersion.$CompilerRelease"
if [ "$CompilerPatch" != "0" ]; then
  FPCVersion="$FPCVersion.$CompilerPatch"
fi
echo " $CompilerVersionStr-$FPCRelease"

Arch=$(rpm --eval "%{_arch}")


#------------------------------------------------------------------------------
# patch sources

SmartStripScript=smart_strip.sh
ReplaceScript=replace_in_files.pl


# set version numbers in all Makefiles
echo "set version numbers in all Makefiles ..."
perl replace_in_files.pl -sR -f '=\d.\d.\d' -r =$CompilerVersionStr -m 'Makefile(.fpc)?' $TmpDir/fpc/*

# update smart_strip.sh
#ATM: not needed: cp $SmartStripScript $TmpDir/fpc/install/

if [ "$PkgType" = "deb" ]; then
  # build fpc debs

  # change debian files
  DebianRulezDir=$TmpDir/fpc/install/debian/
  Date=`date --rfc-822`

  # prepend changelog information, needed for version
  cd $DebianRulezDir
  File=changelog
  OldFile=changelog.old.fpc
  cp $File $OldFile
  echo "fpc ($FPCVersion-$FPCRelease) unstable; urgency=low" > $File
  echo '  * Unofficial snapshot build for lazarus' >> $File
  echo " -- Mattias Gaertner <mattias@freepascal.org>  $Date" >> $File
  echo "" >> $File
  cat $OldFile >> $File
  rm $OldFile
  cd -

  # fix debian/rules
  # - copy the complete examples directory
  # - do not install non existing files Changes.fcl Changes.utils
  cd $DebianRulezDir
  cat rules | \
    sed -e 's/^\(.*mv .*\)uncgi\( .*examples.*\)$/\1???*\2/' \
        -e 's/^.*logs\/Changes\.fcl.*$//' \
        -e 's/^.*logs\/Changes\.utils.*$//' \
    > rules.laz
  cp rules.laz rules # use cp to preserve file attribs
  rm rules.laz
  cd -


  # compile
  cd $TmpDir/fpc
  make debcopy
  cd -
  cd /usr/src/fpc-$FPCVersion
  ./debian/rules binary-arch
  cd -
  
else
  # build fpc rpm

  echo "creating spec file ..."
  SpecFileTemplate=rpm/fpc.spec.template
  SpecFile=rpm/fpc.spec

  # change spec file
  cat $SpecFileTemplate | \
    sed -e 's/^Version: .*/Version: '"$FPCVersion/" \
        -e 's/^Release: .*/Release: '"$FPCRelease/" \
        -e 's/^%define fpcversion .*/%define fpcversion '"$FPCVersion/" \
    > $SpecFile
  #      -e 's/\(%define builddocdir.*\)/%define __strip smart_strip.sh\n\n\1/' \
  #      -e 's/^\%{fpcdir}\/samplecfg .*/%{fpcdir}\/samplecfg %{_libdir}\/fpc\/\\\$version/' \
  
  SrcTGZ=$(rpm/get_rpm_source_dir.sh)/SOURCES/fpc-$CompilerVersionStr-$FPCRelease.source.tar.gz
  echo "creating $SrcTGZ ..."
  tar czf $SrcTGZ -C $TmpDir fpc

  #----------------------------------------------------------------------------
  # compile
  #----------------------------------------------------------------------------
  if [ "$WithDOCS" = "no" ]; then
    export NODOCS=1
  fi
  rpmbuild --nodeps -ba $SpecFile

  echo "The new rpm can be found in $(./rpm/get_rpm_source_dir.sh)/RPMS/$Arch/fpc-$FPCVersion-$FPCRelease.$Arch.rpm"
fi

# end.

