#!/bin/bash

set -x
set -e

#------------------------------------------------------------------------------
# parse parameters
#------------------------------------------------------------------------------
Usage="Usage: $0 [nodocs] [notemp] [deb] <FPCSrcDir> <release>"

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

FPCSourceDir=$1
shift
if [ "x$FPCSourceDir" = "x" ]; then
  echo $Usage
  exit -1
fi

LazRelease=$1
shift
if [ "x$LazRelease" = "x" ]; then
  echo $Usage
  exit -1
fi


#------------------------------------------------------------------------------
# patching
#------------------------------------------------------------------------------

# create a temporary copy of the fpc sources to patch it
TmpDir=/tmp/fpc_patchdir
if [ "$WithTempDir" = "yes" ]; then
  rm -rf $TmpDir
  cp -a $FPCSourceDir $TmpDir
else
  TmpDir=$FPCSourceDir
fi

# retrieve the version information
VersionFile="$TmpDir/compiler/version.pas"
CompilerVersion=`cat $VersionFile | grep ' *version_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerRelease=`cat $VersionFile | grep ' *release_nr *=.*;' | sed -e 's/[^0-9]//g'`
CompilerPatch=`cat $VersionFile | grep ' *patch_nr *=.*;' | sed -e 's/[^0-9]//g'`
LazVersion="$CompilerVersion.$CompilerRelease"
if [ "$CompilerPatch" != "0" ]; then
  LazVersion="$LazVersion.$CompilerPatch"
fi



#------------------------------------------------------------------------------
# patch sources

FPCMakefile=$TmpDir/Makefile
SmartStripScript=smart_strip.sh

# update smart_strip.sh
cp $SmartStripScript $TmpDir/install/

if [ "$PkgType" = "deb" ]; then
  # build fpc debs

  # change debian files
  DebianRulezDir=$TmpDir/install/debian/
  Date=`date --rfc-822`

  # prepend changelog information, needed for version
  cd $DebianRulezDir
  File=changelog
  OldFile=changelog.old.fpc
  cp $File $OldFile
  echo "fpc ($LazVersion-$LazRelease) unstable; urgency=low" > $File
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
  cd $TmpDir/
  make debcopy
  cd -
  cd /usr/src/fpc-$LazVersion
  ./debian/rules binary-arch
  cd -
  
  # ToDo: install latest 1.0.10 tar, fix -dHasUnix,

else
  # build fpc rpm

  SpecFile=$TmpDir/install/fpc.spec
  SrcPatch=fpcsrc-patch
  SmartSripScript=smart_strip.sh

  # patch sources -> This should eventually go to the fpc cvs
  # update smart_strip.sh
  cp $SmartSripScript $TmpDir/install/

  #patch -l -p2 -d $TmpDir/ -i $SrcPatch

  # change spec file
  cat $SpecFile | \
    sed -e 's/^Version: .*/Version: '"$LazVersion/" \
        -e 's/^Release: .*/Release: '"$LazRelease/" \
    > $SpecFile.New
  #      -e 's/\(%define builddocdir.*\)/%define __strip smart_strip.sh\n\n\1/' \
  #      -e 's/^\%{fpcdir}\/samplecfg .*/%{fpcdir}\/samplecfg %{_libdir}\/fpc\/\\\$version/' \
  mv $SpecFile.New $SpecFile
  #if [ "$WithDOCS" = "no" ]; then
    #cat $SpecFile | \
    #  sed -e 's/^\(.*\bmake\b.*\bdocs\b\)/#\1/g' \
    #      -e 's/^\(%doc.*\*\.pdf\)/#\1/g' \
    #> $SpecFile.New
    #mv $SpecFile.New $SpecFile
  #fi

  # change Makefile for new rpmbuild, if not already done
  cd $TmpDir
  if [ -n `grep -q rpmbuild Makefile` ]; then
    cat Makefile | \
      sed -e 's/rpm\( --nodeps -ba .*\)$/rpm\1 || rpmbuild\1/g' \
      > New.Makefile
    mv New.Makefile Makefile
  fi
  cd -
  
  # fix fpc.spec bug: smart_strip.sh is searched at wrong location
  SmartStripWhileBuild="/usr/src/redhat/BUILD/fpc-$CompilerVersion.$CompilerRelease.$CompilerPatch/smart_strip.sh"
  cat $SpecFile | \
    sed -e 's# %{buildroot}/smart_strip.sh# '"$SmartStripWhileBuild"'#' \
    > $SpecFile.New
  mv $SpecFile.New $SpecFile

  #----------------------------------------------------------------------------
  # compile
  #----------------------------------------------------------------------------
  cd $TmpDir
  #make rtl
  #make compiler
  if [ "$WithDOCS" = "no" ]; then
    make rpm NODOCS=1
  else
    make rpm
  fi
  cd -
fi

# end.

