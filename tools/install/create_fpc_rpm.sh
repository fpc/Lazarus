#!/bin/bash

#set -x
set -e

Usage="Usage: $0 devel|stable"

FPCVersion=$1

FPCVersionOk=no
for ver in devel stable; do
  if [ "x$FPCVersion" = "x$ver" ]; then
    FPCVersionOk=yes
  fi
done
if [ "x$FPCVersionOk" = "xno" ]; then
  echo $Usage
  exit -1
fi


# set here the fpc cvs dates for the various versions
if [ "x$FPCVersion" = "xdevel" ]; then
  Year=03
  Month=06
  Day=05
  LazVersion=1.1
fi
if [ "x$FPCVersion" = "xstable" ]; then
  Year=03
  Month=06
  Day=05
  LazVersion=1.0.8
fi


Date=20$Year$Month$Day
LazRelease=laz.$Date
SrcTGZ=fpcsrc-$LazVersion-$LazRelease.tgz
TmpDir=/tmp/fpc$LazVersion
SpecFile=$TmpDir/fpc/install/fpc.spec

# download fpc cvs if necessary
if [ ! -f $SrcTGZ ]; then
  ./create_fpc_export_tgz.sh $SrcTGZ $FPCVersion $Month/$Day/$Year
fi

# unpack source into temporary directory
rm -rf $TmpDir
mkdir -p $TmpDir
cp $SrcTGZ $TmpDir/
cd $TmpDir
tar xzf $SrcTGZ
cd -

# change spec file
cat $SpecFile | \
  sed -e 's/^Version: .*/Version: '"$LazVersion/" \
      -e 's/^Release: .*/Release: '"$LazRelease/" \
      -e 's/^\%{fpcdir}\/samplecfg .*/%{fpcdir}\/samplecfg %{_libdir}\/fpc\/\\\$version/' \
      -e 's/^\(\%doc \%{exampledir}\/\*\)/\1\/*/' \
  > $SpecFile

# change Makefile for new rpmbuild
cd $TmpDir/fpc
cat Makefile | \
  sed -e 's/rpm\( --nodeps -ba .*\)$/rpm\1 || rpmbuild\1/g' \
  > New.Makefile
mv New.Makefile Makefile
cd -

# compile
cd $TmpDir/fpc
make rtl
make compiler
make rpm
#make rpm NODOCS=1
cd -


echo
echo building fpcsrc rpm ...
set -x

# copy src tgz into building directory
cp $SrcTGZ /usr/src/redhat/SOURCES/

# create spec file
SpecFile=fpcsrc-$LazVersion-$LazRelease.spec
cat fpcsrc.spec | \
  sed -e "s/LAZVERSION/$LazVersion/g" -e "s/LAZRELEASE/$LazRelease/" \
  > $SpecFile

# build rpm
rpmbuild -ba $SpecFile || rpm -ba $SpecFile

# end.

