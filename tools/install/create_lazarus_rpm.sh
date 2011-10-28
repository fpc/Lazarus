#!/bin/bash

set -x
set -e

# get date of day
Year=$(date +%y)
Month=$(date +%m)
Day=$(date +%d)

# get installed fpc version
echo "getting installed fpc version ..."
FPCRPM=$(rpm -qa | egrep '^fpc-[0-9]')
if [ "x$FPCRPM" = "x" ]; then
  echo ERROR: fpc rpm not installed
  exit
fi
FPCRPMVersion=$(echo $FPCRPM | sed -e 's/fpc-//g' -e 's/\.i386\|\.i686\|\.x86_64//g')
echo "installed fpc version: $FPCRPMVersion"
FPCSRCRPMVersion=$(echo $FPCRPMVersion | cut -d- -f1)

Date=$Year$Month$Day
LazVersion=$(./get_lazarus_version.sh)
LazRelease='0' # $(echo $FPCRPM | sed -e 's/-/_/g')
RPMSrcDir=$(./rpm/get_rpm_source_dir.sh)
Src=lazarus-$LazVersion-$LazRelease.tar.gz
SrcTGZ=$RPMSrcDir/SOURCES/$Src
SpecFile=rpm/lazarus-$LazVersion-$LazRelease.spec

Arch=$(rpm --eval "%{_arch}")

# download lazarus svn if needed
echo "creating lazarus tgz ..."
#if [ ! -f $SrcTGZ ]; then
  sh create_lazarus_export_tgz.sh $SrcTGZ
#fi

# create spec file
echo "creating lazarus spec file ..."
cat rpm/lazarus.spec.template | \
  sed -e "s/LAZVERSION/$LazVersion/g" \
      -e "s/LAZRELEASE/$LazRelease/g" \
      -e "s/LAZSOURCE/$Src/g" \
      -e "s/FPCBUILDVERSION/2.2.2/g" \
      -e "s/FPCVERSION/$FPCRPMVersion/g" \
      -e "s/FPCSRCVERSION/$FPCSRCRPMVersion/g" \
  > $SpecFile

# build rpm
echo "building rpm ..."
rpm -ba $SpecFile || rpmbuild -ba $SpecFile

echo "The new rpm can be found at $RPMSrcDir/RPMS/$Arch/lazarus-$LazVersion-$LazRelease.$Arch.rpm"

# end.

