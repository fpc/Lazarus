#!/bin/bash

set -x
set -e

Year=02
Month=12
Day=25


Date=20$Year$Month$Day
LazVersion=1.1
LazRelease=laz.$Date
SrcTGZ=fpcsrc-$LazVersion-$LazRelease.tgz
SpecFile=fpcsrc-$LazVersion-$LazRelease.spec

# download fpc cvs if necessary
if [ ! -f $SrcTGZ ]; then
  ./create_fpc_snapshot_tgz.sh $SrcTGZ -D $Month/$Day/$Year
fi

# copy src tgz into building directory
cp $SrcTGZ /usr/src/redhat/SOURCES/

# create spec file
cat fpcsrc.spec | \
  sed -e "s/LAZVERSION/$LazVersion/g" -e "s/LAZRELEASE/$LazRelease/" \
  > $SpecFile

# build rpm
rpm -ba $SpecFile

# end.

