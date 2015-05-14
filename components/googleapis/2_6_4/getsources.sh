#!/bin/bash
#
# Script to get sources for google api if you work with 2.6.4
# Kindly donated by markbass72 <markbass72@gmail.com>
#
function ExitFromScript()
{
echo ERROR: incorrect path!
echo You must execute this script from folder: .../lazarus/components/googleapis/2_6_4
exit
}

pwd|grep "/2_6_4$" || ExitFromScript
echo Folder: OK!

[ ! -d /tmp/googleapi ] && mkdir /tmp/googleapi
svn co http://svn.freepascal.org/svn/fpc/trunk/packages/googleapi/src/ /tmp/googleapi/
cp /tmp/googleapi/* .
rm -r /tmp/googleapi

[ ! -d /tmp/fcl-web ] && mkdir /tmp/fcl-web
svn co http://svn.freepascal.org/svn/fpc/trunk/packages/fcl-web/src/base/ /tmp/fcl-web/
cp /tmp/fcl-web/fpoauth2.pp .
cp /tmp/fcl-web/fphttpwebclient.pp .
cp /tmp/fcl-web/fpwebclient.pp .
cp /tmp/fcl-web/restcodegen.pp .
cp /tmp/fcl-web/restbase.pp .
cp /tmp/fcl-web/fpoauth2ini.pp .
cp /tmp/fcl-web/fpjwt.pp .
rm -r /tmp/fcl-web

echo "-- googleapis sources --"
ls