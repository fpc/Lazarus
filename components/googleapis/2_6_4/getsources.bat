@echo off

rd /s /q %temp%\googleapi
md %temp%\googleapi
del /s /q %temp%\googleapi\*.*
svn co http://svn.freepascal.org/svn/fpc/trunk/packages/googleapi/src/ %temp%\googleapi
copy %temp%\googleapi\*.*
rd /s /q %temp%\googleapi


rd /s /q %temp%\fcl-web
md %temp%\fcl-web
svn co http://svn.freepascal.org/svn/fpc/trunk/packages/fcl-web/src/base/ %temp%\fcl-web
copy %temp%\fcl-web\fpoauth2.pp
copy %temp%\fcl-web\fphttpwebclient.pp
copy %temp%\fcl-web\fpwebclient.pp
copy %temp%\fcl-web\restcodegen.pp
copy %temp%\fcl-web\restbase.pp
copy %temp%\fcl-web\fpoauth2ini.pp
copy %temp%\fcl-web\fpjwt.pp
rd /s /q %temp%\fcl-web

echo "-- googleapis sources --"
dir
pause