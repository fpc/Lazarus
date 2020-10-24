:: This script will be automatically run after building the program 
:: using build mode "CGI Cross WinToLinux" and "Console App WinToLinux"
:: using parameter "FROMIDE" in both cases and with "CONSOLE" for the
:: console app. Using the "FROMIDE" parameter will cause to only deploy
:: the compiled program to the target system.
:: If run without parameters it will deploy all needed files to the
:: target system. So one have to compile the program and then manually
:: run this script from command line, one time.
:: ....
:: * reporter.cgi will be run as %APACHE% user and group, so 
:: permissions have to be set up in such way that the cgi program
:: will be able create the working files. Although some of those files
:: are temporary and would be candidates for the %APACHE% user dir (/tmp)
:: at least one file ".fonts.cache" needs to be persisted. 
:: * The copying of the data dir and content over to cgi-bin
:: resets the group permission so they have to be fixed
:: dbf files         need r/w permission for group %APACHE%
:: cgi-bin directory need r/w permission for group %APACHE%
@echo off
set SCP=c:\PuTTY\PSCP.EXE
set SSH=c:\PuTTY\PLINK.EXE
set TARGET_HOST=
set TARGET_HTDOCS=
set TARGET_CGIBIN=%TARGET_HTDOCS%/cgi-bin
set APACHE=www-data

if "%TARGET_HOST%"=="" goto missingdefines
if "%TARGET_HTDOCS%"=="" goto missingdefines

if "%2"=="CONSOLE" (
	%SCP% reporter_linux %TARGET_HOST%:%TARGET_CGIBIN%/reporter
) else (
	%SCP% reporter_linux.cgi %TARGET_HOST%:%TARGET_CGIBIN%/reporter.cgi
)

if not "%1"=="FROMIDE" (
	REM deploy all files
	%SCP% -r data %TARGET_HOST%:%TARGET_CGIBIN%/
	%SCP% *.lrf %TARGET_HOST%:%TARGET_CGIBIN%/
	%SCP% index.html %TARGET_HOST%:%TARGET_CGIBIN%/
	%SCP% favicon.ico %TARGET_HOST%:%TARGET_HTDOCS%/
	%SSH% %TARGET_HOST% chmod +x %TARGET_CGIBIN%/reporter.cgi
	REM I dont know how to do this, have to do it manually ...
	REM %SSH% %TARGET_HOST% sudo chgrp -R %APACHE% %TARGET_CGIBIN%/data
	REM %SSH% %TARGET_HOST% sudo chgrp    %APACHE% %TARGET_CGIBIN%
)

goto :eof

:missingdefines
echo(Either TARGET_HOST or TARGET_HTDOCS are undefined, fix the deploy.bat script
echo(
echo(example: TARGET_HOST=user@server
echo(example: TARGET_HTDOCS=/home/user/public_html




