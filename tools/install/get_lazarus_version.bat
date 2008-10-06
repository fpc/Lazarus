@ECHO OFF

FOR /F "delims='" %%F IN (%~dps0\..\..\..\ide\version.inc) DO set LAZVERSION=%%F

echo %LAZVERSION%