@ECHO OFF

FOR /F "delims='" %%F IN (%~dp0\..\..\ide\version.inc) DO set LAZVERSION=%%F

echo %LAZVERSION%