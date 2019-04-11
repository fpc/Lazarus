:: ----------------------------------------------------------------------------
:: Batch file to create TAChart chm help files
::
:: Adapt path to fpdoc.exe to your fpc configuration.
:: ----------------------------------------------------------------------------

set fpdoc=C:\lazarus-trunk_fpc304\fpc\3.0.4\bin\i386-win32\fpdoc.exe
%fpdoc% --project=tachart-help.hpr
