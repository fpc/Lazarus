unit BrokenFilenames; 

{$mode objfpc}{$H+}

interface


uses
  Classes,
  CustApp,
  biglettersunit, // must be fixed to BigLettersUnit
  biglettersunit in 'biglettersunit.pas',// -> BigLettersUnit.pas
  biglettersunit in '..\ScanExamples\biglettersunit.pas',// -> BigLettersUnit.pas
  NonExistingUnit1, NonExistingUnit2, SysUtils, NonExistingUnit3,
  {$IFDEF FPC}
  NonExistingUnit4
  {$ELSE}
  NonExistingUnit5
  {$ENDIF};

{$I BROKENincfiles.inc}// must be fixed to brokenincfiles.inc
{$I ../ScanInstances/BROKENincfile2.inc}// must be fixed to ../scaninstances/brokenincfiles.inc

implementation

end.

