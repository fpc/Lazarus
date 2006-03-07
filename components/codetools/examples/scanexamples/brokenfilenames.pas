unit BrokenFilenames; 

{$mode objfpc}{$H+}

interface


uses
  Classes,
  biglettersunit, // must be fixed to BigLettersUnit
  biglettersunit in 'biglettersunit.pas',// -> BigLettersUnit.pas
  biglettersunit in '..\ScanExamples\biglettersunit.pas',// -> ../scanexamples/BigLettersUnit
  SysUtils;

{$I BROKENincfiles.inc}// must be fixed to brokenincfiles.inc
{$I ../ScanExamples/BROKENincfiles.inc}// must be fixed to ../scanexamples/brokenincfiles.inc

implementation

end.

