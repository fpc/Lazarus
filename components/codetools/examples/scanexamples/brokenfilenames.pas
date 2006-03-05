unit BrokenFilenames; 

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils,
  biglettersunit // must be fixed to BigLettersUnit
  ;

{$I BROKENincfiles.inc}// must be fixed to brokenincfiles.inc
{$I ../ScanExamples/BROKENincfiles.inc}// must be fixed to ../scanExamples/brokenincfiles.inc

implementation

end.

