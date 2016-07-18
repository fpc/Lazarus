NewSource := 
    'program FPCUnitProject1;' + sLineBreak
  + sLineBreak
  + '{$mode objfpc}{$H+}' + sLineBreak
  + sLineBreak
  + 'uses' + sLineBreak
  + '  Classes, consoletestrunner;' + sLineBreak
  + sLineBreak
  + 'type' + sLineBreak
  + sLineBreak
  + '  { TMyTestRunner }' + sLineBreak
  + sLineBreak
  + '  TMyTestRunner = class(TTestRunner)' + sLineBreak
  + '  protected' + sLineBreak
  + '  // override the protected methods of TTestRunner to customize its behavior' + sLineBreak
  + '  end;' + sLineBreak
  + sLineBreak
  + 'var' + sLineBreak
  + '  Application: TMyTestRunner;' + sLineBreak
  + sLineBreak
  + 'begin' + sLineBreak
  + '  Application := TMyTestRunner.Create(nil);' + sLineBreak
  + '  Application.Initialize;' + sLineBreak
  + '  Application.Title := ''FPCUnit Console test runner'';' + sLineBreak
  + '  Application.Run;' + sLineBreak
  + '  Application.Free;' + sLineBreak
  + 'end.' + sLineBreak
;
