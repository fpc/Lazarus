NewSource := 
    'program FPCUnitProject1;' + #13
  + #13
  + '{$mode objfpc}{$H+}' + #13
  + #13
  + 'uses' + #13
  + '  Classes, consoletestrunner;' + #13
  + #13
  + 'type' + #13
  + #13
  + '  { TLazTestRunner }' + #13
  + #13
  + '  TMyTestRunner = class(TTestRunner)' + #13
  + '  protected' + #13
  + '  // override the protected methods of TTestRunner to customize its behavior' + #13
  + '  end;' + #13
  + #13
  + 'var' + #13
  + '  Application: TMyTestRunner;' + #13
  + #13
  + 'begin' + #13
  + '  Application := TMyTestRunner.Create(nil);' + #13
  + '  Application.Initialize;' + #13
  + '  Application.Title := ''FPCUnit Console test runner'';' + #13
  + '  Application.Run;' + #13
  + '  Application.Free;' + #13
  + 'end.' + #13
;
