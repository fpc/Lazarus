
{$R *.res}

program cd_test_all;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, customdrawn, mainform
  { you can add units after this };

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.Run;
end.

