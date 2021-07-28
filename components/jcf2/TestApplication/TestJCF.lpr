program TestJCF;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
  cthreads, {$ENDIF} {$IFDEF HASAMIGA}
  athreads, {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  unit1,
  BuildParseTree,
  fShowParseTree,
  Converter, SettingsStream,
  ReturnBefore,
  MoveSpaceToBeforeColon,
  Indenter,
  SysUtils;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

