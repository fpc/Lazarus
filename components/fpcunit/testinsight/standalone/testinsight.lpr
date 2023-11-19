program testinsight;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, testinsightprotocol,
testinsightclient, fpcunittestinsight, frmtestinsight, testinsightserver
  { you can add units after this };

{$R *.res}

var
  TestInsightForm : TTestInsightForm;

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TTestInsightForm, TestInsightForm);
  Application.Run;
end.

