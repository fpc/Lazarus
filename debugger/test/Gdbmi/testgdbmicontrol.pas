unit TestGDBMIControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  CheckLst, testregistry, fpcunit;

type

  { TTestControlForm }

TTestControlForm = class(TForm)
    CheckListBox1: TCheckListBox;
    chkGDB: TCheckListBox;
    CheckWriteLogs: TCheckBox;
    chkFPC: TCheckListBox;
    EdOnlyWatch: TEdit;
    EditLogDir: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    procedure CheckWriteLogsChange(Sender: TObject);
procedure EditLogDirChange(Sender: TObject);
procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  TestControlForm: TTestControlForm;

implementation
uses GuiTestRunner, TestBase;

{$R *.lfm}

{ TTestControlForm }

procedure TTestControlForm.FormShow(Sender: TObject);
var
  i, j: Integer;
  d: TDebuggerList;
  c: TCompilerList;
begin
  OnShow := nil;
  Top := TestRunner.Top;
  Left := TestRunner.Left + TestRunner.Width;

  if DirectoryExistsUTF8(ConfDir+'logs') then
    EditLogDir.Text := ConfDir+'logs'+DirectorySeparator
  else if DirectoryExistsUTF8(ConfDir+'log') then
    EditLogDir.Text := ConfDir+'log'+DirectorySeparator
  else
    EditLogDir.Text := ConfDir;

  j := CheckListBox1.Items.Add('TTestExceptionOne');
  CheckListBox1.Checked[j] := True;
  j := CheckListBox1.Items.Add('TTestWatch');
  CheckListBox1.Checked[j] := True;
  j := CheckListBox1.Items.Add('TTestWatch.Unstable');
  CheckListBox1.Checked[j] := False;
  j := CheckListBox1.Items.Add('TTestWatch.Gdb');
  CheckListBox1.Checked[j] := True;
  j := CheckListBox1.Items.Add('TTestWatch.All');
  CheckListBox1.Checked[j] := True;
  j := CheckListBox1.Items.Add('TTestWatch.Mix');
  CheckListBox1.Checked[j] := True;
  j := CheckListBox1.Items.Add('TTestWatch.Mix.All');
  CheckListBox1.Checked[j] := True;
  j := CheckListBox1.Items.Add('TTestWatch.Cache');
  CheckListBox1.Checked[j] := True;
  j := CheckListBox1.Items.Add('TTestBreakPoint');
  CheckListBox1.Checked[j] := True;
  j := CheckListBox1.Items.Add('TTestBreakPoint.BadAddr');
  CheckListBox1.Checked[j] := True;
  j := CheckListBox1.Items.Add('TTestBreakPoint.BadInterrupt');
  CheckListBox1.Checked[j] := True;
  j := CheckListBox1.Items.Add('TTestBreakPoint.BadInterrupt.All');
  CheckListBox1.Checked[j] := False;

  d := GetDebuggers;
  for i := 0 to d.Count - 1 do begin
    j := chkGDB.Items.Add(d.Name[i]);
    chkGDB.Checked[j] := True;
  end;
  c := GetCompilers;
  for i := 0 to c.Count - 1 do begin
    j := chkFPC.Items.Add(c.Name[i]);
    chkFPC.Checked[j] := True;
  end;
end;

procedure TTestControlForm.EditLogDirChange(Sender: TObject);
begin
  Logdir := EditLogDir.Text;
end;

procedure TTestControlForm.CheckWriteLogsChange(Sender: TObject);
begin
  WriteLog := CheckWriteLogs.Checked;
end;

end.

