unit TestGDBMIControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, CheckLst, testregistry, fpcunit, GuiTestRunner;

type

  { TTestControlForm }

TTestControlForm = class(TForm)
    WriteLogsOnErr: TCheckBox;
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
procedure WriteLogsOnErrChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  TestControlForm: TTestControlForm;

procedure RegisterTestSelectors(ANames: array of string);

implementation
uses TestBase;

var
  TestSelectors: TStringList = nil;

procedure RegisterTestSelectors(ANames: array of string);
var
  i: Integer;
begin
  if TestSelectors = nil then TestSelectors := TStringList.Create;
  for i := low(ANames) to high(ANames) do
    TestSelectors.Add(ANames[i]);
end;

{$R *.lfm}

{ TTestControlForm }

procedure TTestControlForm.FormShow(Sender: TObject);
var
  i, j: Integer;
  d: TDebuggerList;
  c: TCompilerList;
  s: String;
  f: Boolean;
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

  for i := 0 to TestSelectors.Count - 1 do begin
    s := TestSelectors[i];
    f := (s<>'') and (s[1] = '-');
    if f then delete(s,1,1);
    j := CheckListBox1.Items.Add(s);
    CheckListBox1.Checked[j] := not f;
  end;

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

  WriteLog := CheckWriteLogs.Checked;
  WriteLogOnErr := WriteLogsOnErr.Checked;
end;

procedure TTestControlForm.WriteLogsOnErrChange(Sender: TObject);
begin
  WriteLogOnErr := WriteLogsOnErr.Checked;
end;

procedure TTestControlForm.EditLogDirChange(Sender: TObject);
begin
  Logdir := EditLogDir.Text;
end;

procedure TTestControlForm.CheckWriteLogsChange(Sender: TObject);
begin
  WriteLog := CheckWriteLogs.Checked;
end;

finalization
  TestSelectors.Free;
end.

