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
    CheckWriteLogs: TCheckBox;
    EditLogDir: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
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
  j: Integer;
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
  j := CheckListBox1.Items.Add('TTestWatch.Gdb');
  CheckListBox1.Checked[j] := True;
  j := CheckListBox1.Items.Add('TTestWatch.All');
  CheckListBox1.Checked[j] := True;
  j := CheckListBox1.Items.Add('TTestWatch.Mix');
  CheckListBox1.Checked[j] := True;
  j := CheckListBox1.Items.Add('TTestWatch.Cache');
  CheckListBox1.Checked[j] := True;
  j := CheckListBox1.Items.Add('TTestBreakPoint');
  CheckListBox1.Checked[j] := True;

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

