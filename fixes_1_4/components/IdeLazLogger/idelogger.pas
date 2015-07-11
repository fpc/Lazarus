unit idelogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, ButtonPanel,
  Buttons, CheckLst, EditBtn, ExtCtrls, IDEIntf, MenuIntf, LazLoggerBase, LazLogger;

type

  { TIdeLoggerForm }

  TIdeLoggerForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckLogGroups: TCheckListBox;
    LogNameEdit: TFileNameEdit;
    PageControl1: TPageControl;
    Panel1: TPanel;
    TabSheet1: TTabSheet;
    procedure CancelButtonClick(Sender: TObject);
    procedure CheckLogGroupsClickCheck(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LogNameEditChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
    procedure DoApply;
  public
    { public declarations }
  end;

var
  IdeLoggerForm: TIdeLoggerForm;

resourcestring
  IdeLoggerMenuCaption = 'Logging ...';
  IdeLoggerCaption = 'Logging';
  IdeLoggerApply = '&Apply';

procedure Register;

implementation

{$R *.lfm}

{ TIdeLoggerForm }

procedure TIdeLoggerForm.FormCreate(Sender: TObject);
begin
  ButtonPanel1.CloseButton.Caption := IdeLoggerApply;
  ButtonPanel1.CloseButton.Kind := bkCustom;
  ButtonPanel1.CloseButton.OnClick := @CloseButtonClick;
  Caption := IdeLoggerCaption;
end;

procedure TIdeLoggerForm.CloseButtonClick(Sender: TObject);
begin
  DoApply;
end;

procedure TIdeLoggerForm.CheckLogGroupsClickCheck(Sender: TObject);
begin
  ButtonPanel1.CloseButton.Enabled := True;
end;

procedure TIdeLoggerForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TIdeLoggerForm.FormShow(Sender: TObject);
var
  i, j: Integer;
  e: PLazLoggerLogGroup;
begin
  CheckLogGroups.Clear;
  for i := 0 to DebugLogger.LogGroupList.Count - 1 do begin
    e := DebugLogger.LogGroupList[i];
    if lgfAddedByParamParser in e^.Flags then continue;
    j := CheckLogGroups.Items.Add(DebugLogger.LogGroupList[i]^.ConfigName);
    CheckLogGroups.Checked[j] := e^.Enabled;
  end;
  LogNameEdit.FileName := DebugLogger.LogName;
  ButtonPanel1.CloseButton.Enabled := False;
end;

procedure TIdeLoggerForm.LogNameEditChange(Sender: TObject);
begin
  ButtonPanel1.CloseButton.Enabled := True;
end;

procedure TIdeLoggerForm.OKButtonClick(Sender: TObject);
begin
  DoApply;
  Close;
end;

procedure TIdeLoggerForm.DoApply;
var
  i, j: Integer;
  e: PLazLoggerLogGroup;
begin
  for i := 0 to DebugLogger.LogGroupList.Count - 1 do begin
    e := DebugLogger.LogGroupList[i];
    if lgfAddedByParamParser in e^.Flags then continue;
    j := CheckLogGroups.Items.IndexOf(e^.ConfigName);
    if (j >= 0) then
      e^.Enabled := CheckLogGroups.Checked[j];
  end;
  DebugLogger.LogName := LogNameEdit.FileName;
end;

procedure IDELoggerMenuClicked(Sender: TObject);
begin
  IdeLoggerForm.Show;
end;

procedure Register;
begin
  IdeLoggerForm := TIdeLoggerForm.Create(Application);
  RegisterIDEMenuCommand(itmSecondaryTools, 'mnuIdeLogger', IdeLoggerMenuCaption, nil,
    @IDELoggerMenuClicked);
end;

end.

