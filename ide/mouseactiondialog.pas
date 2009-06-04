unit MouseActionDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel, SynEditMouseCmds, LazarusIDEStrConsts;

const
  ButtonName: Array [TMouseButton] of String =
    (dlgMouseOptBtnLeft, dlgMouseOptBtnRight, dlgMouseOptBtnMiddle);
  ClickName: Array [TSynMAClickCount] of String =
    (dlgMouseOptBtn1, dlgMouseOptBtn2, dlgMouseOptBtn3, dlgMouseOptBtn4, dlgMouseOptBtnAny);
  ButtonDirName: Array [TSynMAClickDir] of String =
    (dlgMouseOptBtnUp, dlgMouseOptBtnDown);

type

  { TMouseaActionDialog }

  TMouseaActionDialog = class(TForm)
    ActionBox: TComboBox;
    ActionLabel: TLabel;
    AltCheck: TCheckBox;
    BtnDefault: TButton;
    BtnLabel: TLabel;
    ButtonBox: TComboBox;
    ButtonPanel1: TButtonPanel;
    CaretCheck: TCheckBox;
    ClickBox: TComboBox;
    OptBox: TComboBox;
    CtrlCheck: TCheckBox;
    DirCheck: TCheckBox;
    CapturePanel: TPanel;
    OptLabel: TLabel;
    ShiftCheck: TCheckBox;
    procedure ActionBoxChange(Sender: TObject);
    procedure BtnDefaultClick(Sender: TObject);
    procedure CapturePanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  public
    { public declarations }
    Procedure ResetInputs;
    Procedure ReadFromAction(MAct: TSynEditMouseAction);
    Procedure WriteToAction(MAct: TSynEditMouseAction);
  end;

implementation

const
  BtnToIndex: array [mbLeft..mbMiddle] of Integer = (0, 1, 2);
  ClickToIndex: array [ccSingle..ccAny] of Integer = (0, 1, 2, 3, 4);
  IndexToBtn: array [0..2] of TMouseButton = (mbLeft, mbRight, mbMiddle);
  IndexToClick: array [0..4] of TSynMAClickCount = (ccSingle, ccDouble, ccTriple, ccQuad, ccAny);

{ MouseaActionDialog }

procedure TMouseaActionDialog.FormCreate(Sender: TObject);
var
  i: Integer;
  CName: String;
  mb: TMouseButton;
  cc: TSynMAClickCount;
begin
  Caption := dlgMouseOptDlgTitle;
  CapturePanel.Caption := dlgMouseOptCapture;
  CaretCheck.Caption := dlgMouseOptCaretMove;
  ActionBox.Clear;
  for i:= 0 to emcMax do begin
    CName := MouseCommandName(i);
    if CName <> '' then
      ActionBox.Items.AddObject(CName, TObject(ptrint(i)));
  end;
  ButtonBox.Clear;
  for mb := low(TMouseButton) to high(TMouseButton) do
    ButtonBox.Items.add(ButtonName[mb]);
  ClickBox.Clear;
  for cc:= low(TSynMAClickCount) to high(TSynMAClickCount) do
    ClickBox.Items.add(ClickName[cc]);
  DirCheck.Caption   := dlgMouseOptCheckUpDown;
  ShiftCheck.Caption := dlgMouseOptModShift;
  AltCheck.Caption   := dlgMouseOptModAlt;
  CtrlCheck.Caption  := dlgMouseOptModCtrl;
  ActionLabel.Caption := dlgMouseOptDescAction;
  BtnLabel.Caption := dlgMouseOptDescButton;
  BtnDefault.Caption :=  dlgMouseOptBtnModDef
end;

procedure TMouseaActionDialog.ResetInputs;
begin
  ActionBox.ItemIndex := 0;
  ButtonBox.ItemIndex := 0;
  ClickBox.ItemIndex  := 0;
  DirCheck.Checked    := False;
  ShiftCheck.State := cbGrayed;
  AltCheck.State := cbGrayed;
  CtrlCheck.State := cbGrayed;
end;

procedure TMouseaActionDialog.BtnDefaultClick(Sender: TObject);
begin
  ShiftCheck.State := cbGrayed;
  AltCheck.State := cbGrayed;
  CtrlCheck.State := cbGrayed;
end;

procedure TMouseaActionDialog.ActionBoxChange(Sender: TObject);
begin
  OptBox.Items.CommaText := MouseCommandConfigName
    (TSynEditorMouseCommand(PtrUInt(Pointer(ActionBox.items.Objects[ActionBox.ItemIndex]))));
  if OptBox.Items.Count > 0 then begin
    OptLabel.Caption := OptBox.Items[0];
    OptBox.Items.Delete(0);
    OptBox.Enabled := True;
    OptBox.ItemIndex := 0;
  end else begin
    OptLabel.Caption := '';
    OptBox.Enabled := False
  end;
end;

procedure TMouseaActionDialog.CapturePanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ButtonBox.ItemIndex := BtnToIndex[Button];
  ClickBox.ItemIndex := 0;
  if ssDouble in Shift then ClickBox.ItemIndex := 1;
  if ssTriple in Shift then ClickBox.ItemIndex := 2;
  if ssQuad in Shift then ClickBox.ItemIndex := 3;
  ShiftCheck.Checked := ssShift in Shift ;
  AltCheck.Checked   := ssAlt in Shift ;
  CtrlCheck.Checked  := ssCtrl in Shift ;
end;

procedure TMouseaActionDialog.ReadFromAction(MAct: TSynEditMouseAction);
begin
  ActionBox.ItemIndex := ActionBox.Items.IndexOfObject(TObject(Pointer(PtrUInt(MAct.Command))));
  ButtonBox.ItemIndex := BtnToIndex[MAct.Button];
  ClickBox.ItemIndex  := ClickToIndex[MAct.ClickCount];
  DirCheck.Checked    := MAct.ClickDir = cdUp;
  CaretCheck.Checked := MAct.MoveCaret;
  ShiftCheck.Checked := (ssShift in MAct.ShiftMask) and (ssShift in MAct.Shift);
  if not(ssShift in MAct.ShiftMask) then ShiftCheck.State := cbGrayed;
  AltCheck.Checked := (ssAlt in MAct.ShiftMask) and (ssAlt in MAct.Shift);
  if not(ssAlt in MAct.ShiftMask) then AltCheck.State := cbGrayed;
  CtrlCheck.Checked := (ssCtrl in MAct.ShiftMask) and (ssCtrl in MAct.Shift);
  if not(ssCtrl in MAct.ShiftMask) then CtrlCheck.State := cbGrayed;

  OptBox.Items.CommaText := MouseCommandConfigName(MAct.Command);
  if OptBox.Items.Count > 0 then begin
    OptLabel.Caption := OptBox.Items[0];
    OptBox.Items.Delete(0);
    OptBox.Enabled := True;
    OptBox.ItemIndex := MAct.Option;
  end else begin
    OptLabel.Caption := '';
    OptBox.Enabled := False
  end;
end;

procedure TMouseaActionDialog.WriteToAction(MAct: TSynEditMouseAction);
begin
  MAct.Command := TSynEditorMouseCommand(PtrUInt(Pointer(ActionBox.items.Objects[ActionBox.ItemIndex])));
  MAct.Button := IndexToBtn[ButtonBox.ItemIndex];
  MAct.ClickCount := IndexToClick[ClickBox.ItemIndex];
  MAct.MoveCaret := CaretCheck.Checked;
  if DirCheck.Checked
  then MAct.ClickDir := cdUp
  else MAct.ClickDir := cdDown;
  MAct.Shift := [];
  MAct.ShiftMask := [];
  if ShiftCheck.State <> cbGrayed then MAct.ShiftMask := MAct.ShiftMask + [ssShift];
  if AltCheck.State <> cbGrayed then MAct.ShiftMask := MAct.ShiftMask + [ssAlt];
  if CtrlCheck.State <> cbGrayed then MAct.ShiftMask := MAct.ShiftMask + [ssCtrl];
  if ShiftCheck.Checked then MAct.Shift := MAct.Shift + [ssShift];
  if AltCheck.Checked then MAct.Shift := MAct.Shift + [ssAlt];
  if CtrlCheck.Checked then MAct.Shift := MAct.Shift + [ssCtrl];

  if OptBox.Enabled then
    MAct.Option := OptBox.ItemIndex;
end;

initialization
  {$I mouseactiondialog.lrs}

end.

