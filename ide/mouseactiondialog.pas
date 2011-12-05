unit MouseActionDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, ExtCtrls, StdCtrls, ButtonPanel, Spin,
  SynEditMouseCmds, LazarusIDEStrConsts, KeyMapping, IDECommands;

var
  ButtonName: Array [TSynMouseButton] of String;
  ClickName: Array [TSynMAClickCount] of String;
  ButtonDirName: Array [TSynMAClickDir] of String;

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
    PriorLabel: TLabel;
    OptBox: TComboBox;
    CtrlCheck: TCheckBox;
    DirCheck: TCheckBox;
    CapturePanel: TPanel;
    OptLabel: TLabel;
    ShiftCheck: TCheckBox;
    PriorSpin: TSpinEdit;
    procedure ActionBoxChange(Sender: TObject);
    procedure BtnDefaultClick(Sender: TObject);
    procedure ButtonBoxChange(Sender: TObject);
    procedure CapturePanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    FKeyMap: TKeyCommandRelationList;
  public
    { public declarations }
    Procedure ResetInputs;
    Procedure ReadFromAction(MAct: TSynEditMouseAction);
    Procedure WriteToAction(MAct: TSynEditMouseAction);
    property KeyMap: TKeyCommandRelationList read FKeyMap write FKeyMap;
  end;

  function KeyMapIndexOfCommand(AKeyMap: TKeyCommandRelationList; ACmd: Word) : Integer;

implementation

{$R *.lfm}

const
  BtnToIndex: array [TSynMouseButton] of Integer = (0, 1, 2, 3, 4, 5, 6);
  ClickToIndex: array [ccSingle..ccAny] of Integer = (0, 1, 2, 3, 4);
  IndexToBtn: array [0..6] of TSynMouseButton = (mbLeft, mbRight, mbMiddle, mbExtra1, mbExtra2, mbWheelUp, mbWheelDown);
  IndexToClick: array [0..4] of TSynMAClickCount = (ccSingle, ccDouble, ccTriple, ccQuad, ccAny);

function KeyMapIndexOfCommand(AKeyMap: TKeyCommandRelationList; ACmd: Word): Integer;
var
  i: Integer;
begin
  for i := 0 to AKeyMap.RelationCount - 1 do
    if AKeyMap.Relations[i].Command = ACmd then
      exit(i);
  Result := -1;
end;

{ MouseaActionDialog }

procedure TMouseaActionDialog.FormCreate(Sender: TObject);
var
  i: Integer;
  CName: String;
  mb: TSynMouseButton;
  cc: TSynMAClickCount;
begin
  ButtonName[mbLeft]:=dlgMouseOptBtnLeft;
  ButtonName[mbRight]:=dlgMouseOptBtnRight;
  ButtonName[mbMiddle]:=dlgMouseOptBtnMiddle;
  ButtonName[mbExtra1]:=dlgMouseOptBtnExtra1;
  ButtonName[mbExtra2]:=dlgMouseOptBtnExtra2;
  ButtonName[mbWheelUp]:=dlgMouseOptBtnWheelUp;
  ButtonName[mbWheelDown]:=dlgMouseOptBtnWheelDown;

  ClickName[ccSingle]:=dlgMouseOptBtn1;
  ClickName[ccDouble]:=dlgMouseOptBtn2;
  ClickName[ccTriple]:=dlgMouseOptBtn3;
  ClickName[ccQuad]:=dlgMouseOptBtn4;
  ClickName[ccAny]:=dlgMouseOptBtnAny;

  ButtonDirName[cdUp]:=dlgMouseOptBtnUp;
  ButtonDirName[cdDown]:=dlgMouseOptBtnDown;

  Caption := dlgMouseOptDlgTitle;
  CapturePanel.Caption := dlgMouseOptCapture;
  CapturePanel.ControlStyle := ControlStyle + [csTripleClicks, csQuadClicks];
  CaretCheck.Caption := dlgMouseOptCaretMove;
  ActionBox.Clear;
  for i:= 0 to emcMax do begin
    CName := MouseCommandName(i);
    if CName <> '' then
      ActionBox.Items.AddObject(CName, TObject(ptrint(i)));
  end;
  ButtonBox.Clear;
  for mb := low(TSynMouseButton) to high(TSynMouseButton) do
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
  BtnDefault.Caption :=  dlgMouseOptBtnModDef;
  PriorLabel.Caption := dlgMouseOptPriorLabel;
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

  ActionBoxChange(nil);
  OptBox.ItemIndex := 0;
end;

procedure TMouseaActionDialog.BtnDefaultClick(Sender: TObject);
begin
  ShiftCheck.State := cbGrayed;
  AltCheck.State := cbGrayed;
  CtrlCheck.State := cbGrayed;
end;

procedure TMouseaActionDialog.ButtonBoxChange(Sender: TObject);
begin
  DirCheck.Enabled := not(IndexToBtn[ButtonBox.ItemIndex] in [mbWheelUp, mbWheelDown]);
end;

procedure TMouseaActionDialog.ActionBoxChange(Sender: TObject);
var
  ACmd: TSynEditorMouseCommand;
  i: Integer;
begin
  OptBox.Items.Clear;
  ACmd := TSynEditorMouseCommand(PtrUInt(Pointer(ActionBox.items.Objects[ActionBox.ItemIndex])));
  if ACmd =  emcSynEditCommand then begin
    OptBox.Enabled := True;
    OptBox.Clear;
    for i := 0 to KeyMap.RelationCount - 1 do
      if (KeyMap.Relations[i].Category.Scope = IDECmdScopeSrcEdit) or
         (KeyMap.Relations[i].Category.Scope = IDECmdScopeSrcEditOnly)
      then
        OptBox.Items.AddObject(KeyMap.Relations[i].GetLocalizedName,
                               TObject(Pointer(PtrUInt(KeyMap.Relations[i].Command))));
    OptLabel.Caption := dlgMouseOptionsynCommand;
    OptBox.ItemIndex := 0;
  end
  else
  begin
    OptBox.Items.CommaText := MouseCommandConfigName(ACmd);
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
end;

procedure TMouseaActionDialog.CapturePanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ButtonBox.ItemIndex := BtnToIndex[SynMouseButtonMap[Button]];
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
  PriorSpin.Value := MAct.Priority;

  ActionBoxChange(nil);
  ButtonBoxChange(nil);
  if OptBox.Enabled then begin
    if MAct.Command =  emcSynEditCommand then
      OptBox.ItemIndex := OptBox.Items.IndexOfObject(TObject(Pointer(PtrUInt(MAct.Option))))
    else
      OptBox.ItemIndex := MAct.Option;
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
  MAct.Priority := PriorSpin.Value;

  if OptBox.Enabled then begin
    if MAct.Command =  emcSynEditCommand then begin
      MAct.Option := TSynEditorMouseCommandOpt(PtrUInt(Pointer(OptBox.Items.Objects[OptBox.ItemIndex])));
    end
    else
      MAct.Option := OptBox.ItemIndex;
  end
  else
    MAct.Option := 0;
end;

end.

