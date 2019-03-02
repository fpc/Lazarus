unit MouseActionDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, ExtCtrls, StdCtrls, ButtonPanel, Spin, CheckLst,
  SynEditMouseCmds, LazarusIDEStrConsts, KeyMapping, IDECommands, types;

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
    chkUpRestrict: TCheckListBox;
    ClickBox: TComboBox;
    DirCheck: TCheckBox;
    PaintBox1: TPaintBox;
    PriorLabel: TLabel;
    OptBox: TComboBox;
    CtrlCheck: TCheckBox;
    CapturePanel: TPanel;
    OptLabel: TLabel;
    Opt2Spin: TSpinEdit;
    Opt2Label: TLabel;
    ShiftCheck: TCheckBox;
    PriorSpin: TSpinEdit;
    procedure ActionBoxChange(Sender: TObject);
    procedure BtnDefaultClick(Sender: TObject);
    procedure ButtonBoxChange(Sender: TObject);
    procedure CapturePanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      {%H-}X, {%H-}Y: Integer);
    procedure DirCheckChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
      {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure PaintBox1MouseWheelHorz(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    FKeyMap: TKeyCommandRelationList;
    procedure AddMouseCmd(const S: string);
    procedure FillListbox;
  public
    { public declarations }
    Procedure ResetInputs;
    Procedure ReadFromAction(MAct: TSynEditMouseAction);
    Procedure WriteToAction(MAct: TSynEditMouseAction);
    property KeyMap: TKeyCommandRelationList read FKeyMap write FKeyMap;
  end;

  function KeyMapIndexOfCommand(AKeyMap: TKeyCommandRelationList; ACmd: Word) : Integer;

implementation

uses Math;

{$R *.lfm}

const
  BtnToIndex: array [TSynMouseButton] of Integer = (0, 1, 2, 3, 4, 5, 6, 7 ,8);
  ClickToIndex: array [ccSingle..ccAny] of Integer = (0, 1, 2, 3, 4);
  IndexToBtn: array [0..8] of TSynMouseButton = (mbXLeft, mbXRight, mbXMiddle, mbXExtra1, mbXExtra2, mbXWheelUp, mbXWheelDown, mbXWheelLeft, mbXWheelRight);
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

procedure TMouseaActionDialog.AddMouseCmd(const S: string);
var
  i: Integer;
  s2: String;
begin
  i:=0;
  if IdentToSynMouseCmd(S, i) then begin
    s2 := MouseCommandName(i);
    if s2 = '' then s2 := s;
    ActionBox.Items.AddObject(s2, TObject(ptrint(i)));
  end;
end;

procedure TMouseaActionDialog.FillListbox;
const
  cCheckSize=35;
var
  r: TSynMAUpRestriction;
  s: string;
  i, Len: integer;
begin
  for r := low(TSynMAUpRestriction) to high(TSynMAUpRestriction) do
    case r of
      crLastDownPos:          chkUpRestrict.AddItem(synfMatchActionPosOfMouseDown, nil);
      crLastDownPosSameLine:  chkUpRestrict.AddItem(synfMatchActionLineOfMouseDown, nil);
      crLastDownPosSearchAll: chkUpRestrict.AddItem(synfSearchAllActionOfMouseDown, nil);
      crLastDownButton:       chkUpRestrict.AddItem(synfMatchActionButtonOfMouseDown, nil);
      crLastDownShift:        chkUpRestrict.AddItem(synfMatchActionModifiersOfMouseDown, nil);
      crAllowFallback:        chkUpRestrict.AddItem(synfContinueWithNextMouseUpAction, nil);
      else begin
        WriteStr(s, r);
        chkUpRestrict.AddItem(s, nil);
      end;
    end;

  // update scrollbar
  Len := 0;
  with chkUpRestrict do
  begin
    for i := 0 to Items.Count-1 do
      Len := Max(Len, Canvas.TextWidth(Items[i])+cCheckSize);
    ScrollWidth := Len;
  end;
end;

procedure TMouseaActionDialog.FormCreate(Sender: TObject);
var
  mb: TSynMouseButton;
  cc: TSynMAClickCount;
begin
  ButtonName[mbXLeft]:=dlgMouseOptBtnLeft;
  ButtonName[mbXRight]:=dlgMouseOptBtnRight;
  ButtonName[mbXMiddle]:=dlgMouseOptBtnMiddle;
  ButtonName[mbXExtra1]:=dlgMouseOptBtnExtra1;
  ButtonName[mbXExtra2]:=dlgMouseOptBtnExtra2;
  ButtonName[mbXWheelUp]:=dlgMouseOptBtnWheelUp;
  ButtonName[mbXWheelDown]:=dlgMouseOptBtnWheelDown;
  ButtonName[mbXWheelLeft]:=dlgMouseOptBtnWheelLeft;
  ButtonName[mbXWheelRight]:=dlgMouseOptBtnWheelRight;

  ClickName[ccSingle]:=dlgMouseOptBtn1;
  ClickName[ccDouble]:=dlgMouseOptBtn2;
  ClickName[ccTriple]:=dlgMouseOptBtn3;
  ClickName[ccQuad]:=dlgMouseOptBtn4;
  ClickName[ccAny]:=dlgMouseOptBtnAny;

  FillListbox;

  ButtonDirName[cdUp]:=lisUp;
  ButtonDirName[cdDown]:=lisDown;

  Caption := dlgMouseOptDlgTitle;
  CapturePanel.Caption := dlgMouseOptCapture;
  CapturePanel.ControlStyle := ControlStyle + [csTripleClicks, csQuadClicks];
  CaretCheck.Caption := dlgMouseOptCaretMove;
  ActionBox.Clear;
  GetEditorMouseCommandValues(@AddMouseCmd);
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
  Opt2Label.Caption := dlgMouseOptOpt2Label;
end;

procedure TMouseaActionDialog.ResetInputs;
var
  r: TSynMAUpRestriction;
begin
  ActionBox.ItemIndex := 0;
  ButtonBox.ItemIndex := 0;
  ClickBox.ItemIndex  := 0;
  DirCheck.Checked    := False;
  ShiftCheck.State := cbGrayed;
  AltCheck.State := cbGrayed;
  CtrlCheck.State := cbGrayed;
  for r := low(TSynMAUpRestriction) to high(TSynMAUpRestriction) do
    chkUpRestrict.Checked[ord(r)] := False;

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
  DirCheck.Enabled := not(IndexToBtn[ButtonBox.ItemIndex] in [mbXWheelUp, mbXWheelDown, mbXWheelLeft, mbXWheelRight]);
  chkUpRestrict.Enabled := DirCheck.Enabled and DirCheck.Checked;
end;

procedure TMouseaActionDialog.ActionBoxChange(Sender: TObject);
var
  ACmd: TSynEditorMouseCommand;
  i: Integer;
begin
  OptBox.Items.Clear;
  ACmd := TSynEditorMouseCommand({%H-}PtrUInt(ActionBox.items.Objects[ActionBox.ItemIndex]));
  if ACmd =  emcSynEditCommand then begin
    OptBox.Enabled := True;
    OptBox.Clear;
    for i := 0 to KeyMap.RelationCount - 1 do
      if (KeyMap.Relations[i].Category.Scope = IDECmdScopeSrcEdit) or
         (KeyMap.Relations[i].Category.Scope = IDECmdScopeSrcEditOnly)
      then
        OptBox.Items.AddObject(KeyMap.Relations[i].GetLocalizedName,
                               TObject({%H-}Pointer(PtrUInt(KeyMap.Relations[i].Command))));
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
  ShiftCheck.Checked := ssShift in Shift;
  AltCheck.Checked   := ssAlt in Shift;
  CtrlCheck.Checked  := ssCtrl in Shift;
end;

procedure TMouseaActionDialog.DirCheckChange(Sender: TObject);
begin
  chkUpRestrict.Enabled := DirCheck.Checked;
end;

procedure TMouseaActionDialog.PaintBox1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0
  then ButtonBox.ItemIndex := BtnToIndex[mbXWheelUp]
  else ButtonBox.ItemIndex := BtnToIndex[mbXWheelDown];
  ClickBox.ItemIndex := 4;
  ShiftCheck.Checked := ssShift in Shift;
  AltCheck.Checked   := ssAlt in Shift;
  CtrlCheck.Checked  := ssCtrl in Shift;
end;

procedure TMouseaActionDialog.PaintBox1MouseWheelHorz(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if WheelDelta < 0
  then ButtonBox.ItemIndex := BtnToIndex[mbXWheelLeft]
  else ButtonBox.ItemIndex := BtnToIndex[mbXWheelRight];
  ClickBox.ItemIndex := 4;
  ShiftCheck.Checked := ssShift in Shift;
  AltCheck.Checked   := ssAlt in Shift;
  CtrlCheck.Checked  := ssCtrl in Shift;
end;

procedure TMouseaActionDialog.ReadFromAction(MAct: TSynEditMouseAction);
var
  r: TSynMAUpRestriction;
begin
  ActionBox.ItemIndex := ActionBox.Items.IndexOfObject(TObject({%H-}Pointer(PtrUInt(MAct.Command))));
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
  Opt2Spin.Value := MAct.Option2;
  for r := low(TSynMAUpRestriction) to high(TSynMAUpRestriction) do
    chkUpRestrict.Checked[ord(r)] := r in MAct.ButtonUpRestrictions;

  ActionBoxChange(nil);
  ButtonBoxChange(nil);
  if OptBox.Enabled then begin
    if MAct.Command =  emcSynEditCommand then
      OptBox.ItemIndex := OptBox.Items.IndexOfObject(TObject({%H-}Pointer(PtrUInt(MAct.Option))))
    else
      OptBox.ItemIndex := MAct.Option;
  end;
end;

procedure TMouseaActionDialog.WriteToAction(MAct: TSynEditMouseAction);
var
  r: TSynMAUpRestriction;
begin
  MAct.Command := TSynEditorMouseCommand({%H-}PtrUInt(Pointer(ActionBox.items.Objects[ActionBox.ItemIndex])));
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
  MAct.Option2 := Opt2Spin.Value;
  MAct.ButtonUpRestrictions := [];
  for r := low(TSynMAUpRestriction) to high(TSynMAUpRestriction) do
    if chkUpRestrict.Checked[ord(r)] then
      MAct.ButtonUpRestrictions := MAct.ButtonUpRestrictions + [r];

  if OptBox.Enabled then begin
    if MAct.Command =  emcSynEditCommand then begin
      MAct.Option := TSynEditorMouseCommandOpt({%H-}PtrUInt(OptBox.Items.Objects[OptBox.ItemIndex]));
    end
    else
      MAct.Option := OptBox.ItemIndex;
  end
  else
    MAct.Option := 0;
end;

end.

