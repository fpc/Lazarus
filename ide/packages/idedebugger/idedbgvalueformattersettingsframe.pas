unit IdeDbgValueFormatterSettingsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, CheckLst, StdCtrls, Dialogs, Spin,
  StrUtils, IdeDebuggerValueFormatterIntf, IdeDebuggerWatchValueIntf,
  laz.VirtualTrees, SelectItemDialog, LazDebuggerIntf, IdeDebuggerStringConstants,
  IdeDebuggerValueFormatter, IdeDebuggerDisplayFormats;

type

  { TIdeDbgVarFormatterFrame }

  TIdeDbgVarFormatterFrame = class(TFrame)
    btnAdd: TButton;
    btnDown: TButton;
    btnRemove: TButton;
    btnUp: TButton;
    cbAppendOriginalValue: TComboBox;
    cbNesting: TCheckBox;
    EdName: TEdit;
    lblNestLvl: TLabel;
    lblDesc: TLabel;
    lblName: TLabel;
    lblTypeNames: TLabel;
    lstFormatters: TCheckListBox;
    memoTypeNames: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelOpts: TPanel;
    Panel5: TPanel;
    pnlCurFormatterSetting: TPanel;
    pnlCurrentFormatter: TPanel;
    SpinMinNest: TSpinEdit;
    SpinMaxNest: TSpinEdit;
    Splitter1: TSplitter;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure EdNameEditingDone(Sender: TObject);
    procedure lstFormattersClick(Sender: TObject);
    procedure lstFormattersItemClick(Sender: TObject; Index: integer);
    procedure SpinMaxNestChange(Sender: TObject);
    procedure SpinMinNestChange(Sender: TObject);
  private
    FValFormatterList: TIdeDbgValueFormatterSelectorList;
    FCurIdx: Integer;
    FCurFormatter: TIdeDbgValueFormatterSelector;
    FCurFormatterFrame: TFrame; // ILazDbgValueConverterSettingsFrameIntf

    procedure SetCurFormatter(AValFormatter: TIdeDbgValueFormatterSelector);
    procedure UpdateFormatterPanel;
    procedure FillList;
    procedure UpdateButtons;
    procedure SetValFormatterList(AValue: TIdeDbgValueFormatterSelectorList);
    function  UniqueName(AName: String; AnIgnoreCurrent: Boolean): String;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveCurrent;
    procedure Setup;

    property ValFormatterList: TIdeDbgValueFormatterSelectorList read FValFormatterList write SetValFormatterList;

  end;

implementation

{$R *.lfm}

type
  TVtvData = record
    IsGrp: boolean;
    Grp: TValueDisplayFormatGroup;
    DispForm: TValueDisplayFormat;
  end;
  PVtvData = ^TVtvData;

{ TIdeDbgVarFormatterFrame }

procedure TIdeDbgVarFormatterFrame.btnAddClick(Sender: TObject);
var
  AClassNames: array of string;
  AName: String;
  obj: TIdeDbgValueFormatterSelector;
  i: Integer;
begin
  AClassNames := nil;
  SetLength(AClassNames, ValueFormatterRegistry.Count);
  for i := 0 to ValueFormatterRegistry.Count - 1 do
    AClassNames[i] := ValueFormatterRegistry[i].GetDisplayName;

  AName := '';
  i := ShowChooseItemDialog(DbgOptFrmChooseFormatter, DbgOptFrmPleaseChooseTheFormatterT,
    DbgOptFrmFormatterClass, DbgOptFrmName, AName, AClassNames, '---');

  if (AName = '') or (i < 0) then
    exit;
  AName := UniqueName(AName, False);

  SaveCurrent;
  FCurFormatter := nil;

  obj := TIdeDbgValueFormatterSelector.Create(ValueFormatterRegistry[i]);
  obj.Enabled := True;
  obj.Name := AName;
  FValFormatterList.Add(obj);
  FValFormatterList.Changed := True;
  FillList;
  lstFormatters.ItemIndex := lstFormatters.Count-1;
  lstFormattersClick(nil);
  memoTypeNames.SetFocus;
end;

procedure TIdeDbgVarFormatterFrame.btnRemoveClick(Sender: TObject);
var
  i: Integer;
begin
  SaveCurrent;
  FCurFormatter := nil;
  i := lstFormatters.ItemIndex;
  FValFormatterList.Delete(i);
  FValFormatterList.Changed := True;

  FillList;
  if i >= lstFormatters.Count then
    dec(i);
  lstFormatters.ItemIndex := i;
  lstFormattersClick(nil);
end;

procedure TIdeDbgVarFormatterFrame.btnDownClick(Sender: TObject);
var
  i: Integer;
begin
  SaveCurrent;
  FCurFormatter := nil;
  i := lstFormatters.ItemIndex;
  if (i < 0) or (i >= FValFormatterList.Count-1) then
    exit;
  FValFormatterList.Move(i, i+1);
  FValFormatterList.Changed := True;

  FillList;
  lstFormatters.ItemIndex := i+1;
  lstFormattersClick(nil);
end;

procedure TIdeDbgVarFormatterFrame.btnUpClick(Sender: TObject);
var
  i: Integer;
begin
  SaveCurrent;
  FCurFormatter := nil;
  i := lstFormatters.ItemIndex;
  if (i < 1) or (i > FValFormatterList.Count-1) then
    exit;
  FValFormatterList.Move(i, i-1);
  FValFormatterList.Changed := True;

  FillList;
  lstFormatters.ItemIndex := i-1;
  lstFormattersClick(nil);
end;

procedure TIdeDbgVarFormatterFrame.EdNameEditingDone(Sender: TObject);
var
  s: String;
begin
  s := UniqueName(EdName.Text, True);
  if s <> EdName.Text then
    EdName.Text := s;
end;

procedure TIdeDbgVarFormatterFrame.lstFormattersClick(Sender: TObject);
begin
  SaveCurrent;
  pnlCurrentFormatter.Enabled := lstFormatters.Count > 0;

  FCurIdx := lstFormatters.ItemIndex;
  if (FCurIdx >= FValFormatterList.Count) or (FCurIdx < 0) then begin
    FCurIdx := -1;
    FCurFormatter := nil;
    lblDesc.Caption := '';
    EdName.Text := '';
    memoTypeNames.Text := '';

    PanelOpts.Enabled := False;
    cbNesting.Checked := False;
    SpinMinNest.Value := 0;
    SpinMaxNest.Value := 0;

    cbAppendOriginalValue.ItemIndex := -1;
    cbAppendOriginalValue.Enabled := False;
  end
  else begin
    pnlCurrentFormatter.Enabled := True;
    SetCurFormatter(FValFormatterList[FCurIdx]);
    lblDesc.Caption := FCurFormatter.ValFormatterRegEntry.GetDisplayName;
    EdName.Text := FCurFormatter.Name;
    memoTypeNames.Text := FCurFormatter.MatchTypeNames.Text;

    PanelOpts.Enabled := True;
    cbNesting.Checked := FCurFormatter.LimitByNestLevel;
    SpinMinNest.Value := FCurFormatter.LimitByNestMin;
    SpinMaxNest.Value := FCurFormatter.LimitByNestMax;

    if (FCurFormatter.ValFormatter = nil) or
       (vffPreventOrigValue in FCurFormatter.ValFormatter.SupportedFeatures)
    then begin
      cbAppendOriginalValue.ItemIndex := -1;
      cbAppendOriginalValue.Enabled := False;
    end
    else begin
      cbAppendOriginalValue.ItemIndex := ord(FCurFormatter.OriginalValue);
      cbAppendOriginalValue.Enabled := True;
    end;
  end;


  btnRemove.Enabled := FCurIdx >= 0;
  btnUp.Enabled     := FCurIdx >= 1;
  btnDown.Enabled   := FCurIdx < FValFormatterList.Count - 1;
  pnlCurrentFormatter.Enabled := FCurIdx >= 0;
  pnlCurFormatterSetting.Enabled := FCurFormatter <> nil;

  lstFormattersItemClick(nil, FCurIdx);
end;

procedure TIdeDbgVarFormatterFrame.lstFormattersItemClick(Sender: TObject;
  Index: integer);
begin
  if Index < 0 then begin
    exit;
  end;

  if (FValFormatterList[Index].Enabled <> lstFormatters.Checked[Index]) then begin
    FValFormatterList[Index].Enabled := lstFormatters.Checked[Index];
    FValFormatterList.Changed := True;
  end;

  if FCurIdx <> lstFormatters.ItemIndex then
    lstFormattersClick(nil);

  UpdateButtons;
end;

procedure TIdeDbgVarFormatterFrame.SpinMaxNestChange(Sender: TObject);
begin
  if SpinMinNest.Value > SpinMaxNest.Value then
    SpinMinNest.Value := SpinMaxNest.Value;
end;

procedure TIdeDbgVarFormatterFrame.SpinMinNestChange(Sender: TObject);
begin
  if SpinMaxNest.Value < SpinMinNest.Value then
    SpinMaxNest.Value := SpinMinNest.Value;
end;

procedure TIdeDbgVarFormatterFrame.SetCurFormatter(
  AValFormatter: TIdeDbgValueFormatterSelector);
begin
  FCurFormatter := AValFormatter;
  UpdateFormatterPanel;
end;

procedure TIdeDbgVarFormatterFrame.UpdateFormatterPanel;
type
  TFrameClass = class of TFrame;
var
  C: TClass;
begin
  if FCurFormatterFrame <> nil then
    FreeAndNil(FCurFormatterFrame);

  C := FCurFormatter.ValFormatterRegEntry.GetSettingsFrameClass;
  if (C <> nil) and (FCurFormatter.ValFormatter <> nil) then begin
    FCurFormatterFrame := TFrameClass(C).Create(nil);
    FCurFormatterFrame.Parent := pnlCurFormatterSetting;
    FCurFormatterFrame.Align := alClient;
    (FCurFormatterFrame as ILazDbgIdeValueFormatterSettingsFrameIntf).ReadFrom(FCurFormatter.ValFormatter);
  end;
end;

procedure TIdeDbgVarFormatterFrame.SetValFormatterList(
  AValue: TIdeDbgValueFormatterSelectorList);
begin
  if FValFormatterList = AValue then Exit;
  FValFormatterList := AValue;
  FCurFormatter := nil;

  FillList;
  if FValFormatterList.Count > 0 then begin
    lstFormatters.ItemIndex := 0;
    lstFormattersClick(nil);
  end;
  UpdateButtons;
end;

function TIdeDbgVarFormatterFrame.UniqueName(AName: String;
  AnIgnoreCurrent: Boolean): String;
var
  i: Integer;
  s: String;
begin
  Result := AName;
  if (FValFormatterList.IndexOf(AName) < 0) or
     ( AnIgnoreCurrent and (FCurFormatter <> nil) and (AName = FCurFormatter.Name) )
  then
    exit;

  i := 1;
  s := AName+'('+IntToStr(i)+')';
  while (FValFormatterList.IndexOf(s) >= 0) and
     not( AnIgnoreCurrent and (FCurFormatter <> nil) and (s = FCurFormatter.Name) )
  do begin
    inc(i);
    s := AName+'('+IntToStr(i)+')';
  end;

  Result := s;
end;

procedure TIdeDbgVarFormatterFrame.FillList;
var
  i: Integer;
  obj: TIdeDbgValueFormatterSelector;
begin
  FCurFormatter := nil;

  lstFormatters.Clear;
  for i := 0 to FValFormatterList.Count - 1 do begin
    obj := FValFormatterList[i];
    lstFormatters.AddItem(obj.Name, nil);
    lstFormatters.Checked[i] := obj.Enabled;
  end;
end;

procedure TIdeDbgVarFormatterFrame.UpdateButtons;
begin
  btnAdd.Enabled := ValueFormatterRegistry.Count > 0;
  btnRemove.Enabled := (lstFormatters.Count > 0) and (lstFormatters.ItemIndex >= 0);
  pnlCurrentFormatter.Enabled := FCurFormatter <> nil;
  pnlCurFormatterSetting.Enabled := FCurFormatter <> nil;
end;

constructor TIdeDbgVarFormatterFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Setup;
end;

destructor TIdeDbgVarFormatterFrame.Destroy;
begin
  if FCurFormatterFrame <> nil then
    FreeAndNil(FCurFormatterFrame);
  inherited Destroy;
end;

procedure TIdeDbgVarFormatterFrame.SaveCurrent;
begin
  if (FCurFormatter = nil) or (FCurFormatter.ValFormatter = nil) then
    exit;

  if ( (FCurFormatterFrame <> nil) and
        (FCurFormatterFrame as ILazDbgIdeValueFormatterSettingsFrameIntf).WriteTo(FCurFormatter.ValFormatter)
     ) or
     (TrimSet(FCurFormatter.MatchTypeNames.Text, [#1..#32]) <> TrimSet(memoTypeNames.Text, [#1..#32])) or
     (EdName.Text <> FCurFormatter.Name) or
     (FCurFormatter.LimitByNestLevel <> cbNesting.Checked) or
     (FCurFormatter.LimitByNestMin <> SpinMinNest.Value) or
     (FCurFormatter.LimitByNestMax <> SpinMaxNest.Value)
  then begin
    FValFormatterList.Changed := True;
    FCurFormatter.MatchTypeNames.Text := memoTypeNames.Text;
    FCurFormatter.Name := EdName.Text;

    FCurFormatter.LimitByNestLevel := cbNesting.Checked;
    FCurFormatter.LimitByNestMin := SpinMinNest.Value;
    FCurFormatter.LimitByNestMax := SpinMaxNest.Value;
  end;


  if FCurFormatter.OriginalValue <> TLazDbgIdeValFormatterOriginalValue(cbAppendOriginalValue.ItemIndex) then
    FValFormatterList.Changed := True;
  FCurFormatter.OriginalValue := TLazDbgIdeValFormatterOriginalValue(cbAppendOriginalValue.ItemIndex);
end;

procedure TIdeDbgVarFormatterFrame.Setup;
var
  AvailClass: TLazDbgIdeValueFormatterRegistry;
  i: Integer;
begin
  btnAdd.Caption       := dlgBackConvOptAddNew;
  btnRemove.Caption    := dlgBackConvOptRemove;
  btnUp.Caption        := dlgSortUp;
  btnDown.Caption      := dlgSortDown;
  lblName.Caption      := dlgBackConvOptName;
  lblTypeNames.Caption := dlgBackConvOptMatchTypesByName;

  cbNesting.Caption := dlgBackConvOptNesting;
  lblNestLvl.Caption := dlgBackConvOptNestLvl;

  cbAppendOriginalValue.Clear;
  // same order as enum / ItemIndex to match ord(enum)
  cbAppendOriginalValue.AddItem(ValFormatterOrigValHide, nil); // vfovHide;
  cbAppendOriginalValue.AddItem(ValFormatterOrigValAtEnd, nil); // vfovAtEnd
  cbAppendOriginalValue.AddItem(ValFormatterOrigValAtStart, nil); // vfovAtFront;

  FCurFormatter := nil;
  lblDesc.Caption := '';

  UpdateButtons;
end;

end.

