unit IdeDbgValueFormatterSettingsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, CheckLst, StdCtrls, Dialogs,
  StrUtils, IdeDebuggerValueFormatterIntf, LazDebuggerIntf,
  IdeDebuggerStringConstants, IdeDebuggerValueFormatter, IdeDebuggerUtils;

type

  { TIdeDbgVarFormatterFrame }

  TIdeDbgVarFormatterFrame = class(TFrame)
    btnAdd: TButton;
    btnDown: TButton;
    btnRemove: TButton;
    btnUp: TButton;
    cbAppendOriginalValue: TComboBox;
    clDisplayFormat: TCheckListBox;
    dropAction: TComboBox;
    EdName: TEdit;
    lblDesc: TLabel;
    lblName: TLabel;
    lblTypeNames: TLabel;
    lstFormatters: TCheckListBox;
    memoTypeNames: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel5: TPanel;
    pnlCurFormatterSetting: TPanel;
    pnlCurrentFormatter: TPanel;
    Splitter1: TSplitter;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure EdNameEditingDone(Sender: TObject);
    procedure lstFormattersClick(Sender: TObject);
    procedure lstFormattersItemClick(Sender: TObject; Index: integer);
  private
    FValFormmaterList: TIdeDbgValueFormatterSelectorList;
    FCurIdx: Integer;
    FCurFormatter: TIdeDbgValueFormatterSelector;
    FCurFormatterFrame: TFrame; // ILazDbgValueConverterSettingsFrameIntf

    procedure SetCurFormatter(AValFormatter: TIdeDbgValueFormatterSelector);
    procedure UpdateFormatterPanel;
    procedure FillList;
    procedure UpdateButtons;
    procedure SetValFormmaterList(AValue: TIdeDbgValueFormatterSelectorList);
    function  UniqueName(AName: String; AnIgnoreCurrent: Boolean): String;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveCurrent;
    procedure Setup;

    property ValFormmaterList: TIdeDbgValueFormatterSelectorList read FValFormmaterList write SetValFormmaterList;

  end;

implementation

{$R *.lfm}

{ TIdeDbgVarFormatterFrame }

procedure TIdeDbgVarFormatterFrame.btnAddClick(Sender: TObject);
var
  AName: String;
  obj: TIdeDbgValueFormatterSelector;
  i: Integer;
begin
  AName := InputBox(dlgIdeDbgNewItem, dlgIdeDbgEnterName, '');
  if AName = '' then
    exit;
  AName := UniqueName(AName, False);

  SaveCurrent;
  FCurFormatter := nil;

  i := dropAction.ItemIndex;
  obj := TIdeDbgValueFormatterSelector.Create(TLazDbgIdeValueFormatterRegistryEntryClass(dropAction.Items.Objects[i]));
  obj.Enabled := True;
  obj.Name := AName;
  FValFormmaterList.Add(obj);
  FValFormmaterList.Changed := True;
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
  FValFormmaterList.Delete(i);
  FValFormmaterList.Changed := True;

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
  if (i < 0) or (i >= FValFormmaterList.Count-1) then
    exit;
  FValFormmaterList.Move(i, i+1);
  FValFormmaterList.Changed := True;

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
  if (i < 1) or (i > FValFormmaterList.Count-1) then
    exit;
  FValFormmaterList.Move(i, i-1);
  FValFormmaterList.Changed := True;

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
var
  i: Integer;
begin
  SaveCurrent;
  pnlCurrentFormatter.Enabled := lstFormatters.Count > 0;

  FCurIdx := lstFormatters.ItemIndex;
  if (FCurIdx >= FValFormmaterList.Count) or (FCurIdx < 0) then begin
    FCurIdx := -1;
    FCurFormatter := nil;
    lblDesc.Caption := '';
    EdName.Text := '';
    memoTypeNames.Text := '';
  end
  else begin
    SetCurFormatter(FValFormmaterList[FCurIdx]);
    lblDesc.Caption := FCurFormatter.ValFormatterRegEntry.GetDisplayName;
    EdName.Text := FCurFormatter.Name;
    memoTypeNames.Text := FCurFormatter.MatchTypeNames.Text;
  end;

  cbAppendOriginalValue.ItemIndex := ord(FCurFormatter.OriginalValue);

  for i := 0 to clDisplayFormat.Count - 1 do
    clDisplayFormat.Checked[i] :=
      TWatchDisplayFormat(ptrUInt(clDisplayFormat.Items.Objects[i])) in FCurFormatter.FilterDisplayFormat;

  btnRemove.Enabled := FCurIdx >= 0;
  btnUp.Enabled     := FCurIdx >= 1;
  btnDown.Enabled   := FCurIdx < FValFormmaterList.Count - 1;

  lstFormattersItemClick(nil, FCurIdx);
end;

procedure TIdeDbgVarFormatterFrame.lstFormattersItemClick(Sender: TObject;
  Index: integer);
begin
  if Index < 0 then
    exit;

  if (FValFormmaterList[Index].Enabled <> lstFormatters.Checked[Index]) then begin
    FValFormmaterList[Index].Enabled := lstFormatters.Checked[Index];
    FValFormmaterList.Changed := True;
  end;

  if FCurIdx <> lstFormatters.ItemIndex then
    lstFormattersClick(nil);

  UpdateButtons;
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

procedure TIdeDbgVarFormatterFrame.SetValFormmaterList(
  AValue: TIdeDbgValueFormatterSelectorList);
begin
  if FValFormmaterList = AValue then Exit;
  FValFormmaterList := AValue;
  FCurFormatter := nil;

  FillList;
  if FValFormmaterList.Count > 0 then begin
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
  if (FValFormmaterList.IndexOf(AName) < 0) or
     ( AnIgnoreCurrent and (FCurFormatter <> nil) and (AName = FCurFormatter.Name) )
  then
    exit;

  i := 1;
  s := AName+'('+IntToStr(i)+')';
  while (FValFormmaterList.IndexOf(s) >= 0) and
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
  for i := 0 to FValFormmaterList.Count - 1 do begin
    obj := FValFormmaterList[i];
    lstFormatters.AddItem(obj.Name, nil);
    lstFormatters.Checked[i] := obj.Enabled;
  end;
end;

procedure TIdeDbgVarFormatterFrame.UpdateButtons;
begin
  btnAdd.Enabled := dropAction.Items.Count > 0;
  btnRemove.Enabled := (lstFormatters.Count > 0) and (lstFormatters.ItemIndex >= 0);
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
var
  df: TWatchDisplayFormats;
  i: Integer;
begin
  if (FCurFormatter = nil) or (FCurFormatter.ValFormatter = nil) then
    exit;

  if ( (FCurFormatterFrame <> nil) and
        (FCurFormatterFrame as ILazDbgIdeValueFormatterSettingsFrameIntf).WriteTo(FCurFormatter.ValFormatter)
     ) or
     (TrimSet(FCurFormatter.MatchTypeNames.Text, [#1..#32]) <> TrimSet(memoTypeNames.Text, [#1..#32])) or
     (EdName.Text <> FCurFormatter.Name)
  then begin
    FValFormmaterList.Changed := True;
    FCurFormatter.MatchTypeNames.Text := memoTypeNames.Text;
    FCurFormatter.Name := EdName.Text
  end;


  if FCurFormatter.OriginalValue <> TLazDbgIdeValFormatterOriginalValue(cbAppendOriginalValue.ItemIndex) then
    FValFormmaterList.Changed := True;
  FCurFormatter.OriginalValue := TLazDbgIdeValFormatterOriginalValue(cbAppendOriginalValue.ItemIndex);


  df := [];
  for i := 0 to clDisplayFormat.Count - 1 do
    if clDisplayFormat.Checked[i] then
      df := df + [TWatchDisplayFormat(ptrUInt(clDisplayFormat.Items.Objects[i]))];
  if FCurFormatter.FilterDisplayFormat <> df then
    FValFormmaterList.Changed := True;
  FCurFormatter.FilterDisplayFormat := df;
end;

procedure TIdeDbgVarFormatterFrame.Setup;
var
  AvailClass: TLazDbgIdeValueFormatterRegistry;
  i: Integer;
  df: TWatchDisplayFormat;
begin
  btnAdd.Caption       := dlgBackConvOptAddNew;
  btnRemove.Caption    := dlgBackConvOptRemove;
  lblName.Caption      := dlgBackConvOptName;
  lblTypeNames.Caption := dlgBackConvOptMatchTypesByName;

  cbAppendOriginalValue.Clear;
  // same order as enum / ItemIndex to match ord(enum)
  cbAppendOriginalValue.AddItem(ValFormatterOrigValHide, nil); // vfovHide;
  cbAppendOriginalValue.AddItem(ValFormatterOrigValAtEnd, nil); // vfovAtEnd
  cbAppendOriginalValue.AddItem(ValFormatterOrigValAtStart, nil); // vfovAtFront;

  clDisplayFormat.Clear;
  for df in TWatchDisplayFormat do
    if df <> wdfFloat then
      clDisplayFormat.AddItem(DisplayFormatName(df), TObject(PtrUInt(ord(df))));

  FCurFormatter := nil;
  lblDesc.Caption := '';

  dropAction.Clear;
  AvailClass := ValueFormatterRegistry;
  for i := 0 to AvailClass.Count - 1 do
    dropAction.AddItem(AvailClass[i].GetDisplayName, TObject(AvailClass[i]));
  if AvailClass.Count > 0 then
    dropAction.ItemIndex := 0;

  UpdateButtons;
end;

end.

