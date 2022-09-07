unit IdeDbgValueConverterSettingsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, CheckLst, LCLIntf,
  Dialogs, StrUtils, LazDebuggerValueConverter,
  IdeDebuggerStringConstants, IdeDebuggerBackendValueConv;

type

  { TIdeDbgValConvFrame }

  TIdeDbgValConvFrame = class(TFrame)
    btnAdd: TButton;
    btnUp: TButton;
    btnDown: TButton;
    btnRemove: TButton;
    EdName: TEdit;
    lblName: TLabel;
    lstConverters: TCheckListBox;
    dropAction: TComboBox;
    lblAction: TLabel;
    lblTypeNames: TLabel;
    lblDesc: TLabel;
    memoTypeNames: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlCurConvSetting: TPanel;
    pnlCurrentConv: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    procedure btnAddClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure dropActionChange(Sender: TObject);
    procedure lstConvertersClick(Sender: TObject);
    procedure lstConvertersItemClick(Sender: TObject; Index: integer);
    procedure Splitter1CanOffset(Sender: TObject; var NewOffset: Integer;
      var Accept: Boolean);
  private
    FValConvList: TIdeDbgValueConvertSelectorList;
    FCurIdx: Integer;
    FCurConvConf: TIdeDbgValueConvertSelector;
    FCurConv: TLazDbgValueConverterIntf;
    FCurConvSettings: TLazDbgValueConverterSettingsFrameIntf;

    procedure SetCurConv(AValConv: TIdeDbgValueConvertSelector);
    procedure UpdateConvForClass;
    procedure UpdateConvPanel;
    procedure FillList;
    procedure UpdateButtons;
    procedure SetValConvList(AValue: TIdeDbgValueConvertSelectorList);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveCurrent;
    procedure Setup;

    property ValConvList: TIdeDbgValueConvertSelectorList read FValConvList write SetValConvList;
  end;

implementation

{$R *.lfm}

{ TIdeDbgValConvFrame }

procedure TIdeDbgValConvFrame.btnAddClick(Sender: TObject);
var
  AvailClass: TLazDbgValueConvertRegistry;
  AName: String;
  obj: TIdeDbgValueConvertSelector;
begin
  AName := InputBox(dlgIdeDbgNewItem, dlgIdeDbgEnterName, '');
  if AName = '' then
    exit;

  SaveCurrent;

  FCurConvConf := nil;
  AvailClass := ValueConverterRegistry;
  obj := TIdeDbgValueConvertSelector.Create(AvailClass[0].CreateValueConvertorIntf);
  obj.Enabled := True;
  obj.Name := AName;
//  obj.MatchKinds := obj.Converter.GetSupportedKinds;
  FValConvList.Add(obj);

  FillList;
  lstConverters.ItemIndex := lstConverters.Count-1;
  lstConvertersClick(nil);
  memoTypeNames.SetFocus;
end;

procedure TIdeDbgValConvFrame.btnDownClick(Sender: TObject);
var
  i: Integer;
begin
  SaveCurrent;
  FCurConvConf := nil;
  i := lstConverters.ItemIndex;
  if (i < 0) or (i >= FValConvList.Count-1) then
    exit;
  FValConvList.Move(i, i+1);

  FillList;
  lstConverters.ItemIndex := i+1;
  lstConvertersClick(nil);
end;

procedure TIdeDbgValConvFrame.btnRemoveClick(Sender: TObject);
var
  i: Integer;
begin
  SaveCurrent;
  FCurConvConf := nil;
  i := lstConverters.ItemIndex;
  FValConvList.Delete(i);

  FillList;
  if i >= lstConverters.Count then
    dec(i);
  lstConverters.ItemIndex := i;
  lstConvertersClick(nil);
end;

procedure TIdeDbgValConvFrame.btnUpClick(Sender: TObject);
var
  i: Integer;
begin
  SaveCurrent;
  FCurConvConf := nil;
  i := lstConverters.ItemIndex;
  if (i < 1) or (i > FValConvList.Count-1) then
    exit;
  FValConvList.Move(i, i-1);

  FillList;
  lstConverters.ItemIndex := i-1;
  lstConvertersClick(nil);
end;

procedure TIdeDbgValConvFrame.dropActionChange(Sender: TObject);
begin
  UpdateConvForClass;
end;

procedure TIdeDbgValConvFrame.lstConvertersClick(Sender: TObject);
var
  AvailClass: TLazDbgValueConvertRegistry;
begin
  SaveCurrent;
  pnlCurrentConv.Enabled := lstConverters.Count > 0;

  FCurIdx := lstConverters.ItemIndex;
  if (FCurIdx >= FValConvList.Count) or (FCurIdx < 0) then begin
    FCurIdx := -1;
    FCurConvConf := nil;
    lblDesc.Caption := '';
    EdName.Text := '';
    memoTypeNames.Text := '';
  end
  else begin
    SetCurConv(FValConvList[FCurIdx]);
    lblDesc.Caption := FCurConvConf.Converter.GetRegistryEntry.GetName;
    EdName.Text := FCurConvConf.Name;
    memoTypeNames.Text := FCurConvConf.MatchTypeNames.Text;

    AvailClass := ValueConverterRegistry;
    dropAction.ItemIndex := AvailClass.IndexOfConvertorClass(FCurConvConf.Converter.GetObject.ClassType);
  end;

  btnRemove.Enabled := FCurIdx >= 0;
  btnUp.Enabled     := FCurIdx >= 1;
  btnDown.Enabled   := FCurIdx < FValConvList.Count - 1;

  lstConvertersItemClick(nil, FCurIdx);
end;

procedure TIdeDbgValConvFrame.lstConvertersItemClick(Sender: TObject; Index: integer);
begin
  if Index < 0 then
    exit;

  if (FValConvList[Index].Enabled <> lstConverters.Checked[Index]) then begin
    FValConvList[Index].Enabled := lstConverters.Checked[Index];
    FValConvList.Changed := True;
  end;

  if FCurIdx <> lstConverters.ItemIndex then
    lstConvertersClick(nil);

  UpdateButtons;
end;

procedure TIdeDbgValConvFrame.Splitter1CanOffset(Sender: TObject;
  var NewOffset: Integer; var Accept: Boolean);
begin

end;

procedure TIdeDbgValConvFrame.SetCurConv(AValConv: TIdeDbgValueConvertSelector);
begin
  FCurConvConf := AValConv;
  FCurConv := TLazDbgValueConvertSelectorIntf(FCurConvConf).GetConverter;
  UpdateConvPanel;
end;

procedure TIdeDbgValConvFrame.UpdateConvForClass;
var
  AvailClass: TLazDbgValueConvertRegistry;
begin
  if FCurConvConf = nil then
    exit;

  AvailClass := ValueConverterRegistry;
  if (dropAction.ItemIndex <> AvailClass.IndexOfConvertorClass(FCurConv.GetObject.ClassType)) then begin
    FCurConv := AvailClass[dropAction.ItemIndex].CreateValueConvertorIntf;
    UpdateConvPanel;
  end;
end;

procedure TIdeDbgValConvFrame.UpdateConvPanel;
var
  F: TFrame;
begin
  if FCurConvSettings <> nil then
    FCurConvSettings.Free;

  FCurConvSettings := FCurConv.GetSettingsFrame;
  if FCurConvSettings <> nil then begin
    F := TFrame(FCurConvSettings.GetFrame);
    F.Parent := pnlCurConvSetting;
    F.Align := alClient;
    FCurConvSettings.ReadFrom(FCurConv);
  end;
end;

procedure TIdeDbgValConvFrame.FillList;
var
  i: Integer;
  obj: TIdeDbgValueConvertSelector;
begin
  FCurConvConf := nil;

  lstConverters.Clear;
  for i := 0 to FValConvList.Count - 1 do begin
    obj := FValConvList[i];
    lstConverters.AddItem(obj.Name, nil);
    lstConverters.Checked[i] := obj.Enabled;
  end;
end;

procedure TIdeDbgValConvFrame.UpdateButtons;
begin
  btnAdd.Enabled := ValueConverterRegistry.Count > 0;
  btnRemove.Enabled := (lstConverters.Count > 0) and (lstConverters.ItemIndex >= 0);
  pnlCurrentConv.Enabled := FCurConvConf <> nil;
end;

procedure TIdeDbgValConvFrame.SetValConvList(AValue: TIdeDbgValueConvertSelectorList);
begin
  if FValConvList = AValue then Exit;
  FValConvList := AValue;
  FCurConvConf := nil;

  FillList;

  if FValConvList.Count > 0 then begin
    lstConverters.ItemIndex := 0;
    lstConvertersClick(nil);
  end;
  UpdateButtons;
end;

procedure TIdeDbgValConvFrame.SaveCurrent;
var
  AvailClass: TLazDbgValueConvertRegistry;
begin
  if FCurConvConf = nil then
    exit;

  AvailClass := ValueConverterRegistry;
  if ( (FCurConv = nil) or (FCurConvSettings = nil) or FCurConvSettings.WriteTo(FCurConv) ) or
     (TrimSet(FCurConvConf.MatchTypeNames.Text, [#1..#32]) <> TrimSet(memoTypeNames.Text, [#1..#32])) or
     (FCurConvConf.Converter <> FCurConv) or
     (EdName.Text <> FCurConvConf.Name)
  then begin
    FValConvList.Changed := True;
    FCurConvConf.Converter := FCurConv;
//    FCurConvConf.MatchKinds := FCurConvConf.Converter.GetSupportedKinds;
    FCurConvConf.MatchTypeNames.Text := memoTypeNames.Text;
    FCurConvConf.Name := EdName.Text
  end;
end;

procedure TIdeDbgValConvFrame.Setup;
var
  AvailClass: TLazDbgValueConvertRegistry;
  i: Integer;
begin
  btnAdd.Caption       := dlgBackConvOptAddNew;
  btnRemove.Caption    := dlgBackConvOptRemove;
  lblName.Caption      := dlgBackConvOptName;
  lblTypeNames.Caption := dlgBackConvOptMatchTypesByName;
  lblAction.Caption    := dlgBackConvOptAction;

  FCurConvConf := nil;
  lblDesc.Caption := '';

  dropAction.Clear;
  AvailClass := ValueConverterRegistry;
  for i := 0 to ValueConverterRegistry.Count - 1 do
    dropAction.AddItem(AvailClass[i].GetName, nil);

  UpdateButtons;
end;

constructor TIdeDbgValConvFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Setup;
end;

destructor TIdeDbgValConvFrame.Destroy;
begin
  if FCurConvSettings <> nil then
    FCurConvSettings.Free;

  inherited Destroy;
end;

end.

