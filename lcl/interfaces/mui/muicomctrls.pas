{
 *****************************************************************************
 *                             muicomctrls.pas                               *
 *                              --------------                               *
 *                              MUI ComControls                              *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit muicomctrls;
{$mode objfpc}{$H+}
interface

uses
  controls, muibaseunit, mui, exec, utility, sysutils, strings, Intuition, Types,
  ComCtrls, LCLMessageGlue, LMessages, LCLType, graphics, Math, tagsparamshelper;

type

  { TMUIGauge }

  TMUIGauge = class(TMUIArea)
  private
    FMinPos: Integer;
    FMaxPos: Integer;
    FShowText: Boolean;
    Text: string;
    function GetMaxPos: Integer;
    function GetMinPos: Integer;
    function GetPosition: Integer;
    function GetShowText: boolean;
    procedure SetMaxPos(AValue: Integer);
    procedure SetMinPos(AValue: Integer);
    procedure SetPosition(AValue: Integer);
    procedure SetShowText(AValue: boolean);
    procedure UpdateText;
  public
    Horiz: Boolean;
    constructor Create(AClassName: PChar; const Tags: TATagList); override;
    property Position: Integer read GetPosition write SetPosition;
    property MinPos: Integer read GetMinPos write SetMinPos;
    property MaxPos: Integer read GetMaxPos write SetMaxPos;
    property ShowText: boolean read GetShowText write SetShowText;
  end;

  TMUIBusy = class(TMUIArea)
  private
  public
    constructor Create(AClassName: PChar; const Tags: TATagList); override;
  end;

  TMUIGroup = class(TMUIArea)
  protected
    procedure BasicInitOnCreate(); override;
    procedure InstallHooks; override;
    function GetActivePage: Integer; virtual;
    procedure SetActivePage(AValue: Integer); virtual;
  public
    procedure SetOwnSize; override;
    property ActivePage: Integer read GetActivePage write SetActivePage;
  end;

  TMUIRegister = class(TMUIGroup)
  private
    FActivePage: Integer;
    FTexts: TMUIGroup;
    FRegisterHeight: Integer;
  protected
    procedure BasicInitOnCreate(); override;
    procedure SetParent(const AValue: TMUIObject); override;
    procedure InstallHooks; override;
    procedure SetLeft(ALeft: Integer); override;
    procedure SetTop(ATop: Integer); override;
    procedure SetWidth(AWidth: Integer); override;
    procedure SetHeight(AHeight: Integer); override;
    function GetHeight: Integer; override;
    function GetActivePage: Integer; override;
    procedure SetActivePage(AValue: Integer); override;
    procedure SetVisible(const AValue: boolean); override;
    procedure SetEnabled(const AValue: boolean); override;
    procedure SetColor(const AValue: TColor); override;
    function GetRegisterHeight: Integer;
    function GetPagesNum: Integer;
  public
    ShowTabs: Boolean;
    FNames: array[0..100] of PChar;
    constructor Create(AClassName: PChar; const Tags: TATagList); overload; reintroduce; virtual;
    destructor Destroy; override;
    procedure AddChild(ChildObj: PObject_); override;
    property RegisterHeight: Integer read GetRegisterHeight;
    property NumPages: Integer read GetPagesNum;
  end;

  TMUINumeric = class(TMUIGroup)
  protected
    procedure SetMinValue(AValue: Integer); virtual;
    procedure SetMaxValue(AValue: Integer); virtual;
    procedure SetValue(AValue: Integer); virtual;
    function GetMinValue: Integer; virtual;
    function GetMaxValue: Integer; virtual;
    function GetValue: Integer; virtual;
  public
    constructor Create(AClassName: PChar; const TagList: TATagList); overload; reintroduce; virtual;
    property MinValue: LongInt read GetMinValue write SetMinValue;
    property MaxValue: LongInt read GetMaxValue write SetMaxValue;
    property Value: LongInt read GetValue write SetValue;
  end;

  TMUISlider = class(TMUINumeric)
  private
    FHoriz: Boolean;
  protected
    procedure SetWidth(AWidth: Integer); override;
    procedure SetHeight(AHeight: Integer); override;
    procedure SetHorizontal(AValue: Boolean); virtual;
    function GetHorizontal: Boolean; virtual;
  public
    property Horizontal: Boolean read GetHorizontal write SetHorizontal;
  end;



implementation

{ TMUIGauge }

constructor TMUIGauge.Create(AClassName: PChar; const Tags: TATagList);
begin
  inherited;
  FMinPos := 0;
  FMaxPos := 100;
end;

function TMUIGauge.GetMaxPos: Integer;
begin
  Result := GetAttribute(MUIA_Gauge_Max) + FMinPos;
end;

function TMUIGauge.GetMinPos: Integer;
begin
  Result := FMinPos;
end;

function TMUIGauge.GetPosition: Integer;
begin
  Result := GetAttribute(MUIA_Gauge_Current) + FMinPos;
end;

function TMUIGauge.GetShowText: boolean;
begin
  Result := FShowText;
end;

procedure TMUIGauge.SetMaxPos(AValue: Integer);
begin
  FMaxPos := AValue;
  if FMaxPos - FMinPos > 0 then
  begin
    SetAttribute(MUIA_Gauge_Max, FMaxPos - FMinPos);
    if FShowText then
      UpdateText;
  end;
end;

procedure TMUIGauge.SetMinPos(AValue: Integer);
begin
  FMinPos := AValue;
  SetMaxPos(FMaxPos);
end;

procedure TMUIGauge.SetPosition(AValue: Integer);
begin
  SetAttribute(MUIA_Gauge_Current, AValue - FMinPos);
  if FShowText then
    UpdateText;
end;

procedure TMUIGauge.SetShowText(AValue: boolean);
begin
  FShowText := AValue;
  UpdateText;
end;

procedure TMUIGauge.UpdateText;
var
  Pos: Integer;
begin
  Text := '';
  if FShowText and ((FMaxPos - FMinPos) > 0) then
  begin
    Pos := Position;
    if not Horiz then
      Text := IntToStr(Pos)
    else
      Text := IntToStr(Pos) + ' from ['+IntToStr(FMinPos)+'-'+IntToStr(FMaxPos)+']('+IntToStr(Round(100*(Pos/(FMaxPos-FMinPos))))+'%%)'
  end;
  SetAttribute(MUIA_Gauge_InfoText, PChar(Text));
end;

{ TMUIBusy }

constructor TMUIBusy.Create(AClassName: PChar; const Tags: TATagList);
begin
  inherited Create(AClassName, Tags);
end;

{ TMUIGroup }

function TMUIGroup.GetActivePage: Integer;
begin
  Result := GetAttribute(MUIA_Group_ActivePage);
end;

procedure TMUIGroup.SetActivePage(AValue: Integer);
var
  i: Integer;
  Res: NativeUInt;
begin
  for i := 0 to FChilds.Count - 1 do
  begin
    TMUIObject(FChilds[i]).Visible := i = AValue;
  end;
  SetAttribute(MUIA_Group_ActivePage, AValue);
  MuiApp.AddInvalidatedObject(GetParentWindow);
end;

procedure TMUIGroup.BasicInitOnCreate();
begin
  inherited;
end;

procedure TMUIGroup.InstallHooks;
begin
end;

procedure TMUIGroup.SetOwnSize;
var
  i: longint;
  w,h: LongInt;
begin
  //writeln(self.classname, '-->setownsize');
  if not Assigned(FObject) then
    Exit;
  if BlockRedraw or BlockLayout then
    Exit;
  w := Min(FWidth, LongInt(OBJ_MaxWidth(FObject)));
  w := Max(w, LongInt(OBJ_MinWidth(FObject)));
  h := Min(FHeight, LongInt(OBJ_MaxHeight(FObject)));
  h := Max(h, LongInt(OBJ_MinHeight(FObject)));
  //writeln(self.classname,' setsize ', FLeft, ', ', FTop, ' - ', FWidth, ', ', FHeight,' count: ', Fchilds.Count, ' obj ', HexStr(FObject));
  MUI_Layout(FObject, FLeft, FTop, w, h, 0);
  //writeln(self.classname, '  setsize done');
  for i := 0 to FChilds.Count - 1 do
  begin
    //writeln(self.classname, '  Child ', i);
    if FChilds.Items[i] is TMUIObject then
      TMuiObject(FChilds.Items[i]).SetOwnSize;
  end;
  //writeln(self.classname, '<--setownsize');
end;

{ TMUIRegister }

constructor TMUIRegister.Create(AClassName: PChar; const Tags: TATagList);
begin
  FColor := clNone;
  FActivePage := -1;
  FRegisterHeight := 30;
  //writeln('I got it');
  {FNames[0] := GetMem(100);
  StrCopy(FNames[0], ' ');
  FNames[1] := nil;
  AddTags(tg, [MUIA_Register_Titles, IPTR(@FNames[0])]);
  FTexts := TMUIGroup.create(MUIC_Register, GetTagPtr(tg));}
  ShowTabs := False;
  FTexts := nil;
  //
  Tags.AddTags([
    MUIA_InnerTop, 0,
    MUIA_InnerLeft, 0,
    MUIA_InnerBottom, 0,
    MUIA_InnerRight, 0,
    {$ifndef AROS} // buggy in AROS, without also works (make not visible in TMUIRegister)
    MUIA_Group_PageMode, LTrue,
    {$endif}
    MUIA_Frame, MUIV_Frame_Group
    ]);
  inherited Create(AClassName, Tags);
end;

procedure TMUIRegister.BasicInitOnCreate();
begin
  inherited;
end;

destructor TMUIRegister.Destroy;
begin
  inherited;
  FTexts.Free;
end;

procedure TMUIRegister.InstallHooks;
begin
end;

procedure TMUIRegister.SetParent(const AValue: TMUIObject);
begin
  //writeln(self.classname + '  ' + self.pasobject.classname +' muiregsiter set parent ', AValue.classname, ' ', AValue.pasobject.classname);
  inherited SetParent(AValue);
  if ShowTabs and Assigned(FTexts) then
    FTexts.Parent := AValue;
end;

procedure TMUIRegister.SetLeft(ALeft: Integer);
begin
  inherited SetLeft(ALeft);
  if ShowTabs and Assigned(FTexts) then
    FTexts.Left := ALeft;
end;

procedure TMUIRegister.SetTop(ATop: Integer);
begin
  if ShowTabs and Assigned(FTexts) then
  begin
    inherited SetTop(ATop + RegisterHeight);
    FTexts.Top := ATop;
  end else
  begin
    inherited SetTop(ATop);
  end;
end;

procedure TMUIRegister.SetWidth(AWidth: Integer);
begin
  inherited SetWidth(AWidth);
  if ShowTabs and Assigned(FTexts) then
    FTexts.Width := AWidth;
end;

procedure TMUIRegister.SetHeight(AHeight: Integer);
begin
  if ShowTabs and Assigned(FTexts) then
  begin
    inherited SetHeight(AHeight - RegisterHeight);
    FTexts.Height := RegisterHeight;
  end else
  begin
    inherited SetHeight(AHeight);
  end;
end;

function TMUIRegister.GetHeight: Integer;
begin
  if ShowTabs and Assigned(FTexts) then
  begin
    Result := FHeight + RegisterHeight;
  end else
  begin
    Result := FHeight;
  end;
end;

function TMUIRegister.GetActivePage: Integer;
begin
  Result := GetAttribute(MUIA_Group_ActivePage);
end;

procedure TMUIRegister.SetActivePage(AValue: Integer);
var
  PGIdx: Integer;
begin
  if AValue < 0 then
    AValue := 0;
  FActivePage := AValue;
  TCustomTabControl(PasObject).PageIndex := AValue;
  inherited SetActivePage(AValue);
  if ShowTabs and Assigned(FTexts) and Assigned(FTexts.Obj) then
  begin
    PGIdx := GetAttObj(FTexts.Obj, MUIA_Group_ActivePage);
    if PGIdx <> FActivePage then
    begin
      SetAttObj(FTexts.Obj, [MUIA_Group_ActivePage, AValue]);
    end;
    PasObject.Invalidate;
  end;
end;

procedure TMUIRegister.SetVisible(const AValue: boolean);
begin
  inherited SetVisible(AValue);
  if Assigned(FTexts) then
    FTexts.Visible := AValue;
end;

procedure TMUIRegister.SetEnabled(const AValue: boolean);
begin
  inherited SetEnabled(AValue);
  if Assigned(FTexts) then
    FTexts.Enabled := AValue;
end;

function TMUIRegister.GetRegisterHeight: Integer;
begin
  Result := 0;
  if Assigned(FTexts) then
    Result := GetAttObj(FTexts.Obj, MUIA_InnerTop);
  if Result = 0 then
    Result := FRegisterHeight;
end;

function TMUIRegister.GetPagesNum: Integer;
begin
  Result := FChilds.Count;
end;

procedure TMUIRegister.SetColor(const AValue: TColor);
begin
  FColor := AValue;
  if Assigned(FTexts) then
  begin
    FTexts.Color := FColor;
  end;
end;

function TabIdxFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): LongInt;
var
  MUIRegister: TMUIRegister;
  PGIdx: Integer;
  Mess: TLMNotify;
  NMHdr: tagNMHDR;
begin
  Result := 0;
  if TObject(Hook^.h_Data) is TMUIRegister then
  begin
    MUIRegister := TMUIRegister(Hook^.h_Data);
    PGIdx := MUIRegister.GetAttObj(MUIRegister.FTexts.Obj, MUIA_Group_ActivePage);
    //
    FillChar(Mess, SizeOf(Mess), 0);
    Mess.Msg := LM_Notify;
    FillChar(NMHdr, SizeOf(NMHdr), 0);
    NMHdr.Code := TCN_SELCHANGING;
    NMHdr.hwndFrom := PtrUInt(MUIRegister);
    NMHdr.idFrom := PGIdx;
    Mess.NMHdr := @NMHdr;
    DeliverMessage(MUIRegister.PasObject, Mess);
    // forbidden to change
    if Mess.Result <> 0 then
    begin
      PGIdx := MUIRegister.FActivePage;
      if Assigned(MUIRegister.FTexts) then
        MUIRegister.SetAttObj(MUIRegister.FTexts.Obj, [MUIA_Group_ActivePage, MUIRegister.FActivePage]);
      Exit;
    end;
    MUIRegister.ActivePage := PGIdx;
    TCustomTabControl(MUIRegister.Pasobject).PageIndex := PGIdx;
    FillChar(Mess, SizeOf(Mess), 0);
    Mess.Msg := LM_Notify;
    FillChar(NMHdr, SizeOf(NMHdr), 0);
    NMHdr.Code := TCN_SELCHANGE;
    NMHdr.hwndFrom := PtrUInt(MUIRegister);
    NMHdr.idFrom := PGIdx;
    Mess.NMHdr := @NMHdr;
    DeliverMessage(MUIRegister.PasObject, Mess);
    //LCLSendChangedMsg(MUIRegister.PasObject, PGIdx);
  end;
end;


procedure TMUIRegister.AddChild(ChildObj: PObject_);
var
  i: Integer;
  MyStr: string;
  l,t,w,h: Integer;
  TG: TATagList;
  Tab: TCustomTabControl;
begin
  inherited;
  if not ShowTabs and Assigned(FTexts) then
  begin
    FTexts.Free;
    FTexts := nil;
  end;
  if ShowTabs and (PasObject is TCustomTabControl) then
  begin
    Tab := TCustomTabControl(PasObject);
    if Assigned(FTexts) then
    begin
      l := FTexts.Left;
      t := FTexts.Top;
      w := FTexts.Width;
      h := FTexts.Height;
      FTexts.Free;
    end;
    for i := 0 to Tab.Pages.Count - 1 do
    begin
      if Assigned(FNames[i]) then
        FreeMem(FNames[i]);
      MyStr := Tab.Pages[i];
      FNames[i] := GetMem(Length(FNames) + 1);
      strings.StrCopy(FNames[i], PChar(MyStr));
    end;
    FNames[FChilds.Count + 1] := nil;
    TG.AddTags([
      MUIA_Register_Titles, NativeUInt(@FNames[0]),
      MUIA_Frame, MUIV_Frame_None,
      MUIA_Register_Frame, TagFalse
      ]);
    FTexts := TMUIGroup.create(MUIC_Register, TG);
    FTexts.Top := t;
    FTexts.Left := l;
    FTexts.Width := w;
    FTexts.Height := h;
    FTexts.Parent := Parent;
    FTexts.Color := FColor;
    ConnectHookObject(FTexts.Obj, MUIA_Group_ActivePage, MUIV_EveryTime, @TabIdxFunc);
  end;
end;

{ TMUINumeric }

constructor TMUINumeric.Create(AClassName: PChar; const TagList: TATagList);
begin
  inherited Create(AClassname, TagList);
end;


procedure TMUINumeric.SetMinValue(AValue: Integer);
begin
  if AValue = MinValue then
    Exit;
  SetAttribute(MUIA_Numeric_Min, AValue);
end;

procedure TMUINumeric.SetMaxValue(AValue: Integer);
begin
  if AValue = MaxValue then
    Exit;
  SetAttribute(MUIA_Numeric_Max, AValue);
end;

procedure TMUINumeric.SetValue(AValue: Integer);
begin
  if AValue = GetValue then
    Exit;
  SetAttribute(MUIA_Numeric_Value, AValue);
end;

function TMUINumeric.GetMinValue: Integer;
begin
  Result := GetAttribute(MUIA_Numeric_Min);
end;

function TMUINumeric.GetMaxValue: Integer;
begin
  Result := GetAttribute(MUIA_Numeric_Max);
end;

function TMUINumeric.GetValue: Integer;
begin
  Result := GetAttribute(MUIA_Numeric_Value);
end;

{ TMUISlider }

function TMUISlider.GetHorizontal: Boolean;
begin
  Result := FHoriz;
end;

procedure TMUISlider.SetHorizontal(AValue: Boolean);
begin
  FHoriz := AValue;
end;

procedure TMUISlider.SetWidth(AWidth: Integer);
begin
  //if not FHoriz then
  //  AWidth := 25;
  inherited SetWidth(AWidth);
end;

procedure TMUISlider.SetHeight(AHeight: Integer);
begin
  //if FHoriz then
  //  AHeight := 25;
  inherited SetHeight(AHeight);
end;


end.
