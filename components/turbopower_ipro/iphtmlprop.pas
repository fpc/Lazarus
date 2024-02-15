unit IpHtmlProp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Contnrs, AVL_Tree,
  Graphics,
  IpConst, IpUtils, IpHtmlTypes;

type
  TIpHtmlElemMargin = record
    Style: TIpHtmlElemMarginStyle;
    Size: single; // negative values are not yet supported
  end;

  PIpHtmlPropAFieldsRec = ^TIpHtmlPropAFieldsRec;
  TIpHtmlPropAFieldsRec = packed record
    BaseFontSize: Integer;
    FontSize: Integer;
    FontName: string;
    FontStyle: TFontStyles;
  end;

  PIpHtmlPropBFieldsRec = ^TIpHtmlPropBFieldsRec;
  TIpHtmlPropBFieldsRec = packed record
    FontBaseline: Integer;
    FontColor: TColor;
    LinkColor : TColor;
    VLinkColor : TColor;
    ALinkColor : TColor;
    HoverColor : TColor;
    HoverBgColor : TColor;
    BgColor : TColor;
    Alignment: TIpHtmlAlign;
    VAlignment: TIpHtmlVAlign3;
    Preformatted : Boolean;
    NoBreak : Boolean;
    ElemMarginTop: TIpHtmlElemMargin;
    ElemMarginLeft: TIpHtmlElemMargin;
    ElemMarginBottom: TIpHtmlElemMargin;
    ElemMarginRight: TIpHtmlElemMargin;
  end;

  TIpHtmlPropBase = class;

  { TObjectAVLTree }

  // This container owns its objects. They are freed at the end.
  TObjectAVLTree = class(TAVLTree)
  public
    destructor Destroy; override;
    function Add(AProp: TIpHtmlPropBase): TAVLTreeNode;
    function Remove(AProp: TIpHtmlPropBase): Boolean;
  end;

  { TIpHtmlPropBase }

  TIpHtmlPropBase = class
  private
    FOwner: TObjectAVLTree;
    FUseCount: Integer;
  public
    constructor Create(AOwner: TObjectAVLTree);
  end;

  { TIpHtmlPropA }

  {display properties that affect the font size}
  TIpHtmlPropA = class(TIpHtmlPropBase)
  private
    FPropRec : TIpHtmlPropAFieldsRec;
    FKnownSizeOfSpace: TSize;
    FSizeOfSpaceKnown : Boolean;
    procedure SetBaseFontSize(const Value: Integer);
    procedure SetFontName(const Value: string);
    procedure SetFontSize(const Value: Integer);
    procedure SetFontStyle(const Value: TFontStyles);
  public
    KnownSizeOfHyphen : TSize;
    tmAscent, tmDescent, tmHeight : Integer;
    procedure Assign(const Source: TIpHtmlPropA);
    procedure DecUse;
    procedure IncUse;
    procedure SetKnownSizeOfSpace(const Size:TSize);
  public
    property SizeOfSpaceKnown: Boolean read FSizeOfSpaceKnown;
    property KnownSizeOfSpace : TSize read FKnownSizeOfSpace;
    property BaseFontSize : Integer read FPropRec.BaseFontSize write SetBaseFontSize;
    property FontName : string read FPropRec.FontName write SetFontName;
    property FontSize : Integer read FPropRec.FontSize write SetFontSize;
    property FontStyle : TFontStyles read FPropRec.FontStyle write SetFontStyle;
  end;

  { TIpHtmlPropB }

  {display properties that don't affect the font size}
  TIpHtmlPropB = class(TIpHtmlPropBase)
  private
    FPropRec : TIpHtmlPropBFieldsRec;
  public
    constructor Create(AOwner: TObjectAVLTree);
    procedure Assign(const Source: TIpHtmlPropB);
    procedure DecUse;
    procedure IncUse;
  public
    property FontBaseline : Integer read FPropRec.FontBaseline write FPropRec.FontBaseline;
    property FontColor : TColor read FPropRec.FontColor write FPropRec.FontColor;
    property Alignment : TIpHtmlAlign read FPropRec.Alignment write FPropRec.Alignment;
    property VAlignment : TIpHtmlVAlign3 read FPropRec.VAlignment write FPropRec.VAlignment;
    property LinkColor : TColor read FPropRec.LinkColor write FPropRec.LinkColor;
    property VLinkColor : TColor read FPropRec.VLinkColor write FPropRec.VLinkColor;
    property ALinkColor : TColor read FPropRec.ALinkColor write FPropRec.ALinkColor;
    property HoverColor : TColor read FPropRec.HoverColor write FPropRec.HoverColor;
    property HoverBgColor : TColor read FPropRec.HoverBgColor write FPropRec.HoverBgColor;
    property BgColor : TColor read FPropRec.BgColor write FPropRec.BgColor;
    property Preformatted : Boolean read FPropRec.Preformatted write FPropRec.Preformatted;
    property NoBreak : Boolean read FPropRec.NoBreak write FPropRec.NoBreak;
    property ElemMarginTop: TIpHtmlElemMargin read FPropRec.ElemMarginTop write FPropRec.ElemMarginTop;
    property ElemMarginLeft: TIpHtmlElemMargin read FPropRec.ElemMarginLeft write FPropRec.ElemMarginLeft;
    property ElemMarginBottom: TIpHtmlElemMargin read FPropRec.ElemMarginBottom write FPropRec.ElemMarginBottom;
    property ElemMarginRight: TIpHtmlElemMargin read FPropRec.ElemMarginRight write FPropRec.ElemMarginRight;
  end;

  { TIpHtmlPropsAList }

  TIpHtmlPropsAList = class(TObjectAVLTree)
  private
    FDummyA : TIpHtmlPropA;
  public
    constructor Create;
    function FindPropARec(pRec: PIpHtmlPropAFieldsRec): TIpHtmlPropA;
    procedure ResetCache;
  end;

  { TIpHtmlPropsBList }

  TIpHtmlPropsBList = class(TObjectAVLTree)
  private
    FDummyB : TIpHtmlPropB;
  public
    constructor Create;
    function FindPropBRec(pRec: PIpHtmlPropBFieldsRec): TIpHtmlPropB;
  end;

  { TIpHtmlProps }

  TIpHtmlProps = class
  {-class for holding the currently active style attributes}
  private
    FPropsACache: TIpHtmlPropsAList;
    FPropsBCache: TIpHtmlPropsBList;
    FPropA : TIpHtmlPropA;
    FPropB : TIpHtmlPropB;
    FDelayCache: integer;
    function GetAlignment: TIpHtmlAlign;
    function GetALinkColor: TColor;
    function GetBaseFontSize: Integer;
    function GetBgColor: TColor;
    function GetElemMarginBottom: TIpHtmlElemMargin;
    function GetElemMarginLeft: TIpHtmlElemMargin;
    function GetElemMarginRight: TIpHtmlElemMargin;
    function GetElemMarginTop: TIpHtmlElemMargin;
    function GetFontBaseline: Integer;
    function GetFontColor: TColor;
    function GetFontName: string;
    function GetFontSize: Integer;
    function GetFontStyle: TFontStyles;
    function GetLinkColor: TColor;
    function GetPreformatted: Boolean;
    function GetVAlignment: TIpHtmlVAlign3;
    function GetVLinkColor: TColor;
    function GetHoverColor: TColor;
    function GetHoverBgColor: TColor;
    procedure SetAlignment(const Value: TIpHtmlAlign);
    procedure SetALinkColor(const Value: TColor);
    procedure SetBaseFontSize(const Value: Integer);
    procedure SetBgColor(const Value: TColor);
    procedure SetElemMarginBottom(const AValue: TIpHtmlElemMargin);
    procedure SetElemMarginLeft(const AValue: TIpHtmlElemMargin);
    procedure SetElemMarginRight(const AValue: TIpHtmlElemMargin);
    procedure SetElemMarginTop(const AValue: TIpHtmlElemMargin);
    procedure SetFontBaseline(const Value: Integer);
    procedure SetFontColor(const Value: TColor);
    procedure SetFontName(const Value: string);
    procedure SetFontSize(const Value: Integer);
    procedure SetFontStyle(const Value: TFontStyles);
    procedure SetLinkColor(const Value: TColor);
    procedure SetPreformatted(const Value: Boolean);
    procedure SetVAlignment(const Value: TIpHtmlVAlign3);
    procedure SetVLinkColor(const Value: TColor);
    procedure SetHoverColor(const Value: TColor);
    procedure SetHoverBgColor(const Value: TColor);
    function GetNoBreak: Boolean;
    procedure SetNoBreak(const Value: Boolean);
    procedure CopyPropARecTo(pRec: PIpHtmlPropAFieldsRec);
    procedure CopyPropBRecTo(pRec: PIpHtmlPropBFieldsRec);
    procedure FindOrCreatePropA(pRec: PIpHtmlPropAFieldsRec);
    procedure FindOrCreatePropB(pRec: PIpHtmlPropBFieldsRec);
    procedure SetDelayCache(b: boolean);
    function getDelayCache: boolean;
  protected
  public
    constructor Create(APropsAList: TIpHtmlPropsAList; APropsBList: TIpHtmlPropsBList);
    destructor Destroy; override;
    procedure Assign(Source : TIpHtmlProps);
    procedure CommitCache;
    function IsEqualTo(Compare: TIpHtmlProps): Boolean;
    function AIsEqualTo(Compare: TIpHtmlProps): Boolean;
    function BIsEqualTo(Compare: TIpHtmlProps): Boolean;
    property BaseFontSize : Integer read GetBaseFontSize write SetBaseFontSize;
    property FontName : string read GetFontName write SetFontName;
    property FontSize : Integer read GetFontSize write SetFontSize;
    property FontBaseline : Integer read GetFontBaseline write SetFontBaseline;
    property FontStyle : TFontStyles read GetFontStyle write SetFontStyle;
    property FontColor : TColor read GetFontColor write SetFontColor;
    property Alignment : TIpHtmlAlign read GetAlignment write SetAlignment;
    property VAlignment : TIpHtmlVAlign3 read GetVAlignment write SetVAlignment;
    property LinkColor : TColor read GetLinkColor write SetLinkColor;
    property VLinkColor : TColor read GetVLinkColor write SetVLinkColor;
    property ALinkColor : TColor read GetALinkColor write SetALinkColor;
    property HoverColor : TColor read GetHoverColor write SetHoverColor;
    property HoverBgColor : TColor read GetHoverBgColor write SetHoverBgColor;
    property BgColor : TColor read GetBgColor write SetBgColor;
    property Preformatted : Boolean read GetPreformatted write SetPreformatted;
    property NoBreak : Boolean read GetNoBreak write SetNoBreak;
    property ElemMarginTop: TIpHtmlElemMargin read GetElemMarginTop write SetElemMarginTop;
    property ElemMarginLeft: TIpHtmlElemMargin read GetElemMarginLeft write SetElemMarginLeft;
    property ElemMarginBottom: TIpHtmlElemMargin read GetElemMarginBottom write SetElemMarginBottom;
    property ElemMarginRight: TIpHtmlElemMargin read GetElemMarginRight write SetElemMarginRight;
  public
    property PropA: TIpHtmlPropA read FPropA;
    property PropB: TIpHtmlPropB read FPropB;
    property DelayCache : Boolean read getDelayCache write setDelayCache;
  end;


implementation

function AreHtmlMarginsEqual(Margin1, Margin2: TIpHtmlElemMargin): boolean;
begin
  Result:=(Margin1.Style=Margin2.Style) and (Margin1.Size=Margin2.Size);
end;

{ TObjectAVLTree }

destructor TObjectAVLTree.Destroy;
var
  Enumerator: TAVLTreeNodeEnumerator;
begin
  Enumerator := GetEnumerator;
  while Enumerator.MoveNext do
    TObject(Enumerator.Current.Data).Free;
  Enumerator.Free;
  inherited Destroy;
end;

function TObjectAVLTree.Add(AProp: TIpHtmlPropBase): TAVLTreeNode;
begin
  AProp.FUseCount := 1;  // Initial count. Will not be disposed so quickly.
  Result := inherited Add(AProp);
end;

function TObjectAVLTree.Remove(AProp: TIpHtmlPropBase): Boolean;
begin
  Result := inherited Remove(AProp);
  if Result then
    AProp.Free;
end;

{ TIpHtmlPropBase }

constructor TIpHtmlPropBase.Create(AOwner: TObjectAVLTree);
begin
  inherited Create;
  FOwner := AOwner;
end;

{ TIpHtmlPropA }

procedure TIpHtmlPropA.Assign(const Source: TIpHtmlPropA);
begin
  if Source <> nil then
    Move(Source.FPropRec, FPropRec, sizeof(TIpHtmlPropAFieldsRec));
end;

procedure TIpHtmlPropA.DecUse;
begin
  if FUseCount > 0 then
    Dec(FUseCount);
end;

procedure TIpHtmlPropA.IncUse;
begin
  Inc(FUseCount);
end;

procedure TIpHtmlPropA.SetBaseFontSize(const Value: Integer);
begin
  if Value <> FPropRec.BaseFontSize then begin
    FPropRec.BaseFontSize := Value;
    FSizeOfSpaceKnown := False;
  end;
end;

procedure TIpHtmlPropA.SetFontName(const Value: string);
begin
  if Value <> FPropRec.FontName then begin
    FPropRec.FontName := Value;
    FSizeOfSpaceKnown := False;
  end;
end;

procedure TIpHtmlPropA.SetFontSize(const Value: Integer);
begin
  if Value <> FPropRec.FontSize then begin
    FPropRec.FontSize := Value;
    FSizeOfSpaceKnown := False;
  end;
end;

procedure TIpHtmlPropA.SetFontStyle(const Value: TFontStyles);
begin
  if Value <> FPropRec.FontStyle then begin
    FPropRec.FontStyle := Value;
    FSizeOfSpaceKnown := False;
  end;
end;

procedure TIpHtmlPropA.SetKnownSizeOfSpace(const Size: TSize);
begin
  FKnownSizeOfSpace := Size;
  FSizeOfSpaceKnown := True;
end;

{ TIpHtmlPropB }

constructor TIpHtmlPropB.Create(AOwner: TObjectAVLTree);
begin
  inherited Create(AOwner);
  FPropRec.BgColor := clNone;
  FPropRec.HoverColor := clNone;
  FPropRec.HoverBgColor := clNone;
end;

procedure TIpHtmlPropB.Assign(const Source: TIpHtmlPropB);
begin
  if Source <> nil then
    FPropRec := Source.FPropRec;
end;

procedure TIpHtmlPropB.DecUse;
begin
  Dec(FUseCount);
  if FUseCount < 0 then
    raise EIpHtmlException.Create(SHtmlInternal)
  else if FUseCount = 0 then
    if not FOwner.Remove(Self) then
      raise EIpHtmlException.Create(SHtmlInternal);
end;

procedure TIpHtmlPropB.IncUse;
begin
  Inc(FUseCount);
end;

{ TIpHtmlPropsAList }

function ComparePropARec(Prop1, Prop2: PIpHtmlPropAFieldsRec): integer;
begin
  Result := Prop1^.BaseFontSize - Prop2^.BaseFontSize;
  if Result <> 0 then Exit;
  Result := Prop1^.FontSize - Prop2^.FontSize;
  if Result <> 0 then Exit;
  Result := LongWord(Prop1^.FontStyle) - LongWord(Prop2^.FontStyle);
  if Result <> 0 then Exit;
  Result := CompareStr(Prop1^.FontName, Prop2^.FontName);
end;

function ComparePropAFields(Data1, Data2: Pointer): integer;
begin
  Result := ComparePropARec(@TIpHtmlPropA(Data1).FPropRec, @TIpHtmlPropA(Data2).FPropRec);
end;

function CompareKeyPropA(Key, Data: Pointer): integer;
begin
  Result := ComparePropARec(PIpHtmlPropAFieldsRec(Key), @TIpHtmlPropA(Data).FPropRec);
end;

constructor TIpHtmlPropsAList.Create;
begin
  inherited Create(@ComparePropAFields);
  FDummyA := TIpHtmlPropA.Create(Self);
  Add(FDummyA);
end;

function TIpHtmlPropsAList.FindPropARec(pRec: PIpHtmlPropAFieldsRec): TIpHtmlPropA;
var
  Node: TAVLTreeNode;
begin
  Node := FindKey(pRec, @CompareKeyPropA);
  if Assigned(Node) then
    Result := TIpHtmlPropA(Node.Data)
  else
    Result := Nil;
end;

procedure TIpHtmlPropsAList.ResetCache;
var
  Enumerator: TAVLTreeNodeEnumerator;
  Prop: TIpHtmlPropA;
begin
  Enumerator := GetEnumerator;
  while Enumerator.MoveNext do begin
    Prop := TIpHtmlPropA(Enumerator.Current.Data);
    Prop.FSizeOfSpaceKnown := False;
    Prop.tmHeight := 0;
  end;
  Enumerator.Free;
end;

{ TIpHtmlPropsBList }

function ComparePropBFields(Data1, Data2: Pointer): integer;
begin
  Result := CompareByte(TIpHtmlPropB(Data1).FPropRec,
                        TIpHtmlPropB(Data2).FPropRec, SizeOf(TIpHtmlPropBFieldsRec));
end;

function CompareKeyPropB(Key, Data: Pointer): integer;
begin
  Result := CompareByte(Key^, TIpHtmlPropB(Data).FPropRec, SizeOf(TIpHtmlPropBFieldsRec));
end;

constructor TIpHtmlPropsBList.Create;
begin
  inherited Create(@ComparePropBFields);
  FDummyB := TIpHtmlPropB.Create(Self);
  Add(FDummyB);
end;

function TIpHtmlPropsBList.FindPropBRec(pRec: PIpHtmlPropBFieldsRec): TIpHtmlPropB;
var
  Node: TAVLTreeNode;
begin
  Node := FindKey(pRec, @CompareKeyPropB);
  if Assigned(Node) then
    Result := TIpHtmlPropB(Node.Data)
  else
    Result := Nil;
end;

{ TIpHtmlProps }

constructor TIpHtmlProps.Create(APropsAList: TIpHtmlPropsAList; APropsBList: TIpHtmlPropsBList);
begin
  inherited Create;
  FPropsACache := APropsAList;
  FPropsBCache := APropsBList;
  FPropA := FPropsACache.FDummyA;
  FPropA.IncUse;
  FPropB := FPropsBCache.FDummyB;
  FPropB.IncUse;
  //BgColor := clNone;
end;

destructor TIpHtmlProps.Destroy;
begin
  FPropA.DecUse;
  FPropB.DecUse;
  inherited;
end;

procedure TIpHtmlProps.Assign(Source: TIpHtmlProps);
begin
  if FPropA <> Source.FPropA then begin
    FPropA.DecUse;
    FPropA := Source.FPropA;
    FPropA.IncUse;
  end;
  if FPropB <> Source.FPropB then begin
    FPropB.DecUse;
    FPropB := Source.FPropB;
    FPropB.IncUse;
  end;
end;

function TIpHtmlProps.AIsEqualTo(Compare: TIpHtmlProps): Boolean;
begin
  Result := (FPropA = Compare.FPropA);
end;

function TIpHtmlProps.BIsEqualTo(Compare: TIpHtmlProps): Boolean;
begin
  Result := (FPropB = Compare.FPropB);
end;

function TIpHtmlProps.IsEqualTo(Compare: TIpHtmlProps): Boolean;
begin
  Result := (FPropA = Compare.FPropA) and (FPropB = Compare.FPropB);
end;

function TIpHtmlProps.GetAlignment: TIpHtmlAlign;
begin
  Result := FPropB.Alignment;
end;

function TIpHtmlProps.GetALinkColor: TColor;
begin
  Result := FPropB.ALinkColor;
end;

function TIpHtmlProps.GetBaseFontSize: Integer;
begin
  Result := FPropA.BaseFontSize;
end;

function TIpHtmlProps.GetBgColor: TColor;
begin
  Result := FPropB.BgColor;
end;

function TIpHtmlProps.GetElemMarginBottom: TIpHtmlElemMargin;
begin
  Result:=FPropB.ElemMarginBottom;
end;

function TIpHtmlProps.GetElemMarginLeft: TIpHtmlElemMargin;
begin
  Result:=FPropB.ElemMarginLeft;
end;

function TIpHtmlProps.GetElemMarginRight: TIpHtmlElemMargin;
begin
  Result:=FPropB.ElemMarginRight;
end;

function TIpHtmlProps.GetElemMarginTop: TIpHtmlElemMargin;
begin
  Result:=FPropB.ElemMarginTop;
end;

function TIpHtmlProps.GetFontBaseline: Integer;
begin
  Result := FPropB.FontBaseline;
end;

function TIpHtmlProps.GetFontColor: TColor;
begin
  Result := FPropB.FontColor;
end;

function TIpHtmlProps.GetFontName: string;
begin
  Result := FPropA.FontName;
end;

function TIpHtmlProps.GetFontSize: Integer;
begin
  Result := FPropA.FontSize;
end;

function TIpHtmlProps.GetFontStyle: TFontStyles;
begin
  Result := FPropA.FontStyle;
end;

function TIpHtmlProps.GetLinkColor: TColor;
begin
  Result := FPropB.LinkColor;
end;

function TIpHtmlProps.GetNoBreak: Boolean;
begin
  Result := FPropB.NoBreak;
end;

function TIpHtmlProps.GetPreformatted: Boolean;
begin
  Result := FPropB.Preformatted;
end;

function TIpHtmlProps.GetVAlignment: TIpHtmlVAlign3;
begin
  Result := FPropB.VAlignment;
end;

function TIpHtmlProps.GetVLinkColor: TColor;
begin
  Result := FPropB.VLinkColor;
end;

function TIpHtmlProps.GetHoverColor: TColor;
begin
  Result := FPropB.HoverColor;
end;

function TIpHtmlProps.GetHoverBgColor: TColor;
begin
  Result := FPropB.HoverBgColor;
end;

procedure TIpHtmlProps.CommitCache;
begin
  if FDelayCache > 0 then
  begin
    FDelayCache := 1;
    SetDelayCache(false);
  end;
end;

function TIpHtmlProps.getDelayCache: boolean;
begin
  result := FDelayCache > 0;
end;

procedure TIpHtmlProps.SetDelayCache(b: boolean);
begin
  if b then
    Inc(FDelayCache)
  else if FDelayCache > 0 then
    Dec(FDelayCache);
end;

procedure TIpHtmlProps.CopyPropARecTo(pRec: PIpHtmlPropAFieldsRec);
begin
  pRec^.BaseFontSize := FPropA.FPropRec.BaseFontSize;
  pRec^.FontSize     := FPropA.FPropRec.FontSize;
  pRec^.FontStyle    := FPropA.FPropRec.FontStyle;
  pRec^.FontName     := FPropA.FPropRec.FontName;
  // Cannot use Move() because of a reference counted string.
  //Move(FPropA.FPropRec, pRec^, sizeof(TIpHtmlPropAFieldsRec))
end;

procedure TIpHtmlProps.CopyPropBRecTo(pRec: PIpHtmlPropBFieldsRec);
begin
  Move(FPropB.FPropRec, pRec^, sizeof(TIpHtmlPropBFieldsRec))
end;

procedure TIpHtmlProps.FindOrCreatePropA(pRec: PIpHtmlPropAFieldsRec);
var
  NewPropA : TIpHtmlPropA;
begin
  NewPropA := FPropsACache.FindPropARec(pRec);
  if NewPropA = nil then begin
    NewPropA := TIpHtmlPropA.Create(FPropsACache);
    NewPropA.FPropRec.BaseFontSize := pRec^.BaseFontSize;
    NewPropA.FPropRec.FontSize     := pRec^.FontSize;
    NewPropA.FPropRec.FontStyle    := pRec^.FontStyle;
    NewPropA.FPropRec.FontName     := pRec^.FontName; // Reference counted string.
    FPropsACache.Add(NewPropA);
  end;
  NewPropA.IncUse;
  FPropA.DecUse;
  FPropA := NewPropA;
end;

procedure TIpHtmlProps.FindOrCreatePropB(pRec: PIpHtmlPropBFieldsRec);
var
  NewPropB : TIpHtmlPropB;
begin
  NewPropB := FPropsBCache.FindPropBRec(pRec);
  if NewPropB = nil then begin
    NewPropB := TIpHtmlPropB.Create(FPropsBCache);
    Move(pRec^, NewPropB.FPropRec, sizeof(TIpHtmlPropBFieldsRec));
    FPropsBCache.Add(NewPropB);
  end;
  NewPropB.IncUse;
  FPropB.DecUse;
  FPropB := NewPropB;
end;

// For TIpHtmlPropAFieldsRec

procedure TIpHtmlProps.SetBaseFontSize(const Value: Integer);
var
  Rec : TIpHtmlPropAFieldsRec;
begin
  if Value <> BaseFontSize then begin
    CopyPropARecTo(@Rec);
    Rec.BaseFontSize:=Value;
    FindOrCreatePropA(@Rec);
  end;
end;

procedure TIpHtmlProps.SetFontName(const Value: string);
var
  Rec : TIpHtmlPropAFieldsRec;
begin
  if Value <> FontName then begin
    CopyPropARecTo(@Rec);
    Rec.FontName:=Value;
    FindOrCreatePropA(@Rec);
  end;
end;

procedure TIpHtmlProps.SetFontSize(const Value: Integer);
var
  Rec : TIpHtmlPropAFieldsRec;
begin
  if Value <> FontSize then begin
    CopyPropARecTo(@Rec);
    Rec.FontSize:=Value;
    FindOrCreatePropA(@Rec);
  end;
end;

procedure TIpHtmlProps.SetFontStyle(const Value: TFontStyles);
var
  Rec : TIpHtmlPropAFieldsRec;
begin
  if Value <> FontStyle then begin
    CopyPropARecTo(@Rec);
    Rec.FontStyle:=Value;
    FindOrCreatePropA(@Rec);
  end;
end;

// For TIpHtmlPropBFieldsRec

procedure TIpHtmlProps.SetAlignment(const Value: TIpHtmlAlign);
var
  Rec : TIpHtmlPropBFieldsRec;
begin
  if (Value <> haDefault) and (Value <> Alignment) then begin
    CopyPropBRecTo(@Rec);
    Rec.Alignment:=Value;
    FindOrCreatePropB(@Rec);
  end;
end;

procedure TIpHtmlProps.SetALinkColor(const Value: TColor);
var
  Rec : TIpHtmlPropBFieldsRec;
begin
  if Value <> ALinkColor then begin
    CopyPropBRecTo(@Rec);
    Rec.ALinkColor:=Value;
    FindOrCreatePropB(@Rec);
  end;
end;

procedure TIpHtmlProps.SetBgColor(const Value: TColor);
var
  Rec : TIpHtmlPropBFieldsRec;
begin
  if Value <> BgColor then begin
    CopyPropBRecTo(@Rec);
    Rec.BgColor:=Value;
    FindOrCreatePropB(@Rec);
  end;
end;

procedure TIpHtmlProps.SetFontBaseline(const Value: Integer);
var
  Rec : TIpHtmlPropBFieldsRec;
begin
  if Value <> FontBaseline then begin
    CopyPropBRecTo(@Rec);
    Rec.FontBaseline:=Value;
    FindOrCreatePropB(@Rec);
  end;
end;

procedure TIpHtmlProps.SetFontColor(const Value: TColor);
var
  Rec : TIpHtmlPropBFieldsRec;
begin
  if Value <> FontColor then begin
    CopyPropBRecTo(@Rec);
    Rec.FontColor:=Value;
    FindOrCreatePropB(@Rec);
  end;
end;

procedure TIpHtmlProps.SetLinkColor(const Value: TColor);
var
  Rec : TIpHtmlPropBFieldsRec;
begin
  if Value <> LinkColor then begin
    CopyPropBRecTo(@Rec);
    Rec.LinkColor:=Value;
    FindOrCreatePropB(@Rec);
  end;
end;

procedure TIpHtmlProps.SetElemMarginBottom(const AValue: TIpHtmlElemMargin);
var
  Rec : TIpHtmlPropBFieldsRec;
begin
  if AreHtmlMarginsEqual(AValue,ElemMarginBottom) then exit;
  CopyPropBRecTo(@Rec);
  Rec.ElemMarginBottom:=AValue;
  FindOrCreatePropB(@Rec);
end;

procedure TIpHtmlProps.SetElemMarginLeft(const AValue: TIpHtmlElemMargin);
var
  Rec : TIpHtmlPropBFieldsRec;
begin
  if AreHtmlMarginsEqual(AValue,ElemMarginLeft) then exit;
  CopyPropBRecTo(@Rec);
  Rec.ElemMarginLeft:=AValue;
  FindOrCreatePropB(@Rec);
end;

procedure TIpHtmlProps.SetElemMarginRight(const AValue: TIpHtmlElemMargin);
var
  Rec : TIpHtmlPropBFieldsRec;
begin
  if AreHtmlMarginsEqual(AValue,ElemMarginRight) then exit;
  CopyPropBRecTo(@Rec);
  Rec.ElemMarginRight:=AValue;
  FindOrCreatePropB(@Rec);
end;

procedure TIpHtmlProps.SetElemMarginTop(const AValue: TIpHtmlElemMargin);
var
  Rec : TIpHtmlPropBFieldsRec;
begin
  if AreHtmlMarginsEqual(AValue,ElemMarginTop) then exit;
  CopyPropBRecTo(@Rec);
  Rec.ElemMarginTop:=AValue;
  FindOrCreatePropB(@Rec);
end;

procedure TIpHtmlProps.SetNoBreak(const Value: Boolean);
var
  Rec : TIpHtmlPropBFieldsRec;
begin
  if Value <> NoBreak then begin
    CopyPropBRecTo(@Rec);
    Rec.NoBreak:=Value;
    FindOrCreatePropB(@Rec);
  end;
end;

procedure TIpHtmlProps.SetPreformatted(const Value: Boolean);
var
  Rec : TIpHtmlPropBFieldsRec;
begin
  if Value <> Preformatted then begin
    CopyPropBRecTo(@Rec);
    Rec.Preformatted:=Value;
    FindOrCreatePropB(@Rec);
  end;
end;

procedure TIpHtmlProps.SetVAlignment(const Value: TIpHtmlVAlign3);
var
  Rec : TIpHtmlPropBFieldsRec;
begin
  if Value <> VAlignment then begin
    CopyPropBRecTo(@Rec);
    Rec.VAlignment:=Value;
    FindOrCreatePropB(@Rec);
  end;
end;

procedure TIpHtmlProps.SetVLinkColor(const Value: TColor);
var
  Rec : TIpHtmlPropBFieldsRec;
begin
  if Value <> VLinkColor then begin
    CopyPropBRecTo(@Rec);
    Rec.VLinkColor:=Value;
    FindOrCreatePropB(@Rec);
  end;
end;

procedure TIpHtmlProps.SetHoverColor(const Value: TColor);
var
  Rec : TIpHtmlPropBFieldsRec;
begin
  if Value <> HoverColor then begin
    CopyPropBRecTo(@Rec);
    Rec.HoverColor:=Value;
    FindOrCreatePropB(@Rec);
  end;
end;

procedure TIpHtmlProps.SetHoverBgColor(const Value: TColor);
var
  Rec : TIpHtmlPropBFieldsRec;
begin
  if Value <> HoverBgColor then begin
    CopyPropBRecTo(@Rec);
    Rec.HoverBgColor:=Value;
    FindOrCreatePropB(@Rec);
  end;
end;

end.

