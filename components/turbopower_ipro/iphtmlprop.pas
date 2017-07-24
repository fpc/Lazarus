unit iphtmlprop;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, contnrs, Graphics,
  IpConst, IpUtils;

type

  TIpHtmlAlign = (haDefault, haLeft, haCenter, haRight, haJustify, haChar, haUnknown);
  TIpHtmlVAlign = (hvaTop, hvaMiddle, hvaBottom);
  TIpHtmlVAlign3 = (hva3Top, hva3Middle, hva3Bottom, hva3Baseline, hva3Default);

  TIpHtmlElemMarginStyle = (
    hemsAuto, // use default
    hemsPx    // pixel
    );

  TIpHtmlElemMargin = record
    Style: TIpHtmlElemMarginStyle;
    Size: single; // negative values are not yet supported
  end;

  TFontNameStr = string[50];
  TIpHtmlPropAFieldsRec = record
    BaseFontSize: Integer;
    FontSize: Integer;
    FontStyle: TFontStyles;
    FontName: TFontNameStr;
  end;

  TIpHtmlPropBFieldsRec = record
    FontBaseline: Integer;
    Alignment: TIpHtmlAlign;
    FontColor: TColor;
    VAlignment: TIpHtmlVAlign3;
    LinkColor : TColor;
    VLinkColor : TColor;
    ALinkColor : TColor;
    HoverColor : TColor;
    HoverBgColor : TColor;
    BgColor : TColor;
    Preformatted : Boolean;
    NoBreak : Boolean;
    ElemMarginTop: TIpHtmlElemMargin;
    ElemMarginLeft: TIpHtmlElemMargin;
    ElemMarginBottom: TIpHtmlElemMargin;
    ElemMarginRight: TIpHtmlElemMargin;
  end;

  TIpHtmlPropsAList = class;

  { TIpHtmlPropA }

  {display properties that affect the font size}
  TIpHtmlPropA = class
  private
    FOwner: TIpHtmlPropsAList;
    FPropRec : TIpHtmlPropAFieldsRec;
    FUseCount: Integer;
    FKnownSizeOfSpace: TSize;
    FSizeOfSpaceKnown : Boolean;
    procedure SetBaseFontSize(const Value: Integer);
    procedure SetFontName(const Value: TFontNameStr);
    procedure SetFontSize(const Value: Integer);
    procedure SetFontStyle(const Value: TFontStyles);
  public
    KnownSizeOfHyphen : TSize;
    tmAscent, tmDescent, tmHeight : Integer;
    constructor Create(AOwner: TIpHtmlPropsAList);
    destructor Destroy; override;
    procedure Assign(const Source: TIpHtmlPropA);
    procedure DecUse;
    procedure IncUse;
    procedure SetKnownSizeOfSpace(const Size:TSize);
  public
    property SizeOfSpaceKnown: Boolean read FSizeOfSpaceKnown;
    property KnownSizeOfSpace : TSize read FKnownSizeOfSpace;
    property BaseFontSize : Integer read FPropRec.BaseFontSize write SetBaseFontSize;
    property FontName : TFontNameStr read FPropRec.FontName write SetFontName;
    property FontSize : Integer read FPropRec.FontSize write SetFontSize;
    property FontStyle : TFontStyles read FPropRec.FontStyle write SetFontStyle;
    property UseCount : Integer read FUseCount write FUseCount;
  end;

  TIpHtmlPropsBList = class;

  { TIpHtmlPropB }

  {display properties that don't affect the font size}
  TIpHtmlPropB = class
  private
    FOwner: TIpHtmlPropsBList;
    FPropRec : TIpHtmlPropBFieldsRec;
    FUseCount: Integer;
  public
    constructor Create(AOwner: TIpHtmlPropsBList);
    destructor Destroy; override;
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
    property UseCount : Integer read FUseCount write FUseCount;
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
    FDirtyA, FDirtyB: Boolean;
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
    procedure CopyPropARecTo(var pRec: TIpHtmlPropAFieldsRec);
    procedure CopyPropBRecTo(var pRec: TIpHtmlPropBFieldsRec);
    procedure CopyPropARecFrom(var pRec: TIpHtmlPropAFieldsRec);
    procedure CopyPropBRecFrom(var pRec: TIpHtmlPropBFieldsRec);
    procedure FindOrCreatePropA(var pRec: TIpHtmlPropAFieldsRec);
    procedure FindOrCreatePropB(var pRec: TIpHtmlPropBFieldsRec);
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

  { TIpHtmlPropsAList and TIpHtmlPropsBList }

  TIpHtmlPropsAList = class(TObjectList)
  private
    FDummyA : TIpHtmlPropA;
    function GetItem(Index: Integer): TIpHtmlPropA;
    procedure SetItem(Index: Integer; AValue: TIpHtmlPropA);
  public
    constructor Create;
    destructor Destroy; override;
    function FindPropARec(var pRec: TIpHtmlPropAFieldsRec): TIpHtmlPropA;
    procedure ResetCache;
    property Items[Index: Integer]: TIpHtmlPropA read GetItem write SetItem; default;
  end;

  TIpHtmlPropsBList = class(TObjectList)
  private
    FDummyB : TIpHtmlPropB;
    function GetItem(Index: Integer): TIpHtmlPropB;
    procedure SetItem(Index: Integer; AValue: TIpHtmlPropB);
  public
    constructor Create;
    destructor Destroy; override;
    function FindPropBRec(var pRec: TIpHtmlPropBFieldsRec): TIpHtmlPropB;
    property Items[Index: Integer]: TIpHtmlPropB read GetItem write SetItem; default;
  end;


implementation

function AreHtmlMarginsEqual(Margin1, Margin2: TIpHtmlElemMargin): boolean;
begin
  Result:=(Margin1.Style=Margin2.Style) and (Margin1.Size=Margin2.Size);
end;

{ TIpHtmlPropA }

constructor TIpHtmlPropA.Create(AOwner: TIpHtmlPropsAList);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TIpHtmlPropA.Destroy;
begin
  inherited Destroy;
end;

procedure TIpHtmlPropA.Assign(const Source: TIpHtmlPropA);
begin
  if Source <> nil then begin
    Move(Source.FPropRec, FPropRec, sizeof(TIpHtmlPropAFieldsRec));
  end;
end;

procedure TIpHtmlPropA.DecUse;
begin
  if FUseCount > 0 then Dec(FUseCount);
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

procedure TIpHtmlPropA.SetFontName(const Value: TFontNameStr);
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

constructor TIpHtmlPropB.Create(AOwner: TIpHtmlPropsBList);
begin
  inherited Create;
  FOwner := AOwner;
  FPropRec.BgColor := -1;
  FPropRec.HoverColor := -1;
  FPropRec.HoverBgColor := -1;
end;

destructor TIpHtmlPropB.Destroy;
begin
  inherited Destroy;
end;

procedure TIpHtmlPropB.Assign(const Source: TIpHtmlPropB);
begin
  if Source <> nil then
    FPropRec := Source.FPropRec;
    //Move(Source.FPropRec, FPropRec, sizeof(TIpHtmlPropBFieldsRec));
end;

procedure TIpHtmlPropB.DecUse;
begin
  Dec(FUseCount);
  if FUseCount < 0 then
    raise EIpHtmlException.Create(SHtmlInternal)
  else if FUseCount = 0 then
    if FOwner.Remove(Self) = -1 then
      raise EIpHtmlException.Create(SHtmlInternal);
end;

procedure TIpHtmlPropB.IncUse;
begin
  Inc(FUseCount);
end;

{ TIpHtmlProps }

constructor TIpHtmlProps.Create(APropsAList: TIpHtmlPropsAList; APropsBList: TIpHtmlPropsBList);
begin
  FPropsACache := APropsAList;
  FPropsBCache := APropsBList;
  FPropA := FPropsACache.FDummyA;
  FPropA.IncUse;
  FPropB := FPropsBCache.FDummyB;
  FPropB.IncUse;
  //BgColor := -1;
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
  if b then Inc(FDelayCache)
  else if FDelayCache > 0 then
    Dec(FDelayCache);

  if (not b) and (FDelayCache = 0) then
  begin
      if FDirtyA then
      begin
        //Finish/Commit transaction
        FDirtyA := False;
      end;
      if FDirtyB then
      begin
        //Finish/Commit transaction
        FDirtyB := False;
      end;
  end;
end;

procedure TIpHtmlProps.CopyPropARecTo(var pRec: TIpHtmlPropAFieldsRec);
begin
    Move(FPropA.FPropRec, pRec, sizeof(TIpHtmlPropAFieldsRec))
end;

procedure TIpHtmlProps.CopyPropBRecTo(var pRec: TIpHtmlPropBFieldsRec);
begin
    Move(FPropB.FPropRec, pRec, sizeof(TIpHtmlPropBFieldsRec))
end;

procedure TIpHtmlProps.CopyPropARecFrom(var pRec: TIpHtmlPropAFieldsRec);
begin
    Move(pRec, FPropA.FPropRec, sizeof(TIpHtmlPropAFieldsRec));
end;

procedure TIpHtmlProps.CopyPropBRecFrom(var pRec: TIpHtmlPropBFieldsRec);
begin
    Move(pRec, FPropB.FPropRec, sizeof(TIpHtmlPropBFieldsRec));
end;

procedure TIpHtmlProps.FindOrCreatePropA(var pRec: TIpHtmlPropAFieldsRec);
var
  NewPropA : TIpHtmlPropA;
begin
  if FDirtyA then
    // we are in a transaction updating a new unique entry
    CopyPropARecFrom(pRec)
  else
  begin
    NewPropA := FPropsACache.FindPropARec(pRec);
    if NewPropA = nil then begin
      NewPropA := TIpHtmlPropA.Create(FPropsACache);
      Move(pRec, NewPropA.FPropRec, sizeof(TIpHtmlPropAFieldsRec));
      //Start Transaction if DelayCache is set
      if DelayCache then FDirtyA := True;
      FPropsACache.Add(NewPropA);
    end;
    NewPropA.IncUse;
    FPropA.DecUse;
    FPropA := NewPropA;
  end;
end;

procedure TIpHtmlProps.FindOrCreatePropB(var pRec: TIpHtmlPropBFieldsRec);
var
  NewPropB : TIpHtmlPropB;
begin
  if FDirtyB then
    //we are in a transaction updating a new unique entry
    CopyPropBRecFrom(pRec)
  else
  begin
    NewPropB := FPropsBCache.FindPropBRec(pRec);
    if NewPropB = nil then begin
      NewPropB := TIpHtmlPropB.Create(FPropsBCache);
      Move(pRec, NewPropB.FPropRec, sizeof(TIpHtmlPropBFieldsRec));
      //Start Transaction if DelayCache is set
      if DelayCache then FDirtyB := True;
      FPropsBCache.Add(NewPropB);
    end;
    NewPropB.IncUse;
    FPropB.DecUse;
    FPropB := NewPropB;
  end;
end;

procedure TIpHtmlProps.SetAlignment(const Value: TIpHtmlAlign);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if (Value <> haDefault) and (Value <> Alignment) then begin
    CopyPropBRecTo(pRec);
    pRec.Alignment:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetALinkColor(const Value: TColor);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> ALinkColor then begin
    CopyPropBRecTo(pRec);
    pRec.ALinkColor:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetBaseFontSize(const Value: Integer);
var
  pRec : TIpHtmlPropAFieldsRec;
begin
  if Value <> BaseFontSize then begin
    CopyPropARecTo(pRec);
    pRec.BaseFontSize:=Value;
    FindOrCreatePropA(pRec);
  end;
end;

procedure TIpHtmlProps.SetBgColor(const Value: TColor);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> BgColor then begin
    CopyPropBRecTo(pRec);
    pRec.BgColor:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetFontBaseline(const Value: Integer);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> FontBaseline then begin
    CopyPropBRecTo(pRec);
    pRec.FontBaseline:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetFontColor(const Value: TColor);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> FontColor then begin
    CopyPropBRecTo(pRec);
    pRec.FontColor:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetFontName(const Value: string);
var
  pRec : TIpHtmlPropAFieldsRec;
begin
  if Value <> FontName then begin
    CopyPropARecTo(pRec);
    pRec.FontName:=Value;
    FindOrCreatePropA(pRec);
  end;
end;

procedure TIpHtmlProps.SetFontSize(const Value: Integer);
var
  pRec : TIpHtmlPropAFieldsRec;
begin
  if Value <> FontSize then begin
    CopyPropARecTo(pRec);
    pRec.FontSize:=Value;
    FindOrCreatePropA(pRec);
  end;
end;

procedure TIpHtmlProps.SetFontStyle(const Value: TFontStyles);
var
  pRec : TIpHtmlPropAFieldsRec;
begin
  if Value <> FontStyle then begin
    CopyPropARecTo(pRec);
    pRec.FontStyle:=Value;
    FindOrCreatePropA(pRec);
  end;
end;

procedure TIpHtmlProps.SetLinkColor(const Value: TColor);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> LinkColor then begin
    CopyPropBRecTo(pRec);
    pRec.LinkColor:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetElemMarginBottom(const AValue: TIpHtmlElemMargin);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if AreHtmlMarginsEqual(AValue,ElemMarginBottom) then exit;
  CopyPropBRecTo(pRec);
  pRec.ElemMarginBottom:=AValue;
  FindOrCreatePropB(pRec);
end;

procedure TIpHtmlProps.SetElemMarginLeft(const AValue: TIpHtmlElemMargin);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if AreHtmlMarginsEqual(AValue,ElemMarginLeft) then exit;
  CopyPropBRecTo(pRec);
  pRec.ElemMarginLeft:=AValue;
  FindOrCreatePropB(pRec);
end;

procedure TIpHtmlProps.SetElemMarginRight(const AValue: TIpHtmlElemMargin);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if AreHtmlMarginsEqual(AValue,ElemMarginRight) then exit;
  CopyPropBRecTo(pRec);
  pRec.ElemMarginRight:=AValue;
  FindOrCreatePropB(pRec);
end;

procedure TIpHtmlProps.SetElemMarginTop(const AValue: TIpHtmlElemMargin);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if AreHtmlMarginsEqual(AValue,ElemMarginTop) then exit;
  CopyPropBRecTo(pRec);
  pRec.ElemMarginTop:=AValue;
  FindOrCreatePropB(pRec);
end;

procedure TIpHtmlProps.SetNoBreak(const Value: Boolean);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> NoBreak then begin
    CopyPropBRecTo(pRec);
    pRec.NoBreak:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetPreformatted(const Value: Boolean);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> Preformatted then begin
    CopyPropBRecTo(pRec);
    pRec.Preformatted:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetVAlignment(const Value: TIpHtmlVAlign3);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> VAlignment then begin
    CopyPropBRecTo(pRec);
    pRec.VAlignment:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetVLinkColor(const Value: TColor);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> VLinkColor then begin
    CopyPropBRecTo(pRec);
    pRec.VLinkColor:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetHoverColor(const Value: TColor);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> HoverColor then begin
    CopyPropBRecTo(pRec);
    pRec.HoverColor:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

procedure TIpHtmlProps.SetHoverBgColor(const Value: TColor);
var
  pRec : TIpHtmlPropBFieldsRec;
begin
  if Value <> HoverBgColor then begin
    CopyPropBRecTo(pRec);
    pRec.HoverBgColor:=Value;
    FindOrCreatePropB(pRec);
  end;
end;

{ TIpHtmlPropsAList }

constructor TIpHtmlPropsAList.Create;
begin
  inherited Create;
  FDummyA := TIpHtmlPropA.Create(Self);
  FDummyA.UseCount := 1;
  Add(FDummyA);
end;

destructor TIpHtmlPropsAList.Destroy;
begin
  inherited Destroy;
end;

procedure TIpHtmlPropsAList.ResetCache;
var
  i : Integer;
begin
  for i := 0 to Pred(Count) do begin
    Items[i].FSizeOfSpaceKnown := False;
    Items[i].tmHeight := 0;
  end;
end;

function TIpHtmlPropsAList.FindPropARec(var pRec: TIpHtmlPropAFieldsRec): TIpHtmlPropA;
var
  i: Integer;
begin
  for i := 0 to Pred(Count) do begin
    Result := Items[i];
    if CompareByte(Result.FPropRec, pRec, sizeof(TIpHtmlPropAFieldsRec)) = 0 then
       exit;
  end;
  Result := nil;
end;

// Getter / Setter

function TIpHtmlPropsAList.GetItem(Index: Integer): TIpHtmlPropA;
begin
  Result := TIpHtmlPropA(inherited Items[Index]);
end;

procedure TIpHtmlPropsAList.SetItem(Index: Integer; AValue: TIpHtmlPropA);
begin
  inherited Items[Index] := AValue;
end;


{ TIpHtmlPropsBList }

constructor TIpHtmlPropsBList.Create;
begin
  inherited Create;
  FDummyB := TIpHtmlPropB.Create(Self);
  FDummyB.UseCount := 1;
  Add(FDummyB);
end;

destructor TIpHtmlPropsBList.Destroy;
begin
  inherited Destroy;
end;

function TIpHtmlPropsBList.FindPropBRec(var pRec: TIpHtmlPropBFieldsRec): TIpHtmlPropB;
var
  i: Integer;
begin
  for i := 0 to Pred(Count) do begin
    Result := Items[i];
    if CompareByte(Result.FPropRec, pRec, sizeof(TIpHtmlPropBFieldsRec)) = 0 then
       exit;
  end;
  Result := nil;
end;

// Getter / Setter

function TIpHtmlPropsBList.GetItem(Index: Integer): TIpHtmlPropB;
begin
  Result := TIpHtmlPropB(inherited Items[Index]);
end;

procedure TIpHtmlPropsBList.SetItem(Index: Integer; AValue: TIpHtmlPropB);
begin
  inherited Items[Index] := AValue;
end;

end.

