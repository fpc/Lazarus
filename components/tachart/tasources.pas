{

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Authors: Alexander Klenin

}

unit TASources;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, SysUtils, TAChartUtils;

type
  EEditableSourceRequired = class(EChartError);
  EListenerError = class(EChartError);

  TChartDataItem = record
    X, Y: Double;
    Color: TColor;
    Text: String;
  end;
  PChartDataItem = ^TChartDataItem;

  { TListener }

  TListener = class
  private
    FIsListening: Boolean;
  public
    procedure Forget; virtual;
    procedure Notify; virtual; abstract;
    property IsListening: Boolean read FIsListening;
  end;

  { TCustomChartSource }

  TCustomChartSource = class(TComponent)
  private
    FListeners: array of TListener;
    FUpdateCount: Integer;
    function FindListener(AListener: TListener): Integer;
  protected
    FExtent: TDoubleRect;
    FExtentIsValid: Boolean;
    FValuesTotal: Double;
    FValuesTotalIsValid: Boolean;
    function GetCount: Integer; virtual; abstract;
    function GetItem(AIndex: Integer): PChartDataItem; virtual; abstract;
    procedure InvalidateCaches;
    procedure Notify;
  public
    destructor Destroy; override;
  public
    procedure AfterDraw; virtual;
    procedure BeforeDraw; virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: Boolean; inline;
    procedure Subscribe(AListener: TListener);
    procedure Unsubscribe(AListener: TListener);
  public
    function Extent: TDoubleRect; virtual;
    function ValuesTotal: Double; virtual;
    function XOfMax: Double;
    function XOfMin: Double;

    property Count: Integer read GetCount;
    property Item[AIndex: Integer]: PChartDataItem read GetItem; default;
  end;

  { TListChartSource }

  TListChartSource = class(TCustomChartSource)
  private
    FData: TList;
    FDataPoints: TStrings;
    procedure ClearCaches;
    procedure SetDataPoints(AValue: TStrings);
    procedure UpdateCachesAfterAdd(AX, AY: Double);
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): PChartDataItem; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function Add(AX, AY: Double; const ALabel: String; AColor: TColor): Integer;
    procedure Clear;
    procedure CopyForm(ASource: TCustomChartSource);
    procedure Delete(AIndex: Integer); inline;
    procedure SetXValue(AIndex: Integer; AValue: Double);
    procedure SetYValue(AIndex: Integer; AValue: Double);
  published
    property DataPoints: TStrings read FDataPoints write SetDataPoints;
  end;

  { TMWCRandomGenerator }

  // Mutliply-with-carry random number generator.
  // Algorithm by George Marsaglia.
  // A generator is incapsulated in a class to allow using many simultaneous
  // random sequences, each determined by its own seed.
  TMWCRandomGenerator = class
  private
    FHistory: array [0..4] of LongWord;
    procedure SetSeed(const AValue: Integer);
  public
    function Get: LongWord;
    function GetInRange(AMin, AMax: Integer): Integer;
    property Seed: Integer write SetSeed;
  end;

  { TRandomChartSource }

  TRandomChartSource = class(TCustomChartSource)
  private
    FPointsNumber: Integer;
    FRandomX: Boolean;
    FRandSeed: Integer;
    FXMax: Double;
    FXMin: Double;
    FYMax: Double;
    FYMin: Double;
  private
    FCurIndex: Integer;
    FCurItem: TChartDataItem;
    FRNG: TMWCRandomGenerator;

    procedure SetPointsNumber(const AValue: Integer);
    procedure SetRandomX(const AValue: Boolean);
    procedure SetRandSeed(const AValue: Integer);
    procedure SetXMax(const AValue: Double);
    procedure SetXMin(const AValue: Double);
    procedure SetYMax(const AValue: Double);
    procedure SetYMin(const AValue: Double);
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): PChartDataItem; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property PointsNumber: Integer
      read FPointsNumber write SetPointsNumber default 0;
    property RandomX: Boolean read FRandomX write SetRandomX default false;
    property RandSeed: Integer read FRandSeed write SetRandSeed;
    property XMax: Double read FXMax write SetXMax;
    property XMin: Double read FXMin write SetXMin;
    property YMax: Double read FYMax write SetYMax;
    property YMin: Double read FYMin write SetYMin;
  end;

  TUserDefinedChartSource = class;

  TGetChartDataItemEvent = procedure (
    ASource: TUserDefinedChartSource; AIndex: Integer;
    var AItem: TChartDataItem) of object;

  { TUserDefinedChartSource }

  TUserDefinedChartSource = class(TCustomChartSource)
  private
    FItem: TChartDataItem;
    FOnGetChartDataItem: TGetChartDataItemEvent;
    FPointsNumber: Integer;
    procedure SetOnGetChartDataItem(const AValue: TGetChartDataItemEvent);
    procedure SetPointsNumber(const AValue: Integer);
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): PChartDataItem; override;
  public
    procedure Reset; inline;
  published
    property OnGetChartDataItem: TGetChartDataItemEvent
      read FOnGetChartDataItem write SetOnGetChartDataItem;
    property PointsNumber: Integer
      read FPointsNumber write SetPointsNumber default 0;
  end;

function DoublePoint(const ACoord: TChartDataItem): TDoublePoint; inline; overload;
procedure Register;
procedure SetDataItemDefaults(var AItem: TChartDataItem);

implementation

uses
  LCLIntf, Math, StrUtils;

type

  { TListChartSourceStrings }

  TListChartSourceStrings = class(TStrings)
  private
    FSource: TListChartSource;
    procedure Parse(const AString: String; ADataItem: PChartDataItem);
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    procedure Put(Index: Integer; const S: string); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

function DoublePoint(const ACoord: TChartDataItem): TDoublePoint;
begin
  Result.X := ACoord.X;
  Result.Y := ACoord.Y;
end;

procedure Register;
begin
  RegisterComponents(
    CHART_COMPONENT_IDE_PAGE,
    [TListChartSource, TRandomChartSource, TUserDefinedChartSource]);
end;

procedure SetDataItemDefaults(var AItem: TChartDataItem);
begin
  AItem.X := 0;
  AItem.Y := 0;
  AItem.Color := clTAColor;
  AItem.Text := '';
end;

{ TCustomChartSource }

procedure TCustomChartSource.AfterDraw;
begin
  // empty
end;

procedure TCustomChartSource.BeforeDraw;
begin
  // empty
end;

procedure TCustomChartSource.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

destructor TCustomChartSource.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FListeners) do
    FListeners[i].Forget;
  FListeners := nil;
  inherited Destroy;
end;

procedure TCustomChartSource.EndUpdate;
begin
  Dec(FUpdateCount);
  Notify;
end;

function TCustomChartSource.Extent: TDoubleRect;
var
  i: Integer;
begin
  if FExtentIsValid then exit(FExtent);
  FExtent := EmptyExtent;
  for i := 0 to Count - 1 do
    with Item[i]^ do begin
      UpdateMinMax(X, FExtent.a.X, FExtent.b.X);
      UpdateMinMax(Y, FExtent.a.Y, FExtent.b.Y);
    end;
  FExtentIsValid := true;
  Result := FExtent;
end;

function TCustomChartSource.FindListener(AListener: TListener): Integer;
begin
  for Result := 0 to High(FListeners) do
    if FListeners[Result] = AListener then exit;
  Result := -1;
end;

procedure TCustomChartSource.InvalidateCaches;
begin
  FExtentIsValid := false;
  FValuesTotalIsValid := false;
end;

function TCustomChartSource.IsUpdating: Boolean; inline;
begin
  Result := FUpdateCount > 0;
end;

procedure TCustomChartSource.Notify;
var
  i: Integer;
begin
  if IsUpdating then exit;
  for i := 0 to High(FListeners) do
    FListeners[i].Notify;
end;

procedure TCustomChartSource.Subscribe(AListener: TListener);
begin
  if AListener.IsListening then
    raise EListenerError.Create('Listener subscribed twice');
  if FindListener(AListener) >= 0 then
    raise EListenerError.Create('Duplicate listener');
  AListener.FIsListening := true;
  SetLength(FListeners, Length(FListeners) + 1);
  FListeners[High(FListeners)] := AListener;
end;

procedure TCustomChartSource.Unsubscribe(AListener: TListener);
var
  i, j: Integer;
begin
  if not AListener.IsListening then
    raise EListenerError.Create('Listener not subscribed');
  AListener.FIsListening := false;
  j := FindListener(AListener);
  if j < 0 then
    raise EListenerError.Create('Listener not found');
  for i := j + 1 to High(FListeners) do
    FListeners[i] := FListeners[i + 1];
  SetLength(FListeners, Length(FListeners) - 1);
end;

function TCustomChartSource.ValuesTotal: Double;
var
  i: Integer;
begin
  if FValuesTotalIsValid then exit(FValuesTotal);
  FValuesTotal := 0;
  for i := 0 to Count - 1 do
    FValuesTotal += Item[i]^.Y;
  FValuesTotalIsValid := true;
  Result := FValuesTotal;
end;

function TCustomChartSource.XOfMax: Double;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Item[i]^ do
      if Y = Extent.b.Y then exit(X);
  Result := 0.0;
end;

function TCustomChartSource.XOfMin: Double;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Item[i]^ do
      if Y = Extent.a.Y then exit(X);
  Result := 0.0;
end;

{ TListChartSourceStrings }

procedure TListChartSourceStrings.Clear;
begin
  FSource.Clear;
end;

procedure TListChartSourceStrings.Delete(Index: Integer);
begin
  FSource.Delete(Index);
end;

function TListChartSourceStrings.Get(Index: Integer): string;
begin
  with FSource[Index]^ do
    Result := Format('%g|%g|%s|%s',
      [X, Y, IfThen(Color = clTAColor, '', '$' + IntToHex(Color, 6)), Text]);
end;

function TListChartSourceStrings.GetCount: Integer;
begin
  Result := FSource.Count;
end;

procedure TListChartSourceStrings.Insert(Index: Integer; const S: string);
var
  item: PChartDataItem;
begin
  New(item);
  FSource.FData.Insert(Index, item);
  Parse(S, item);
  FSource.UpdateCachesAfterAdd(item^.X, item^.Y);
end;

procedure TListChartSourceStrings.Parse(
  const AString: String; ADataItem: PChartDataItem);
var
  p: Integer;

  function NextPart: String;
  begin
    Result := ExtractSubstr(AString, p, ['|']);
  end;

begin
  p := 1;
  with ADataItem^ do begin
    X := StrToFloatDef(NextPart, 0.0);
    Y := StrToFloatDef(NextPart, 0.0);
    Color := StrToIntDef(NextPart, clTAColor);
    Text := NextPart;
  end;
end;

procedure TListChartSourceStrings.Put(Index: Integer; const S: string);
begin
  Parse(S, FSource[Index]);
end;

{ TListChartSource }

function TListChartSource.Add(
  AX, AY: Double; const ALabel: String; AColor: TColor): Integer;
var
  pcc: PChartDataItem;
begin
  New(pcc);
  pcc^.X := AX;
  pcc^.Y := AY;
  pcc^.Color := AColor;
  pcc^.Text := ALabel;
  UpdateCachesAfterAdd(AX, AY);

  // We keep data points ordered by X coordinate.
  // Note that this leads to O(N^2) time except
  // for the case of adding already ordered points.
  // So, is the user wants to add many (>10000) points to a graph,
  // he should pre-sort them to avoid performance penalty.
  Result := FData.Count;
  while (Result > 0) and (Item[Result - 1]^.X > AX) do
    Dec(Result);
  FData.Insert(Result, pcc);
  Notify;
end;

procedure TListChartSource.Clear; inline;
var
  i: Integer;
begin
  for i := 0 to FData.Count - 1 do
    Dispose(Item[i]);
  FData.Clear;
  ClearCaches;
  Notify;
end;

procedure TListChartSource.ClearCaches;
begin
  FExtent := EmptyExtent;
  FExtentIsValid := true;
  FValuesTotal := 0;
  FValuesTotalIsValid := true;
end;

procedure TListChartSource.CopyForm(ASource: TCustomChartSource);
var
  i: Integer;
begin
  BeginUpdate;
  try
    Clear;
    for i := 0 to ASource.Count - 1 do
      with ASource[i]^ do
        Add(X, Y, Text, Color);
  finally
    EndUpdate;
  end;
end;

constructor TListChartSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData := TList.Create;
  FDataPoints := TListChartSourceStrings.Create;
  TListChartSourceStrings(FDataPoints).FSource := Self;
  ClearCaches;
end;

procedure TListChartSource.Delete(AIndex: Integer);
begin
  with Item[AIndex]^ do begin
    FExtentIsValid :=
      (FExtent.a.X < X) and (X < FExtent.b.X) and
      (FExtent.a.Y < Y) and (Y < FExtent.b.Y);
    if FValuesTotalIsValid then
      FValuesTotal -= Y;
  end;
  Dispose(Item[AIndex]);
  FData.Delete(AIndex);
  Notify;
end;

destructor TListChartSource.Destroy;
begin
  Clear;
  FDataPoints.Free;
  FData.Free;
  inherited Destroy;
end;

function TListChartSource.GetCount: Integer;
begin
  Result := FData.Count;
end;

function TListChartSource.GetItem(AIndex: Integer): PChartDataItem;
begin
  Result := PChartDataItem(FData.Items[AIndex]);
end;

procedure TListChartSource.SetDataPoints(AValue: TStrings);
begin
  if FDataPoints = AValue then exit;
  BeginUpdate;
  try
    FDataPoints.Assign(AValue);
  finally
    EndUpdate;
  end;
end;

procedure TListChartSource.SetXValue(AIndex: Integer; AValue: Double);
var
  i: Integer;
  oldX: Double;
begin
  // TODO: Ensure that points are sorted by X.

  oldX := Item[AIndex]^.X;
  Item[AIndex]^.X := AValue;

  if FExtentIsValid then begin
    if AValue <= FExtent.a.X then FExtent.a.X := AValue
    else if AValue >= FExtent.b.X then FExtent.b.X := AValue
    else begin
      if oldX = FExtent.b.X then begin
        FExtent.b.X := NegInfinity;
        for i := 0 to Count - 1 do
          FExtent.b.X := Max(FExtent.b.X, Item[i]^.X);
      end;
      if oldX = FExtent.a.X then begin
        FExtent.a.X := Infinity;
        for i := 0 to Count - 1 do
          FExtent.a.X := Min(FExtent.a.X, Item[i]^.X);
      end;
    end;
  end;
  Notify;
end;

procedure TListChartSource.SetYValue(AIndex: Integer; AValue: Double);
var
  i: Integer;
  oldY: Double;
begin
  oldY := Item[AIndex]^.Y;
  Item[AIndex]^.Y := AValue;
  if FValuesTotalIsValid then
    FValuesTotal += AValue - oldY;

  if FExtentIsValid then begin
    if AValue <= FExtent.a.Y then FExtent.a.Y := AValue
    else if AValue >= FExtent.b.Y then FExtent.b.Y := AValue
    else begin
      if oldY = FExtent.b.Y then begin
        FExtent.b.Y := NegInfinity;
        for i := 0 to Count - 1 do
          FExtent.b.Y := Max(FExtent.b.Y, Item[i]^.Y);
      end;
      if oldY = FExtent.a.Y then begin
        FExtent.a.Y := Infinity;
        for i := 0 to Count - 1 do
          FExtent.a.Y := Min(FExtent.a.Y, Item[i]^.Y);
      end;
    end;
  end;
  Notify;
end;

procedure TListChartSource.UpdateCachesAfterAdd(AX, AY: Double);
begin
  if FExtentIsValid then begin
    UpdateMinMax(AX, FExtent.a.X, FExtent.b.X);
    UpdateMinMax(AY, FExtent.a.Y, FExtent.b.Y);
  end;
  if FValuesTotalIsValid then
    FValuesTotal += AY;
end;

{ TMWCRandomGenerator }

function TMWCRandomGenerator.Get: LongWord;
const
  MULT: array [0..4] of UInt64 = (5115, 1776, 1492, 2111111111, 1);
var
  i: Integer;
  s: UInt64;
begin
  s := 0;
  for i := 0 to High(FHistory) do
    s += MULT[i] * FHistory[i];
  FHistory[3] := FHistory[2];
  FHistory[2] := FHistory[1];
  FHistory[1] := FHistory[0];
  FHistory[4] := Hi(s);
  FHistory[0] := Lo(s);
  Result := FHistory[0];
end;

function TMWCRandomGenerator.GetInRange(AMin, AMax: Integer): Integer;
var
  m: UInt64;
begin
  m := AMax - AMin + 1;
  m *= Get;
  // m is now equidistributed on [0, (2^32-1) * range],
  // so its upper double word is equidistributed on [0, range].
  Result := Integer(Hi(m)) + AMin;
end;

procedure TMWCRandomGenerator.SetSeed(const AValue: Integer);
var
  i: Integer;
begin
  FHistory[0] := AValue;
  // Use trivial LCG for seeding
  for i := 1 to High(FHistory) do
    FHistory[i] := Lo(Int64(FHistory[i - 1]) * 29943829 - 1);
  // Skip some initial values to increase randomness.
  for i := 1 to 20 do
    Get;
end;

{ TRandomChartSource }

constructor TRandomChartSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCurItem.Color := clTAColor;
  FRNG := TMWCRandomGenerator.Create;
  RandSeed := GetTickCount;
end;

destructor TRandomChartSource.Destroy;
begin
  FRNG.Free;
  inherited Destroy;
end;

function TRandomChartSource.GetCount: Integer;
begin
  Result := FPointsNumber;
end;

function TRandomChartSource.GetItem(AIndex: Integer): PChartDataItem;
begin
  if FCurIndex > AIndex then begin
    FRNG.Seed := FRandSeed;
    FCurIndex := -1;
  end;
  while FCurIndex < AIndex do begin
    Inc(FCurIndex);
    if XMax <= XMin then
      FCurItem.X := XMin
    else begin
      if FRandomX then
        FCurItem.X := FRNG.Get / High(LongWord)
      else
        FCurItem.X := FCurIndex / (Count - 1);
      FCurItem.X := FCurItem.X * (XMax - XMin) + XMin;
    end;
    if YMax <= YMin then
      FCurItem.Y := YMin
    else
      FCurItem.Y := FRNG.Get / High(LongWord) * (YMax - YMin) + YMin;
  end;
  Result := @FCurItem;
end;

procedure TRandomChartSource.SetPointsNumber(const AValue: Integer);
begin
  if FPointsNumber = AValue then exit;
  FPointsNumber := AValue;
  InvalidateCaches;
  Notify;
end;

procedure TRandomChartSource.SetRandomX(const AValue: Boolean);
begin
  if FRandomX = AValue then exit;
  FRandomX := AValue;
  InvalidateCaches;
  Notify;
end;

procedure TRandomChartSource.SetRandSeed(const AValue: Integer);
begin
  if FRandSeed = AValue then exit;
  FRandSeed := AValue;
  FRNG.Seed := AValue;
  FCurIndex := -1;
  InvalidateCaches;
  Notify;
end;

procedure TRandomChartSource.SetXMax(const AValue: Double);
begin
  if FXMax = AValue then exit;
  FXMax := AValue;
  InvalidateCaches;
  Notify;
end;

procedure TRandomChartSource.SetXMin(const AValue: Double);
begin
  if FXMin = AValue then exit;
  FXMin := AValue;
  InvalidateCaches;
  Notify;
end;

procedure TRandomChartSource.SetYMax(const AValue: Double);
begin
  if FYMax = AValue then exit;
  FYMax := AValue;
  InvalidateCaches;
  Notify;
end;

procedure TRandomChartSource.SetYMin(const AValue: Double);
begin
  if FYMin = AValue then exit;
  FYMin := AValue;
  InvalidateCaches;
  Notify;
end;

{ TListener }

procedure TListener.Forget;
begin
  FIsListening := false;
end;

{ TUserDefinedChartSource }

function TUserDefinedChartSource.GetCount: Integer;
begin
  Result := FPointsNumber;
end;

function TUserDefinedChartSource.GetItem(AIndex: Integer): PChartDataItem;
begin
  SetDataItemDefaults(FItem);
  if Assigned(FOnGetChartDataItem) then
    FOnGetChartDataItem(Self, AIndex, FItem);
  Result := @FItem;
end;

procedure TUserDefinedChartSource.Reset;
begin
  InvalidateCaches;
  Notify;
end;

procedure TUserDefinedChartSource.SetOnGetChartDataItem(
  const AValue: TGetChartDataItemEvent);
begin
  if FOnGetChartDataItem = AValue then exit;
  FOnGetChartDataItem := AValue;
  Reset;
end;

procedure TUserDefinedChartSource.SetPointsNumber(const AValue: Integer);
begin
  if FPointsNumber = AValue then exit;
  FPointsNumber := AValue;
  Reset;
end;

end.

