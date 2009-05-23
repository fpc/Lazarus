unit TASources;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, SysUtils, TAChartUtils;

type
  EFixedSourceRequired = class(EChartError);

  TChartDataItem = record
    X, Y: Double;
    Color: TColor;
    Text: String;
  end;
  PChartDataItem = ^TChartDataItem;

  TSimpleNotifyEvent = procedure of object;

  { TCustomChartSource }

  TCustomChartSource = class(TComponent)
  protected
    function GetCount: Integer; virtual; abstract;
    function GetItem(AIndex: Integer): PChartDataItem; virtual; abstract;
  public
    function Extent: TDoubleRect; virtual; abstract;
    function ValuesTotal: Double; virtual; abstract;
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
    FExtent: TDoubleRect;
    FExtentIsValid: Boolean;
    FOnSetDataPoints: TSimpleNotifyEvent;
    FUpdateCount: Integer;
    FValuesTotal: Double;
    FValuesTotalIsValid: Boolean;
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
    procedure BeginUpdate;
    procedure Clear;
    procedure Delete(AIndex: Integer); inline;
    procedure EndUpdate;
    function Extent: TDoubleRect; override;
    procedure InvalidateValues; inline;
    function IsUpdating: Boolean; inline;
    procedure SetXValue(AIndex: Integer; AValue: Double);
    procedure SetYValue(AIndex: Integer; AValue: Double);
    function ValuesTotal: Double; override;

    property OnSetDataPoints: TSimpleNotifyEvent
      read FOnSetDataPoints write FOnSetDataPoints;
  published
    property DataPoints: TStrings read FDataPoints write SetDataPoints;
  end;

function DoublePoint(const ACoord: TChartDataItem): TDoublePoint; inline; overload;
procedure Register;

implementation

uses
  Math, StrUtils;

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
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TListChartSource]);
end;

{ TCustomChartSource }

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
    Result := Format('%g|%g|%d|%s', [X, Y, Color, Text]);
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
    Color := StrToIntDef(NextPart, 0);
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
end;

procedure TListChartSource.BeginUpdate;
begin
  Inc(FUpdateCount);
  FValuesTotalIsValid := false;
  FExtentIsValid := false;
end;

procedure TListChartSource.Clear; inline;
var
  i: Integer;
begin
  for i := 0 to FData.Count - 1 do
    Dispose(Item[i]);
  FData.Clear;
  ClearCaches;
end;

procedure TListChartSource.ClearCaches;
begin
  FExtent := EmptyExtent;
  FExtentIsValid := true;
  FValuesTotal := 0;
  FValuesTotalIsValid := true;
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
end;

destructor TListChartSource.Destroy;
begin
  Clear;
  FDataPoints.Free;
  FData.Free;
  inherited Destroy;
end;

procedure TListChartSource.EndUpdate;
begin
  Dec(FUpdateCount);
end;

function TListChartSource.Extent: TDoubleRect;
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

function TListChartSource.GetCount: Integer;
begin
  Result := FData.Count;
end;

function TListChartSource.GetItem(AIndex: Integer): PChartDataItem;
begin
  Result := PChartDataItem(FData.Items[AIndex]);
end;

procedure TListChartSource.InvalidateValues; inline;
begin
  FValuesTotalIsValid := false;
end;

function TListChartSource.IsUpdating: Boolean; inline;
begin
  Result := FUpdateCount > 0;
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
  if Assigned(FOnSetDataPoints) then
    FOnSetDataPoints;
end;

procedure TListChartSource.SetXValue(AIndex: Integer; AValue: Double);
var
  i: Integer;
  oldX: Double;
begin
  // TODO: Ensure that points are sorted by X.

  oldX := Item[AIndex]^.X;
  Item[AIndex]^.X := AValue;

  if not FExtentIsValid then exit;

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

procedure TListChartSource.SetYValue(AIndex: Integer; AValue: Double);
var
  i: Integer;
  oldY: Double;
begin
  oldY := Item[AIndex]^.Y;
  Item[AIndex]^.Y := AValue;
  if FValuesTotalIsValid then
    FValuesTotal += AValue - oldY;

  if not FExtentIsValid then exit;

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

procedure TListChartSource.UpdateCachesAfterAdd(AX, AY: Double);
begin
  if FExtentIsValid then begin
    UpdateMinMax(AX, FExtent.a.X, FExtent.b.X);
    UpdateMinMax(AY, FExtent.a.Y, FExtent.b.Y);
  end;
  if FValuesTotalIsValid then
    FValuesTotal += AY;
end;

function TListChartSource.ValuesTotal: Double;
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

end.

