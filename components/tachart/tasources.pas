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
    function ValuesTotal: Double; virtual; abstract;

    property Count: Integer read GetCount;
    property Item[AIndex: Integer]: PChartDataItem read GetItem; default;
  end;

  { TListChartSource }

  TListChartSource = class(TCustomChartSource)
  private
    FData: TList;
    FDataPoints: TStrings;
    FOnSetDataPoints: TSimpleNotifyEvent;
    FValuesTotal: Double;
    FValuesTotalIsValid: Boolean;
    procedure SetDataPoints(AValue: TStrings);
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): PChartDataItem; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function Add(X, Y: Double; const XLabel: String; Color: TColor): Integer;
    procedure Clear;
    procedure Delete(AIndex: Integer); inline;
    procedure InvalidateValues; inline;
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
  StrUtils;

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
  X, Y: Double; const XLabel: String; Color: TColor): Integer;
var
  pcc: PChartDataItem;
begin
  New(pcc);
  pcc^.X := X;
  pcc^.Y := Y;
  pcc^.Color := Color;
  pcc^.Text := XLabel;

  // We keep data points ordered by X coordinate.
  // Note that this leads to O(N^2) time except
  // for the case of adding already ordered points.
  // So, is the user wants to add many (>10000) points to a graph,
  // he should pre-sort them to avoid performance penalty.
  Result := FData.Count;
  while (Result > 0) and (Item[Result - 1]^.X > X) do
    Dec(Result);
  FData.Insert(Result, pcc);
end;

procedure TListChartSource.Clear; inline;
var
  i: Integer;
begin
  for i := 0 to FData.Count - 1 do
    Dispose(Item[i]);
  FData.Clear;
  FValuesTotal := 0;
  FValuesTotalIsValid := true;
end;

constructor TListChartSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData := TList.Create;
  FDataPoints := TListChartSourceStrings.Create;
  TListChartSourceStrings(FDataPoints).FSource := Self;
  FValuesTotal := 0;
  FValuesTotalIsValid := true;
end;

procedure TListChartSource.Delete(AIndex: Integer);
begin
  if FValuesTotalIsValid then
    FValuesTotal -= Item[AIndex]^.Y;
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

procedure TListChartSource.SetDataPoints(AValue: TStrings);
begin
  if FDataPoints = AValue then exit;
  FDataPoints.Assign(AValue);
  if Assigned(FOnSetDataPoints) then
    FOnSetDataPoints;
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

