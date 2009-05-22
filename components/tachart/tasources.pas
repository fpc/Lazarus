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

  { TCustomChartSource }

  TCustomChartSource = class
  protected
    function GetCount: Integer; virtual; abstract;
    function GetItem(AIndex: Integer): PChartDataItem; virtual; abstract;
  public
    property Count: Integer read GetCount;
    property Item[AIndex: Integer]: PChartDataItem read GetItem; default;
  end;

  { TFixedChartSource }

  TListChartSource = class(TCustomChartSource)
  private
    FData: TList;
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): PChartDataItem; override;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(X, Y: Double; const XLabel: String; Color: TColor): Integer;
    procedure Clear;
    procedure Delete(AIndex: Integer); inline;
  end;

function DoublePoint(const ACoord: TChartDataItem): TDoublePoint; inline; overload;

implementation

function DoublePoint(const ACoord: TChartDataItem): TDoublePoint;
begin
  Result.X := ACoord.X;
  Result.Y := ACoord.Y;
end;

{ TListChartSource }

function TListChartSource.Add(
  X, Y: Double; const XLabel: String; Color: TColor): Integer;
var
  pcc: PChartDataItem;
begin
  New(pcc);
  pcc^.x := X;
  pcc^.y := Y;
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
end;

constructor TListChartSource.Create;
begin
  inherited Create;
  FData := TList.Create;
end;

procedure TListChartSource.Delete(AIndex: Integer);
begin
  Dispose(Item[AIndex]);
  FData.Delete(AIndex);
end;

destructor TListChartSource.Destroy;
begin
  Clear;
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

end.

