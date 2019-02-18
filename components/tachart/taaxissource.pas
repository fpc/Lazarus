{

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}

unit TAAxisSource;

{$H+}

interface

uses
  Classes, TACustomSource, TAChartAxis;

type
  TCustomAxisChartSource = class(TCustomChartSource)
  strict private
    FAxisFrom: TChartAxis;
    FAxisTo: TChartAxis;
    FItem: TChartDataItem;
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): PChartDataItem; override;
    procedure SetXCount(AValue: Cardinal); override;
    procedure SetYCount(AValue: Cardinal); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function IsSorted: Boolean; override;

    property AxisFrom: TChartAxis read FAxisFrom write FAxisFrom;
    property AxisTo: TChartAxis read FAxisTo write FAxisTo;
  end;

implementation

uses
  TAChartUtils;

{ TCustomAxisChartSource }

constructor TCustomAxisChartSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItem.Color := clTAColor;
  FItem.YList := nil;
end;

destructor TCustomAxisChartSource.Destroy;
begin
  inherited Destroy;
end;

function TCustomAxisChartSource.GetCount: Integer;
begin
  if AxisFrom = nil then
    Result := 0
  else
    Result := AxisFrom.ValueCount;
end;

function TCustomAxisChartSource.GetItem(AIndex: Integer): PChartDataItem;
var
  v: Double;
begin
  Result := @FItem;
  if AxisFrom = nil then exit;
  with AxisFrom.Value[AIndex] do begin
    FItem.Text := FText;
    v := FValue;
  end;
  if AxisFrom.Transformations <> nil then
    v := AxisFrom.Transformations.AxisToGraph(v);
  if (AxisTo <> nil) and (AxisTo.Transformations <> nil) then
    v := AxisTo.Transformations.GraphToAxis(v);
  FItem.X := v;
  FItem.Y := v;
end;

function TCustomAxisChartSource.IsSorted: Boolean;
begin
  Result := true;
end;

procedure TCustomAxisChartSource.SetXCount(AValue: Cardinal);
begin
  Unused(AValue);
  raise EXCountError.Create('Cannot set XCount');
end;

procedure TCustomAxisChartSource.SetYCount(AValue: Cardinal);
begin
  Unused(AValue);
  raise EYCountError.Create('Cannot set YCount');
end;

end.

