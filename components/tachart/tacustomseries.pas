{

 Basic types for TAChart series.

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
unit TACustomSeries;

{$H+}

interface

uses
  Classes, Graphics, SysUtils,
  TAChartUtils, TAGraph, TASources, TATypes;

type
  TChartGetMarkEvent = procedure (
    out AFormattedMark: String; AIndex: Integer) of object;

  { TChartSeries }

  TChartSeries = class(TBasicChartSeries)
  private
    FBuiltinSource: TCustomChartSource;
    FListener: TListener;
    FMarks: TChartMarks;
    FOnGetMark: TChartGetMarkEvent;
    FSource: TCustomChartSource;

    function GetSource: TCustomChartSource;
    function IsSourceStored: boolean;
    procedure SetMarks(const AValue: TChartMarks);
    procedure SetOnGetMark(const AValue: TChartGetMarkEvent);
    procedure SetSource(AValue: TCustomChartSource);
  protected
    procedure AfterAdd; override;
    procedure AfterDraw; override;
    procedure BeforeDraw; override;
    function ColorOrDefault(AColor: TColor; ADefault: TColor = clTAColor): TColor;
    procedure DrawLegend(ACanvas: TCanvas; const ARect: TRect); override;
    procedure GetCoords(AIndex: Integer; out AG: TDoublePoint; out AI: TPoint);
    function GetLegendCount: Integer; override;
    function GetLegendWidth(ACanvas: TCanvas): Integer; override;
    function GetXMaxVal: Integer;
    procedure SetActive(AValue: Boolean); override;
    procedure SetDepth(AValue: TChartDistance); override;
    procedure SetShowInLegend(AValue: Boolean); override;
    procedure SetZPosition(AValue: TChartDistance); override;
    procedure StyleChanged(Sender: TObject);
    procedure UpdateBounds(var ABounds: TDoubleRect); override;
    procedure UpdateParentChart;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function Add(AValue: Double; XLabel: String; Color: TColor): Integer; inline;
    function AddXY(X, Y: Double; XLabel: String; Color: TColor): Integer; virtual; overload;
    function AddXY(X, Y: Double): Integer; overload; inline;
    procedure Clear; inline;
    function Count: Integer; inline;
    function DefaultFormattedMark(AIndex: integer): String;
    procedure Delete(AIndex: Integer); virtual;
    function Extent: TDoubleRect; virtual;
    function FormattedMark(AIndex: integer): String;
    function IsEmpty: Boolean; override;
    function ListSource: TListChartSource;
    property Source: TCustomChartSource
      read GetSource write SetSource stored IsSourceStored;
  published
    property Active default true;
    property Marks: TChartMarks read FMarks write SetMarks;
    property ShowInLegend;
    property Title;
    property ZPosition;
  published
    property OnGetMark: TChartGetMarkEvent read FOnGetMark write SetOnGetMark;
  end;

implementation

type

  { TChartSeriesListener }

  TChartSeriesListener = class(TListener)
  private
    FSeries: TChartSeries;
  public
    constructor Create(ASeries: TChartSeries);
    procedure Forget; override;
    procedure Notify; override;
  end;

{ TChartSeriesListener }

constructor TChartSeriesListener.Create(ASeries: TChartSeries);
begin
  FSeries := ASeries;
end;

procedure TChartSeriesListener.Forget;
begin
  inherited Forget;
  FSeries.FSource := nil;
end;

procedure TChartSeriesListener.Notify;
begin
  FSeries.UpdateParentChart;
end;

{ TChartSeries }

function TChartSeries.Add(AValue: Double; XLabel: String; Color: TColor): Integer;
begin
  Result := AddXY(GetXMaxVal + 1, AValue, XLabel, Color);
end;

function TChartSeries.AddXY(X, Y: Double; XLabel: String; Color: TColor): Integer;
begin
  Result := ListSource.Add(X, Y, XLabel, Color);
end;

function TChartSeries.AddXY(X, Y: Double): Integer;
begin
  Result := AddXY(X, Y, '', clTAColor);
end;

procedure TChartSeries.AfterAdd;
begin
  FMarks.SetOwner(FChart);
end;

procedure TChartSeries.AfterDraw;
begin
  Source.AfterDraw;
end;

procedure TChartSeries.BeforeDraw;
begin
  Source.BeforeDraw;
end;

procedure TChartSeries.Clear;
begin
  ListSource.Clear;
end;

function TChartSeries.ColorOrDefault(AColor: TColor; ADefault: TColor): TColor;
begin
  Result := AColor;
  if Result <> clTAColor then exit;
  Result := ADefault;
  if Result <> clTAColor then exit;
  Result := SeriesColor;
end;

function TChartSeries.Count: Integer;
begin
  Result := Source.Count;
end;

constructor TChartSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FActive := true;
  FListener := TChartSeriesListener.Create(Self);
  FBuiltinSource := TListChartSource.Create(Self);
  FBuiltinSource.Name := 'Builtin';
  FBuiltinSource.Subscribe(FListener);
  FMarks := TChartMarks.Create(FChart);
  FShowInLegend := true;
end;

function TChartSeries.DefaultFormattedMark(AIndex: integer): String;
var
  total, percent: Double;
begin
  total := Source.ValuesTotal;
  with Source[AIndex]^ do begin
    if total = 0 then
      percent := 0
    else
      percent := Y / total * 100;
    Result := Format(FMarks.Format, [y, percent, Text, total, X]);
  end;
end;

procedure TChartSeries.Delete(AIndex: Integer);
begin
  ListSource.Delete(AIndex);
end;

destructor TChartSeries.Destroy;
begin
  if FListener.IsListening then
    Source.Unsubscribe(FListener);
  FBuiltinSource.Free;
  FMarks.Free;

  inherited Destroy;
end;

procedure TChartSeries.DrawLegend(ACanvas: TCanvas; const ARect: TRect);
begin
  ACanvas.TextOut(ARect.Right + 3, ARect.Top, Title);
end;

function TChartSeries.Extent: TDoubleRect;
begin
  Result := Source.Extent;
end;

function TChartSeries.FormattedMark(AIndex: integer): String;
begin
  if Assigned(FOnGetMark) then
    FOnGetMark(Result, AIndex)
  else
    Result := DefaultFormattedMark(AIndex);
end;

procedure TChartSeries.GetCoords(
  AIndex: Integer; out AG: TDoublePoint; out AI: TPoint);
begin
  AG := DoublePoint(Source[AIndex]^);
  AI := ParentChart.GraphToImage(AG);
end;

function TChartSeries.GetLegendCount: Integer;
begin
  Result := 1;
end;

function TChartSeries.GetLegendWidth(ACanvas: TCanvas): Integer;
begin
  Result := ACanvas.TextWidth(Title);
end;

function TChartSeries.GetSource: TCustomChartSource;
begin
  if Assigned(FSource) then
    Result := FSource
  else
    Result := FBuiltinSource;
end;

function TChartSeries.GetXMaxVal: Integer;
begin
  if Count > 0 then
    Result := Round(Source[Count - 1]^.X)
  else
    Result := 0;
end;

function TChartSeries.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TChartSeries.IsSourceStored: boolean;
begin
  Result := FSource <> nil;
end;

function TChartSeries.ListSource: TListChartSource;
begin
  if not (Source is TListChartSource) then
    raise EEditableSourceRequired.Create('Editable chart source required');
  Result := Source as TListChartSource;
end;

procedure TChartSeries.SetActive(AValue: Boolean);
begin
  FActive := AValue;
  UpdateParentChart;
end;

procedure TChartSeries.SetDepth(AValue: TChartDistance);
begin
  if FDepth = AValue then exit;
  FDepth := AValue;
  UpdateParentChart;
end;

procedure TChartSeries.SetMarks(const AValue: TChartMarks);
begin
  if FMarks = AValue then exit;
  FMarks.Assign(AValue);
end;

procedure TChartSeries.SetOnGetMark(const AValue: TChartGetMarkEvent);
begin
  if FOnGetMark = AValue then exit;
  FOnGetMark := AValue;
  UpdateParentChart;
end;

procedure TChartSeries.SetShowInLegend(AValue: Boolean);
begin
  if FShowInLegend = AValue then exit;
  FShowInLegend := AValue;
  UpdateParentChart;
end;

procedure TChartSeries.SetSource(AValue: TCustomChartSource);
begin
  if FSource = AValue then exit;
  if FListener.IsListening then
    Source.Unsubscribe(FListener);
  FSource := AValue;
  Source.Subscribe(FListener);
  UpdateParentChart;
end;

procedure TChartSeries.SetZPosition(AValue: TChartDistance);
begin
  if FZPosition = AValue then exit;
  FZPosition := AValue;
  UpdateParentChart;
end;

procedure TChartSeries.StyleChanged(Sender: TObject);
begin
  UpdateParentChart;
end;

procedure TChartSeries.UpdateBounds(var ABounds: TDoubleRect);
begin
  if not Active or (Count = 0) then exit;
  with Extent do begin
    if a.X < ABounds.a.X then ABounds.a.X := a.X;
    if a.Y < ABounds.a.Y then ABounds.a.Y := a.Y;
    if b.X > ABounds.b.X then ABounds.b.X := b.X;
    if b.Y > ABounds.b.Y then ABounds.b.Y := b.Y;
  end;
end;

procedure TChartSeries.UpdateParentChart;
begin
  if ParentChart <> nil then ParentChart.Invalidate;
end;

end.

