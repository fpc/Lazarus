unit TALegend;

{$H+}

interface

uses
  Classes, SysUtils, Graphics, TAChartUtils, TATypes;

type
  { TChartLegend }

  TChartLegend = class(TChartElement)
  private
    FAlignment: TLegendAlignment;
    FFont: TFont;
    FFrame: TChartPen;

    procedure SetAlignment(AValue: TLegendAlignment);
    procedure SetFont(AValue: TFont);
    procedure SetFrame(AValue: TChartPen);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TLegendAlignment
      read FAlignment write SetAlignment default laRight;
    property Font: TFont read FFont write SetFont;
    property Frame: TChartPen read FFrame write SetFrame;
    property Visible default false;
  end;

implementation

uses
  FPCanvas;

{ TChartLegend }

procedure TChartLegend.Assign(Source: TPersistent);
begin
  if Source is TChartLegend then
    with TChartLegend(Source) do
      Self.FAlignment := FAlignment;

  inherited Assign(Source);
end;

constructor TChartLegend.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FAlignment := laRight;
  Visible := false;

  InitHelper(TFPCanvasHelper(FFont), TFont);
  InitHelper(TFPCanvasHelper(FFrame), TChartPen);
end;

destructor TChartLegend.Destroy;
begin
  FFont.Free;
  FFrame.Free;

  inherited;
end;

procedure TChartLegend.SetAlignment(AValue: TLegendAlignment);
begin
  if FAlignment = AValue then exit;
  FAlignment := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartLegend.SetFrame(AValue: TChartPen);
begin
  FFrame.Assign(AValue);
  StyleChanged(Self);
end;

end.

