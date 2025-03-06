unit frmDateTime;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, StdCtrls, ExtCtrls,
  TAGraph, TAIntervalSources, TASeries, TASources, TATools;

type
  TDateTimeFrame = class(TFrame)
    CbAlternateFormat: TCheckBox;
    CbSuppressPrevUnit: TCheckBox;
    ChartDateTime: TChart;
    ChartDateTimeLineSeries1: TLineSeries;
    ChartToolset1ZoomIn: TZoomClickTool;
    ChartToolset1ZoomOut: TZoomClickTool;
    ChartToolsetDateTime: TChartToolset;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    Label1: TLabel;
    Panel1: TPanel;
    rcsDates: TRandomChartSource;
    procedure CbAlternateFormatChange(Sender: TObject);
    procedure CbSuppressPrevUnitChange(Sender: TObject);
    procedure DateTimeIntervalChartSource1DateTimeStepChange(Sender: TObject;
      ASteps: TDateTimeStep);
  private

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

uses
  TypInfo;

constructor TDateTimeFrame.Create(AOwner: TComponent);
begin
  inherited;
  with rcsDates do begin
    XMin := Now - 5 * 365;
    XMax := Now + 5 * 365;
    PointsNumber := 10 * 365 * 24;
  end;
end;

procedure TDateTimeFrame.DateTimeIntervalChartSource1DateTimeStepChange(Sender:
  TObject; ASteps: TDateTimeStep);
begin
  Label1.Caption := GetEnumName(TypeInfo(TDateTimeStep), ord(ASteps));
end;

procedure TDateTimeFrame.CbSuppressPrevUnitChange(Sender: TObject);
begin
  DateTimeIntervalChartSource1.SuppressPrevUnit := CbSuppressPrevUnit.Checked;
end;

procedure TDateTimeFrame.CbAlternateFormatChange(Sender: TObject);
begin
  with DateTimeIntervalChartSource1.DateTimeStepFormat do
    if CbAlternateFormat.Checked then begin
      WeekFormat := 'dd"."mmm"."';
      DayFormat := 'dd"."mm"."';
      HourFormat := 'dd". "hh:nn';
      SecondFormat := 'nn"."ss';
      MillisecondFormat := 'ss"."zzz';
    end else begin
      WeekFormat := DEFAULT_WEEK_FORMAT;
      DayFormat := DEFAULT_DAY_FORMAT;
      HourFormat := DEFAULT_HOUR_FORMAT;
      SecondFormat := DEFAULT_SECOND_FORMAT;
      MillisecondFormat := DEFAULT_MILLISECOND_FORMAT;
    end;
end;

end.

