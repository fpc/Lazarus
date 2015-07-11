unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASources, TASeries, TATransformations,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, types,
  TACustomSeries, TATools, TAFuncSeries;

type

  { TForm1 }

  TForm1 = class(TForm)
    CbShowVertGrid: TCheckBox;
    Chart1ConstantLine2: TConstantLine;
    Chart1ConstantLine3: TConstantLine;
    Chart1ConstantLine4: TConstantLine;
    Chart1FuncSeries2: TFuncSeries;
    Chart2ConstantLine1: TConstantLine;
    Chart2ConstantLine2: TConstantLine;
    Chart2ConstantLine3: TConstantLine;
    Chart2ConstantLine4: TConstantLine;
    Chart1ConstantLine1: TConstantLine;
    Chart1LineSeries2: TLineSeries;
    Chart1LineSeries3: TLineSeries;
    Chart1LineSeries4: TLineSeries;
    Chart2FuncSeries2: TFuncSeries;
    Chart2LineSeries1: TLineSeries;
    Chart2LineSeries2: TLineSeries;
    Chart2LineSeries3: TLineSeries;
    Chart2LineSeries4: TLineSeries;
    Chart1LineSeries1: TLineSeries;
    Chart2: TChart;
    Chart1: TChart;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ChartAxisTransformations2: TChartAxisTransformations;
    ChartAxisTransformations2AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ChartAxisTransformations3: TChartAxisTransformations;
    ChartAxisTransformations3AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointDragTool1: TDataPointDragTool;
    ChartToolset2: TChartToolset;
    ChartToolset2DataPointDragTool1: TDataPointDragTool;
    CbLineAtDataOnly: TCheckBox;
    CbShowHorGrid: TCheckBox;
    CbShowArrow: TCheckBox;
    CbInverted: TCheckBox;
    CbBiDiMode: TCheckBox;
    CbMarksAtDataOnly: TCheckBox;
    CbUseMax: TCheckBox;
    CbUseMin: TCheckBox;
    CbGrouped: TCheckBox;
    CbShowFrame: TCheckBox;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    LblXExtentIgnored: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RandomChartSource11: TRandomChartSource;
    RandomChartSource12: TRandomChartSource;
    RandomChartSource13: TRandomChartSource;
    RandomChartSource14: TRandomChartSource;
    RandomChartSource21: TRandomChartSource;
    RandomChartSource22: TRandomChartSource;
    RandomChartSource23: TRandomChartSource;
    RandomChartSource24: TRandomChartSource;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure CbShowFrameChange(Sender: TObject);
    procedure CbGroupedChange(Sender: TObject);
    procedure CbUseMaxChange(Sender: TObject);
    procedure CbUseMinChange(Sender: TObject);
    procedure Chart1FuncSeries2Calculate(const AX: Double; out AY: Double);
    procedure Chart2FuncSeries2Calculate(const AX: Double; out AY: Double);
    procedure CheckBox1Change(Sender: TObject);
    procedure DataPointDragTool_AfterMouseMove(ATool: TChartTool; APoint: TPoint);
    procedure CbLineAtDataOnlyChange(Sender: TObject);
    procedure CbShowGridChange(Sender: TObject);
    procedure CbShowArrowChange(Sender: TObject);
    procedure CbInvertedChange(Sender: TObject);
    procedure CbBiDiModeChange(Sender: TObject);
    procedure CbMarksAtDataOnlyChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Math, TAChartUtils;

{ TForm1 }

procedure TForm1.CbLineAtDataOnlyChange(Sender: TObject);
var
  i: Integer;
begin
  for i:=1 to 3 do begin
    Chart1.AxisList[i].AtDataOnly := CbLineAtDataOnly.Checked;
    Chart2.AxisList[i].AtDataOnly := CbLineAtDataOnly.Checked;
  end;
end;

procedure TForm1.CbUseMaxChange(Sender: TObject);
var
  i: Integer;
begin
  for i:=1 to 3 do begin
    Chart1.AxisList[i].Range.UseMax := CbUseMax.Checked;
    Chart2.AxisList[i].Range.UseMax := CbUseMax.Checked;
  end;
end;

procedure TForm1.CbGroupedChange(Sender: TObject);
var
  i: Integer;
begin
  if CbGrouped.Checked then begin
    for i:=1 to 3 do begin
      Chart1.AxisList[i].Group := 1;
      Chart2.AxisList[i].Group := 1;
    end;
  end else begin
    for i:=1 to 3 do begin
      Chart1.AxisList[i].Group := 0;
      Chart2.AxisList[i].Group := 0;
    end;
  end;
end;

procedure TForm1.CbShowFrameChange(Sender: TObject);
begin
  Chart1.Frame.Visible := CbShowFrame.Checked;
  Chart2.Frame.Visible := CbShowFrame.Checked;
end;

procedure TForm1.CbUseMinChange(Sender: TObject);
var
  i: Integer;
begin
  for i:=1 to 3 do begin
    Chart1.AxisList[i].Range.UseMin := CbUseMin.Checked;
    Chart2.AxisList[i].Range.UseMin := CbUseMin.Checked;
  end;
end;

procedure TForm1.Chart1FuncSeries2Calculate(const AX: Double; out AY: Double);
begin
  AY := sin(5 * AX);
end;

procedure TForm1.Chart2FuncSeries2Calculate(const AX: Double; out AY: Double);
begin
  AY := sin(5*AX)*0.5 + 0.5;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  Chart1FuncSeries2.Active := Checkbox1.Checked;
  Chart1LineSeries2.Active := not Checkbox1.Checked;
  Chart2FuncSeries2.Active := Checkbox1.Checked;
  Chart2LineSeries2.Active := not Checkbox1.Checked;
end;

// This code is used by both ChartToolSets, for Chart1 and Chart2.
procedure TForm1.DataPointDragTool_AfterMouseMove(ATool: TChartTool;
  APoint: TPoint);
const
  MIN_SIZE = 0.5;
  MIN_DISTANCE = 0.1;
var
  pos, prevPos, nextPos: Double;
  ser: TConstantLine;
  ex: TDoubleRect;
  ls: array[1..4] of TConstantLine;
begin
  UnUsed(APoint);

  ser := TConstantLine(TDataPointDragTool(ATool).Series);
  if ser = nil then
    exit;

  pos := ser.Position;
  ex := ser.ParentChart.GetFullExtent;

  if ser.ParentChart = Chart1 then begin
    ls[1] := Chart1ConstantLine1;
    ls[2] := Chart1ConstantLine2;
    ls[3] := Chart1ConstantLine3;
    ls[4] := Chart1ConstantLine4;
  end else begin
    ls[1] := Chart2ConstantLine1;
    ls[2] := Chart2ConstantLine2;
    ls[3] := Chart2ConstantLine3;
    ls[4] := Chart2ConstantLine4;
  end;

  if ser = ls[1] then begin
    prevPos := TDoublePointBoolArr(ex.a)[PageControl1.ActivepageIndex=0] + MIN_SIZE;
    nextPos := ls[2].Position - MIN_DISTANCE;
    ser.Position := EnsureRange(pos, prevPos, nextPos);
    ChartAxisTransformations1AutoscaleAxisTransform1.MaxValue := ser.Position;
  end else
  if ser = ls[2] then begin
    prevPos := ls[1].Position + MIN_DISTANCE;
    nextPos := ls[3].Position - MIN_SIZE;
    ser.Position := EnsureRange(pos, prevPos, nextPos);
    ChartAxisTransformations2AutoscaleAxisTransform1.MinValue := ser.Position;
  end else
  if ser = ls[3] then begin
    prevPos := ls[2].Position + MIN_SIZE;
    nextPos := ls[4].Position - MIN_DISTANCE;
    ser.Position := EnsureRange(pos, prevPos, nextPos);
    ChartAxisTransformations2AutoscaleAxisTransform1.MaxValue := ser.Position;
  end else
  if ser = ls[4] then begin
    prevPos := ls[3].Position + MIN_DISTANCE;
    nextPos := TDoublePointBoolArr(ex.b)[PageControl1.ActivepageIndex=0] - MIN_SIZE;
    ser.Position := EnsureRange(pos, prevPos, nextPos);
    ChartAxisTransformations3AutoscaleAxisTransform1.MinValue := ser.Position;
  end;
end;

procedure TForm1.CbShowGridChange(Sender: TObject);
var
  i: Integer;
begin
  Chart1.AxisList[0].Grid.Visible := CbShowVertGrid.Checked;
  Chart2.AxisList[0].Grid.Visible := cbShowHorGrid.Checked;
  for i:=1 to 3 do begin
    Chart1.AxisList[i].Grid.Visible := CbShowHorGrid.Checked;
    Chart2.AxisList[i].Grid.Visible := CbShowVertGrid.Checked;
  end;
end;

procedure TForm1.CbShowArrowChange(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to 3 do begin
    Chart1.AxisList[i].Arrow.Visible := CbShowArrow.Checked;
    Chart2.AxisList[i].Arrow.Visible := CbShowArrow.Checked;
  end;
end;

procedure TForm1.CbInvertedChange(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to 3 do begin
    Chart1.AxisList[i].Inverted := CbInverted.Checked;
    Chart2.AxisList[i].Inverted := CbInverted.Checked;
  end;
end;

procedure TForm1.CbBiDiModeChange(Sender: TObject);
begin
  if CbBiDiMode.Checked then
    Chart1.BiDiMode := bdRightToLeft
  else
    Chart1.BiDiMode := bdLeftToRight;
  Chart2.BiDiMode := Chart1.BiDiMode;
end;

procedure TForm1.CbMarksAtDataOnlyChange(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to 3 do begin
    Chart1.AxisList[i].Marks.AtDataOnly := CbMarksAtDataOnly.Checked;
    Chart2.AxisList[i].Marks.AtDataOnly := CbMarksAtDataOnly.Checked;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  LblXExtentIgnored.Visible := PageControl1.ActivePageIndex = 1;
end;

{ The autoscale transformations are shared between both pages. Before changing
  to the new page we make sure that the constant lines series which indicate the
  pane limits are at the correct position. }
procedure TForm1.PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
begin
  UnUsed(AllowChange);
  case PageControl1.ActivePageIndex of
    0: begin
         Chart2ConstantLine1.Position := ChartAxisTransformations1AutoScaleAxisTransform1.Maxvalue;
         Chart2ConstantLine2.Position := ChartAxisTransformations2AutoScaleAxisTransform1.MinValue;
         Chart2ConstantLine3.Position := ChartAxisTransformations2AutoScaleAxisTransform1.Maxvalue;
         Chart2ConstantLine4.Position := ChartAxisTransformations3AutoScaleAxisTransform1.Minvalue;
       end;
    1: begin
         Chart1ConstantLine1.Position := ChartAxisTransformations1AutoScaleAxisTransform1.Maxvalue;
         Chart1ConstantLine2.Position := ChartAxisTransformations2AutoScaleAxisTransform1.MinValue;
         Chart1ConstantLine3.Position := ChartAxisTransformations2AutoScaleAxisTransform1.Maxvalue;
         Chart1ConstantLine4.Position := ChartAxisTransformations3AutoScaleAxisTransform1.Minvalue;
       end;
  end;
end;

end.

