unit frmBars;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, StdCtrls, ExtCtrls,
  TAGraph, TAChartUtils, TASeries, TASources, TATools;

type
  TBarsFrame = class(TFrame)
    cbConstBarWidth: TCheckBox;
    chBars: TChart;
    chBarsBarSeries1: TBarSeries;
    ctBars: TChartToolset;
    ctBarsDataPointDragTool1: TDataPointDragTool;
    Panel2: TPanel;
    RandomChartSource1: TRandomChartSource;
    rbDragX: TRadioButton;
    rbDragY: TRadioButton;
    procedure cbConstBarWidthChange(Sender: TObject);
    procedure ctBarsDataPointDragTool1Drag(ASender: TDataPointDragTool; var
      AGraphPoint: TDoublePoint);
    procedure rbDragXChange(Sender: TObject);
    procedure rbDragYChange(Sender: TObject);
  private

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TBarsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Points in the RandomChartSource cannot be edited --> copy to a list source
  chBarsBarSeries1.ListSource.CopyFrom(RandomChartSource1);
end;

procedure TBarsFrame.cbConstBarWidthChange(Sender: TObject);
begin
  if cbConstBarWidth.Checked then
    chBarsBarSeries1.BarWidthStyle := bwPercentMin
  else
    chBarsBarSeries1.BarWidthStyle := bwPercent;
end;

procedure TBarsFrame.ctBarsDataPointDragTool1Drag(ASender: TDataPointDragTool;
  var AGraphPoint: TDoublePoint);
begin
  if rbDragY.Checked then begin
    // Only allow vertical dragging.
    AGraphPoint.X := ASender.Origin.X;
    ctBarsDataPointDragTool1.ActiveCursor := crSizeNS;
  end else
  if rbDragX.Checked then begin
    // Only allow horizontal dragging
    AGraphPoint.Y := ASender.Origin.Y;
    ctBarsDataPointDragTool1.ActiveCursor := crSizeWE;
  end;
end;

procedure TBarsFrame.rbDragXChange(Sender: TObject);
begin
  if rbDragY.Checked then begin
    chBarsBarSeries1.ToolTargets := chBarsBarSeries1.ToolTargets - [nptCustom];
    ctBarsDataPointDragTool1.Targets := ctBarsDataPointDragTool1.Targets - [nptCustom];
  end else begin
    chBarsBarSeries1.ToolTargets := chBarsBarSeries1.ToolTargets + [nptCustom];
    ctBarsDataPointDragTool1.Targets := ctBarsDataPointDragTool1.Targets + [nptCustom];
  end;
end;

procedure TBarsFrame.rbDragYChange(Sender: TObject);
begin
  if rbDragY.Checked then begin
    chBarsBarSeries1.ToolTargets := chBarsBarSeries1.ToolTargets - [nptCustom];
    ctBarsDataPointDragTool1.Targets := ctBarsDataPointDragTool1.Targets - [nptCustom];
  end else begin
    chBarsBarSeries1.ToolTargets := chBarsBarSeries1.ToolTargets + [nptCustom];
    ctBarsDataPointDragTool1.Targets := ctBarsDataPointDragTool1.Targets + [nptCustom];
  end;
end;

end.

