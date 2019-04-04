unit ceMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, Types,
  TAGraph, TATools, TAChartAxis, TATextElements, TASeries, TASources, TALegend,
  TAChartImageList, ceAxisDlg;

type

  { TMainForm }

  TMainForm = class(TForm)
    Chart1: TChart;
    Chart1AreaSeries1: TAreaSeries;
    Chart1BarSeries1: TBarSeries;
    Chart1LineSeries1: TLineSeries;
    ChartImageList1: TChartImageList;
    ChartToolset1: TChartToolset;
    ChartToolset1AxisClickTool1: TAxisClickTool;
    ChartToolset1DataPointClickTool1: TDataPointClickTool;
    ChartToolset1LegendClickTool1: TLegendClickTool;
    ChartToolset1TitleFootClickTool1: TTitleFootClickTool;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnuSeries: TMenuItem;
    mnuChartLegend: TMenuItem;
    mnuLeftAxisTitle: TMenuItem;
    mnuBottomAxisTitle: TMenuItem;
    MenuItem2: TMenuItem;
    mnuChartFooter: TMenuItem;
    mnuChartTitle: TMenuItem;
    mnuSettings: TMenuItem;
    RandomChartSource1: TRandomChartSource;
    RandomChartSource2: TRandomChartSource;
    RandomChartSource3: TRandomChartSource;
    procedure ChartToolset1AxisClickTool1Click(ASender: TChartTool;
      Axis: TChartAxis; AHit: TChartAxisHitTests);
    procedure ChartToolset1DataPointClickTool1PointClick(ATool: TChartTool;
      APoint: TPoint);
    procedure ChartToolset1LegendClickTool1Click(ASender: TChartTool;
      ALegend: TChartLegend);
    procedure ChartToolset1TitleFootClickTool1Click(ASender: TChartTool;
      ATitle: TChartTitle);
    procedure FormCreate(Sender: TObject);
    procedure mnuBottomAxisTitleClick(Sender: TObject);
    procedure mnuChartFooterClick(Sender: TObject);
    procedure mnuChartLegendClick(Sender: TObject);
    procedure mnuChartTitleClick(Sender: TObject);
    procedure mnuLeftAxisTitleClick(Sender: TObject);
  private
    procedure EditAxis(AAxis: TChartAxis; APage: TAxisEditorPage);
    procedure EditLegend(ALegend: TChartLegend);
    procedure EditSeries(ASeries: TBasicChartSeries);
    procedure EditTitleFoot(ATitle: TChartTitle);
    procedure MenuSeriesClick(Sender: TObject);

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  TAChartUtils, TACustomSeries,
  ceTitleFootDlg, ceLegendDlg, ceSeriesDlg;

{ TMainForm }

procedure TMainForm.ChartToolset1TitleFootClickTool1Click(ASender: TChartTool;
  ATitle: TChartTitle);
begin
  Unused(ASender);
  EditTitleFoot(ATitle);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
  item: TMenuItem;
begin
  ChartImageList1.Chart := Chart1;

  for i := 0 to Chart1.SeriesCount-1 do begin
    item := TMenuItem.Create(MainMenu1);
    item.Caption := TCustomChartSeries(Chart1.Series[i]).Title;
    item.OnClick := @MenuSeriesClick;
    item.Tag := PtrInt(Chart1.Series[i]);
    item.ImageIndex := ChartImageList1.FirstSeriesIndex + i;
    mnuSeries.Add(item);
  end;
end;

procedure TMainForm.mnuBottomAxisTitleClick(Sender: TObject);
begin
  EditAxis(Chart1.BottomAxis, aepTitle);
end;

procedure TMainForm.ChartToolset1AxisClickTool1Click(ASender: TChartTool;
  Axis: TChartAxis; AHit: TChartAxisHitTests);
var
  pg: TAxisEditorPage;
begin
  Unused(ASender);
  if (ahtTitle in AHit) then
    pg := aepTitle
  else if (ahtLabels in AHit) then
    pg := aepLabels
  else if (ahtLine in AHit) then
    pg := aepLine
  else if (ahtGrid in AHit) then
    pg := aepGrid
  else
    exit;
  EditAxis(Axis, pg);
end;

procedure TMainForm.ChartToolset1DataPointClickTool1PointClick(ATool: TChartTool;
  APoint: TPoint);
begin
  Unused(APoint);
  EditSeries(TDataPointClickTool(ATool).Series);
end;

procedure TMainForm.ChartToolset1LegendClickTool1Click(ASender: TChartTool;
  ALegend: TChartLegend);
begin
  Unused(ASender);
  EditLegend(ALegend);
end;

procedure TMainForm.EditAxis(AAxis: TChartAxis; APage: TAxisEditorPage);
var
  F: TAxisEditor;
begin
  F := TAxisEditor.Create(nil);
  try
    F.Prepare(AAxis, 'Edit chart axis "%s"');
    F.Page := APage;
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TMainForm.EditLegend(ALegend: TChartLegend);
var
  F: TLegendEditor;
begin
  F := TLegendEditor.Create(nil);
  try
    F.Prepare(ALegend, 'Edit chart legend');
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TMainForm.EditSeries(ASeries: TBasicChartSeries);
var
  F: TSeriesEditor;
begin
  F := TSeriesEditor.Create(nil);
  try
    F.Prepare(ASeries, 'Edit series "%s"');
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TMainForm.EditTitleFoot(ATitle: TChartTitle);
var
  F: TtitleFootEditor;
  s: String;
begin
  F := TTitleFootEditor.Create(nil);
  try
    s := 'Edit chart %s';
    if ATitle = Chart1.Title then s := Format(s, ['title']) else s := Format(s, ['footer']);
    F.Prepare(ATitle, s);
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TMainForm.MenuSeriesClick(Sender: TObject);
var
  ser: TBasicChartSeries;
begin
  ser := TBasicChartSeries(TMenuItem(Sender).Tag);
  EditSeries(ser);
end;

procedure TMainForm.mnuChartFooterClick(Sender: TObject);
begin
  EditTitleFoot(Chart1.Foot);
end;

procedure TMainForm.mnuChartLegendClick(Sender: TObject);
begin
  EditLegend(Chart1.Legend);
end;

procedure TMainForm.mnuChartTitleClick(Sender: TObject);
begin
  EditTitleFoot(Chart1.Title);
end;

procedure TMainForm.mnuLeftAxisTitleClick(Sender: TObject);
begin
  EditAxis(Chart1.LeftAxis, aepTitle);
end;

end.

