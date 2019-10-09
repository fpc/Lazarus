unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASources, TASeries, Forms, Controls,
  Graphics, Dialogs, Spin, StdCtrls, ExtCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    ChartTop: TChart;
    ChartTopLineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    ChartBottom: TChart;
    ChartBottomLineSeries1: TLineSeries;
    Chart2LineSeries2: TLineSeries;
    ChartRight: TChart;
    ChartLeft: TChart;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblInfo: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    RandomChartSource1: TRandomChartSource;
    RandomChartSource2: TRandomChartSource;
    EdLabelSizeLeft: TSpinEdit;
    EdLabelSizeRight: TSpinEdit;
    EdPositionLeft: TSpinEdit;
    EdLabelSizeBottom: TSpinEdit;
    EdLabelSizeTop: TSpinEdit;
    EdPositionBottom: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure EdLabelSizeTopChange(Sender: TObject);
    procedure EdPositionBottomChange(Sender: TObject);
    procedure EdLabelSizeLeftChange(Sender: TObject);
    procedure EdLabelSizeRightChange(Sender: TObject);
    procedure EdPositionLeftChange(Sender: TObject);
    procedure EdLabelSizeBottomChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1Change(nil);
end;

procedure TForm1.PageControl1Change(Sender: TObject);
const
  AXIS_NAMES: array[0..1] of String = ('left and right', 'top and bottom');
begin
  lblInfo.Caption := Format('The %s axis lines are at different ' +
    'positions. Increase "LabelSize" to align them.', [AXIS_NAMES[PageControl1.PageIndex]]);
end;

{ Charts on page "stacked" }
procedure TForm1.EdLabelSizeLeftChange(Sender: TObject);
begin
  ChartTop.LeftAxis.LabelSize := EdLabelSizeLeft.Value;
  ChartBottom.LeftAxis.LabelSize := EdLabelSizeLeft.Value;
end;

procedure TForm1.EdLabelSizeRightChange(Sender: TObject);
begin
  ChartTop.AxisList[2].LabelSize := EdLabelSizeRight.Value;
  ChartBottom.AxisList[2].LabelSize := EdLabelSizeRight.Value;
end;

procedure TForm1.EdPositionLeftChange(Sender: TObject);
begin
  ChartTop.LeftAxis.Position := EdPositionLeft.Value;
  ChartBottom.LeftAxis.Position := EdPositionLeft.Value;
end;

{ Charts on page "side-by-side" }
procedure TForm1.EdLabelSizeBottomChange(Sender: TObject);
begin
  ChartLeft.BottomAxis.LabelSize := EdLabelSizeBottom.Value;
  ChartRight.BottomAxis.LabelSize := EdLabelSizeBottom.Value;
end;

procedure TForm1.EdLabelSizeTopChange(Sender: TObject);
begin
  ChartLeft.AxisList[2].LabelSize := EdLabelSizeTop.Value;
  ChartRight.AxisList[2].LabelSize := EdLabelSizeTop.Value;
end;

procedure TForm1.EdPositionBottomChange(Sender: TObject);
begin
  ChartLeft.BottomAxis.Position := EdPositionBottom.Value;
  ChartRight.BottomAxis.Position := EdPositionBottom.Value;
end;


end.

