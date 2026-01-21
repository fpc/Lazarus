{ Demonstrates how a chart can be painted by means of the AggPas library,
  either directly or via the corresponding GUIConnector.
  
  Note that the AggPas version coming with Lazarus is unmaintained at the moment 
  and rather buggy.

  Font support works fine only on Windows.

  On Linux, a list of font directories is stored in an internal list
  (FFontDirList) which very probably is not complete. It can be extended by
  calling the drawer's AddFontDir. The property FontDir addresses the first
  directory in the list. Writing to it adds the specified directory to the top
  of the list, or if it already is contained, moves it to the top.

  On macOS (cocoa), text output is not supported at all.

  By default, system colors are not supported and usually rendered as black.
  Therefore, use clWhite instead of clWindow for Chart.BackColor etc.
  If you absolute need system color support assign the function
  ChartColorSysToFPColor (in unit TADrawerCanvas) to property DoChartColorToFPColor
  of the chart's drawer:

    Chart.Drawer.DoChartColorToFPColor := @ChartColorSysToFPColor.

  Note, however, that this pulls in the LCL.
  }

unit Main;

{$mode objfpc}{$H+}

{.$DEFINE USE_SYSTEM_COLORS}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, Forms, Controls,
  Graphics, FPCanvas, Dialogs, 
  Agg_LCL, Agg_FPImage,
  TAGraph, TAGUIConnectorAggPas, TASeries, TASources, TADrawerAggPas, TADrawUtils;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    cbAggPas: TCheckBox;
    Chart: TChart;
    ChartConstantLine: TConstantLine;
    ChartLineSeries: TLineSeries;
    ChartPieSeries: TPieSeries;
    ChartGUIConnectorAggPas: TChartGUIConnectorAggPas;
    ChartPaintBox: TPaintBox;
    BottomPanel: TPanel;
    RandomChartSource: TRandomChartSource;
    procedure cbAggPasClick(Sender: TObject);
    procedure Chart1AfterPaint(ASender: TChart);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ChartPaintBoxPaint(Sender: TObject);
  private
    FAggCanvas: TAggLCLCanvas;
    FBmp: TBitmap;
  end;

var
  MainForm: TMainForm; 

implementation

{$R *.lfm}

uses
  TAChartUtils, TADrawerCanvas;

{ TMainForm }

procedure TMainForm.cbAggPasClick(Sender: TObject);
begin
  if cbAggPas.Checked then
  begin
    Chart.GUIConnector := ChartGUIConnectorAggPas;
    {$IFDEF USE_SYSTEM_COLORS}
    Chart.Drawer.DoChartColorToFPColor := @ChartColorSysToFPColor;
    {$ENDIF}
  end else
    Chart.GUIConnector := nil;
end;

procedure TMainForm.Chart1AfterPaint(ASender: TChart);
begin
  Unused(ASender);
  ChartPaintBox.Invalidate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FBmp := TBitmap.Create;
  FAggCanvas := TAggLCLCanvas.Create;
  FAggCanvas.Image.PixelFormat := afpimRGBA32;
  
  {$IFDEF LCLWin32}
  ChartLineSeries.Transparency := 128;
  {$ENDIF}

  {$IFDEF USE_SYSTEM_COLORS}
  Chart.Color := clForm;
  Chart.BackColor := clWindow;
  Chart.Legend.BackgroundBrush.Color := clDefault;
  Chart.Frame.Color := clBtnShadow;
  {$ENDIF}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FAggCanvas.Free;
  FBmp.Free;
end;

procedure TMainForm.ChartPaintBoxPaint(Sender: TObject);
var
  d: IChartDrawer;
begin
  FAggCanvas.Width := ChartPaintBox.Width;
  FAggCanvas.Height := ChartPaintBox.Height;

  Chart.DisableRedrawing;
  Chart.Title.Text.Text := 'AggPas';
  d := TAggPasDrawer.Create(FAggCanvas);
  d.DoGetFontOrientation := @CanvasGetFontOrientationFunc;
  // The next instruction is required if system colors are used
  {$IFDEF USE_SYSTEM_COLORS}
  d.DoChartColorToFPColor := @ChartColorSysToFPColor;
  {$ENDIF}
  Chart.Draw(d, ChartPaintBox.Canvas.ClipRect);
  Chart.Title.Text.Text := 'Standard';
  Chart.EnableRedrawing;

  // On Windows the order of red, green and blue is blue-green-red, while on 
  // others it is red-green-blue. In principle, AggPas can handle this, but
  // since it is not maintained ATM, I chose the "easy way" to swap the red
  // and blue bytes.
  {$IFDEF LINUX}
  SwapRedBlue(FAggCanvas.Image.IntfImg);
  {$ENDIF}
  FBmp.LoadFromIntfImage(FAggCanvas.Image.IntfImg);
  ChartPaintBox.Canvas.Draw(0, 0, FBmp);
end;

end.

