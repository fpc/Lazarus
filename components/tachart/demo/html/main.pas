unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TASources, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnCopyToClipboard: TButton;
    BtnSaveWMF: TButton;
    BtnSaveSVG: TButton;
    Chart: TChart;
    CgHTML: TCheckGroup;
    RedSource: TRandomChartSource;
    BlueSource: TRandomChartSource;
    RedSeries: TLineSeries;
    BlueSeries: TLineSeries;
    BottomPanel: TPanel;
    procedure BtnCopyToClipboardClick(Sender: TObject);
    procedure BtnSaveWMFClick(Sender: TObject);
    procedure BtnSaveSVGClick(Sender: TObject);
    procedure CgHTMLItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  TAChartUtils, {$IFDEF WINDOWS}TADrawerWMF,{$ENDIF} TADrawerSVG;

{ TMainForm }

procedure TMainForm.BtnCopyToClipboardClick(Sender: TObject);
begin
  Chart.CopyToClipboardBitmap;
end;

procedure TMainForm.BtnSaveWMFClick(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  with Chart do
    Draw(TWindowsMetafileDrawer.Create('test.wmf'), Rect(0, 0, Width, Height));
  ShowMessage('Chart saved to file "test.wmf"');
  {$ENDIF}
end;

procedure TMainForm.BtnSaveSVGClick(Sender: TObject);
begin
  Chart.SaveToSVGFile('test.svg');
  ShowMessage('Chart saved to file "test.svg"');
end;

procedure TMainForm.CgHTMLItemClick(Sender: TObject; Index: integer);
var
  tf: TChartTextFormat;
begin
  if CgHTML.Checked[Index] then tf := tfHTML else tf := tfNormal;
  case Index of
    0: Chart.Title.TextFormat := tf;
    1: Chart.Foot.TextFormat := tf;
    2: Chart.Legend.TextFormat := tf;
    3: Chart.BottomAxis.Title.TextFormat := tf;
    4: Chart.LeftAxis.Title.TextFormat := tf;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CgHTML.Checked[0] := Chart.Title.TextFormat = tfHTML;
  CgHTML.Checked[1] := Chart.Foot.TextFormat = tfHTML;
  CgHTML.Checked[2] := Chart.Legend.TextFormat = tfHTML;
  CgHTML.Checked[3] := Chart.BottomAxis.Title.TextFormat = tfHTML;
  CgHTML.Checked[4] := Chart.LeftAxis.Title.TextFormat = tfHTML;

  {$IFDEF WINDOWS}
  Chart.Foot.Text[1] := '<font name="Times New Roman" color="blue">' + Chart.Foot.Text[1] + '</font>';
  {$ELSE}
  BtnSaveWMF.Hide;
  {$ENDIF}
end;

end.

