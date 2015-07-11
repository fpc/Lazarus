unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnSaveToMetafile: TButton;
    btnCopyToClipboard: TButton;
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1LineSeries1: TLineSeries;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    procedure btnSaveToMetafileClick(Sender: TObject);
    procedure btnCopyToClipboardClick(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  TADrawerWMF;

{ TForm1 }

procedure TForm1.btnSaveToMetafileClick(Sender: TObject);
begin
  Chart1.SaveToWMF('test.wmf');
  { or:
  with Chart1 do
    Draw(TWindowsMetafileDrawer.Create('test.wmf'), Rect(0, 0, Width, Height));
    }
end;

procedure TForm1.btnCopyToClipboardClick(Sender: TObject);
begin
  Chart1.CopyToClipboardMetaFile;
  { or:
  with Chart1 do
    Draw(TWindowsMetafileDrawer.Create(''), Rect(0, 0, Width, Height));
    // Setting the file name to an empty string results in copying the chart to
    // the clipboard as a windows metafile.
    }
end;

end.

