unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, Spin, StdCtrls, SysUtils, FileUtil, LResources,
  Forms, Controls, Graphics, Dialogs, TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1AreaSeries1: TAreaSeries;
    Chart1LineSeries1: TLineSeries;
    Chart1PieSeries1: TPieSeries;
    cbUseSidebar: TCheckBox;
    lblSymbolWidth: TLabel;
    lblSpacing: TLabel;
    lblMargin: TLabel;
    ListChartSource1: TListChartSource;
    ListChartSource2: TListChartSource;
    pnControls: TPanel;
    rgAlignment: TRadioGroup;
    RandomChartSource1: TRandomChartSource;
    seSymbolWidth: TSpinEdit;
    seSpacing: TSpinEdit;
    seMargin: TSpinEdit;
    procedure cbUseSidebarChange(Sender: TObject);
    procedure rgAlignmentClick(Sender: TObject);
    procedure seMarginChange(Sender: TObject);
    procedure seSpacingChange(Sender: TObject);
    procedure seSymbolWidthChange(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

uses
  TALegend;

{ TForm1 }

procedure TForm1.cbUseSidebarChange(Sender: TObject);
begin
  Chart1.Legend.UseSidebar := cbUseSidebar.Checked;
end;

procedure TForm1.rgAlignmentClick(Sender: TObject);
begin
  with Chart1.Legend do
    case rgAlignment.ItemIndex of
      0: Alignment := laTopLeft;
      1: Alignment := laBottomLeft;
      2: Alignment := laTopRight;
      3: Alignment := laBottomRight;
    end;
end;

procedure TForm1.seMarginChange(Sender: TObject);
begin
  Chart1.Legend.Margin := seMargin.Value;
end;

procedure TForm1.seSpacingChange(Sender: TObject);
begin
  Chart1.Legend.Spacing := seSpacing.Value;
end;

procedure TForm1.seSymbolWidthChange(Sender: TObject);
begin
  Chart1.Legend.SymbolWidth := seSymbolWidth.Value;
end;

initialization
  {$I main.lrs}

end.

