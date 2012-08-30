unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, Spin, StdCtrls, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, TACustomSource, TAGraph, TASeries, TASources,
  TAAnimatedSource;

type
  TForm1 = class(TForm)
    btnStartStop: TButton;
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    lblSkipped: TLabel;
    ListChartSource1: TListChartSource;
    Panel1: TPanel;
    rgMethod: TRadioGroup;
    seTime: TSpinEdit;
    procedure btnStartStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgMethodClick(Sender: TObject);
    procedure seTimeChange(Sender: TObject);
  private
    FAnimatedSource: TCustomAnimatedChartSource;
    procedure OnGetItem(
      ASource: TCustomAnimatedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure OnStop(ASource: TCustomAnimatedChartSource);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Math;

{ TForm1 }

procedure TForm1.btnStartStopClick(Sender: TObject);
begin
  if FAnimatedSource.IsAnimating then
    FAnimatedSource.Stop
  else
    FAnimatedSource.Start;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FAnimatedSource := TCustomAnimatedChartSource.Create(Self);
  FAnimatedSource.Origin := ListChartSource1;
  FAnimatedSource.AnimationInterval := 30;
  FAnimatedSource.OnGetItem := @OnGetItem;
  FAnimatedSource.OnStop := @OnStop;
  seTimeChange(nil);
  Chart1BarSeries1.Source := FAnimatedSource;
  FAnimatedSource.Start;
end;

procedure TForm1.OnGetItem(
  ASource: TCustomAnimatedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
begin
  case rgMethod.ItemIndex of
  0: AItem.Y *= ASource.Progress;
  1:
    if ASource.Count * ASource.Progress < AIndex then
      AItem.Y := 0;
  2:
    case Sign(Trunc(ASource.Count * ASource.Progress) - AIndex) of
      0: AItem.Y *= Frac(ASource.Count * ASource.Progress);
      -1: AItem.Y := 0;
    end;
  end;
end;

procedure TForm1.OnStop(ASource: TCustomAnimatedChartSource);
begin
  lblSkipped.Caption := Format('Skipped frames: %d', [ASource.SkippedFramesCount]);
end;

procedure TForm1.rgMethodClick(Sender: TObject);
begin
  FAnimatedSource.Start;
end;

procedure TForm1.seTimeChange(Sender: TObject);
begin
  FAnimatedSource.AnimationTime := seTime.Value;
end;

end.

