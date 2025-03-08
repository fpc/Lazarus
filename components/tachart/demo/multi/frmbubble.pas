unit frmBubble;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls,
  TAGraph, TASources, TAMultiSeries;

type
  TBubbleFrame = class(TFrame)
    BubbleSeries: TBubbleSeries;
    Chart: TChart;
    lcsBubble: TListChartSource;
  private

  public

  end;

implementation

{$R *.lfm}

end.

