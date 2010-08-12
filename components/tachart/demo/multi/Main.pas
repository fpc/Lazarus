unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    chBubble: TChart;
    Chart1BubbleSeries1: TBubbleSeries;
    lcsBubble: TListChartSource;
    PageControl1: TPageControl;
    tsBubble: TTabSheet;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

end.

