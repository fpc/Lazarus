unit main;

{$mode objfpc}{$H+}

interface

uses
  LCLVersion,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ComCtrls, ExtCtrls,
  TAGraph, TASeries, TASources, TAStyles, TACustomSeries, TACustomSource,
  frmGeneral, frmErrorRange;

type

  { TForm1 }

  TForm1 = class(TForm)
    PageControl1: TPageControl;
    PgGeneral: TTabSheet;
    PgErrorRange: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    FGeneralFrame: TGeneralFrame;
    FErrorRangeFrame: TErrorRangeFrame;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Math, TAChartUtils, TATextElements;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FGeneralFrame := TGeneralFrame.Create(Self);
  FGeneralFrame.Parent := PgGeneral;
  FGeneralFrame.Align := alClient;

  FErrorRangeFrame := TErrorRangeFrame.Create(Self);
  FErrorRangeFrame.Parent := PgErrorRange;
  FErrorRangeFrame.Align := alClient;
end;

end.

