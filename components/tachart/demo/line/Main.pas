unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, Spin, StdCtrls, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, Types, TAGraph, TASeries, TASources, TATools,
  TATransformations, TACustomSeries, TADrawUtils, TATypes,
  frmFast, frmPointers, frmCustomDrawPointer, frmGetPointerStyle,
  frmOscilloscope, frmColorEach;

type

  { TMainForm }

  TMainForm = class(TForm)
    PageControl1: TPageControl;
    tsColorEach: TTabSheet;
    tsCustomDrawPointer: TTabSheet;
    tsGetPointerStyle: TTabSheet;
    tsOwnerDrawnPointer: TTabSheet;
    tsOscilloscope: TTabSheet;
    tsPointers: TTabSheet;
    tsFast: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    FFastDrawingFrame: TFastDrawingFrame;
    FPointersFrame: TPointersFrame;
    FCustomDrawPointerFrame: TCustomDrawPointerFrame;
    FGetPointerStyleFrame: TGetPointerStyleFrame;
    FOscilloscopeFrame: TOscilloscopeFrame;
    FColorEachDataPointFrame: TColorEachDataPointFrame;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLIntf, IntfGraphics, TAChartUtils, TAEnumerators;

type
  TLineSeriesEnum =
    specialize TFilteredChartSeriesEnumeratorFactory<TLineSeries>;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FFastDrawingFrame := TFastDrawingFrame.Create(Self);
  FFastDrawingFrame.Parent := tsFast;
  FFastDrawingFrame.Align := alClient;

  FPointersFrame := TPointersFrame.Create(Self);
  FPointersFrame.Parent := tsPointers;
  FPointersFrame.Align := alClient;

  FCustomDrawPointerFrame := TCustomDrawPointerFrame.Create(Self);
  FCustomDrawPointerFrame.Parent := tsCustomDrawPointer;
  FCustomDrawPointerFrame.Align := alClient;

  FGetPointerStyleFrame := TGetPointerStyleFrame.Create(Self);
  FGetPointerStyleFrame.Parent := tsGetPointerStyle;
  FGetPointerStyleFrame.Align := alClient;

  FOscilloscopeFrame := TOscilloscopeFrame.Create(Self);
  FOscilloscopeFrame.Parent := tsOscilloscope;
  FOscilloscopeFrame.Align := alClient;

  FColorEachDataPointFrame := TColorEachDataPointFrame.Create(Self);
  FColorEachDataPointFrame.Parent := tsColorEach;
  FColoreachDataPointFrame.Align := alClient;
end;

procedure TMainForm.PageControl1Change(Sender: TObject);
begin
  FOscilloscopeFrame.Timer.Enabled := PageControl1.ActivePage = tsOscilloscope;
end;

end.

