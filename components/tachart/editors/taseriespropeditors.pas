unit TASeriesPropEditors;

{$H+}

interface

procedure Register;

implementation

uses
  Graphics, Classes, Math, PropEdits, SysUtils, LCLIntf, typinfo,
  TATypes, TADrawUtils, TADrawerCanvas, TACustomSeries, TASeries, TALegend, TAGraph;

type
  TAxisIndexPropertyEditor = class(TOrdinalPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function OrdValueToVisualValue(AOrdValue: Longint): String; override;
    procedure GetValues(AProc: TGetStrProc); override;
    procedure SetValue(const ANewValue: String); override;
  end;

  TSeriesPointerStylePropertyEditor = class(TEnumPropertyEditor)
  private
    procedure DrawPointer(ACanvas: TCanvas; ARect: TRect;
      AStyle: TSeriesPointerStyle; APenColor, ABrushColor: TColor);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure ListMeasureWidth(const {%H-}CurValue: ansistring; {%H-}AIndex:integer;
      {%H-}ACanvas: TCanvas;  var AWidth: Integer); override;
    procedure ListDrawValue(const CurValue: ansistring; {%H-}AIndex:integer;
      ACanvas: TCanvas;  const ARect: TRect; AState: TPropEditDrawState); override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      AState:TPropEditDrawState); override;
  end;


procedure Register;
begin
  RegisterPropertyEditor(
    TypeInfo(TChartAxisIndex), TCustomChartSeries, '', TAxisIndexPropertyEditor);
  RegisterPropertyEditor(
    TypeInfo(TSeriesPointerStyle), TSeriesPointer, '', TSeriesPointerStylePropertyEditor);
end;

{ TAxisIndexPropertyEditor }

function TAxisIndexPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TAxisIndexPropertyEditor.GetValues(AProc: TGetStrProc);
var
  s: TCustomChartSeries;
  ch: TChart;
  i: Integer;
begin
  s := GetComponent(0) as TCustomChartSeries;
  ch := s.ParentChart;
  AProc('-1 None');
  for i := 0 to ch.AxisList.Count - 1 do
    AProc(IntToStr(i) + ' ' + ch.AxisList[i].DisplayName);
end;

function TAxisIndexPropertyEditor.OrdValueToVisualValue(
  AOrdValue: Longint): String;
var
  s: TCustomChartSeries;
  ch: TChart;
begin
  s := GetComponent(0) as TCustomChartSeries;
  ch := s.ParentChart;
  Result := IntToStr(AOrdValue) + ' ';
  if InRange(AOrdValue, 0, ch.AxisList.Count - 1) then
    Result += ch.AxisList[AOrdValue].DisplayName
  else
    Result += 'None';
end;

procedure TAxisIndexPropertyEditor.SetValue(const ANewValue: String);
var
  v: Integer;
  code: Word;
begin
  Val(ANewValue, v, code);
  if code > 0 then
    Val(Copy(ANewValue, 1, code - 1), v, code);
  SetOrdValue(Max(v, Low(TChartAxisIndex)));
end;


{ TSeriesPointerStylePropertyEditor }

procedure TSeriesPointerStylePropertyEditor.DrawPointer(ACanvas: TCanvas;
  ARect: TRect; AStyle: TSeriesPointerStyle; APenColor, ABrushColor: TColor);
var
  id: IChartDrawer;
  pointer: TSeriesPointer;
  c: TPoint;
begin
  pointer := TSeriesPointer.Create(nil);
  try
    pointer.Style := AStyle;
    Pointer.HorizSize := (ARect.Right - ARect.Left) div 2 - 1;
    Pointer.VertSize := (ARect.Bottom - ARect.Top) div 2 - 1;
    Pointer.Brush.Color := ABrushColor;
    Pointer.Pen.Color := APenColor;
    c := Point((ARect.Left + ARect.Right) div 2, (ARect.Top + ARect.Bottom) div 2);
    pointer.Draw(TCanvasDrawer.Create(ACanvas), c, ABrushColor);
  finally
    pointer.Free;
  end;
end;

function TSeriesPointerStylePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := (inherited GetAttributes) + [paCustomDrawn];
end;

procedure TSeriesPointerStylePropertyEditor.ListMeasureWidth(
  const CurValue: ansistring; AIndex:integer; ACanvas: TCanvas;
  var AWidth: Integer);
begin
  AWidth := 130;
end;

procedure TSeriesPointerStylePropertyEditor.ListDrawValue(const CurValue: ansistring;
  AIndex:integer; ACanvas: TCanvas; const ARect: TRect; AState: TPropEditDrawState);
const
  MARGIN = 2;
var
  lRect: TRect;
  oldPenColor, oldBrushColor: TColor;
  oldPenStyle: TPenStyle;
  i: Integer;
begin
  lRect := ARect;
  lRect.Right := lRect.Left + (lRect.Bottom - lRect.Top); // * 3 div 2;
  InflateRect(lRect, -MARGIN, -MARGIN);
  with ACanvas do
  try
    // save off things
    oldPenColor := Pen.Color;
    oldBrushColor := Brush.Color;
    oldPenStyle := Pen.Style;

    try
      // white out the background
      Brush.Color := clWindow;
      FillRect(ARect);

      // set things up and do work
      i := GetEnumValue(GetPropInfo^.PropType, CurValue);
      DrawPointer(ACanvas, lRect, TSeriesPointerStyle(i), clWindowText, clWindow);
    finally
      // restore the things we twiddled with
      Brush.Color := oldBrushColor;
      Pen.Style := oldPenStyle;
      Pen.Color := oldPenColor;
    end;
  finally
    lRect := Rect(lRect.Right + 2*MARGIN, ARect.Top, ARect.Right, ARect.Bottom);
    inherited ListDrawValue(CurValue, -1, ACanvas, lRect, AState);
  end;
end;

procedure TSeriesPointerStylePropertyEditor.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; AState:TPropEditDrawState);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, -1, ACanvas, ARect, [pedsInEdit])
  else
    inherited PropDrawValue(ACanvas, ARect, AState);
end;

end.

