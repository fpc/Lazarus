unit TASeriesPropEditors;

{$H+}

interface

procedure Register;

implementation

uses
  Classes, Math, PropEdits, SysUtils, TACustomSeries, TAGraph;

type
  TAxisIndexPropertyEditor = class(TOrdinalPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function OrdValueToVisualValue(AOrdValue: Longint): String; override;
    procedure GetValues(AProc: TGetStrProc); override;
    procedure SetValue(const ANewValue: String); override;
  end;

procedure Register;
begin
  RegisterPropertyEditor(
    TypeInfo(TChartAxisIndex), TCustomChartSeries, '', TAxisIndexPropertyEditor);
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

end.

