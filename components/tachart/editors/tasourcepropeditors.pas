unit TASourcePropEditors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits;

type
  TDataPointsPropertyEditor = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: AnsiString; override;
  end;

procedure Register;

implementation

uses
  Controls,
  TASources, TADataPointsEditor;

procedure Register;
begin
  RegisterPropertyEditor(
    TypeInfo(TStrings), TListChartSource, 'DataPoints',
    TDataPointsPropertyEditor);
end;


{ TDataPointsPropertyEditor }

procedure TDataPointsPropertyEditor.Edit;
begin
  if DataPointsEditor(GetComponent(0) as TListChartSource) then
    Modified;
end;

function TDataPointsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paMultiSelect, paReadOnly, paRevertable];
end;

function TDataPointsPropertyEditor.GetValue: AnsiString;
begin
  Result := (GetObjectValue as TStrings).Text;
end;


end.

