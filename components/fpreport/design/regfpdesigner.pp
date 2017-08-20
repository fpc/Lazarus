{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Report data component property editor for object inspector.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit regfpdesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpreport, ideintf, propedits, ObjInspStrConsts, frmreportmemoedit;

Type

  { TDataComponentPropertyEditor }

  TDataComponentPropertyEditor = class(TComponentPropertyEditor)
  Protected
    Function GetReport : TFPCustomReport;
  Public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;



Procedure RegisterFPReportPropEditors;

implementation

Procedure RegisterFPReportPropEditors;

begin
  RegisterPropertyEditor(TypeInfo(TFPReportData), TFPreportElement, 'Data', TDataComponentPropertyEditor);
end;

{ TDataComponentPropertyEditor }

function TDataComponentPropertyEditor.GetReport: TFPCustomReport;
Var
  C : TPersistent;
begin
  Result:=Nil;
  C:=GetComponent(0);
  if C is TFPCustomReport then
    Result:=C as TFPCustomReport
  else if C is TFPReportElement then
    Result:=TFPReportElement(C).Report;
end;

procedure TDataComponentPropertyEditor.GetValues(Proc: TGetStrProc);

Var
  Report : TFPCustomReport;
  I : Integer;

begin
  Report:=GetReport;
  proc(oisNone);
  if Assigned(Report) then
    For I:=0 to Report.ReportData.Count-1 do
      Proc(Report.ReportData[i].Data.Name);
end;

procedure TDataComponentPropertyEditor.SetValue(const NewValue: ansistring);

Var
  Report:TFPCustomReport;
  RD : TFPReportData;

begin
  if NewValue=GetValue then exit;
  RD:=nil;
  if (NewValue<>oisNone) then
    begin
    Report:=GetReport;
    if Assigned(Report) then
      RD:=Report.ReportData.FindReportData(NewValue);
    end;
  SetPtrValue(RD);
end;

end.

