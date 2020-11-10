{
 **********************************************************************
  This file is part of the Free Pascal run time library.

  See the file COPYING.FPC, included in this distribution,
  for details about the license.
 **********************************************************************

 Registration of components and property editors for dbexport package.

 Copyright (c) 2007 by Michael Van Canneyt, member of the Free Pascal development team

}
unit regdbexport;

{$mode objfpc}{$H+}
// XMLXSDExport unit was introduced in FPC 2.5.1.
{$DEFINE HASXMLXSDEXPORT}

interface

uses
  Classes, SysUtils, lresources, dbPropEdits, propedits,
  componenteditors, fpdataexporter, dialogs, sdb_consts;

Type
  { TExportFieldProperty }

  TExportFieldProperty = class(TFieldProperty)
  Public
    procedure FillValues(const Values: TStringList); override;
  end;

  { TDataExporterComponentEditor }

  TDataExporterComponentEditor = class(TComponentEditor)
  private
    procedure ExecuteExporter(Ex: TFPDataExporter);
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

Procedure Register;

implementation

{$R dbexportimg.res}

uses
  fpdbexport,
  fpstdExports,
  fpcsvexport,
  fpfixedexport,
  fpsimplexmlexport,
  fpsimplejsonexport,
  fptexexport,
  fpsqlexport,
  fprtfexport,
  fpdbfexport
  {$IFDEF HASXMLXSDEXPORT}
  ,fpxmlxsdexport
  {$ENDIF}
  ;

{ TDataExporterComponentEditor }

function TDataExporterComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

function TDataExporterComponentEditor.GetVerb(Index: Integer): string;
begin
  Case Index of
    0: Result:=SExecute;
  else
    Result:=Inherited GetVerb(Index)
  end;
end;

procedure TDataExporterComponentEditor.ExecuteExporter(Ex : TFPDataExporter);

Var
  B: Boolean;

begin
  If Assigned(Ex) then
    If Not Assigned(Ex.Dataset) then
      ShowMessage(SErrNoDatasetAssigned)
    else
      begin
      B:=Not Ex.Dataset.Active;
      If B then
        Try
          Ex.Dataset.Open;
        except
          On E : Exception do
            begin
            ShowMessage(Format(SErrOpeningDataset,[E.Message]));
            Exit;
            end;
        end;
      Try
        Ex.Execute;
      Finally
        If B then Ex.Dataset.Close;
      end;
      end;
end;

procedure TDataExporterComponentEditor.ExecuteVerb(Index: Integer);

begin
  Case Index of
    0 : ExecuteExporter(GetComponent as TFPDataExporter);
  else
    Inherited
  end
end;


{ TExportFieldProperty }

procedure TExportFieldProperty.FillValues(const Values: TStringList);

Var
  FI : TExportFieldItem;

begin
  FI:=TExportFieldItem(GetComponent(0));
  If Assigned(FI.Exporter) and Assigned(FI.Exporter.Dataset) then
    FI.Exporter.Dataset.GetFieldNames(Values);
end;
Procedure Register;

begin
  RegisterStdFormats;
{  RegisterFixedExportFormat;
  RegisterSQLExportFormat;
  RegisterSimpleXMLExportFormat;
  RegisterSimpleJSONExportFormat;
  RegisterDBFExportFormat;
  RegisterTexExportFormat;
  RegisterRTFExportFormat;
  Register}

  RegisterComponents('Data Export',[TCSVExporter,
                                    TFixedLengthExporter,
                                    TSQLExporter,
                                    {$IFDEF HASXMLXSDEXPORT}
                                    TXMLXSDExporter,
                                    {$ENDIF}
                                    TSimpleXMLExporter,
                                    TSimpleJSONExporter,
                                    TFPDBFExport,
                                    TTexExporter,
                                    TRTFExporter,
                                    TStandardExportFormats,
                                    TFPDataExporter]);
  RegisterPropertyEditor(TypeInfo(string), TExportFieldItem, 'FieldName', TExportFieldProperty);
  RegisterComponentEditor(TFPDataExporter,TDataExporterComponentEditor) ;
end;

end.

