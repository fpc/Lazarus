{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Register FPReport components in the Lazarus IDE.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit lazideregfpreport;

{$mode objfpc}{$H+}

{$IFNDEF WINDOWS}
{$DEFINE USEFPIMAGE}
{$ENDIF}

interface

uses
  Classes, SysUtils, controls, fpreport, fpjsonreport, fpreportdb, fpreportdesigner,
  fpreportpdfexport,
{$IFDEF USEFPIMAGE}
  fpreporthtmlexport,
  fpreportfpimageexport,
  cfgfpreporthtmlexport,
  cfgfpreportimageexport,
{$ENDIF}
  fpreportlclexport,
  fpreportformexport,
  fpreportpreview,
  fpreportprinterexport,
  cfgfpreportpdfexport,
  fpreportjson,
  fpreportcontnr,
  ComponentEditors,
  forms,
  menus;

Type

  { TFPReportComponentEditor }

  TFPReportComponentEditor = Class (TComponentEditor)
  private
    procedure AssignLocalData;
    procedure DesignReport;
    procedure PreviewReport;
    procedure RunReport;
  Public
    Procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    Function Report : TFPCustomReport;
  end;

  { TFPReportDesignerComponentEditor }

  TFPReportDesignerComponentEditor = Class (TComponentEditor)
  Public
    Procedure ExecuteVerb(Index: Integer); override;
    Procedure PrepareItem(Index: Integer; const AnItem: TMenuItem); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    Function ReportDesigner : TFPReportDesigner;
  end;

  { TFPReportExportComponentEditor }

  TFPReportExportComponentEditor = Class (TComponentEditor)
  Public
    Procedure ExecuteVerb(Index: Integer); override;
    Procedure PrepareItem(Index: Integer; const AnItem: TMenuItem); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    Function Exporter : TFPReportExporter;
  end;

procedure Register;

implementation

Uses regfpdesigner, LResources, frmideselectreportdata;


procedure Register;

begin
  RegisterFPReportPropEditors;
  RegisterComponentEditor(TFPCustomReport,TFPReportComponentEditor);
  RegisterComponentEditor(TFPReportExporter,TFPReportExportComponentEditor);
  RegisterComponentEditor(TFPReportDesigner,TFPReportDesignerComponentEditor);
  {$I fpreporticons.lrs}
  RegisterComponents('FPReport',[
     TFPReport,
     TFPJSONReport,
     TFPReportDesigner,
     TFPReportUserData,
     TFPReportDatasetData,
     TFPReportJSONData,
     TFPReportCollectionData,
     TFPReportObjectListData,
{$IFDEF USEFPIMAGE}
     TFPReportExportHTML,
     TFPReportExportfpImage,
{$ENDIF}
     TFPReportExportPDF,
     TFPreportPreviewExport,
     TFPreportPrinterExport
  ]);
end;

Resourcestring
  SVerbDesignReport = 'Design report';
  SVerbAssignLocalData = 'Assign local data';
  SVerbPreview = 'Preview report';
  SVerbExportReport = 'Export report';
  SVerbRunReport = 'Run report';
  SVerbOpenDesigner = 'Open designer';
  SVerbExportConfigure = 'Configuration...';

{ TFPReportDesignerComponentEditor }

procedure TFPReportDesignerComponentEditor.ExecuteVerb(Index: Integer);
begin
  if Not Assigned(ReportDesigner) then
    exit;
  Case index of
    0 :
      begin
      ReportDesigner.Execute;
      Designer.Modified;
      end;
  else
    inherited ExecuteVerb(Index)
  end;
end;

procedure TFPReportDesignerComponentEditor.PrepareItem(Index: Integer;
  const AnItem: TMenuItem);
begin
  inherited PrepareItem(Index, AnItem);
  if (Index=0) then
    anItem.Enabled:=Assigned(ReportDesigner) and Assigned(ReportDesigner.Report);
end;

function TFPReportDesignerComponentEditor.GetVerb(Index: Integer): string;
begin
  Case Index of
    0 : Result:=SVerbOpenDesigner;
  else
    Result:=inherited GetVerb(Index);
  end;
end;

function TFPReportDesignerComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

function TFPReportDesignerComponentEditor.ReportDesigner: TFPReportDesigner;
begin
  if Component is TFPReportDesigner then
    Result:=Component as TFPReportDesigner
  else
    Result:=Nil;
end;

{ TFPReportExportComponentEditor }

procedure TFPReportExportComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0 : Exporter.Execute;
    1 : if Exporter.ShowConfig then
          Designer.Modified;
  else
    inherited ExecuteVerb(Index);
  end;
end;

procedure TFPReportExportComponentEditor.PrepareItem(Index: Integer;
  const AnItem: TMenuItem);

Var
  I : Integer;
  B : Boolean;
begin
  inherited PrepareItem(Index, AnItem);
  Case Index of
    0 : anItem.Enabled:=Assigned(Exporter) and Assigned(Exporter.Report);
    1 :
      begin
      B:=Assigned(Exporter);
      if B then
        B:=Assigned(ReportExportManager.ExporterConfigHandler(TFPReportExporterClass(Exporter.ClassType)));
      anItem.enabled:=B;
      end;
    end;
end;

function TFPReportExportComponentEditor.GetVerb(Index: Integer): string;
begin
  Case Index of
    0 : Result:=SVerbExportReport;
    1 : Result:=SVerbExportConfigure;
  else
    Result:=inherited GetVerb(Index);
  end;
end;

function TFPReportExportComponentEditor.GetVerbCount: Integer;
begin
  Result:=0;
  if Exporter=Nil then
    exit;
  Result:=2;
end;

function TFPReportExportComponentEditor.Exporter: TFPReportExporter;
begin
  if Component is TFPReportExporter then
    Result:=Component as TFPReportExporter
  else
    Result:=Nil;
end;


{ TFPReportComponentEditor }

procedure TFPReportComponentEditor.PreviewReport;

Var
  F : TFPreportPreviewExport;

begin
  F:=TFPreportPreviewExport.Create(Application);
  try
    F.AutoRun:=True;
    F.Report:=Report;
    F.Execute;
  finally
    F.Free;
  end;
end;

procedure TFPReportComponentEditor.RunReport;

begin
  Report.RunReport;
end;

procedure TFPReportComponentEditor.DesignReport;

Var
  F : TFPReportDesigner;

begin
  F:=TFPReportDesigner.Create(Application);
  try
    F.Report:=Self.Report;
    F.Options:=AllReportDesignOptions-[rdoManageData];
    F.ModalWindow:=True;
    F.Execute;
    Designer.Modified;
  finally
    F.Free;
  end;
end;

procedure TFPReportComponentEditor.AssignLocalData;

  Procedure GetLocalData(L : TFPList);

  Var
    O,C : TComponent;

  begin
    O:=Report.Owner;
    While (O<>Nil) do
      begin
      For C in O do
        if C is TFPReportData then
          L.Add(C);
      if (O is TCustomForm) or (O is TDataModule) or (O is TFrame) then
        O:=Nil
      else
        O:=O.Owner;
      end;
  end;

Var
  All,Selected : TFPList;
  F : TSelectReportDataSourcesForm;
  I : integer;

begin
  Selected:=nil;
  F:=nil;
  AlL:=TFPList.Create;
  try
    GetLocalData(All);
    F:=TSelectReportDataSourcesForm.Create(Application);
    F.AllData:=All;
    if (F.ShowModal=mrOK) then
      begin
      Selected:=TFPList.Create;
      F.GetSelected(Selected);
      Report.ReportData.Clear;
      For I:=0 to Selected.Count-1 do
        Report.ReportData.AddReportData(TFPReportData(Selected[i]));
      Designer.Modified;
      end;
  finally
    F.Free;
    Selected.Free;
    AlL.Free;
  end;
end;

procedure TFPReportComponentEditor.ExecuteVerb(Index: Integer);
begin
  if Report=Nil then
     exit;
  Case Index of
    0 : DesignReport;
    1 : AssignLocalData;
    2 : PreviewReport;
    3 : RunReport;
  end;
end;

function TFPReportComponentEditor.GetVerb(Index: Integer): string;
begin
  Case Index of
    0 : Result:=SVerbDesignReport;
    1 : Result:=SVerbAssignLocalData;
    2 : Result:=SVerbPreview;
    3 : Result:=SVerbRunReport;
  else
    Result:='';
  end
end;

function TFPReportComponentEditor.GetVerbCount: Integer;
begin
  Result:=4;
end;

function TFPReportComponentEditor.Report: TFPCustomReport;
begin
  If (Component is TFPCustomReport) then
    Result:=Component as TFPCustomReport
  else
    Result:=nil
end;

end.

