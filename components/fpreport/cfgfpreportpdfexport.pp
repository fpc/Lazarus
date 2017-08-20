{
    This file is part of the Free Component Library.
    Copyright (c) 2016 Michael Van Canneyt, member of the Free Pascal development team

    Configure FPReport to PDF export dialog to be used in LCL preview.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit cfgfpreportpdfexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ButtonPanel, ExtCtrls, fpreport, fpreportpdfexport;

type
{
poOutLine, poCompressText, poCompressFonts, poCompressImages, poUseRawJPEG
}
  { TConfigPDFExportForm }

  TConfigPDFExportForm = class(TForm)
    BPExport: TButtonPanel;
    CBCompress: TCheckBox;
    CBOutline: TCheckBox;
    FEPDF: TFileNameEdit;
    GBOptions: TGroupBox;
    LFEPDF: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    procedure LocalizeForm;
    { private declarations }
  public
    { public declarations }
    Procedure ConfigToForm(AExporter: TFPReportExportPDF);
    Procedure FormToConfig(AExporter: TFPReportExportPDF);
  end;

  { TPDFConfigObj }

  TPDFConfigObj = Class
  public
    Procedure RegisterHandler;
    procedure DoConfig(Sender: TObject; AExporter: TFPReportExporter; var Cancelled: Boolean);
  end;

var
  ConfigPDFExportForm: TConfigPDFExportForm;

Procedure RegisterPDFExportConfig;

implementation

uses fppdf;

{$R *.lfm}

Var
  Cfg : TPDFConfigObj;

Resourcestring
  sConfigPDFFormCaption = 'Export to PDF';
  SPDFFileName    = 'FileName';
  SPDFFilters = 'PDF Files|*.pdf|All files|*.*';
  SPDFCompress = 'Compress';
  SPDFOutLine = 'Outline' ;
  SPDFOptions = 'Options';

Procedure RegisterPDFExportConfig;

begin
  FreeAndNil(Cfg);
  Cfg:=TPDFConfigObj.Create;
  Cfg.RegisterHandler;
end;

{ TConfigPDFExportForm }
Const
  CompressedOptions = [poCompressText, poCompressFonts, poCompressImages];

procedure TConfigPDFExportForm.LocalizeForm;

begin
  Caption:=sConfigPDFFormCaption;
  LFEPDF.Caption:= SPDFFileName;
  FEPDF.Filter:=SPDFFilters;
  CBCompress.Caption:=SPDFCompress;
  CBOutline.Caption:=SPDFOutLine;
  GBOptions.Caption:=SPDFOptions;
end;

procedure TConfigPDFExportForm.FormCreate(Sender: TObject);
begin
  LocalizeForm;
end;

procedure TConfigPDFExportForm.ConfigToForm(AExporter: TFPReportExportPDF);
begin
  CBCompress.Checked:=(CompressedOptions*AExporter.Options)<>[];
  CBOutline.Checked:=(poOutLine in AExporter.Options);
end;

procedure TConfigPDFExportForm.FormToConfig(AExporter: TFPReportExportPDF);
begin
  if CBCompress.Checked then
    AExporter.Options:=AExporter.Options+CompressedOptions
  else
  AExporter.Options:=AExporter.Options-CompressedOptions;
  if CBOutline.Checked then
    AExporter.Options:=AExporter.Options+[poOutline]
  else
    AExporter.Options:=AExporter.Options-[poOutline];
  AExporter.FileName:=FEPDF.FileName;
end;


{ TPDFConfigObj }

procedure TPDFConfigObj.RegisterHandler;
begin
  if ReportExportManager.FindExporter(TFPReportExportPDF.Name)<>Nil then
    ReportExportManager.RegisterConfigHandler(TFPReportExportPDF.Name,@DoConfig);
end;

procedure TPDFConfigObj.DoConfig(Sender: TObject; AExporter: TFPReportExporter;
  var Cancelled: Boolean);
begin
  Cancelled:=True;
  With TConfigPDFExportForm.Create(Application) do
    try
      ConfigToForm(AExporter as TFPReportExportPDF);
      Cancelled:=ShowModal<>mrOK;
      if not Cancelled then
        FormToConfig(AExporter as TFPReportExportPDF);
    finally
      Free;
    end;
end;

initialization
  RegisterPDFExportConfig;
finalization
  FreeAndNil(Cfg);
end.

