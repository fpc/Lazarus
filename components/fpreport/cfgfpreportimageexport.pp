{
    This file is part of the Free Component Library.
    Copyright (c) 2016 Michael Van Canneyt, member of the Free Pascal development team

    Configure FPReport to fpimage export dialog to be used in LCL preview.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit cfgfpreportimageexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ButtonPanel, ExtCtrls, Spin, fpreport, fpreportfpimageexport;

type
  { TConfigFPImageExportForm }

  TConfigFPImageExportForm = class(TForm)
    BPExport: TButtonPanel;
    CBDPI: TComboBox;
    ESeparator: TEdit;
    FEBaseFileName: TFileNameEdit;
    GBOptions: TGroupBox;
    LCBDPI: TLabel;
    LSEDigits: TLabel;
    LFEPDF: TLabel;
    LESeparator: TLabel;
    SEDigits: TSpinEdit;
    procedure FormCreate(Sender: TObject);
  private
    procedure GetImageFilter;
    procedure LocalizeForm;
    { private declarations }
  public
    { public declarations TFPReportExportfpImage } 
    Procedure ConfigToForm(AExporter: TFPReportExportfpImage);
    Procedure FormToConfig(AExporter: TFPReportExportfpImage);
  end;

  { TFPImageConfigObj }

  TFPImageConfigObj = Class
  public
    Procedure RegisterHandler;
    procedure DoConfig(Sender: TObject; AExporter: TFPReportExporter; var Cancelled: Boolean);
  end;

var
  ConfigPDFExportForm: TConfigFPImageExportForm;

Procedure RegisterPDFExportConfig;

implementation

uses fpimage, fppdf;

{$R *.lfm}

Var
  Cfg : TFPImageConfigObj;

Resourcestring
  sConfigImageFormCaption = 'Export to Images';
  SImageFileName    = '&FileName';
  SImageFilters = 'PDF Files|*.pdf|All files|*.*';
  SImageDPI = '&DPI';
  SImageDigits = '&Min. digits' ;
  SImageSeparator = '&Separator' ;
  SIMageOptions = 'Options';
  SFiles = 'Files';

Procedure RegisterPDFExportConfig;

begin
  FreeAndNil(Cfg);
  Cfg:=TFPImageConfigObj.Create;
  Cfg.RegisterHandler;
end;

{ TConfigFPImageExportForm }

procedure TConfigFPImageExportForm.LocalizeForm;


begin
  Caption:=sConfigImageFormCaption;
  LFEPDF.Caption:= SImageFileName;
  FEBaseFileName.Filter:=SImageFilters;
  LCBDPI.Caption:=SImageDPI;
  LSEDigits.Caption:=SImageDigits;
  GBOptions.Caption:=SImageOptions;
  LESeparator.Caption:=SImageSeparator;
  GetImageFilter;
end;

procedure TConfigFPImageExportForm.GetImageFilter;

Var
  I : Integer;
  S,TN : String;

begin
  S:='';
  with ImageHandlers do
    For I:=0 to Count-1 do
      begin
      TN:=TypeNames[I];
      if (S<>'') then
        S:=S+'|';

      S:=S+TN+' '+SFiles+'|*'+{$IFDEF VER2_6_4}DefaultExtention[TN]{$else}DefaultExtension[TN]{$ENDIF};
      end;
  FEBaseFileName.Filter:=S;
end;

procedure TConfigFPImageExportForm.FormCreate(Sender: TObject);
begin
  LocalizeForm;
end;

procedure TConfigFPImageExportForm.ConfigToForm(AExporter: TFPReportExportfpImage);
begin
  CBDPI.Text:=IntToStr(AExporter.DPI);
end;

procedure TConfigFPImageExportForm.FormToConfig(AExporter: TFPReportExportfpImage);
Var
  Sep : String;

begin
  AExporter.BaseFileName:=FEBaseFileName.FileName;
  Sep:=StringReplace(ESeparator.Caption,'%','%%',[]);
  if SEDigits.Value=1 then
    AExporter.SequenceFormat:=Sep+'%d'
  else
    AExporter.SequenceFormat:=Sep+'%.'+IntToStr(SEDigits.Value)+'d';
end;


{ TFPImageConfigObj }

procedure TFPImageConfigObj.RegisterHandler;
begin
  if ReportExportManager.FindExporter(TFPReportExportfpImage.Name)<>Nil then
    ReportExportManager.RegisterConfigHandler(TFPReportExportfpImage.Name,@DoConfig);
end;

procedure TFPImageConfigObj.DoConfig(Sender: TObject; AExporter: TFPReportExporter;
  var Cancelled: Boolean);
begin
  Cancelled:=True;
  With TConfigFPImageExportForm.Create(Application) do
    try
      ConfigToForm(AExporter as TFPReportExportfpImage);
      Cancelled:=ShowModal<>mrOK;
      if not Cancelled then
        FormToConfig(AExporter as TFPReportExportfpImage);
    finally
      Free;
    end;
end;

initialization
  RegisterPDFExportConfig;
finalization
  FreeAndNil(Cfg);
end.

