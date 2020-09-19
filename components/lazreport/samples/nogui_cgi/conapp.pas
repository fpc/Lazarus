unit conApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustApp, UnitReporter;

type

  { TReporterApp }

  TReporterApp = class(TCustomApplication)
  private
    fExportBackend: TExportBackend;
    function  GetReporter: TReporter;
    procedure RunReport(reportName:string; reportFile:string='');
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

var
  Application: TReporterApp;

implementation

{ TReporterApp }

function TReporterApp.GetReporter: TReporter;
begin
  result := TReporter.create;
  result.ExportBackend := fExportBackend;
end;

procedure TReporterApp.RunReport(reportName: string; reportFile: string);
var
  fReporter: TReporter;
begin
  if reportFile='' then
    reportFile := reportName + '.lrf';

  fReporter := GetReporter;
  fReporter.ExportBackend := fExportBackend;
  try
    case reportName of
      'disks':
        fReporter.PrepareDisksReport;
      'cross':
        fReporter.PrepareCrossTabReport;
      'lrcode':
        if not fReporter.PrepareLRCodeReport then begin
          WriteLn('Error while producing the LRCodeReport');
          exit;
        end;
      else
        fReporter.LoadReport(reportFile);
    end;

    if fReporter.ProcessExportReport(reportName) then
      WriteLn(reportName, ' report successfully exported')
    else
      WriteLn(reportName, ' report failed');
  finally
    fReporter.Free;
  end;
end;

procedure TReporterApp.DoRun;
var
  ErrorMsg: String;
begin
  fExportBackend := ebPowerPDF;

  // quick check parameters
  ErrorMsg := CheckOptions('idcrxh', ['help','images','disks','code','cross','back:']);
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('back') then begin
    ErrorMsg := GetOptionValue('back');
    case lowercase(ErrorMsg) of
      'powerpdf': fExportBackend := ebPowerPDF;
      'fclpdf':   fExportBackend := ebFCLPDF;
      'txt':      fExportBackend := ebTxt;
      'csv':      fExportBackend := ebCSV;
      'html':     fExportBackend := ebHtml;
      'htmldiv':  fExportBackend := ebHtmlDiv;
      'bmp':      fExportBackend := ebBmp;
      'jpg':      fExportBackend := ebJpg;
      'png':      fExportBackend := ebPng;
      'ods':      fExportBackend := ebOpenDoc;
      'xls':      fExportBackend := ebXLS;
      'xlsx':     fExportBackend := ebOOXML;
      else begin
        Writeln('Invalid export backend: ',ErrorMsg);
        WriteHelp;
        Terminate;
        exit;
      end;
    end;
  end;

  if HasOption('i', 'images') then begin
    RunReport('images');
    Terminate;
    exit;
  end;

  if HasOption('d', 'disks') then begin
    RunReport('disks');
    Terminate;
    exit;
  end;

  if HasOption('c', 'code') then begin
    RunReport('lrcode');
    Terminate;
    exit;
  end;

  if HasOption('r', 'rrect') then begin
    RunReport('roundrect', 'frroundrecttester.lrf');
    Terminate;
    exit;
  end;

  if HasOption('x', 'cross') then begin
    RunReport('cross', 'demo_cross.lrf');
    Terminate;
    exit;
  end;

  // stop program loop
  Terminate;
end;

constructor TReporterApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TReporterApp.Destroy;
begin
  inherited Destroy;
end;

procedure TReporterApp.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExtractFileName(ExeName), ' -h');
  WriteLn;
  WriteLn('   --back=[powerpdf*|fclpdf|txt|csv|html|htmldiv|xls|xlsx|odf|bmp|jpg|png]');
  WriteLn('-x,--cross CrossTab Report');
  WriteLn('-d,--disks Disk records inventory report (60+ pages)');
  WriteLn('-i,--images=[mono|all*] Bitmap monochrome or all formats Report');
  WriteLn('-c,--code LRCodeReport Report');
  WriteLn('-r,--rrect RoundRect Report');
  WriteLn;
  WriteLn('* = default');
end;

procedure InitApp;
begin
  Application := TReporterApp.Create(nil);
end;

procedure DoneApp;
begin
  Application.Free;
end;

initialization
  InitApp;

finalization
  DoneApp;

end.

