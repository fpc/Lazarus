unit UnitWeb;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, CustApp, httpdefs, fpHTTP, fpWeb, webutil, UnitReporter;

type

  { TWebDM }

  TWebDM = class(TFPWebModule)
    procedure crosstabRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure defaultactionRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure lrcodereportRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure reportedisksRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure reporteimgRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure roundrectRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    fReporter: TReporter;
    procedure ProcessExportReport(const reportName: string; ARequest:TRequest;
        AResponse: TResponse; var Handled: boolean);
  public

  end;

var
  WebDM: TWebDM;
  weblog: Text;

implementation

{$R *.lfm}

procedure ServeFile(aFilename: string; AResponse: TResponse);
var
  F: TFileStream;
begin
  case lowercase(ExtractFileExt(aFilename)) of
    '.ico':         AResponse.ContentType := 'image/x-icon';
    '.bmp':         AResponse.ContentType := 'image/bmp';
    '.png':         AResponse.ContentType := 'image/png';
    '.jpg','.jpeg': AResponse.ContentType := 'image/jpeg';
    '.pdf':         AResponse.ContentType := 'application/pdf';
    '.html','.htm': AResponse.ContentType := 'text/html';
    '.txt':         AResponse.ContentType := 'text/plain';
    '.csv':         AResponse.ContentType := 'text/plain'; // 'text/csv';
    '.xls':         AResponse.ContentType := 'application/vnd.ms-excel';
    '.xlsx':        AResponse.ContentType := 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet';
    '.ods':         AResponse.ContentType := 'application/vnd.oasis.opendocument.spreadsheet';
    else            AResponse.ContentType := 'Application/octet-stream';
  end;
  F := TFileStream.Create(aFilename, fmOpenRead + fmShareDenyWrite);
  try
    AResponse.ContentLength := F.Size;
    AResponse.ContentStream := F;
    AResponse.SendContent;
    AResponse.ContentStream := nil;
  finally
    F.Free;
  end;
end;

{ TWebDM }

procedure TWebDM.defaultactionRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  IndexHtml: TStringList;

  procedure DumpTheRequest(origin:string);
  var
    Dump: TStringList;
  begin
    Dump := TStringList.Create;
    Dump.Add(format('<h2>%s</h2>',[origin]));
    Dump.Add(format('<h3>currentDir=%s',[GetCurrentDir]));
    DumpRequest(ARequest, Dump, true);
    AResponse.Contents.AddStrings(Dump);
    Dump.Free;
  end;

begin
  if ARequest.URI='/favicon.ico' then begin
    ServeFile('favicon.ico', AResponse);
    Handled := true;
  end else
  if ARequest.PathInfo='/Terminate' then begin
    AResponse.Contents.Text := '<h1>Bye!</h1>';
    handled := true;
    CustomApplication.Terminate;
  end else
  if ARequest.PathInfo='' then begin
    if FileExists('index.html') then begin
      IndexHtml := TStringList.Create;
      IndexHtml.LoadFromFile('index.html');
      AResponse.Contents.Text :=
        StringReplace(IndexHtml.Text, '%uri%', ARequest.ScriptName, []);
      IndexHtml.Free;
    end else begin
      //DumpTheRequest('URI='+ARequest.URI);
      AResponse.Contents.Text :=
        '<html><head><title>LazReport NO-GUI Tester</title></head><body>'+
        '<h3>The file index.html was not found!</h3>'+
        '<form action="'+ ARequest.ScriptName + '/web/lrcodereport" method="GET" target="docview">'+
        '<input type="hidden" name="backend" value="powerpdf">'+
        '<input type="submit" value="Test LRCodeReport"></form>'+
        '<iframe id="iframe" name="docview" title="Report Viewer" height="550px" width="80%"></iframe>'+
        '</body></html>';
    end;
    Handled := true;
  end else begin
    DumpTheRequest('Final catch!');
    Handled := true;
  end;
end;

procedure TWebDM.crosstabRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  fReporter.PrepareCrossTabReport;
  ProcessExportReport('crosstab', ARequest, AResponse, Handled);
end;

procedure TWebDM.DataModuleCreate(Sender: TObject);
begin
  fReporter := TReporter.create;
end;

procedure TWebDM.DataModuleDestroy(Sender: TObject);
begin
  fReporter.Free;
end;

procedure TWebDM.lrcodereportRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  if fReporter.PrepareLRCodeReport then
    ProcessExportReport('lrcode', ARequest, AResponse, Handled)
  else begin
    AResponse.ContentType := 'text/html';
    AResponse.Content := '<html><body>Error testing lrcodereport</body></html>';
  end;
end;

procedure TWebDM.reportedisksRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  fReporter.PrepareDisksReport;
  ProcessExportReport('disks', ARequest, AResponse, Handled);
end;

procedure TWebDM.reporteimgRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  fReporter.LoadReport('images.lrf');
  ProcessExportReport('images', ARequest, AResponse, Handled);
end;

procedure TWebDM.roundrectRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  fReporter.LoadReport('frroundrecttester.lrf');
  ProcessExportReport('roundrect', ARequest, AResponse, Handled);
end;

procedure SaveRequest(ARequest: TRequest; info:string);
var
  n: Integer;
  Dump: TStringList;
  filename: string;
begin
  n := 0;
  repeat
    filename := format('Request_%.3d.html',[n]);
    inc(n);
  until not FileExists(filename);

  Dump := TStringList.Create;
  Dump.Add(format('<h3>%s</h3>',[Info]));
  Dump.Add(format('<h3>currentDir=%s</h3>',[GetCurrentDir]));
  DumpRequest(ARequest, Dump, true);
  Dump.SaveToFile(filename);
  Dump.Free;
end;

procedure TWebDM.ProcessExportReport(const reportName: string;
  ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
begin
  //SaveRequest(ARequest, 'Report: '+reportName);
  case ARequest.QueryFields.Values['backend'] of
    'powerpdf': fReporter.ExportBackend := ebPowerPDF;
    'fclpdf':   fReporter.ExportBackend := ebFCLPDF;
    'htmldiv':  fReporter.ExportBackend := ebHtmlDiv;
    'img':
      case ARequest.QueryFields.Values['imgext'] of
        'bmp':  fReporter.ExportBackend := ebBmp;
        'png':  fReporter.ExportBackend := ebPng;
        'jpg':  fReporter.ExportBackend := ebJpg;
      end;
    'sheet':
      case ARequest.QueryFields.Values['shext'] of
        'xls':  fReporter.ExportBackend := ebXLS;
        'xlsx': fReporter.ExportBackend := ebOOXML;
        'odf':  fReporter.ExportBackend := ebOpenDoc;
      end;
    'text':     fReporter.ExportBackend := ebTxt;
    'csv':      fReporter.ExportBackend := ebCSV;
  end;
  if fReporter.ProcessExportReport(reportName) then begin
    if FileExists(fReporter.ExportedFile) then
      ServeFile(fReporter.ExportedFile, AResponse);
    Handled := true;
  end else begin
    AResponse.ContentType := 'text/html';
    AResponse.Content := '<html><body>Error exporting report "'+reportName+'"</body></html>';
  end;
end;

initialization
  RegisterHTTPModule('web', TWebDM);
end.

