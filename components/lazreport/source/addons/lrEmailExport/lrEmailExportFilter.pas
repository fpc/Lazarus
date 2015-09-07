unit lrEmailExportFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,
  // LazUtils
  LazFileUtils, LazUTF8, UTF8Process,
  // LazReport
  LR_Class, lr_PreviewToolsAbstract;

type
  TEmailApp = class;

  { TlrEmailExportFilter }

  TlrEmailExportFilter = class(TlrPreviewToolsAbstract)
  private
    FAttachmentFormat: string;
    FBody: TStrings;
    FDefEmailApp: string;
    FEmail: string;
    FEmailList: TStrings;
    FFromAdress: string;
    FFromName: string;
    FSubject: string;
    FEmailAppName:string;
    FEmailAttachFileName:string;
    procedure SetBody(AValue: TStrings);
    procedure SetEmailList(AValue: TStrings);
    function DoExport(FEmailApp:TEmailApp):boolean;
  protected
    function ProcessSetup:boolean; override;
    function ProcessTool:boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property EmailAttachFileName:string read FEmailAttachFileName;
  published
    property AttachmentFormat: string read FAttachmentFormat write FAttachmentFormat;
    property EmailList:TStrings read FEmailList write SetEmailList;
    property Email:string read FEmail write FEmail;
    property MessageSubject:string read FSubject write FSubject;
    property MessageBody:TStrings read FBody write SetBody;
    property FromName:string read FFromName write FFromName;
    property FromAdress:string read FFromAdress write FFromAdress;
    property DefEmailApp:string read FDefEmailApp write FDefEmailApp;
  end;

  { TEmailApp }

  TEmailApp = class
  private
    FProcess:TProcessUTF8;
  protected
    ///FAttachFileName:string;
    FFilter:TlrEmailExportFilter;
    class function AppFileName:string;virtual;abstract;
    function MakeParams:string;virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure MakeEmail(AFilter:TlrEmailExportFilter);virtual;
    class function AppName:string;virtual;abstract;
    class function AppValid:boolean;virtual;
  end;

  TEmailAppClass = class of TEmailApp;


procedure Register;
procedure RegisterEmailApp(AppClass:TEmailAppClass);

resourcestring
  sSendToEmail       = 'Send to email...';
  sMessage           = 'Message';
  sAttachment        = 'Attachment';
  sAdress            = 'Adress';
  sSubject           = 'Subject';
  sText              = 'Text';
  sFormat            = 'Format';
  sApplicationName   = 'Application name';
  sUseMailProgram    = 'Use mail program';
  sDirectSend        = 'Direct send';
  sFromName          = 'From name';
  sFromAdress        = 'From adress';
  sHost              = 'Host';
  sPort              = 'Port';
  sLogin             = 'Login';
  sPassword          = 'Password';
  sEmail             = 'Email';
  sAccount           = 'Account';


implementation
uses lrEmailExportFilterSetup, lrEmailAppFreeSoft
  {$IFDEF WINDOWS}
    , lrEmailAppTheBat, lrEmailAppMS
  {$ENDIF}
  ;

procedure Register;
begin
  RegisterComponents('LazReport',[TlrEmailExportFilter]);
end;

var
  EmailAppArray:array of TEmailAppClass = nil;

procedure RegisterEmailApp(AppClass: TEmailAppClass);
var
  C:integer;
  i: Integer;
begin
  C:=Length(EmailAppArray);
  for i:=0 to C-1 do
    if EmailAppArray[i] = AppClass then
      exit;
  SetLength(EmailAppArray, C+1);
  EmailAppArray[C]:=AppClass;
end;

{$R lremailexport.res}


{ TEmailApp }

function TEmailApp.MakeParams: string;
begin
  Result:='';
end;

constructor TEmailApp.Create;
begin
  inherited Create;
  FProcess:=TProcessUTF8.Create(nil);
end;

destructor TEmailApp.Destroy;
begin
  FreeAndNil(FProcess);
  inherited Destroy;
end;

procedure TEmailApp.MakeEmail(AFilter: TlrEmailExportFilter);
begin
  FFilter:=AFilter;
  FProcess.CommandLine:=AppFileName+' '+MakeParams;
  FProcess.Execute;
end;

class function TEmailApp.AppValid: boolean;
begin
  Result:=FileExistsUTF8(AppFileName);
end;

{ TlrEmailExportFilter }

procedure TlrEmailExportFilter.SetEmailList(AValue: TStrings);
begin
  FEmailList.Assign(AValue);
end;

function TlrEmailExportFilter.DoExport(FEmailApp: TEmailApp): boolean;
begin
  Result:=false;
  if not Assigned(FEmailApp) then exit;
  try
    FEmailApp.MakeEmail(Self);
    Result:=true;
  finally
    FreeAndNil(FEmailApp);
  end;
end;

procedure TlrEmailExportFilter.SetBody(AValue: TStrings);
begin
  FBody.Assign(AValue);
end;

function TlrEmailExportFilter.ProcessSetup: boolean;
var
  lrEmailExportFilterSetupForm: TlrEmailExportFilterSetupForm;
  i: Integer;
begin
  Result:=inherited ProcessSetup;
  if Result then
  begin
    lrEmailExportFilterSetupForm:=TlrEmailExportFilterSetupForm.CreateSetupForm(Self);

    lrEmailExportFilterSetupForm.cbAppName.Items.Clear;
    for i:=0 to Length(EmailAppArray)-1 do
      if EmailAppArray[i].AppValid then
        lrEmailExportFilterSetupForm.cbAppName.Items.Add(EmailAppArray[i].AppName);

    lrEmailExportFilterSetupForm.SetDefaultEmailApp;

    if lrEmailExportFilterSetupForm.ShowModal = mrOK then
    begin
      AttachmentFormat:=lrEmailExportFilterSetupForm.cbFilterList.Text;
      FEmailAppName:=lrEmailExportFilterSetupForm.cbAppName.Text;
      FEmail:=lrEmailExportFilterSetupForm.cbEmailList.Text;
      FSubject:=lrEmailExportFilterSetupForm.cbSubject.Text;
      MessageBody.Text:=lrEmailExportFilterSetupForm.edtBoby.Text;
    end
    else
      Result:=false;

    lrEmailExportFilterSetupForm.Free;
  end;
end;

function TlrEmailExportFilter.ProcessTool: boolean;
var
  FilterClass: TfrExportFilterClass;
  SExt:string;
  i: Integer;
  FEmailApp:TEmailApp;
begin
  FilterClass:=nil;
  for i:=0 to ExportFilters.Count - 1 do
    if (ExportFilters[i].FilterDesc = AttachmentFormat) then
    begin
      FilterClass := ExportFilters[i].ClassRef;
      SExt:=ExtractFileExt(ExportFilters[i].FilterExt);
      break;
    end;
  if not Assigned(FilterClass) then exit;
  FEmailAttachFileName:=SysToUTF8(GetTempDir(false))+'Export'+SExt;
  FDoc.ExportTo(FilterClass, FEmailAttachFileName);

  for i:=0 to Length(EmailAppArray)-1 do
    if EmailAppArray[i].AppName = FEmailAppName then
    begin
      FEmailApp:=EmailAppArray[i].Create;
      break;
    end;

  Result:=DoExport(FEmailApp);
end;

constructor TlrEmailExportFilter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEmailList:=TStringList.Create;
  FBody:=TStringList.Create;
  FCaption:='Send by email';
end;

destructor TlrEmailExportFilter.Destroy;
begin
  FreeAndNil(FBody);
  FreeAndNil(FEmailList);
  inherited Destroy;
end;

finalization
  SetLength(EmailAppArray, 0);
end.

