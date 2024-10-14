{
  Dialog for checking, downloading, updating pas2js

  Working:
    - changing $(Pas2js) and $(Pas2jsSrcDir)
    - downloading current zip and unzip
    - set compileserver.exe in simplewebservergui

  ToDo:
    - download pas2js via https checking cert
    - download zip: delete old files
}
unit FrmPas2jsInstaller;

{$mode ObjFPC}{$H+}

{$IF FPC_FULLVERSION>30300}
  {off $DEFINE HasSSL}
{$ENDIF}

interface

uses
  Classes, SysUtils, fphttpclient, Zipper,
  opensslsockets, // opensslsockets is needed for https download on windows
  LazFileUtils, FPCAdds, LazLoggerBase, FileUtil,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  IDEUtils, IDEDialogs,
  SimpleWebSrvController,
  StrPas2JSDesign, PJSDsgnOptions, PJSController, FrmPas2jsProgressDlg;

type

  { TPas2jsDownloadReleaseThread }

  TPas2jsDownloadReleaseThread = class(TThread)
  private
    FHttpClient: TFPHTTPClient;
    procedure OnWorkerHeaders(Sender: TObject); // in worker thread
    procedure OnWorkerProgress(Sender: TObject; const aContentLength, aCurrentPos: Int64); // in worker thread
    procedure OnWorkerShowRedirect(Sender: TObject; const ASrc: String; // in worker thread
      var ADest: String);
    procedure OnSyncProgress; // in main thread
    procedure OnSyncFinish; // in main thread
    {$IFDEF HasSSL}
    procedure DoHaveSocketHandler(Sender: TObject; AHandler: TSocketHandler);
    procedure DoVerifyCertificate(Sender: TObject; AHandler: TSSLSocketHandler; var aAllow: Boolean);
    {$ENDIF}
  public
    URL: String;
    Stream: TMemoryStream;
    ContentLength, CurrentPos: Int64;
    OnProgress: TNotifyEvent;
    OnFinish: TNotifyEvent;
    ErrorMsg: string;
    procedure Execute; override; // in worker thread
    destructor Destroy; override;
    property HttpClient: TFPHTTPClient read FHttpClient;
  end;

  { TPas2jsInstallerDialog }

  TPas2jsInstallerDialog = class(TForm)
    ApplyButton: TButton;
    BtnPanel: TPanel;
    DownloadButton: TButton;
    FPCExeLabel: TLabel;
    FPCSrcDirBrowseButton: TButton;
    CloseButton: TButton;
    DetailsGroupBox: TGroupBox;
    DetailsMemo: TMemo;
    FPCExeBrowseButton: TButton;
    FPCExeComboBox: TComboBox;
    FPCSrcDirComboBox: TComboBox;
    FPCGroupBox: TGroupBox;
    FPCSrcDirLabel: TLabel;
    FPCSrcDirVersionLabel: TLabel;
    Pas2jsExeBrowseButton: TButton;
    Pas2jsExeComboBox: TComboBox;
    Pas2jsExeGroupBox: TGroupBox;
    Pas2jsSrcDirBrowseBtn: TButton;
    Pas2jsSrcDirComboBox: TComboBox;
    Pas2jsSrcDirGroupBox: TGroupBox;
    Pas2jsSrcVersionLabel: TLabel;
    procedure ApplyButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure DownloadReleaseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FPCExeBrowseButtonClick(Sender: TObject);
    procedure FPCSrcDirBrowseButtonClick(Sender: TObject);
    procedure Pas2jsExeBrowseButtonClick(Sender: TObject);
    procedure Pas2jsSrcDirBrowseBtnClick(Sender: TObject);
  private
    FHTTPClient: TFPHTTPClient;
    FFoundCompileserver: string;
    FFoundPas2jsCfg: string;
    FFoundPas2jsExe: string;
    FFoundSystemPas: string;
    FLastCheckedPas2js: boolean;
    FLastCheckedPas2jsExe: String;
    FLastCheckedPas2jsSrcDir: String;
    FOldFPCExe: string;
    FOldFPCSrcDir: string;
    FOldPas2jsExe: string;
    FOldPas2jsSrcDir: string;
    FReleaseURL: string;
    FZipStream: TMemoryStream;
    FDownloadReleaseThread: TPas2jsDownloadReleaseThread;
    procedure OnCloseUnzipStream(Sender: TObject; var AStream: TStream);
    procedure OnDownloadReleaseFinish(Sender: TObject);
    procedure OnOpenUnzipStream(Sender: TObject; var AStream: TStream);
    procedure OnProgressCancelClick(Sender: TObject);
    procedure OnStartDownloadRelease(Sender: TObject);
    procedure OnUnzipStartFile(Sender: TObject; const AFileName: String);
    procedure UpdateButtons;
    function NeedsApply: boolean;
    function CheckPas2js: boolean;
    procedure OnDownloadReleaseProgress(Sender: TObject);
    procedure UnzipRelease(aDirectory: String);
    procedure Apply;
    procedure CheckSimpleWebserver(SetServerIfEmpty: boolean);
    function ShowProgressDialog(aCaption, ANote: string; const OnExecute: TNotifyEvent): boolean;
    function DirectoryIsEmpty(aDir: string): boolean;
  protected
  public
    procedure Init;
    property ReleaseURL: string read FReleaseURL write FReleaseURL;
  end;

var
  Pas2jsInstallerDialog: TPas2jsInstallerDialog;

function ShowPas2jsInstallerDialog: boolean; // returns true if pas2js looks ok and user did not cancel

implementation

function ShowPas2jsInstallerDialog: boolean;
begin
  Pas2jsInstallerDialog:=TPas2jsInstallerDialog.Create(nil);
  try
    Pas2jsInstallerDialog.Init;
    Result:=Pas2jsInstallerDialog.ShowModal=mrOk;
  finally
    Pas2jsInstallerDialog.Free;
  end;
end;

{$R *.lfm}

{ TPas2jsDownloadReleaseThread }

procedure TPas2jsDownloadReleaseThread.OnWorkerShowRedirect(Sender: TObject;
  const ASrc: String; var ADest: String);
begin
  Debugln('TPas2jsDownloadReleaseThread.ShowRedirect Following redirect from '+ASrc+'  ==> '+ADest);
end;

procedure TPas2jsDownloadReleaseThread.OnSyncProgress();
begin
  if Assigned(OnProgress) then
    OnProgress(Self);
end;

procedure TPas2jsDownloadReleaseThread.OnSyncFinish;
begin
  if Assigned(OnFinish) then
    OnFinish(Self);
end;

procedure TPas2jsDownloadReleaseThread.OnWorkerHeaders(Sender: TObject);
//Var
//  I : Integer;
begin
  //debugln('TPas2jsInstallerDialog.DoHeaders Response headers received:');
  //With (Sender as TFPHTTPClient) do
  //  For I:=0 to ResponseHeaders.Count-1 do
  //    debugln('TPas2jsInstallerDialog.DoHeaders '+ResponseHeaders[i]);
end;

procedure TPas2jsDownloadReleaseThread.OnWorkerProgress(Sender: TObject;
  const aContentLength, aCurrentPos: Int64);
begin
  ContentLength:=aContentLength;
  CurrentPos:=aCurrentPos;
  Synchronize(@OnSyncProgress);
end;

{$IFDEF HasSSL}
procedure TPas2jsDownloadReleaseThread.DoHaveSocketHandler(Sender: TObject;
  AHandler: TSocketHandler);
Var
  SSLHandler: TSSLSocketHandler absolute aHandler;
begin
  if aHandler is TSSLSocketHandler then
  begin
    SSLHandler.CertificateData.TrustedCertsDir:='/etc/ssl/certs/';
  end;
end;

procedure TPas2jsDownloadReleaseThread.DoVerifyCertificate(Sender: TObject;
  AHandler: TSSLSocketHandler; var aAllow: Boolean);
Var
  S : String;
begin
  debugln('TPas2jsDownloadReleaseThread.DoVerifyCertificateSSL Certificate verification requested, allowing');
  S:=TEncoding.ASCII.GetAnsiString( aHandler.CertificateData.Certificate.Value);
  debugln('TPas2jsDownloadReleaseThread.DoVerifyCertificate Cert: '+S);
  aAllow:=True;
end;
{$ENDIF}

procedure TPas2jsDownloadReleaseThread.Execute;
begin
  FHTTPClient:=TFPHTTPClient.Create(Nil);
  FHTTPClient.AllowRedirect:=True;
  FHTTPClient.OnRedirect:=@OnWorkerShowRedirect;
  FHTTPClient.OnDataReceived:=@OnWorkerProgress;
  FHTTPClient.OnHeaders:=@OnWorkerHeaders;
  FHTTPClient.IOTimeout:=30000;
  FHTTPClient.ConnectTimeout:=60000;
  {$IFDEF HasSSL}
  Client.VerifySSlCertificate:=True;
  Client.OnVerifySSLCertificate:=@DoVerifyCertificate;
  Client.AfterSocketHandlerCreate:=@DoHaveSocketHandler;
  {$ENDIF}

  try
    HttpClient.Get(URL,Stream);
  except
    on E: Exception do
      ErrorMsg:=E.Message;
  end;
  if not Terminated then
    Synchronize(@OnSyncFinish);
end;

destructor TPas2jsDownloadReleaseThread.Destroy;
begin
  FHttpClient.Free;
  inherited Destroy;
end;

{ TPas2jsInstallerDialog }

procedure TPas2jsInstallerDialog.FormCreate(Sender: TObject);
begin
  Caption:=pjsdPas2jsInstaller;

  Pas2jsExeGroupBox.Caption:=pjsdPas2jsExecutable;
  Pas2jsExeBrowseButton.Hint:=pjsdBrowse;

  Pas2jsSrcDirGroupBox.Caption:=pjsdPas2jsSourceDirectory;
  Pas2jsSrcDirBrowseBtn.Hint:=pjsdBrowse;

  FPCGroupBox.Caption:=pjsdFreePascalCompilerUsedForCompilingToolsAndPas2jsIt;
  FPCExeLabel.Caption:=pjsdFPCExecutable;
  FPCExeBrowseButton.Hint:=pjsdBrowse;
  FPCSrcDirLabel.Caption:=pjsdFPCSourceDirectory;
  FPCSrcDirBrowseButton.Hint:=pjsdBrowse;

  DetailsGroupBox.Caption:=pjsdDetails;
  DetailsMemo.Clear;

  DownloadButton.Caption:=pjsdDownloadRelease;
  ApplyButton.Caption:=pjsdApply;
  CloseButton.Caption:=pjsdClose;
end;

procedure TPas2jsInstallerDialog.FPCExeBrowseButtonClick(Sender: TObject);
var
  aDialog: TOpenDialog;
  AFilename: String;
begin
  aDialog:=TOpenDialog.Create(nil);
  try
    //InputHistories.ApplyFileDialogSettings(aDialog);
    aDialog.Options:=aDialog.Options+[ofPathMustExist];
    aDialog.Title:=pjsdSelectFreePascalCompilerExecutable;
    if not aDialog.Execute then exit;
    AFilename:=CleanAndExpandFilename(aDialog.Filename);
    SetComboBoxText(FPCExeComboBox,AFilename,cstFilename,30);
    // ToDo CheckCompiler([mbOk]);
    UpdateButtons;
  finally
    aDialog.Free;
  end;
end;

procedure TPas2jsInstallerDialog.FPCSrcDirBrowseButtonClick(Sender: TObject);
var
  aDialog: TSelectDirectoryDialog;
  AFilename: String;
begin
  aDialog:=TSelectDirectoryDialog.Create(nil);
  try
    //InputHistories.ApplyFileDialogSettings(aDialog);
    aDialog.Options:=aDialog.Options+[ofPathMustExist];
    aDialog.Title:=pjsdSelectFreePascalSourceDirectory;
    if not aDialog.Execute then exit;
    AFilename:=CleanAndExpandDirectory(aDialog.Filename);
    SetComboBoxText(FPCSrcDirComboBox,AFilename,cstFilename,30);
    // ToDo CheckCompiler([mbOk]);
    UpdateButtons;
  finally
    aDialog.Free;
  end;
end;

procedure TPas2jsInstallerDialog.CloseButtonClick(Sender: TObject);
begin
  // restore options
  PJSOptions.CompilerFilename:=FOldPas2jsExe;
  PJSOptions.Pas2jsSrcDir:=FOldPas2jsSrcDir;
  PJSOptions.FPCExe:=FOldFPCExe;
  PJSOptions.FPCSrcDir:=FOldFPCSrcDir;

  if NeedsApply then
    ModalResult:=mrCancel
  else if CheckPas2js then
    ModalResult:=mrOk
  else
    ModalResult:=mrCancel;
end;

procedure TPas2jsInstallerDialog.DownloadReleaseButtonClick(Sender: TObject);
var
  aDialog: TSelectDirectoryDialog;
  aDir, s, WebSrvExe: String;
begin
  DetailsMemo.Clear;

  // check if there is an URL
  if ReleaseURL='' then
  begin
    s:=Format(pjsdThereIsNoReleaseForTarget, [GetCompiledTargetCPU,
      GetCompiledTargetOS]);
    DetailsMemo.Lines.Add(Format(pjsdError2, [s]));
    IDEMessageDialog(pjsdError, s, mtError, [mbOk, mbCancel]);
    exit;
  end;

  // confirm download
  if IDEMessageDialog(pjsdConfirmation, pjsdDownloadPas2jsRelease,
    mtConfirmation, [mbOk, mbCancel])<>mrOk then
    exit;

  // select target directory
  aDialog:=TSelectDirectoryDialog.Create(nil);
  FHTTPClient:=nil;
  try
    //InputHistories.ApplyFileDialogSettings(aDialog);
    //aDialog.Options:=aDialog.Options+[ofPathMustExist];
    aDir:=CleanAndExpandDirectory(Pas2jsSrcDirComboBox.Text);
    if (not DirectoryExists(aDir)) or (not DirectoryIsEmpty(aDir)) then
    begin
      // ask for a directory
      aDialog.Title:=pjsdSelectDirectoryWhereToExtractPas2js;
      if aDir<>'' then
        aDialog.InitialDir:=aDir;
      if not aDialog.Execute then exit;
      aDir:=CleanAndExpandDirectory(aDialog.Filename);
      if not DirectoryExists(aDir) then
      begin
        if not ForceDirectoriesUTF8(aDir) then
        begin
          s:=Format(pjsdUnableToCreateDirectory, [aDir]);
          DetailsMemo.Lines.Add(Format(pjsdError2, [s]));
          IDEMessageDialog(pjsdError, s, mtError, [mbOk]);
          exit;
        end;
      end;

      // set Pas2jsSrcDir
      SetComboBoxText(Pas2jsSrcDirComboBox,aDir,cstFilename,30);
    end;

    // download
    s:=Format(pjsdDownloading, [ReleaseURL]);
    DetailsMemo.Lines.Add(Format(pjsdNote, [s]));
    DebugLn(['Note: TPas2jsInstallerDialog.DownloadReleaseButtonClick ',s]);
    FZipStream:=TMemoryStream.Create;

    if not ShowProgressDialog(pjsdDownloading2, ReleaseURL, @OnStartDownloadRelease) then
      exit;

    s:=Format(pjsdDownloadedBytes, [IntToStr(FZipStream.Size)]);
    DetailsMemo.Lines.Add(Format(pjsdNote, [s]));
    debugln(['Note: TPas2jsInstallerDialog.DownloadReleaseButtonClick ',s]);

    // unzip
    UnzipRelease(aDir);

    // set Pas2js compile exe
    if FFoundPas2jsExe='' then
    begin
      IDEMessageDialog(pjsdError, pjsdMissing+' pas2js'+GetExeExt, mtError, [mbOk]);
      exit;
    end;
    SetComboBoxText(Pas2jsExeComboBox,FFoundPas2jsExe,cstFilename,30);

    if FFoundPas2jsCfg='' then
    begin
      IDEMessageDialog(pjsdError, pjsdMissing+' pas2js.cfg', mtError, [mbOk]);
      exit;
    end;
    if FFoundSystemPas='' then
    begin
      IDEMessageDialog(pjsdError, pjsdMissing+' system.pas', mtError, [mbOk]);
      exit;
    end;
    if FFoundCompileserver='' then
    begin
      IDEMessageDialog(pjsdError, pjsdMissing+' compileserver'+GetExeExt,mtError, [mbOk]);
      exit;
    end;

    Apply;

    // set simple web server
    WebSrvExe:=SimpleWebServerController.GetDefaultServerExe;
    if (FFoundCompileserver<>'') and (CompareFilenames(WebSrvExe,FFoundCompileserver)<>0) then
    begin
      if IDEMessageDialog(pjsdConfirmation, Format(
        pjsdChangeSimpleWebServerFromTo, [sLineBreak, WebSrvExe, sLineBreak,
        sLineBreak, FFoundCompileserver, sLineBreak]), mtConfirmation, [mbYes,
        mbNo])=mrYes then
      begin
        SimpleWebServerController.Options.ServerExe:=FFoundCompileserver;
        SimpleWebServerController.Options.SaveSafe;
      end;
    end;

  finally
    aDialog.Free;
    FreeAndNil(FZipStream);
    FHTTPClient.Free;
    UpdateButtons;
  end;
end;

procedure TPas2jsInstallerDialog.ApplyButtonClick(Sender: TObject);
begin
  Apply;
end;

procedure TPas2jsInstallerDialog.Pas2jsExeBrowseButtonClick(Sender: TObject);
var
  aDialog: TOpenDialog;
  AFilename: String;
begin
  aDialog:=TOpenDialog.Create(nil);
  try
    //InputHistories.ApplyFileDialogSettings(aDialog);
    aDialog.Options:=aDialog.Options+[ofPathMustExist];
    aDialog.Title:=pjsdSelectPas2jsExecutable;
    if not aDialog.Execute then exit;
    AFilename:=CleanAndExpandFilename(aDialog.Filename);
    SetComboBoxText(Pas2jsExeComboBox,AFilename,cstFilename,30);
    // ToDo CheckCompiler([mbOk]);
    UpdateButtons;
  finally
    aDialog.Free;
  end;
end;

procedure TPas2jsInstallerDialog.Pas2jsSrcDirBrowseBtnClick(Sender: TObject);
var
  aDialog: TSelectDirectoryDialog;
  AFilename: String;
begin
  aDialog:=TSelectDirectoryDialog.Create(nil);
  try
    //InputHistories.ApplyFileDialogSettings(aDialog);
    aDialog.Options:=aDialog.Options+[ofPathMustExist];
    aDialog.Title:=pjsdSelectPas2jsSourceDirectory;
    if not aDialog.Execute then exit;
    AFilename:=CleanAndExpandDirectory(aDialog.Filename);
    SetComboBoxText(Pas2jsSrcDirComboBox,AFilename,cstFilename,30);
    // ToDo CheckCompiler([mbOk]);
    UpdateButtons;
  finally
    aDialog.Free;
  end;
end;

procedure TPas2jsInstallerDialog.UpdateButtons;
begin
  if NeedsApply then
  begin
    ApplyButton.Enabled:=true;
    CloseButton.Caption:=pjsdCancel;
  end else begin
    ApplyButton.Enabled:=false;
    CloseButton.Caption:=pjsdClose;
  end;
end;

procedure TPas2jsInstallerDialog.OnOpenUnzipStream(Sender: TObject;
  var AStream: TStream);
begin
  AStream:=FZipStream;
end;

procedure TPas2jsInstallerDialog.OnProgressCancelClick(Sender: TObject);
begin
  debugln(['TPas2jsInstallerDialog.OnProgressCancelClick']);
  if FHTTPClient<>nil then
    FHTTPClient.Terminate;
end;

procedure TPas2jsInstallerDialog.OnStartDownloadRelease(Sender: TObject);
// called when progress dialog is shown
begin
  FDownloadReleaseThread:=TPas2jsDownloadReleaseThread.Create(true);
  FDownloadReleaseThread.FreeOnTerminate:=false;
  FDownloadReleaseThread.URL:=ReleaseURL;
  FDownloadReleaseThread.Stream:=FZipStream;
  FDownloadReleaseThread.OnProgress:=@OnDownloadReleaseProgress;
  FDownloadReleaseThread.OnFinish:=@OnDownloadReleaseFinish;
  FDownloadReleaseThread.Start;
end;

procedure TPas2jsInstallerDialog.OnCloseUnzipStream(Sender: TObject;
  var AStream: TStream);
begin
  if AStream=FZipStream then
    FZipStream:=nil;
end;

procedure TPas2jsInstallerDialog.OnDownloadReleaseFinish(Sender: TObject);
begin
  debugln(['TPas2jsInstallerDialog.OnDownloadReleaseFinish ']);
  if Pas2jsProgressDialog.ModalResult<>mrNone then exit;

  if FDownloadReleaseThread.ErrorMsg<>'' then
  begin
    Pas2jsProgressDialog.ModalResult:=mrCancel;
    DetailsMemo.Lines.Add(Format(pjsdError2, [FDownloadReleaseThread.ErrorMsg]) );
    IDEMessageDialog(pjsdError, Format(pjsdDownloadError, [sLineBreak+
      FDownloadReleaseThread.ErrorMsg]), mtError, [mbOk]);
  end else begin
    Pas2jsProgressDialog.ModalResult:=mrOk;
  end;
end;

procedure TPas2jsInstallerDialog.OnUnzipStartFile(Sender: TObject;
  const AFileName: String);
var
  ShortFilename: String;
begin
  //debugln(['TPas2jsInstallerDialog.OnUnzipStartFile ',AFileName,' ...']);
  ShortFilename:=ExtractFileName(AFileName);
  if ShortFilename='pas2js'+GetExeExt then
    FFoundPas2jsExe:=AFileName
  else if ShortFilename='pas2js.cfg' then
    FFoundPas2jsCfg:=AFileName
  else if ShortFilename='compileserver'+GetExeExt then
    FFoundCompileserver:=AFileName
  else if ShortFilename='system.pas' then
    FFoundSystemPas:=AFilename;
end;

function TPas2jsInstallerDialog.NeedsApply: boolean;
var
  CurPas2jsExe, CurPas2jsSrcDir, CurFPCExe, CurFPCSrcDir: TCaption;
begin
  CurPas2jsExe:=Pas2jsExeComboBox.Text;
  CurPas2jsSrcDir:=Pas2jsSrcDirComboBox.Text;
  CurFPCExe:=FPCExeComboBox.Text;
  CurFPCSrcDir:=FPCSrcDirComboBox.Text;
  Result:=(CurPas2jsExe<>FOldPas2jsExe)
      or (CurPas2jsSrcDir<>FOldPas2jsSrcDir)
      or (CurFPCExe<>FOldFPCExe)
      or (CurFPCSrcDir<>FOldFPCSrcDir);
end;

function TPas2jsInstallerDialog.CheckPas2js: boolean;
var
  NewPas2jsExe, NewPas2jsSrcDir: String;
begin
  NewPas2jsExe:=PJSOptions.GetParsedCompilerFilename;
  NewPas2jsSrcDir:=PJSOptions.GetParsedPas2jsSrcDir;
  if (NewPas2jsExe<>FLastCheckedPas2jsExe)
      or (NewPas2jsSrcDir<>FLastCheckedPas2jsSrcDir) then
  begin
    FLastCheckedPas2js:=false;
    FLastCheckedPas2jsExe:=NewPas2jsExe;
    FLastCheckedPas2jsSrcDir:=NewPas2jsSrcDir;
    if (NewPas2jsExe='') or not FileExistsUTF8(NewPas2jsExe) then
    else if not FileIsExecutable(NewPas2jsExe) then
    else
      FLastCheckedPas2js:=true;
  end;
  Result:=FLastCheckedPas2js;
end;

procedure TPas2jsInstallerDialog.OnDownloadReleaseProgress(Sender: TObject);
var
  Bar: TProgressBar;
begin
  if Pas2jsProgressDialog=nil then exit;
  Bar:=Pas2jsProgressDialog.ProgressBar1;
  If (FDownloadReleaseThread.ContentLength=0) then
  begin
    //DebugLN(['TPas2jsInstallerDialog.DoProgress Reading headers : ',FDownloadReleaseThread.CurrentPos,' Bytes.']);
    Bar.Style:=pbstMarquee;
  end else If (FDownloadReleaseThread.ContentLength=-1) then
  begin
    //DebugLN(['TPas2jsInstallerDialog.DoProgress Reading data (no length available) : ',FDownloadReleaseThread.CurrentPos,' Bytes.']);
    Bar.Style:=pbstMarquee;
  end else begin
    //DebugLN(['TPas2jsInstallerDialog.DoProgress Reading data : ',FDownloadReleaseThread.CurrentPos,' Bytes of ',FDownloadReleaseThread.ContentLength]);
    Bar.Style:=pbstNormal;
    Bar.Max:=FDownloadReleaseThread.ContentLength;
    Bar.Position:=FDownloadReleaseThread.CurrentPos;
  end;
end;

procedure TPas2jsInstallerDialog.UnzipRelease(aDirectory: String);

  procedure Check(Title, Param: string);
  begin
    if Param<>'' then
    begin
      debugln(['Note: Found ',Title,': ',Param]);
      DetailsMemo.Lines.Add(pjsdNote2+': '+Format(pjsdFound, [Title])+': '+Param
        );
    end else begin
      debugln(['Error: Missing ',Title]);
      DetailsMemo.Lines.Add(pjsdError+': '+Format(pjsdMissing2, [Title]));
    end;
  end;

var
  Zip: TUnZipper;
begin
  FZipStream.Position:=0;
  FFoundPas2jsExe:='';
  FFoundPas2jsCfg:='';
  FFoundCompileserver:='';
  FFoundCompileserver:='';
  FFoundSystemPas:='';

  Zip:=TUnZipper.Create;
  try
    Zip.OnOpenInputStream:=@OnOpenUnzipStream;
    Zip.OnCloseInputStream:=@OnCloseUnzipStream;
    Zip.OutputPath:=aDirectory;
    Zip.OnStartFile:=@OnUnzipStartFile;
    Zip.UnZipAllFiles;
    debugln(['Note: TPas2jsInstallerDialog.UnzipRelease completed']);
    Check('exe',FFoundPas2jsExe);
    Check('cfg',FFoundPas2jsCfg);
    Check('compileserver',FFoundCompileserver);
    Check('system.pas',FFoundSystemPas);
  finally
    Zip.Free;
  end;
end;

procedure TPas2jsInstallerDialog.Apply;
var
  CurPas2jsExe, CurPas2jsSrcDir, CurFPCExe, CurFPCSrcDir: TCaption;
begin
  CurPas2jsExe:=Pas2jsExeComboBox.Text;
  CurPas2jsSrcDir:=Pas2jsSrcDirComboBox.Text;
  CurFPCExe:=FPCExeComboBox.Text;
  CurFPCSrcDir:=FPCSrcDirComboBox.Text;

  // todo: sanity check

  PJSOptions.CompilerFilename:=CurPas2jsExe;
  PJSOptions.Pas2jsSrcDir:=CurPas2jsSrcDir;
  PJSOptions.FPCExe:=CurFPCExe;
  PJSOptions.FPCSrcDir:=CurFPCSrcDir;

  FOldPas2jsExe:=PJSOptions.CompilerFilename;
  FOldPas2jsSrcDir:=PJSOptions.Pas2jsSrcDir;
  FOldFPCExe:=PJSOptions.FPCExe;
  FOldFPCSrcDir:=PJSOptions.FPCSrcDir;

  TPJSController.Instance.StoreMacros;
  If PJSOptions.Modified then
    PJSOptions.Save;

  UpdateButtons;
end;

procedure TPas2jsInstallerDialog.CheckSimpleWebserver(SetServerIfEmpty: boolean
  );
var
  WebSrvExe: String;
begin
  WebSrvExe:=SimpleWebServerController.GetDefaultServerExe;
  if (WebSrvExe<>'') and FileExists(WebSrvExe) then
    exit;
  if (FFoundCompileserver<>'') and SetServerIfEmpty then
  begin
    SimpleWebServerController.Options.ServerExe:=FFoundCompileserver;
    SimpleWebServerController.Options.SaveSafe;
  end;
end;

function TPas2jsInstallerDialog.ShowProgressDialog(aCaption, ANote: string;
  const OnExecute: TNotifyEvent): boolean;
var
  i: Integer;
begin
  Result:=false;
  if Pas2jsProgressDialog<>nil then
  begin
    debugln(['TPas2jsInstallerDialog.ShowProgressDialog Pas2jsProgressDialog<>nil']);
    exit;
  end;

  Pas2jsProgressDialog:=TPas2jsProgressDialog.Create(Self);
  Pas2jsProgressDialog.Caption:=aCaption;
  Pas2jsProgressDialog.NoteLabel.Caption:=ANote;
  Pas2jsProgressDialog.OnShow:=OnExecute;
  Result:=Pas2jsProgressDialog.ShowModal=mrOk;
  Pas2jsProgressDialog.Release;
  if FDownloadReleaseThread<>nil then
  begin
    FDownloadReleaseThread.Terminate;
    if FDownloadReleaseThread.HttpClient<>nil then
    begin
      FDownloadReleaseThread.HttpClient.Terminate;
    end;
    try
      for i:=1 to 1000 do
      begin
        if FDownloadReleaseThread.Finished then break;
        Sleep(10);
        Application.ProcessMessages;
      end;
    finally
      FDownloadReleaseThread.Free;
      FDownloadReleaseThread:=nil;
    end;
  end;
end;

function TPas2jsInstallerDialog.DirectoryIsEmpty(aDir: string): boolean;
var
  Info: TRawByteSearchRec;
begin
  aDir:=AppendPathDelim(aDir);
  if FindFirst(aDir+GetAllFilesMask,faAnyFile,Info)=0 then
  begin
    repeat
      case Info.Name of
      '','.','..': ;
      else
        Result:=false;
        break;
      end;
    until FindNext(Info)<>0;
  end;
  FindCloseUTF8(Info);
end;

procedure TPas2jsInstallerDialog.Init;
begin
  FOldPas2jsExe:=PJSOptions.CompilerFilename;
  FOldPas2jsSrcDir:=PJSOptions.Pas2jsSrcDir;
  FOldFPCExe:=PJSOptions.FPCExe;
  FOldFPCSrcDir:=PJSOptions.FPCSrcDir;

  SetComboBoxText(Pas2jsExeComboBox,PJSOptions.CompilerFilename,cstFilename,30);
  SetComboBoxText(Pas2jsSrcDirComboBox,PJSOptions.Pas2jsSrcDir,cstFilename,30);
  SetComboBoxText(FPCExeComboBox,PJSOptions.FPCExe,cstFilename,30);
  SetComboBoxText(FPCSrcDirComboBox,PJSOptions.FPCSrcDir,cstFilename,30);

  FReleaseURL:='https://getpas2js.freepascal.org/downloads/';
  {$IF defined(MSWindows)}
  FReleaseURL+='windows/pas2js-win64-x86_64-current.zip';
  {$ELSEIF defined(Darwin) and defined(CPU64)}
  FReleaseURL+='darwin/pas2js-darwin-x86_64-current.zip';
  {$ELSEIF defined(Darwin) and defined(CPUAarch64)}
  FReleaseURL+='darwin/pas2js-darwin-aarch64-current.zip';
  {$ELSEIF defined(Linux) and defined(CPU64)}
  FReleaseURL+='linux/pas2js-linux-x86_64-current.zip';
  {$ELSE}
  FReleaseURL:='';
  {$ENDIF}

  UpdateButtons;
end;

end.

