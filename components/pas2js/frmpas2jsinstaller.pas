{
  Dialog for checking, downloading, updating pas2js

  Working:
    - changing $(Pas2js) and $(Pas2jsSrcDir)
    - downloading current zip and unzip
    - set compileserver.exe in simplewebservergui

  ToDo:
    - download progress
    - download pas2js via https
    - download zip: delete old files
    - resourcestrings
}
unit FrmPas2jsInstaller;

{$mode ObjFPC}{$H+}

{$IF FPC_FULLVERSION>30300}
  {off $DEFINE HasSSL}
{$ENDIF}

interface

uses
  Classes, SysUtils, fphttpclient, ssockets, sslsockets, Zipper,
  LazFileUtils, FPCAdds, LazLoggerBase, FileUtil,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  IDEUtils, IDEDialogs,
  SimpleWebSrvController,
  StrPas2JSDesign, PJSDsgnOptions, PJSController;

type

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
    FLastCheckedPas2jsExe: String;
    FLastCheckedPas2jsSrcDir: String;
    FLastCheckedPas2js: boolean;
    FOldPas2jsExe: string;
    FOldPas2jsSrcDir: string;
    FOldFPCExe: string;
    FOldFPCSrcDir: string;
    FReleaseURL: string;
    FZipStream: TMemoryStream;
    FFoundPas2jsExe: string;
    FFoundPas2jsCfg: string;
    FFoundCompileserver: string;
    FFoundSystemPas: string;
    procedure OnCloseUnzipStream(Sender: TObject; var AStream: TStream);
    procedure OnOpenUnzipStream(Sender: TObject; var AStream: TStream);
    procedure OnUnzipStartFile(Sender: TObject; const AFileName: String);
    procedure UpdateButtons;
    function NeedsApply: boolean;
    function CheckPas2js: boolean;
    {$IFDEF HasSSL}
    procedure DoHaveSocketHandler(Sender: TObject; AHandler: TSocketHandler);
    procedure DoVerifyCertificate(Sender: TObject; AHandler: TSSLSocketHandler; var aAllow: Boolean);
    {$ENDIF}
    procedure DoProgress(Sender: TObject; Const ContentLength, CurrentPos: Int64);
    procedure DoHeaders(Sender: TObject);
    procedure ShowRedirect(Sender: TObject; Const ASrc: String; Var ADest: String);
    procedure UnzipRelease(aDirectory: String);
    procedure Apply;
    procedure CheckSimpleWebserver(SetServerIfEmpty: boolean);
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

{ TPas2jsInstallerDialog }

procedure TPas2jsInstallerDialog.FormCreate(Sender: TObject);
begin
  Caption:='Pas2js Installer';

  Pas2jsExeGroupBox.Caption:='Pas2js executable';
  Pas2jsExeBrowseButton.Hint:='Browse';

  Pas2jsSrcDirComboBox.Caption:='Pas2js source directory';
  Pas2jsSrcDirBrowseBtn.Hint:='Browse';

  FPCGroupBox.Caption:='Free Pascal Compiler used for compiling tools and pas2js itself';
  FPCExeLabel.Caption:='FPC executable:';
  FPCExeBrowseButton.Hint:='Browse';
  FPCSrcDirLabel.Caption:='FPC source directory:';
  FPCSrcDirBrowseButton.Hint:='Browse';

  DetailsGroupBox.Caption:='Details';
  DetailsMemo.Clear;

  DownloadButton.Caption:='Download Release';
  ApplyButton.Caption:='Apply';
  CloseButton.Caption:='Close';
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
    aDialog.Title:='Select Free Pascal Compiler executable';
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
    aDialog.Title:='Select Free Pascal source directory';
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
  Client: TFPHTTPClient;
begin
  DetailsMemo.Clear;

  // check if there is an URL
  if ReleaseURL='' then
  begin
    s:='There is no release for target "'+GetCompiledTargetCPU+'-'+GetCompiledTargetOS+'".';
    DetailsMemo.Lines.Add('Error: '+s);
    IDEMessageDialog('Error',s,mtError,[mbOk,mbCancel]);
    exit;
  end;

  // confirm download
  if IDEMessageDialog('Confirmation','Download Pas2js Release?',mtConfirmation,[mbOk,mbCancel])<>mrOk then
    exit;

  // select target directory
  aDialog:=TSelectDirectoryDialog.Create(nil);
  Client:=nil;
  try
    //InputHistories.ApplyFileDialogSettings(aDialog);
    //aDialog.Options:=aDialog.Options+[ofPathMustExist];
    aDialog.Title:='Select directory where to extract Pas2js';
    if not aDialog.Execute then exit;
    aDir:=CleanAndExpandDirectory(aDialog.Filename);
    if not DirectoryExists(aDir) then
    begin
      if not ForceDirectoriesUTF8(aDir) then
      begin
        s:='Unable to create directory "'+aDir+'".';
        DetailsMemo.Lines.Add('Error: '+s);
        IDEMessageDialog('Error',s,mtError,[mbOk]);
        exit;
      end;
    end;

    // download
    Client:=TFPHTTPClient.Create(Nil);
    Client.AllowRedirect:=True;
    Client.OnRedirect:=@ShowRedirect;
    Client.OnDataReceived:=@DoProgress;
    Client.OnHeaders:=@DoHeaders;
    {$IF FPC_FULLVERSION>30300}
    Client.VerifySSlCertificate:=True;
    Client.OnVerifySSLCertificate:=@DoVerifyCertificate;
    Client.AfterSocketHandlerCreate:=@DoHaveSocketHandler;
    {$ENDIF}
    s:='Downloading "'+ReleaseURL+'" ...';
    DetailsMemo.Lines.Add('Note: '+s);
    DebugLn(['Note: TPas2jsInstallerDialog.DownloadReleaseButtonClick ',s]);
    FZipStream:=TMemoryStream.Create;
    Client.Get(ReleaseURL,FZipStream);
    s:='Downloaded '+IntToStr(FZipStream.Size)+' bytes';
    DetailsMemo.Lines.Add('Note: '+s);
    debugln(['Note: TPas2jsInstallerDialog.DownloadReleaseButtonClick ',s]);


    // ToDo: progress meter
    // ToDo: test timeout or wrong url

    // unzip
    UnzipRelease(aDir);

    // set Pas2jsSrcDir
    SetComboBoxText(Pas2jsSrcDirComboBox,aDir,cstFilename,30);

    // set Pas2js compile exe
    if FFoundPas2jsExe='' then
    begin
      IDEMessageDialog('Error','Missing pas2js'+GetExeExt,mtError,[mbOk]);
      exit;
    end;
    SetComboBoxText(Pas2jsExeComboBox,FFoundPas2jsExe,cstFilename,30);

    if FFoundPas2jsCfg='' then
    begin
      IDEMessageDialog('Error','Missing pas2js.cfg',mtError,[mbOk]);
      exit;
    end;
    if FFoundSystemPas='' then
    begin
      IDEMessageDialog('Error','Missing system.pas',mtError,[mbOk]);
      exit;
    end;
    if FFoundCompileserver='' then
    begin
      IDEMessageDialog('Error','Missing compileserver'+GetExeExt,mtError,[mbOk]);
      exit;
    end;

    Apply;

    // set simple web server
    WebSrvExe:=SimpleWebServerController.GetDefaultServerExe;
    if (FFoundCompileserver<>'') and (CompareFilenames(WebSrvExe,FFoundCompileserver)<>0) then
    begin
      if IDEMessageDialog('Confirmation','Change Simple Web Server from'+sLineBreak
        +WebSrvExe+sLineBreak
        +'to'+sLineBreak
        +FFoundCompileserver+sLineBreak
        +'?',mtConfirmation,[mbYes,mbNo])=mrYes then
      begin
        SimpleWebServerController.Options.ServerExe:=FFoundCompileserver;
        SimpleWebServerController.Options.SaveSafe;
      end;
    end;

  finally
    aDialog.Free;
    FreeAndNil(FZipStream);
    Client.Free;
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
    aDialog.Title:='Select pas2js source directory';
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
    CloseButton.Caption:='Cancel';
  end else begin
    ApplyButton.Enabled:=false;
    CloseButton.Caption:='Close';
  end;
end;

procedure TPas2jsInstallerDialog.OnOpenUnzipStream(Sender: TObject;
  var AStream: TStream);
begin
  AStream:=FZipStream;
end;

procedure TPas2jsInstallerDialog.OnCloseUnzipStream(Sender: TObject;
  var AStream: TStream);
begin
  if AStream=FZipStream then
    FZipStream:=nil;
end;

procedure TPas2jsInstallerDialog.OnUnzipStartFile(Sender: TObject;
  const AFileName: String);
var
  ShortFilename: String;
begin
  debugln(['TPas2jsInstallerDialog.OnUnzipStartFile ',AFileName,' ...']);
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

{$IFDEF HasSSL}
procedure TPas2jsInstallerDialog.DoHaveSocketHandler(Sender: TObject;
  AHandler: TSocketHandler);
Var
  SSLHandler: TSSLSocketHandler absolute aHandler;
begin
  if aHandler is TSSLSocketHandler then
  begin
    SSLHandler.CertificateData.TrustedCertsDir:='/etc/ssl/certs/';
  end;
end;

procedure TPas2jsInstallerDialog.DoVerifyCertificate(Sender: TObject;
  AHandler: TSSLSocketHandler; var aAllow: Boolean);
Var
  S : String;
begin
  debugln('TPas2jsInstallerDialog.DoVerifyCertificateSSL Certificate verification requested, allowing');
  S:=TEncoding.ASCII.GetAnsiString( aHandler.CertificateData.Certificate.Value);
  debugln('TPas2jsInstallerDialog.DoVerifyCertificate Cert: '+S);
  aAllow:=True;
end;
{$ENDIF}

procedure TPas2jsInstallerDialog.DoProgress(Sender: TObject;
  const ContentLength, CurrentPos: Int64);
begin
  exit;

  If (ContentLength=0) then
    DebugLN(['TPas2jsInstallerDialog.DoProgress Reading headers : ',CurrentPos,' Bytes.'])
  else If (ContentLength=-1) then
    DebugLN(['TPas2jsInstallerDialog.DoProgress Reading data (no length available) : ',CurrentPos,' Bytes.'])
  else
    DebugLN(['TPas2jsInstallerDialog.DoProgress Reading data : ',CurrentPos,' Bytes of ',ContentLength]);
end;

procedure TPas2jsInstallerDialog.DoHeaders(Sender: TObject);
Var
  I : Integer;
begin
  debugln('TPas2jsInstallerDialog.DoHeaders Response headers received:');
  With (Sender as TFPHTTPClient) do
    For I:=0 to ResponseHeaders.Count-1 do
      debugln('TPas2jsInstallerDialog.DoHeaders '+ResponseHeaders[i]);
end;

procedure TPas2jsInstallerDialog.ShowRedirect(Sender: TObject;
  const ASrc: String; var ADest: String);
begin
  Debugln(['TPas2jsInstallerDialog.ShowRedirect Following redirect from ',ASrc,'  ==> ',ADest]);
end;

procedure TPas2jsInstallerDialog.UnzipRelease(aDirectory: String);

  procedure Check(Title, Param: string);
  begin
    if Param<>'' then
    begin
      debugln(['Note: Found ',Title,': ',Param]);
      DetailsMemo.Lines.Add('Note: Found '+Title+': '+Param);
    end else begin
      debugln(['Error: Missing ',Title]);
      DetailsMemo.Lines.Add('Error: Missing '+Title);
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

  {$IFDEF HasSSL}
  FReleaseURL:='https';
  {$ELSE}
  FReleaseURL:='http';
  {$ENDIF}
  FReleaseURL+='://getpas2js.freepascal.org/downloads/';

  {$IF defined(MSWindows)}
  FReleaseURL+='windows/pas2js-win64-x86_64-current.zip';
  {$ELSEIF defined(Darwin) and defined(CPU64)}
  FReleaseURL+='darwin/pas2js-darwin-x86_64-current.zip';
  {$ELSEIF defined(Linux) and defined(CPU64)}
  FReleaseURL+='linux/pas2js-linux-x86_64-current.zip';
  {$ELSE}
  FReleaseURL:='';
  {$ENDIF}

  UpdateButtons;
end;

end.

