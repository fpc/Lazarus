{$mode objfpc}
{$h+}
unit pkglnet;

interface

uses
  SysUtils, Classes,
  uriparser,
  llnet, llftp, llhttp, pkgdownload,pkgoptions, fprepos;

Type

  { TLNetDownloader }

  TLNetDownloader = Class(TBaseDownloader)
   private
    FQuit: Boolean;
    FFTP: TLFTPClient;
    FHTTP: TLHTTPClient;
    FOutStream: TStream;
    URI: TURI;
   protected
    // callbacks
    function OnHttpClientInput(ASocket: TLHTTPClientSocket; ABuffer: pchar;
      ASize: Integer): Integer;
    procedure OnLNetDisconnect(aSocket: TLSocket);
    procedure OnHttpDoneInput(aSocket: TLHTTPClientSocket);
    procedure OnLNetError(const msg: string; aSocket: TLSocket);
    procedure OnFTPControl(aSocket: TLSocket);
    procedure OnFTPReceive(aSocket: TLSocket);
    procedure OnFTPSuccess(aSocket: TLSocket; const aStatus: TLFTPStatus);
    procedure OnFTPFailure(aSocket: TLSocket; const aStatus: TLFTPStatus);
    // overrides
    procedure FTPDownload(Const URL : String; Dest : TStream); override;
    procedure HTTPDownload(Const URL : String; Dest : TStream); override;
   public
    constructor Create(AOwner : TComponent); override;
  end;

implementation

uses
  pkgglobals,
  pkgmessages;

{ TLNetDownloader }

function TLNetDownloader.OnHttpClientInput(ASocket: TLHTTPClientSocket;
  ABuffer: pchar; ASize: Integer): Integer;
begin
  Result:=FOutStream.Write(aBuffer[0], aSize);
end;

procedure TLNetDownloader.OnLNetDisconnect(aSocket: TLSocket);
begin
  FQuit:=True;
end;

procedure TLNetDownloader.OnHttpDoneInput(aSocket: TLHTTPClientSocket);
begin
  ASocket.Disconnect;
  FQuit:=True;
end;

procedure TLNetDownloader.OnLNetError(const msg: string; aSocket: TLSocket);
begin
  Error(msg);
  FQuit:=True;
end;

procedure TLNetDownloader.OnFTPControl(aSocket: TLSocket);
var
  s: string;
begin
  FFTP.GetMessage(s); // have to empty OS buffer, write the info if you wish to debug
end;

procedure TLNetDownloader.OnFTPReceive(aSocket: TLSocket);
const
  BUF_SIZE = 65536; // standard OS recv buffer size
var
  Buf: array[1..BUF_SIZE] of Byte;
begin
  FOutStream.Write(Buf[1], FFTP.GetData(Buf[1], BUF_SIZE));
end;

procedure TLNetDownloader.OnFTPSuccess(aSocket: TLSocket;
  const aStatus: TLFTPStatus);
begin
  FFTP.Disconnect;
  FQuit:=True;
end;

procedure TLNetDownloader.OnFTPFailure(aSocket: TLSocket;
  const aStatus: TLFTPStatus);
begin
  FFTP.Disconnect;
  Error(SErrDownloadFailed,['FTP',EncodeURI(URI),'']);
  FQuit:=True;
end;

procedure TLNetDownloader.FTPDownload(const URL: String; Dest: TStream);
begin
  FOutStream:=Dest;
  Try
    { parse URL }
    URI:=ParseURI(URL);

    if URI.Port = 0 then
      URI.Port := 21;

    FFTP.Connect(URI.Host, URI.Port);
    while not FFTP.Connected and not FQuit do
      FFTP.CallAction;

    if not FQuit then begin
      FFTP.Authenticate(URI.Username, URI.Password);
      FFTP.ChangeDirectory(URI.Path);
      FFTP.Retrieve(URI.Document);
      while not FQuit do
        FFTP.CallAction;
    end;
  finally
    FOutStream:=nil;
  end;
end;

procedure TLNetDownloader.HTTPDownload(const URL: String; Dest: TStream);
begin
  FOutStream:=Dest;
  Try
    { parse aURL }
    URI := ParseURI(URL);

    if URI.Port = 0 then
      URI.Port := 80;

    FHTTP.Host := URI.Host;
    FHTTP.Method := hmGet;
    FHTTP.Port := URI.Port;
    FHTTP.URI := URI.Path + URI.Document;
    FHTTP.SendRequest;

    FQuit:=False;
    while not FQuit do
      FHTTP.CallAction;
    if FHTTP.Response.Status<>HSOK then
      Error(SErrDownloadFailed,['HTTP',EncodeURI(URI),FHTTP.Response.Reason]);
  Finally
    FOutStream:=nil; // to be sure
  end;
end;

constructor TLNetDownloader.Create(AOwner: TComponent);
begin
  inherited;

  FFTP:=TLFTPClient.Create(Self);
  FFTP.Timeout:=1000;
  FFTP.StatusSet:=[fsRetr]; // watch for success/failure of retreives only
  FFTP.OnError:=@OnLNetError;
  FFTP.OnControl:=@OnFTPControl;
  FFTP.OnReceive:=@OnFTPReceive;
  FFTP.OnSuccess:=@OnFTPSuccess;
  FFTP.OnFailure:=@OnFTPFailure;

  FHTTP:=TLHTTPClient.Create(Self);
  FHTTP.Timeout := 1000; // go by 1s times if nothing happens
  FHTTP.OnDisconnect := @OnLNetDisconnect;
  FHTTP.OnDoneInput := @OnHttpDoneInput;
  FHTTP.OnError := @OnLNetError;
  FHTTP.OnInput := @OnHttpClientInput;
end;

initialization
  RegisterDownloader('lnet',TLNetDownloader);
end.
