unit LNetHTTPDataProvider;

{$mode objfpc}{$H+}

interface

uses
  Forms, Classes, SysUtils, IpHtml, IpMsg, IpUtils, lnetcomponents, Graphics, lhttp;
  
  type
  
  { TIpHTTPDataProvider }

  TIpHTTPDataProvider = class(TIpAbstractHtmlDataProvider)
  private
    fLastType: String;
    procedure HttpError(const msg: string; aSocket: TLSocket);
    function HttpInput(ASocket: TLHTTPClientSocket; ABuffer: pchar; ASize: dword): dword;
    procedure HttpInputDone(ASocket: TLHTTPClientSocket);
    procedure HttpProcessHeader(ASocket: TLHTTPClientSocket);
    procedure HttpCanWrite(ASocket: TLHTTPClientSocket; var OutputEof: TWriteBlockStatus);
    procedure HttpDisconnect(aSocket: TLSocket);
    
    function GetURL(const AURL: String; JustHeader: Boolean = False): TStream;
    function GetHostAndURI(const fURL: String; var AHost: String; var AURI: String): Boolean;
  protected
    function DoGetHtmlStream(const URL: string;
      PostData: TIpFormDataEntity) : TStream; override;
    function DoCheckURL(const URL: string;
      var ContentType: string): Boolean; override;
    procedure DoLeave(Html: TIpHtml); override;
    procedure DoReference(const URL: string); override;
    procedure DoGetImage(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture); override;
    function CanHandle(const URL: string): Boolean; override;
    function BuildURL(const OldURL, NewURL: string): string; override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  end;
  
  TLHttpClientEx = class(TLHttpClientComponent)
  private
    Stream: TStream;
    Waiting: Boolean;
  end;


implementation

uses
  FPImage,
  {fpreadgif,} // doesn't exist yet!
  FPReadbmp,
  FPReadxpm,
  FPReadJPEg,
  FPReadpng,
  FPWritebmp,
  IntFGraphics;

{ TIpHTTPDataProvider }

procedure TIpHTTPDataProvider.HttpError(const msg: string; aSocket: TLSocket);
begin
  TLHttpClientEx(TLHttpClientSocket(ASocket).Connection).Waiting := False;
  //WriteLn('Error occured: ', msg);

end;

function TIpHTTPDataProvider.HttpInput(ASocket: TLHTTPClientSocket;
  ABuffer: pchar; ASize: dword): dword;
begin
  if TLHttpClientEx(ASocket.Connection).Stream = nil then
    TLHttpClientEx(ASocket.Connection).Stream := TMemoryStream.Create;
  Result := TLHttpClientEx(ASocket.Connection).Stream.Write(ABuffer^, ASize);


end;

procedure TIpHTTPDataProvider.HttpInputDone(ASocket: TLHTTPClientSocket);
begin
  TLHttpClientEx(ASocket.Connection).Waiting := False;
  aSocket.Disconnect;
  //WriteLn('InputDone');
end;

procedure TIpHTTPDataProvider.HttpProcessHeader(ASocket: TLHTTPClientSocket);
begin
  //WriteLn('Process Header');
  //WriteLn(ASocket.Parameters[hpContentType]);
  fLastType := ASocket.Parameters[hpContentType];
end;

procedure TIpHTTPDataProvider.HttpCanWrite(ASocket: TLHTTPClientSocket;
  var OutputEof: TWriteBlockStatus);
begin
    //WriteLn('OnCanWrite');
end;

procedure TIpHTTPDataProvider.HttpDisconnect(aSocket: TLSocket);
begin
  TLHttpClientEx(TLHttpClientSocket(ASocket).Connection).Waiting := False;
  //WriteLn('Disconnected');
end;


function TIpHTTPDataProvider.GetURL(const AURL: String; JustHeader: Boolean = False): TStream;
var
  fHost, fURI: String;
  fHttpClient: TLHttpClientEx;
begin
  Result := nil;
  if not GetHostAndURI(AURL, fHost, fURI) then Exit(nil);
  //WriteLn('Result := True');
  fHttpClient := TLHttpClientEx.Create(Owner);
  fHttpClient.OnInput := @HttpInput;
  fHttpClient.OnError := @HttpError;
  fHttpClient.OnDoneInput := @HttpInputDone;
  fHttpClient.OnProcessHeaders := @HttpProcessHeader;
  fHttpClient.OnCanWrite := @HttpCanWrite;
  fHttpClient.OnDisconnect := @HttpDisconnect;

  fHttpClient.Host := fHost;
  fHttpClient.Port := 80;
  if JustHeader then
    fHttpClient.Method := hmHead
  else
    fHttpClient.Method := hmGet;
  fHttpClient.URI := fURI;

  fHttpClient.SendRequest;

  fHttpClient.Waiting := True;
  while fHttpClient.Waiting do begin
    //WriteLn('InFirstLoop');
    Application.HandleMessage;
    if csDestroying in ComponentState then Exit;
  end;
  //WriteLn('LeftLoop');

  Result := fHttpClient.Stream;
  Result.Position := 0;
  //fDataStream.SaveToFile('temp.txt');
  //Application.Terminate;
  fHttpClient.Free;
end;

function TIpHTTPDataProvider.GetHostAndURI(const fURL: String; var AHost: String; var AURI: String): Boolean;
var
  fPos: Integer;
begin
  fPos := Pos('://', fUrl);
  if fPos = 0 then Exit(False);
  Result := True;
  AHost := Copy(fURL, fPos+3, Length(fURL));
  
  
  fPos := Pos('/', AHost);
  if fPos = 0 then begin
    AURI:='/';
    Exit(True);
  end;
  AURI := Copy(AHost, fPos, Length(AHost));
  AHost := Copy(AHost, 1, fPos-1);
  //WriteLn('Got Host: ',AHost);
  //WriteLn('Got URI : ',AURI);
end;

function TIpHTTPDataProvider.DoGetHtmlStream(const URL: string;
  PostData: TIpFormDataEntity): TStream;
begin
  Result := GetURL(URL);
end;

function TIpHTTPDataProvider.DoCheckURL(const URL: string;
  var ContentType: string): Boolean;
var
  TmpStream: TStream;
begin
  //WriteLn('Want content type: "', ContentType,'" for Url:',URL);
  Result := True;
  TmpStream := GetURL(URL, True);
  if TmpStream <> nil then FreeAndNil(TmpStream);
  ContentType := fLastType;//}'text/html';
end;

procedure TIpHTTPDataProvider.DoLeave(Html: TIpHtml);
begin

end;

procedure TIpHTTPDataProvider.DoReference(const URL: string);
begin

end;

procedure TIpHTTPDataProvider.DoGetImage(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
var
Stream: TMemoryStream = nil;
ImageClass: TFPCustomImageReaderClass;
ImageReader: TFPCustomImageReader;
OutImage: TFPWriterBMP= nil;
Img : TFPMemoryImage = nil;
FileExt: String;
begin


  FileExt := ExtractFileExt(URL);
  if FileExt[1] = '.' then Delete(FileExt,1,1);
  ImageClass := GetFPImageReaderForFileExtension(FileExt);

  if ImageClass = nil then begin
    Stream := TMemoryStream(GetURL(URL));
    //FreeAndNil(Stream);

    if Pos('image/', fLastType) = 1 then FileExt := Copy(fLastType, 7, Length(fLastType));
        //FileExt := ExtractFileExt(fLastType);
    //WriteLn('Got FIleExt ',FileExt, ' for ',fLastType);
    ImageClass := GetFPImageReaderForFileExtension(FileExt);
  end;
  
  //WriteLn('Getting Image ',(Url), ' Extension=',FileExt,' Image=nil=',BoolToStr(ImageClass=nil));
  if ImageClass <> nil then begin
    ImageReader := ImageClass.Create;
    try
      Picture := TPicture.Create;
      Picture.Graphic := TBitmap.Create;
      if Stream = nil then Stream := TMemoryStream(GetURL(URL));
      if Stream = nil then exit;
      Img := TFPMemoryImage.Create(0,0);
      Img.UsePalette:=False;
      Img.LoadFromStream(Stream, ImageReader);
      Stream.Free;
      Stream := TMemoryStream.Create;
      OutImage := TFPWriterBMP.Create;

      Img.SaveToStream(Stream, OutImage);

      Stream.Position := 0;
      Picture.Graphic.LoadFromStream(Stream);

    finally
      if Assigned(OutImage) then OutImage.Free;
      if Assigned(Img) then Img.Free;
      if Assigned(ImageReader) then ImageReader.Free;
      if Assigned(Stream) then Stream.Free;
    end;
  end
  else begin
    // Couldn't find the picture we wanted.
    FreeAndNil(Stream);
    Picture := nil;
  end;
end;

function TIpHTTPDataProvider.CanHandle(const URL: string): Boolean;
begin
  //WriteLn('Can Handle: ', URL);
  Result := True;
end;

function TIpHTTPDataProvider.BuildURL(const OldURL, NewURL: string): string;
begin
  Result := Iputils.BuildURL(OldURL, NewURL);
end;

constructor TIpHTTPDataProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TIpHTTPDataProvider.Destroy;
begin
  inherited Destroy;
end;

end.

