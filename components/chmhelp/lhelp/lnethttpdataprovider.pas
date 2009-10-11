unit LNetHTTPDataProvider;

{$mode objfpc}{$H+}

interface

uses
  Forms, Classes, SysUtils, IpHtml, IpMsg, IpUtils, lnetcomponents, Graphics, lhttp, lnet;
  
  type

  TIpHTTPDataProvider = class;

  TGettingURLCB = procedure(AProvider: TIpHTTPDataProvider; AURL: String) of object;
  
  { TIpHTTPDataProvider }

  TIpHTTPDataProvider = class(TIpAbstractHtmlDataProvider)
  private
    fLastType: String;
    fCachedStreams: TStringList;
    fCachedEmbeddedObjects: TStringList;
    procedure AddObjectToCache(ACache: TStringList; AURL: String; AStream: TStream);
    procedure ClearCache;
    procedure ClearCachedObjects;
    function GetCachedURL(AURL: String): TStream;
    function GetCachedObject(AURL: String): TStream;
    procedure HttpError(const msg: string; aSocket: TLSocket);
    function HttpInput(ASocket: TLHTTPClientSocket; ABuffer: pchar; ASize: LongInt): LongInt;
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
    function DoGetStream(const URL: string): TStream; override;
    function CanHandle(const URL: string): Boolean; override;
    function BuildURL(const OldURL, NewURL: string): string; override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  end;
  
  TLHttpClientEx = class(TLHTTPClientComponent)
  //TLHttpClientEx = class(TLHTTPClient)
  private
    Stream: TStream;
    Waiting: Boolean;
    HeaderOnly: Boolean;
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

procedure TIpHTTPDataProvider.AddObjectToCache ( ACache: TStringList;
  AURL: String; AStream: TStream ) ;
var
  TmpStream: TStream;
begin
  TmpStream := TMemoryStream.Create;
  AStream.Position := 0;
  TmpStream.CopyFrom(AStream, AStream.Size);
  ACache.AddObject(AURL, TmpStream);
  AStream.Position := 0;
end;

procedure TIpHTTPDataProvider.ClearCache;
var
  i: Integer;
begin
  for i := 0 to fCachedStreams.Count-1 do
    if fCachedStreams.Objects[i] <> nil then
      fCachedStreams.Objects[i].Free;
  fCachedStreams.Clear;

end;

procedure TIpHTTPDataProvider.ClearCachedObjects;
var
  i: Integer;
begin
  for i := 0 to fCachedStreams.Count-1 do
    if fCachedEmbeddedObjects.Objects[i] <> nil then
      fCachedEmbeddedObjects.Objects[i].Free;
  fCachedEmbeddedObjects.Clear;


end;

function TIpHTTPDataProvider.GetCachedURL ( AURL: String ) : TStream;
var
  i: Integer;
begin
  Result := nil;
  if Trim(AURL) = '' then
    Exit;
  for i := 0 to fCachedStreams.Count-1 do
    if fCachedStreams.Strings[i] = AURL then
    begin
      if fCachedStreams.Objects[i] = nil then break;
      Result := TMemoryStream.Create;
      TStream(fCachedStreams.Objects[i]).Position := 0;
      Result.CopyFrom(TStream(fCachedStreams.Objects[i]), TStream(fCachedStreams.Objects[i]).Size);
      Result.Position := 0;
      break;
    end;
  //WriteLn(AURL,' in cache = ', Result <> nil);
  if Result = nil then
    Result := GetCachedObject(AURL);

end;

function TIpHTTPDataProvider.GetCachedObject ( AURL: String ) : TStream;
var
  i: Integer;
begin
  Result := nil;
  if Trim(AURL) = '' then
    Exit;
  for i := 0 to fCachedEmbeddedObjects.Count-1 do
    if fCachedEmbeddedObjects.Strings[i] = AURL then
    begin
      if fCachedEmbeddedObjects.Objects[i] = nil then break;
      Result := TMemoryStream.Create;
      TStream(fCachedEmbeddedObjects.Objects[i]).Position := 0;
      Result.CopyFrom(TStream(fCachedEmbeddedObjects.Objects[i]), TStream(fCachedEmbeddedObjects.Objects[i]).Size);
      Result.Position := 0;
      break;
    end;
  //WriteLn(AURL,' in cached objects = ', Result <> nil);

end;

procedure TIpHTTPDataProvider.HttpError(const msg: string; aSocket: TLSocket);
begin
  TLHttpClientEx(ASocket.Creator).Waiting := False;
  //writeLn('Error occured: ', msg);

end;

function TIpHTTPDataProvider.HttpInput(ASocket: TLHTTPClientSocket;
  ABuffer: pchar; ASize: LongInt): LongInt;
begin
  //WriteLN(ASocket.Creator.ClassName);
  if TLHttpClientEx(ASocket.Creator).Stream = nil then
    TLHttpClientEx(ASocket.Creator).Stream := TMemoryStream.Create;
  Result := TLHttpClientEx(ASocket.Creator).Stream.Write(ABuffer^, ASize);


end;

procedure TIpHTTPDataProvider.HttpInputDone(ASocket: TLHTTPClientSocket);
begin
  TLHttpClientEx(ASocket.Creator).Waiting := False;
  aSocket.Disconnect;
  //WriteLn('InputDone');
end;

procedure TIpHTTPDataProvider.HttpProcessHeader(ASocket: TLHTTPClientSocket);
var
  i: TLHTTPParameter;
begin
  //WriteLn('Process Header');
  //for i := Low(TLHTTPParameterArray) to High(TLHTTPParameterArray) do
  //  if ASocket.Parameters[i] <> ''  then
  //  WriteLn(ASocket.Parameters[i]);
  //WriteLn(ASocket.Parameters[hpContentType]);
  fLastType := ASocket.Parameters[hpContentType];
  if TLHttpClientEx(ASocket.Creator).HeaderOnly then
    TLHttpClientEx(ASocket.Creator).Waiting := False;
end;

procedure TIpHTTPDataProvider.HttpCanWrite(ASocket: TLHTTPClientSocket;
  var OutputEof: TWriteBlockStatus);
begin
    //WriteLn('OnCanWrite');
end;

procedure TIpHTTPDataProvider.HttpDisconnect(aSocket: TLSocket);
begin
  TLHttpClientEx(ASocket.Creator).Waiting := False;
  //WriteLn('Disconnected');
end;


function TIpHTTPDataProvider.GetURL(const AURL: String; JustHeader: Boolean = False): TStream;
var
  fHost, fURI: String;
  fHttpClient: TLHttpClientEx;
begin
  Result := nil;

  if JustHeader = False then
    Result := GetCachedURL(AURL);
  //WriteLN('Getting: ', AURL);
  if Result = nil then
  begin
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
    fHttpClient.HeaderOnly := JustHeader;
    if JustHeader then
      fHttpClient.Method := hmHead
    else
      fHttpClient.Method := hmGet;
    fHttpClient.URI := fURI;

    fHttpClient.SendRequest;
    //WriteLn('Sending Request');

    fHttpClient.Waiting := True;
    {while fHttpClient.Waiting = True do
      begin
        fHttpClient.CallAction;
        Sleep(1);
      end;}

    while fHttpClient.Waiting do begin
      //WriteLn('InFirstLoop');
      Application.HandleMessage;
      if csDestroying in ComponentState then Exit;
    end;
    //WriteLn('LeftLoop');

    Result:= fHttpClient.Stream;
    if Result <> nil then
      Result.Position := 0;
    //fDataStream.SaveToFile('temp.txt');
    //Application.Terminate;
    fHttpClient.Free;
  end;
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
  Result := GetCachedURL(URL);
  if Result = nil then
  begin
    Result := GetURL(URL);
    if Result <> nil then
      AddObjectToCache(fCachedStreams, URL, Result);
  end;
end;

function TIpHTTPDataProvider.DoCheckURL(const URL: string;
  var ContentType: string): Boolean;
var
  TmpStream: TStream;
begin
  //WriteLn('Want content type: "', ContentType,'" for Url:',URL);
  Result := True;
  //TmpStream := GetCachedURL(URL);
  //if TmpStream = nil then
  //begin
    TmpStream := GetURL(URL, True);
  //  if TmpStream <> nil then
  //    AddObjectToCache(fCachedStreams, URL, TmpStream);
  //end;

  if TmpStream <> nil then FreeAndNil(TmpStream);
  ContentType := fLastType;//}'text/html';
end;

procedure TIpHTTPDataProvider.DoLeave(Html: TIpHtml);
begin
  ClearCache;
end;

procedure TIpHTTPDataProvider.DoReference(const URL: string);
begin

end;

procedure TIpHTTPDataProvider.DoGetImage(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
var
  Stream: TStream;
  FileExt: String;
begin
  //DebugLn('Getting Image ',(Url));
  Picture := nil;

  FileExt := ExtractFileExt(URL);

  Picture := TPicture.Create;
  try
    Stream := GetCachedObject(URL);
    if Stream = nil then
    begin
      Stream := GetURL(URL);
      if Stream <> nil then
        AddObjectToCache(fCachedEmbeddedObjects, URL, Stream);
    end;

    if Assigned(Stream) then
    begin
      Stream.Position := 0;
      Picture.LoadFromStreamWithFileExt(Stream, FileExt);
    end
    else
      Picture.Graphic := TBitmap.Create;
  except
    try
      Picture.Free;
    finally
      Picture := TPicture.Create;
      Picture.Graphic := TBitmap.Create;
    end;
  end;
  Stream.Free;
end;

function TIpHTTPDataProvider.DoGetStream ( const URL: string ) : TStream;
begin
  Result := GetCachedObject(URL);
  if Result = nil then
  begin
    Result := GetURL(URL);
    if Result <> nil then
      AddObjectToCache(fCachedEmbeddedObjects, URL, Result);
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
  fCachedEmbeddedObjects := TStringList.Create;
  fCachedStreams := TStringList.Create;
end;

destructor TIpHTTPDataProvider.Destroy;
begin
  ClearCache;
  ClearCachedObjects;
  fCachedStreams.Free;
  fCachedEmbeddedObjects.Free;
  inherited Destroy;
end;

end.

