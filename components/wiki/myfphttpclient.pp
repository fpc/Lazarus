{ This unit is a copy from fpc 2.7.1.
  It implements the chunked download required by Wiki webserver.
}

{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by the Free Pascal development team

    HTTP client component.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit myfphttpclient;

{$IF FPC_FULLVERSION>=20701}
{$ERROR this unit is only needed for fpc < 2.7.1, which lack the chunked http feature. }
{$ENDIF}

{ ---------------------------------------------------------------------
  Todo:
  * Proxy support ?
  * Easy calls for POST/DELETE/etc.
  ---------------------------------------------------------------------}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ssockets, httpdefs, uriparser, base64;

Const
  ReadBufLen = 4096;

Type
  { TFPCustomHTTPClient }
  TFPCustomHTTPClient = Class(TComponent)
  private
    FCookies: TStrings;
    FHTTPVersion: String;
    FRequestBody: TStream;
    FRequestHeaders: TStrings;
    FResponseHeaders: TStrings;
    FResponseStatusCode: Integer;
    FResponseStatusText: String;
    FServerHTTPVersion: String;
    FSocket : TInetSocket;
    FBuffer : Ansistring;
    function CheckContentLength: Integer;
    function CheckTransferEncoding: string;
    function GetCookies: TStrings;
    procedure SetCookies(const AValue: TStrings);
    procedure SetRequestHeaders(const AValue: TStrings);
  protected
    // Parse response status line. Saves status text and protocol, returns numerical code. Exception if invalid line.
    Function ParseStatusLine(AStatusLine : String) : Integer;
    // Construct server URL for use in request line.
    function GetServerURL(URI: TURI): String;
    // Read 1 line of response. Fills FBuffer
    function ReadString: String;
    // Check if response code is in AllowedResponseCodes. if not, an exception is raised.
    function CheckResponseCode(ACode: Integer;  const AllowedResponseCodes: array of Integer): Boolean; virtual;
    // Read response from server, and write any document to Stream.
    procedure ReadResponse(Stream: TStream;  const AllowedResponseCodes: array of Integer); virtual;
    // Read server response line and headers. Returns status code.
    Function ReadResponseHeaders : integer; virtual;
    // Allow header in request ? (currently checks only if non-empty and contains : token)
    function AllowHeader(var AHeader: String): Boolean; virtual;
    // Connect to the server. Must initialize FSocket.
    procedure ConnectToServer(const AHost: String; APort: Integer); virtual;
    // Disconnect from server. Must free FSocket.
    procedure DisconnectFromServer; virtual;
    // Run method AMethod, using request URL AURL. Write Response to Stream, and headers in ResponseHeaders.
    // If non-empty, AllowedResponseCodes contains an array of response codes considered valid responses.
    Procedure DoMethod(Const AMethod,AURL : String; Stream : TStream; Const AllowedResponseCodes : Array of Integer); virtual;
    // Send request to server: construct request line and send headers and request body.
    procedure SendRequest(const AMethod: String; URI: TURI); virtual;
  Public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    // Request Header management
    // Return index of header, -1 if not present.
    Function IndexOfHeader(Const AHeader : String) : Integer;
    // Add header, replacing an existing one if it exists.
    Procedure AddHeader(Const AHeader,AValue : String);
    // Return header value, empty if not present.
    Function GetHeader(Const AHeader : String) : String;
    // General-purpose call.
    Procedure HTTPMethod(Const AMethod,AURL : String; Stream : TStream; Const AllowedResponseCodes : Array of Integer); virtual;
    // Execute GET on server, store result in Stream, File, StringList or string
    Procedure Get(Const AURL : String; Stream : TStream);
    Procedure Get(Const AURL : String; const LocalFileName : String);
    Procedure Get(Const AURL : String; Response : TStrings);
    Function Get(Const AURL : String) : String;
    // Simple post
    // Post URL, and Requestbody. Return response in Stream, File, TstringList or String;
    procedure Post(const URL: string; const Response: TStream);
    procedure Post(const URL: string; Response : TStrings);
    procedure Post(const URL: string; const LocalFileName: String);
    function Post(const URL: string) : String;
    // Post Form data (www-urlencoded).
    // Formdata in string (urlencoded) or TStrings (plain text) format.
    // Form data will be inserted in the requestbody.
    // Return response in Stream, File, TStringList or String;
    Procedure FormPost(const URL, FormData: string; const Response: TStream);
    Procedure FormPost(const URL : string; FormData:  TStrings; const Response: TStream);
    Procedure FormPost(const URL, FormData: string; const Response: TStrings);
    Procedure FormPost(const URL : string; FormData:  TStrings; const Response: TStrings);
    function FormPost(const URL, FormData: string): String;
    function FormPost(const URL: string; FormData : TStrings): String;
    // Post a file
    Procedure FileFormPost(const AURL, AFieldName, AFileName: string; const Response: TStream);
  Protected
    // Before request properties.
    // Additional headers for request. Host; and Authentication are automatically added.
    Property RequestHeaders : TStrings Read FRequestHeaders Write SetRequestHeaders;
    // Cookies. Set before request to send cookies to server.
    // After request the property is filled with the cookies sent by the server.
    Property Cookies : TStrings Read GetCookies Write SetCookies;
    // Optional body to send (mainly in POST request)
    Property RequestBody : TStream read FRequestBody Write FRequestBody;
    // used HTTP version when constructing the request.
    Property HTTPversion : String Read FHTTPVersion Write FHTTPVersion;
    // After request properties.
    // After request, this contains the headers sent by server.
    Property ResponseHeaders : TStrings Read FResponseHeaders;
    // After request, HTTP version of server reply.
    Property ServerHTTPVersion : String Read FServerHTTPVersion;
    // After request, HTTP response status of the server.
    Property ResponseStatusCode : Integer Read FResponseStatusCode;
    // After request, HTTP response status text of the server.
    Property ResponseStatusText : String Read FResponseStatusText;
  end;

  TFPHTTPClient = Class(TFPCustomHTTPClient)
  Public
    Property RequestHeaders;
    Property RequestBody;
    Property ResponseHeaders;
    Property HTTPversion;
    Property ServerHTTPVersion;
    Property ResponseStatusCode;
    Property ResponseStatusText;
    Property Cookies;
  end;
  EHTTPClient = Class(Exception);

Function EncodeURLElement(S : String) : String;
Function DecodeURLElement(Const S : String) : String;

implementation

resourcestring
  SErrInvalidProtocol = 'Invalid protocol : "%s"';
  SErrReadingSocket = 'Error reading data from socket';
  SErrInvalidProtocolVersion = 'Invalid protocol version in response: "%s"';
  SErrInvalidStatusCode = 'Invalid response status code: %s';
  SErrUnexpectedResponse = 'Unexpected response status code: %d';
  SErrChunkTooBig = 'Chunk too big';
  SErrChunkLineEndMissing = 'Chunk line end missing';

Const
  CRLF = #13#10;

function EncodeURLElement(S: String): String;

Const
  NotAllowed = [ ';', '/', '?', ':', '@', '=', '&', '#', '+', '_', '<', '>',
                 '"', '%', '{', '}', '|', '\', '^', '~', '[', ']', '`' ];

var
  i, o, l : Integer;
  h: string[2];
  P : PChar;
  c: AnsiChar;
begin
  l:=Length(S);
  If (l=0) then Exit;
  SetLength(Result,l*3);
  P:=Pchar(Result);
  for I:=1 to L do
    begin
    C:=S[i];
    O:=Ord(c);
    if (O<=$20) or (O>=$7F) or (c in NotAllowed) then
      begin
      P^ := '%';
      Inc(P);
      h := IntToHex(Ord(c), 2);
      p^ := h[1];
      Inc(P);
      p^ := h[2];
      Inc(P);
      end
    else
      begin
      P^ := c;
      Inc(p);
      end;
    end;
  SetLength(Result,P-PChar(Result));
end;

function DecodeURLElement(Const S: AnsiString): AnsiString;

var
  i,l,o : Integer;
  c: AnsiChar;
  p : pchar;
  h : string;

begin
  l := Length(S);
  if l=0 then exit;
  SetLength(Result, l);
  P:=PChar(Result);
  i:=1;
  While (I<=L) do
    begin
    c := S[i];
    if (c<>'%') then
      begin
      P^:=c;
      Inc(P);
      end
    else if (I<L-1) then
      begin
      H:='$'+Copy(S,I+1,2);
      o:=StrToIntDef(H,-1);
      If (O>=0) and (O<=255) then
        begin
        P^:=char(O);
        Inc(P);
        Inc(I,2);
        end;
      end;
    Inc(i);
  end;
  SetLength(Result, P-Pchar(Result));
end;

{ TFPCustomHTTPClient }

procedure TFPCustomHTTPClient.SetRequestHeaders(const AValue: TStrings);
begin
  if FRequestHeaders=AValue then exit;
  FRequestHeaders.Assign(AValue);
end;

function TFPCustomHTTPClient.IndexOfHeader(const AHeader: String): Integer;
Var
  L : Integer;
  H : String;
begin
  H:=LowerCase(Aheader);
  l:=Length(AHeader);
  Result:=Requestheaders.Count-1;
  While (Result>=0) and ((LowerCase(Copy(RequestHeaders[Result],1,l)))<>h) do
    Dec(Result);
end;

procedure TFPCustomHTTPClient.AddHeader(const AHeader, AValue: String);

Var
  J: Integer;
begin
  j:=IndexOfHeader(Aheader);
  if (J<>-1) then
    RequestHeaders.Delete(j);
  RequestHeaders.Add(AHeader+': '+Avalue);
end;

function TFPCustomHTTPClient.GetHeader(const AHeader: String): String;

Var
  I : Integer;

begin
  I:=indexOfHeader(AHeader);
  Result:=RequestHeaders[i];
  I:=Pos(':',Result);
  if (I=0) then
    I:=Length(Result);
  Delete(Result,1,I);
end;

Function TFPCustomHTTPClient.GetServerURL(URI : TURI) : String;

Var
  D : String;

begin
  D:=URI.Path;
  If (D[1]<>'/') then
    D:='/'+D;
  If (D[Length(D)]<>'/') then
    D:=D+'/';
  Result:=D+URI.Document;
  if (URI.Params<>'') then
    Result:=Result+'?'+URI.Params;
end;

procedure TFPCustomHTTPClient.ConnectToServer(Const AHost : String; APort : Integer);

begin
  if Aport=0 then
    Aport:=80;
  FSocket:=TInetSocket.Create(AHost,APort);
end;

procedure TFPCustomHTTPClient.DisconnectFromServer;

begin
  FreeAndNil(FSocket);
end;

function TFPCustomHTTPClient.AllowHeader(Var AHeader : String) : Boolean;

begin
  Result:=(AHeader<>'') and (Pos(':',AHeader)<>0);
end;

procedure TFPCustomHTTPClient.SendRequest(Const AMethod : String; URI : TURI);

Var
  S,L : String;
  I : Integer;

begin
  S:=Uppercase(AMethod)+' '+GetServerURL(URI)+' '+'HTTP/'+FHTTPVersion+CRLF;
  If (URI.Username<>'') then
    S:=S+'Authorization: Basic ' + EncodeStringBase64(URI.UserName+ ':' + URI.Password)+CRLF;
  S:=S+'Host: '+URI.Host;
  If (URI.Port<>0) then
    S:=S+':'+IntToStr(URI.Port);
  S:=S+CRLF;
  If Assigned(RequestBody) and (IndexOfHeader('Content-length')=-1) then
    AddHeader('Content-length',IntToStr(RequestBody.Size));
  For I:=0 to FRequestHeaders.Count-1 do
    begin
    l:=FRequestHeaders[i];
    If AllowHeader(L) then
      S:=S+L+CRLF;
    end;
  if Assigned(FCookies) then
    begin
    L:='Cookie:';
    For I:=0 to FCookies.Count-1 do
      begin
      If (I>0) then
        L:=L+'; ';
      L:=L+FCookies[i];
      end;
    if AllowHeader(L) then
      S:=S+L+CRLF;
    end;
  S:=S+CRLF;
  FSocket.WriteBuffer(S[1],Length(S));
  If Assigned(FRequestBody) then
    FSocket.CopyFrom(FRequestBody,FRequestBody.Size);
end;

function TFPCustomHTTPClient.ReadString : String;

  Procedure FillBuffer;

  Var
    R : Integer;

  begin
    SetLength(FBuffer,ReadBufLen);
    r:=FSocket.Read(FBuffer[1],ReadBufLen);
    If r<0 then
      Raise EHTTPClient.Create(SErrReadingSocket);
    if (r<ReadBuflen) then
      SetLength(FBuffer,r);
  end;

Var
  CheckLF,Done : Boolean;
  P,L : integer;

begin
  Result:='';
  Done:=False;
  CheckLF:=False;
  Repeat
    if Length(FBuffer)=0 then
      FillBuffer;
    if Length(FBuffer)=0 then
      Done:=True
    else if CheckLF then
      begin
      If (FBuffer[1]<>#10) then
        Result:=Result+#13
      else
        begin
        Delete(FBuffer,1,1);
        Done:=True;
        end;
      end;
    if not Done then
      begin
      P:=Pos(#13#10,FBuffer);
      If P=0 then
        begin
        L:=Length(FBuffer);
        CheckLF:=FBuffer[L]=#13;
        if CheckLF then
          Result:=Result+Copy(FBuffer,1,L-1)
        else
          Result:=Result+FBuffer;
        FBuffer:='';
        end
      else
        begin
        Result:=Result+Copy(FBuffer,1,P-1);
        Delete(FBuffer,1,P+1);
        Done:=True;
        end;
      end;
  until Done;
end;
Function GetNextWord(Var S : String) : string;

Const
  WhiteSpace = [' ',#9];

Var
  P : Integer;

begin
  While (Length(S)>0) and (S[1] in WhiteSpace) do
    Delete(S,1,1);
  P:=Pos(' ',S);
  If (P=0) then
   P:=Pos(#9,S);
  If (P=0) then
    P:=Length(S)+1;
  Result:=Copy(S,1,P-1);
  Delete(S,1,P);
end;

Function TFPCustomHTTPClient.ParseStatusLine(AStatusLine : String) : Integer;

Var
  S : String;

begin
  S:=Uppercase(GetNextWord(AStatusLine));
  If (Copy(S,1,5)<>'HTTP/') then
    Raise EHTTPClient.CreateFmt(SErrInvalidProtocolVersion,[S]);
  Delete(S,1,5);
  FServerHTTPVersion:=S;
  S:=GetNextWord(AStatusLine);
  Result:=StrToIntDef(S,-1);
  if Result=-1 then
   Raise EHTTPClient.CreateFmt(SErrInvalidStatusCode,[S]);
  FResponseStatusText:=AStatusLine;
end;

Function TFPCustomHTTPClient.ReadResponseHeaders : Integer;

  Procedure DoCookies(S : String);

  Var
    P : Integer;
    C : String;

  begin
    If Assigned(FCookies) then
      FCookies.Clear;
    P:=Pos(':',S);
    Delete(S,1,P);
    Repeat
      P:=Pos(';',S);
      If (P=0) then
        P:=Length(S)+1;
      C:=Trim(Copy(S,1,P-1));
      Cookies.Add(C);
      Delete(S,1,P);
    Until (S='');
  end;

Const
  SetCookie = 'set-cookie';

Var
  StatusLine,S : String;

begin
  StatusLine:=ReadString;
  Result:=ParseStatusLine(StatusLine);
  Repeat
    S:=ReadString;
    if (S<>'') then
      begin
      ResponseHeaders.Add(S);
      If (LowerCase(Copy(S,1,Length(SetCookie)))=SetCookie) then
        DoCookies(S);
      end
  Until (S='');
end;

Function TFPCustomHTTPClient.CheckResponseCode(ACode : Integer; Const AllowedResponseCodes : Array of Integer) : Boolean;

Var
  I : Integer;

begin
  Result:=(High(AllowedResponseCodes)=-1);
  if not Result then
    begin
    I:=Low(AllowedResponseCodes);
    While (Not Result) and (I<=High(AllowedResponseCodes)) do
      begin
      Result:=(AllowedResponseCodes[i]=ACode);
      Inc(I);
      end
    end;
end;

Function TFPCustomHTTPClient.CheckContentLength: Integer;

Const CL ='content-length:';

Var
  S : String;
  I : integer;

begin
  Result:=-1;
  I:=0;
  While (Result=-1) and (I<FResponseHeaders.Count) do
    begin
    S:=Trim(LowerCase(FResponseHeaders[i]));
    If (Copy(S,1,Length(Cl))=Cl) then
      begin
      Delete(S,1,Length(CL));
      Result:=StrToIntDef(Trim(S),-1);
      end;
    Inc(I);
    end;
end;

Function TFPCustomHTTPClient.CheckTransferEncoding: string;

Const CL ='transfer-encoding:';

Var
  S : String;
  I : integer;

begin
  Result:='';
  I:=0;
  While (I<FResponseHeaders.Count) do
    begin
    S:=Trim(LowerCase(FResponseHeaders[i]));
    If (Copy(S,1,Length(Cl))=Cl) then
      begin
      Delete(S,1,Length(CL));
      Result:=Trim(S);
      exit;
      end;
    Inc(I);
    end;
end;

function TFPCustomHTTPClient.GetCookies: TStrings;
begin
  If (FCookies=Nil) then
    FCookies:=TStringList.Create;
  Result:=FCookies;
end;

procedure TFPCustomHTTPClient.SetCookies(const AValue: TStrings);
begin
  if GetCookies=AValue then exit;
  GetCookies.Assign(AValue);
end;

procedure TFPCustomHTTPClient.ReadResponse(Stream: TStream; Const AllowedResponseCodes : Array of Integer);

  Function Transfer(LB : Integer) : Integer;

  begin
    Result:=FSocket.Read(FBuffer[1],LB);
    If Result<0 then
      Raise EHTTPClient.Create(SErrReadingSocket);
    if (Result>0) then
      Stream.Write(FBuffer[1],Result);
  end;

  Procedure ReadChunkedResponse;
  { HTTP 1.1 chunked response:
    There is no content-length. The response consists of several chunks of
    data, each
    - beginning with a line
      - starting with a hex number DataSize,
      - an optional parameter,
      - ending with #13#10,
    - followed by the data,
    - ending with #13#10 (not in DataSize),
    It ends when the DataSize is 0.
    After the last chunk there can be a some optional entity header fields.
    This trailer is not yet implemented. }
  var
    BufPos: Integer;

    function FetchData(out Cnt: integer): boolean;

    begin
      SetLength(FBuffer,ReadBuflen);
      Cnt:=FSocket.Read(FBuffer[1],length(FBuffer));
      If Cnt<0 then
        Raise EHTTPClient.Create(SErrReadingSocket);
      SetLength(FBuffer,Cnt);
      BufPos:=1;
      Result:=Cnt>0;
    end;

    Function ReadData(Data: PByte; Cnt: integer): integer;

    var
      l: Integer;
    begin
      Result:=0;
      while Cnt>0 do
        begin
        l:=length(FBuffer)-BufPos+1;
        if l=0 then
          if not FetchData(l) then
            exit; // end of stream
        if l>Cnt then
          l:=Cnt;
        System.Move(FBuffer[BufPos],Data^,l);
        inc(BufPos,l);
        inc(Data,l);
        inc(Result,l);
        dec(Cnt,l);
      end;
    end;

  var
    c: char;
    ChunkSize: Integer;
    l: Integer;
  begin
    BufPos:=1;
    repeat
      // read ChunkSize
      ChunkSize:=0;
      repeat
        if ReadData(@c,1)<1 then exit;
        case c of
        '0'..'9': ChunkSize:=ChunkSize*16+ord(c)-ord('0');
        'a'..'f': ChunkSize:=ChunkSize*16+ord(c)-ord('a')+10;
        'A'..'F': ChunkSize:=ChunkSize*16+ord(c)-ord('A')+10;
        else break;
        end;
        if ChunkSize>1000000 then
          Raise EHTTPClient.Create(SErrChunkTooBig);
      until false;
      // read till line end
      while (c<>#10) do
        if ReadData(@c,1)<1 then exit;
      if ChunkSize=0 then exit;
      // read data
      repeat
        l:=length(FBuffer)-BufPos+1;
        if l=0 then
          if not FetchData(l) then
            exit; // end of stream
        if l>ChunkSize then
          l:=ChunkSize;
        if l>0 then
          begin
          // copy chunk data to output
          Stream.Write(FBuffer[BufPos],l);
          inc(BufPos,l);
          dec(ChunkSize,l);
          end;
      until ChunkSize=0;
      // read #13#10
      if ReadData(@c,1)<1 then exit;
      if c<>#13 then
        Raise EHTTPClient.Create(SErrChunkLineEndMissing);
      if ReadData(@c,1)<1 then exit;
      if c<>#10 then
        Raise EHTTPClient.Create(SErrChunkLineEndMissing);
      // next chunk
    until false;
  end;

Var
  L,LB,R : Integer;
begin
  SetLength(FBuffer,0);
  FResponseStatusCode:=ReadResponseHeaders;
  if not CheckResponseCode(FResponseStatusCode,AllowedResponseCodes) then
    Raise EHTTPClient.CreateFmt(SErrUnexpectedResponse,[ResponseStatusCode]);
  if CompareText(CheckTransferEncoding,'chunked')=0 then
    ReadChunkedResponse
  else
    begin
    // Write remains of buffer to output.
    LB:=Length(FBuffer);
    If (LB>0) then
      Stream.WriteBuffer(FBuffer[1],LB);
    // Now read the rest, if any.
    SetLength(FBuffer,ReadBuflen);
    L:=CheckContentLength;
    If (L>LB) then
      begin
      // We cannot use copyfrom, it uses ReadBuffer, and this is dangerous with sockets
      L:=L-LB;
      Repeat
        LB:=ReadBufLen;
        If (LB>L) then
          LB:=L;
        R:=Transfer(LB);
        L:=L-R;
      until (L=0) or (R=0);
      end
    else if L<0 then
      begin
      // No content-length, so we read till no more data available.
      Repeat
        R:=Transfer(ReadBufLen);
      until (R=0);
      end;
    end;
end;

procedure TFPCustomHTTPClient.DoMethod(Const AMethod,AURL: String; Stream: TStream; Const AllowedResponseCodes : Array of Integer);

Var
  URI : TURI;

begin
  FResponseHeaders.Clear;
  URI:=ParseURI(AURL);
  If (Lowercase(URI.Protocol)<>'http') then
   Raise EHTTPClient.CreateFmt(SErrInvalidProtocol,[URI.Protocol]);
  ConnectToServer(URI.Host,URI.Port);
  try
    SendRequest(AMethod,URI);
    ReadResponse(Stream,AllowedResponseCodes);
  finally
    DisconnectFromServer;
  end;
end;

constructor TFPCustomHTTPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRequestHeaders:=TStringList.Create;
  FResponseHeaders:=TStringList.Create;
  FHTTPVersion:='1.1';
end;

destructor TFPCustomHTTPClient.Destroy;
begin
  FreeAndNil(FRequestHeaders);
  FreeAndNil(FResponseHeaders);
  inherited Destroy;
end;

procedure TFPCustomHTTPClient.HTTPMethod(const AMethod, AURL: String;
  Stream: TStream; const AllowedResponseCodes: array of Integer);
begin
  DoMethod(AMethod,AURL,Stream,AllowedResponseCodes);
end;

procedure TFPCustomHTTPClient.Get(Const AURL: String; Stream: TStream);
begin
  DoMethod('GET',AURL,Stream,[200]);
end;

procedure TFPCustomHTTPClient.Get(Const AURL: String; const LocalFileName: String);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(LocalFileName,fmCreate);
  try
    Get(AURL,F);
  finally
    F.Free;
  end;
end;

procedure TFPCustomHTTPClient.Get(const AURL: String; Response: TStrings);
begin
  Response.Text:=Get(AURL);
end;

function TFPCustomHTTPClient.Get(Const AURL: String): String;

Var
  SS : TStringStream;

begin
  SS:=TStringStream.Create('');
  try
    Get(AURL,SS);
    Result:=SS.Datastring;
  finally
    SS.Free;
  end;
end;

procedure TFPCustomHTTPClient.Post(const URL: string; const Response: TStream);
begin
  DoMethod('POST',URL,Response,[]);
end;

procedure TFPCustomHTTPClient.Post(const URL: string; Response: TStrings);
begin
  Response.Text:=Post(URL);
end;

procedure TFPCustomHTTPClient.Post(const URL: string;
  const LocalFileName: String);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(LocalFileName,fmCreate);
  try
    Post(URL,F);
  finally
    F.Free;
  end;
end;

function TFPCustomHTTPClient.Post(const URL: string): String;
Var
  SS : TStringStream;
begin
  SS:=TStringStream.Create('');
  try
    Post(URL,SS);
    Result:=SS.Datastring;
  finally
    SS.Free;
  end;
end;

procedure TFPCustomHTTPClient.FormPost(const URL, FormData: string;
  const Response: TStream);

begin
  RequestBody:=TStringStream.Create(FormData);
  try
    AddHeader('Content-Type','application/x-www-form-urlencoded');
    Post(URL,Response);
  finally
    RequestBody.Free;
    RequestBody:=Nil;
  end;
end;

procedure TFPCustomHTTPClient.FormPost(const URL: string; FormData: TStrings;
  const Response: TStream);

Var
  I : Integer;
  S,N,V : String;

begin
  S:='';
  For I:=0 to FormData.Count-1 do
    begin
    If (S<>'') then
      S:=S+'&';
    FormData.GetNameValue(i,n,v);
    S:=S+EncodeURLElement(N)+'='+EncodeURLElement(V);
    end;
  FormPost(URL,S,Response);
end;

procedure TFPCustomHTTPClient.FormPost(const URL, FormData: string;
  const Response: TStrings);
begin
  Response.Text:=FormPost(URL,FormData);
end;

procedure TFPCustomHTTPClient.FormPost(const URL: string; FormData: TStrings;
  const Response: TStrings);
begin
  Response.Text:=FormPost(URL,FormData);
end;

function TFPCustomHTTPClient.FormPost(const URL, FormData: string): String;
Var
  SS : TStringStream;
begin
  SS:=TStringStream.Create('');
  try
    FormPost(URL,FormData,SS);
    Result:=SS.Datastring;
  finally
    SS.Free;
  end;
end;

function TFPCustomHTTPClient.FormPost(const URL: string; FormData: TStrings
  ): String;
Var
  SS : TStringStream;
begin
  SS:=TStringStream.Create('');
  try
    FormPost(URL,FormData,SS);
    Result:=SS.Datastring;
  finally
    SS.Free;
  end;
end;

procedure TFPCustomHTTPClient.FileFormPost(const AURL, AFieldName, AFileName: string; const Response: TStream);

Var
  S, Sep : string;
  SS : TStringStream;
  F : TFileStream;
begin
  Sep:=Format('%.8x_multipart_boundary',[Random($ffffff)]);
  AddHeader('Content-type','multipart/form-data; boundary='+Sep);
  S:='--'+Sep+CRLF;
  s:=s+Format('content-disposition: form-data; name="%s"; filename="%s"'+CRLF,[AFieldName,AFileName]);
  s:=s+'Content-Type: Application/octet-string'+CRLF+CRLF;
  SS:=TStringStream.Create(s);
  try
    SS.Seek(0,soFromEnd);
    F:=TFileStream.Create(AFileName,fmOpenRead);
    try
      SS.CopyFrom(F,F.Size);
    finally
      F.Free;
    end;
    S:=CRLF+'--'+Sep+'--'+CRLF;
    SS.WriteBuffer(S[1],Length(S));
    SS.Position:=0;
    RequestBody:=SS;
    Post(AURL,Response);
  finally
   RequestBody:=Nil;
   SS.Free;
  end;
end;

end.

