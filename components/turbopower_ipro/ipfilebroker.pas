(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Internet Professional
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

(* Part of Ipbroker.pas allowing to use local files  Armin <diehl@nordrhein.de> Jun 2006 *)

unit Ipfilebroker;

{$I ipdefine.inc}

interface

{$IFDEF IP_LAZARUS}
uses Classes, SysUtils, LResources, Graphics, LCLProc, LazFileUtils, LazUTF8,
     ipconst, iputils, iphtml, ipmsg;
{$ELSE}
uses
  Windows, SysUtils, Graphics, Classes, Dialogs, ShellApi,
  IpConst, IpUtils, {IpSock, IpCache,} IpHtml, {IpHttp,} IpMsg, IpStrms{, IpFtp};
{$ENDIF}

const
  IP_DEFAULT_SCHEME : string = 'HTTP';

{$IFDEF IP_LAZARUS}
function expandLocalHtmlFileName (URL : string) : string;
{$ENDIF}

type

  TIpGetHtmlDataEvent =
    procedure(Sender : TObject; const URL : string; const PostData : TIpFormDataEntity; var Stream : TStream) of object;
  TIpGetImageDataEvent =
    procedure(Sender : TIpHtmlNode; const URL : string; var Picture : TPicture) of object;
  TIpLeaveHtmlDocumentEvent =
    procedure(Sender : TIpHtml) of object;
  TIpCheckURLEvent =
    procedure(Sender : TObject; const URL : string; var Available :Boolean; var ContentType : string) of object;
  TIpReportReferenceEvent =
    procedure(Sender : TObject; const URL : string) of object;
  TIpExternalResourceEvent =
    procedure(Sender : TObject; const URL : string) of object;
  TIpCanHandleEvent =
    function(Sender : TObject; const URL : string) : Boolean of object;


  TIpCustomHtmlDataProvider = class(TIpAbstractHtmlDataProvider)
  private
    FProtocols : TStrings;
    FGetHtml : TIpGetHtmlDataEvent;
    FGetImage : TIpGetImageDataEvent;
    FLeave : TIpLeaveHtmlDocumentEvent;
    FCheckURL : TIpCheckURLEvent;
    FReportReference : TIpReportReferenceEvent;
    FCanHandle : TIpCanHandleEvent;
    function GetProtocols : TStrings;
    procedure SetProtocols(const Value : TStrings);
  protected
    // Nothing
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function DoGetHtmlStream(const URL : string; PostData : TIpFormDataEntity) : TStream; override;
    function DoCheckURL(const URL : string; var ContentType : string) : Boolean; override;
    procedure DoLeave(Html : TIpHtml); override;
    procedure DoReference(const URL : string); override;
    procedure DoGetImage(Sender : TIpHtmlNode; const URL : string; var Picture : TPicture); override;
    function GetHtmlStream(const URL : string; PostData : TIpFormDataEntity) : TStream; virtual;
    function CheckURL(const URL : string; var ContentType : string) : Boolean; virtual;
    procedure Leave(Html : TIpHtml); virtual;
    procedure Reference(const URL : string); virtual;
    procedure GetImage(Sender : TIpHtmlNode; const URL : string; var Picture : TPicture); virtual;
    function CanHandle(const URL : string) : Boolean; override;
    function BuildURL(const Old, New : string) : string; override;
    property HandledProtocols : TStrings read GetProtocols write SetProtocols;
    property OnCanHandle : TIpCanHandleEvent read FCanHandle write FCanhandle;
    property OnGetHtml : TIpGetHtmlDataEvent read FGetHtml write FGetHtml;
    property OnGetImage : TIpGetImageDataEvent read FGetImage write FGetImage;
    property OnLeave : TIpLeaveHtmlDocumentEvent read FLeave write FLeave;
    property OnCheckURL : TIpCheckURLEvent read FCheckURL write FCheckURL;
    property OnReportReference : TIpReportReferenceEvent read FReportReference write FReportReference;
  published
    // Nothing
  end;

  TIpHtmlDataProvider = class(TIpCustomHtmlDataProvider)
  public
  published
    property HandledProtocols;
    property OnCanHandle;
    property OnGetHtml;
    property OnGetImage;
    property OnLeave;
    property OnCheckURL;
    property OnReportReference;
  end;

  { TIpFileDataProvider }

  TIpFileDataProvider = class(TIpCustomHtmlDataProvider)
  private
    FOldURL : string;
  public
    constructor Create(AOwner : TComponent); override;
    function GetHtmlStream(const URL : string; PostData : TIpFormDataEntity) : TStream; override;
    {$IFDEF IP_LAZARUS}
    function DoGetStream(const URL: string): TStream; override;
    {$ENDIF}
    function CheckURL(const URL : string; var ContentType : string) : Boolean; override;
    procedure Leave(Html : TIpHtml); override;
    procedure Reference(const URL : string); override;
    procedure GetImage(Sender : TIpHtmlNode; const URL : string; var Picture : TPicture); override;
    function CanHandle(const URL : string) : Boolean; override;
  end;

procedure Register;

implementation

{$IFDEF IP_LAZARUS}
function expandLocalHtmlFileName (URL : string) : string;
begin
  if pos ('FILE://', ansiuppercase(URL)) = 0 then
    result := 'file://'+DOSToNetPath(ExpandFileNameUTF8(URL))
  else
    result := URL;
end;
{$ENDIF}

{ TIpCustomHtmlDataProvider }
constructor TIpCustomHtmlDataProvider.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FProtocols := TStringList.Create;
end;

destructor TIpCustomHtmlDataProvider.Destroy;
begin
  FProtocols.Free;
  inherited Destroy;
end;

function TIpCustomHtmlDataProvider.BuildURL(const Old,
  New : string) : string;
begin
  Result := IpUtils.BuildURL(Old, New);
  {$IFDEF IP_LAZARUS}
  //DebugLn('TIpCustomHtmlDataProvider.BuildURL Old="',old,'" new="',New,'"');
  {$ENDIF}
end;

function TIpCustomHtmlDataProvider.CanHandle(const URL : string) : Boolean;
var
  AddrRec : TIpAddrRec;
begin
  Initialize(AddrRec);
  if Assigned(FCanHandle) then begin
    Result := FCanHandle(self, URL);
  end
  else begin
    Result := False;
    IpParseURL(URL, AddrRec);
    if AddrRec.Scheme = '' then begin
      if FProtocols.Count > 1 then
        AddrRec.Scheme := FProtocols[1]
      else
        AddrRec.Scheme := IP_DEFAULT_SCHEME;
    end;
    if FProtocols.IndexOf(UpperCase(AddrRec.Scheme)) > -1 then
      Result := True
  end;
  Initialize(AddrRec);
  Finalize(AddrRec);
end;

function TIpCustomHtmlDataProvider.CheckURL(const URL : string;
  var ContentType : string) : Boolean;
begin
  ContentType := '';
  Result := False;
end;

function TIpCustomHtmlDataProvider.DoCheckURL(const URL : string;
      var ContentType : string) : Boolean;
begin
  Result := False;
  if Assigned(FCheckURL) then
    FCheckURL(Self, URL, Result, ContentType)
  else
    Result := CheckURL(URL, ContentType);
end;

procedure TIpCustomHtmlDataProvider.DoGetImage(Sender : TIpHtmlNode;
  const URL : string; var Picture : TPicture);
begin
  if Assigned(OnGetImage) then begin
    OnGetImage(Sender, URL, Picture)
  end
  else
    GetImage(Sender, URL, Picture);

  if (Picture <> nil) then begin
    if not (Picture is TPicture) then
      raise Exception.Create(ProviderUnknownPicture);
  end;
end;

procedure TIpCustomHtmlDataProvider.DoLeave;
begin
  if assigned(FLeave) then
    FLeave(Html)
  else
    Leave(Html);
end;

procedure TIpCustomHtmlDataProvider.DoReference(const URL : string);
begin
  if assigned(FReportReference) then
    FReportReference(Self, URL)
  else
    Reference(URL);
end;

function TIpCustomHtmlDataProvider.DoGetHtmlStream(const URL : string;
  PostData : TIpFormDataEntity) : TStream;
begin
  Result := nil;
  if Assigned(FGetHtml) then
    FGetHtml(Self, URL, PostData, Result)
  else
    Result := GetHtmlStream(URL, PostData);
end;

function TIpCustomHtmlDataProvider.GetHtmlStream(const URL : string;
  PostData : TIpFormDataEntity) : TStream;
begin
  { return defaults }
  Result := nil;
end;

procedure TIpCustomHtmlDataProvider.GetImage(Sender : TIpHtmlNode;
  const URL : string; var Picture : TPicture);
begin
  { return defaults }
  Picture := nil;
end;

function TIpCustomHtmlDataProvider.GetProtocols : TStrings;
begin
  Result := FProtocols;
end;

procedure TIpCustomHtmlDataProvider.Leave(Html : TIpHtml);
begin
  { do nothing }
end;

procedure TIpCustomHtmlDataProvider.Reference(const URL : string);
begin
  { do nothing }
end;

procedure TIpCustomHtmlDataProvider.SetProtocols(const Value : TStrings);
begin
  FProtocols.Assign(Value);
end;

{ TIpFileDataProvider }
constructor TIpFileDataProvider.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  HandledProtocols.Add('FILE');
end;

function TIpFileDataProvider.CanHandle(const URL : string) : Boolean;
var
  FileAddrRec : TIpAddrRec;
  ContentType, FN : string;
begin
  Initialize(FileAddrRec);
  {$IFDEF IP_LAZARUS}
  //DebugLn('TIpFileDataProvider.CanHandle('+URL+')');
  {$ENDIF}
  FN := BuildURL(FOldURL, URL);
  IpParseURL(FN, FileAddrRec);
  FN := NetToDosPath(FileAddrRec.Path);
  {$IFDEF IP_LAZARUS}
  //DebugLn('TIpFileDataProvider.CanHandle FN="'+FN+'"');
  {$ENDIF}
  ContentType := UpperCase(GetLocalContent(FN));
  Result := (FileExistsUTF8(FN)) and ((Pos('TEXT/HTML', ContentType) > 0) or
    (Pos('IMAGE/', ContentType) > 0));
  Finalize(FileAddrRec);
end;

function TIpFileDataProvider.CheckURL(const URL : string;
  var ContentType : string) : Boolean;
var
  FileAddrRec : TIpAddrRec;
  FN : string;
begin
  Initialize(FileAddrRec);
  IpParseURL(URL, FileAddrRec);
  FN := NetToDosPath(FileAddrRec.Path);
  Result := FileExistsUTF8(FN);
  ContentType := GetLocalContent(FN);
  Finalize(FileAddrRec);
end;

function TIpFileDataProvider.GetHtmlStream(const URL : string;
  PostData : TIpFormDataEntity) : TStream;
var
  FileAddrRec : TIpAddrRec;
  FN : string;
begin
  Initialize(FileAddrRec);
  IpParseURL(URL, FileAddrRec);
  FN := NetToDosPath(FileAddrRec.Path);
  Result := TMemoryStream.Create;
  TMemoryStream(Result).LoadFromFile(UTF8ToSys(FN));
  FOldURL := URL;
  Finalize(FileAddrRec);
end;

{$IFDEF IP_LAZARUS}
function TIpFileDataProvider.DoGetStream(const URL: string): TStream;
var
  FileAddrRec : TIpAddrRec;
  FN : string;
begin
  Initialize(FileAddrRec);
  IpParseURL(URL, FileAddrRec);
  FN := NetToDosPath(FileAddrRec.Path);
  Result := TMemoryStream.Create;
  try
    TMemoryStream(Result).LoadFromFile(UTF8ToSys(FN));
  except
    Result.Free;
    Result:=nil;
  end;
  Finalize(FileAddrRec);
end;
{$ENDIF}

procedure TIpFileDataProvider.GetImage(Sender : TIpHtmlNode;
  const URL : string; var Picture : TPicture);
var
  FileAddrRec : TIpAddrRec;
  Content, FN : string;
begin
  Initialize(FileAddrRec);
  Picture := nil;
  IpParseURL(URL, FileAddrRec);
  FN := NetToDosPath(FileAddrRec.Path);
  Content := UpperCase(GetLocalContent(FN));
  if Pos('IMAGE/', Content) > 0 then begin
    try
      Picture := TPicture.Create;
      Picture.LoadFromFile(FN);
    except
      on EInvalidGraphic do begin
        Picture.Free;
        Picture := nil;
      end;
    end;
  end;
  Finalize(FileAddrRec);
end;

procedure TIpFileDataProvider.Leave(Html : TIpHtml);
begin
  inherited Leave(Html);
  { Do Nothing }
end;

procedure TIpFileDataProvider.Reference(const URL : string);
begin
  inherited Reference(URL);
  { Do Nothing }
end;

procedure Register;
begin
  RegisterComponents('IPro', [TIpFileDataProvider, TIpHTMLDataProvider]);
end;


end.
