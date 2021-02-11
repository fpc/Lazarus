{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Copyright (C) <2005> <Andrew Haines> chmdataprovider.pas

}
unit ChmDataProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chmreader,
  FPImage, FPReadgif, FPReadbmp, FPReadxpm, FPReadJPEG, FPReadpng, FPWritebmp,
  FPWritePNG,
  // LCL
  Graphics, LCLType, Controls, IntFGraphics,
  // LazUtils
  LazFileUtils, LazLoggerBase,
  // Turbopower IPro
  IpHtml, iputils, IpMsg,
  // ChmHelp
  LHelpStrConsts;

type

  THelpPopupEvent = procedure(HelpFile: String; URL: String);
  THtmlPageLoadStreamEvent = procedure (var AStream: TStream) of object;

  { TCHMFileListPublic }

  TCHMFileListPublic = class(TChmFileList)
  end;

  { TIpChmDataProvider }

  TIpChmDataProvider = class(TIpAbstractHtmlDataProvider)
  private
    fChms: TCHMFileListPublic;
    fCurrentPage: String;
    fCurrentPath: String;
    FOnGetHtmlPage: THtmlPageLoadStreamEvent;
    fOnHelpPopup: THelpPopupEvent;
    function GetChms: TChmFileList;
    function StripInPageLink(AURL: String): String;
  protected
    function DoGetHtmlStream(const URL: string;
      {%H-}PostData: TIpFormDataEntity) : TStream; override;
    function DoCheckURL(const URL: string;
      var ContentType: string): Boolean; override;
    procedure DoLeave({%H-}Html: TIpHtml); override;
    procedure DoReference(const {%H-}URL: string); override;
    procedure DoGetImage(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture); override;
    function CanHandle(const URL: string): Boolean; override;
    function BuildURL(const OldURL, NewURL: string): string; override;
    function GetDirsParents(ADir: String): TStringList;
    function DoGetStream(const URL: string): TStream; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetHtmlText(AURL: String): RawByteString;
    property Chms: TChmFileList read GetChms;
    procedure DoOpenChm ( const AFile: String; ACloseCurrent: Boolean );
    procedure DoCloseChms;
    property OnHelpPopup: THelpPopupEvent read fOnHelpPopup write fOnHelpPopup;
    property CurrentPage: String read fCurrentPage;
    property CurrentPath: String read fCurrentPath write fCurrentPath;
    property OnGetHtmlPage: THtmlPageLoadStreamEvent read FOnGetHtmlPage write FOnGetHtmlPage;
  end;

implementation

{ TIpChmDataProvider }

function TIpChmDataProvider.StripInPageLink ( AURL: String ) : String;
var
  i: LongInt;
begin
  Result := AURL;
  i := Pos('#', Result);
  if i > 0 then
    SetLength(Result, i-1);
end;

function TIpChmDataProvider.GetChms: TChmFileList;
begin
  Result:= fChms;
end;

function TIpChmDataProvider.GetHtmlText ( AURL: String ) : RawByteString;
var
  stream: TStream;
  ms: TMemoryStream;
begin
  Result := '';
  stream := DoGetHtmlStream(AURL, nil);
  if stream = nil then
    exit;
  try
    if stream.Size > 0 then
    begin
      // The stream created by DoGetHtmlStream can be read only once!
      // --> buffer to memory stream
      ms := TMemoryStream.Create;
      try
        ms.CopyFrom(stream, 0);
        SetLength(Result, ms.Size);
        Move(ms.Memory^, Result[1], ms.Size);
      finally
        ms.Free;
      end;
    end;
  finally
    stream.Free;
  end;
end;

procedure TIpChmDataProvider.DoOpenChm ( const AFile: String;
  ACloseCurrent: Boolean ) ;
begin
  if fChms.IsAnOpenFile(AFile) then Exit;
  if ACloseCurrent then DoCloseChms;
  if not FileExistsUTF8(AFile) or DirectoryExistsUTF8(AFile) then
    Exit;
  TCHMFileListPublic(fChms).OpenNewFile(AFile);
  // Code for Indexes has been moved to the OpenFile handler
end;

procedure TIpChmDataProvider.DoCloseChms;
begin
  if assigned(fChms) then
    while Chms.Count > 0 do fChms.Delete(0);
end;

function TIpChmDataProvider.DoGetHtmlStream(const URL: string;
  PostData: TIpFormDataEntity): TStream;
var
  Msg:string;
begin
  //WriteLn('DoGetHtmlStream() Url: ', URL);
  Result := fChms.GetObject(StripInPageLink(URL));
  // If for some reason we were not able to get the page return something so that
  // we don't cause an AV
  if Result = nil then begin
    Result := TMemoryStream.Create;
    Msg := '<html><span align="center">' + slhelp_PageCannotBeFound + '</span></html>';
    Result.Write(Pointer(Msg)^, Length(Msg));
  end;
  Result.Position:= 0;
  if Assigned(FOnGetHtmlPage) then
      FOnGetHtmlPage(Result);
   Result.Position:= 0;
end;

function TIpChmDataProvider.DoCheckURL(const URL: string;
  var ContentType: string): Boolean;
var
  Reader: TChmReader = nil;
begin
  Result:= true;
  //DebugLn('CD DoCheckURL() Url: ', URL);
  Result := fChms.ObjectExists(StripInPageLink(Url), Reader) > 0;
  if Result then begin
    ContentType := 'text/html';
    fCurrentPath := ExtractFilePath(Url);
    fCurrentPage := URL;
    //DebugLn('CD checked url: ', URL);
  end;
end;

procedure TIpChmDataProvider.DoLeave(Html: TIpHtml);
begin
  // For free a data resources
  //DebugLn('CD Left: ');
end;

procedure TIpChmDataProvider.DoReference(const URL: string);
begin
  // For get all references from document
  // DebugLn('CD Reference=',URL);
end;

procedure TIpChmDataProvider.DoGetImage(Sender: TIpHtmlNode; const URL: string;
  var Picture: TPicture);
var
  Stream: TMemoryStream;
  FileExt: String;
begin
  //DebugLn('CD Getting Image ',(Url));
  FileExt := ExtractFileExt(URL);

  Picture := TPicture.Create;
  Stream := fChms.GetObject('/'+URL);
  try
    if Assigned(Stream) then
    begin
      Stream.Position := 0;
      Picture.LoadFromStreamWithFileExt(Stream, FileExt);
    end;
  except
    // only happens if it's an image type we can't handle
  end;
  if Stream <> nil then
    Stream.Free;
end;

function TIpChmDataProvider.CanHandle(const URL: string): Boolean;
var
  Reader: TChmReader = nil;
begin
  //DebugLn('CD CanHandle() Url: ', URL);
  Result:=True;
  if Pos('Java', URL) = 1 then
    Result := False;

  // Here is opened the new chm file if required
  if (fChms.ObjectExists(StripInPageLink(url), Reader)= 0) and
     (fChms.ObjectExists(StripInPageLink(BuildUrl(fCurrentPath,Url)), Reader) = 0)
  then
    Result := False;
  if (not Result) and (Pos('#', URL) = 1) then
    Result := True;
  //DebugLn('CD CanHandle() ResultUrl: ', Result);
end;

function TIpChmDataProvider.BuildURL(const OldURL, NewURL: string): string;
var
  X: LongInt;
  fNewURL: String;
  ParentDirs: TStringList;
  RemoveDirCount: Integer;
begin
  Result := NewURL;

  fNewURL := NewURL;
  if OldURL = '' then
    exit;

  if Pos('ms-its:', NewURL) = 1 then begin
    if Pos('#', NewURL) = 0 then
      exit;
    X := Pos('::', NewURL);
    if NewURL[X+2] = '/' then    // NewURL is complete and absolute --> nothing to do
      exit;
    fNewURL := Copy(fNewURL, X+3, MaxInt);
  end;

  ParentDirs := GetDirsParents(OldURL);
  try
    RemoveDirCount := 0;
    repeat
      X := Pos('../', fNewURL);
      if X > 0 then
      begin
        Delete(fNewURL, X, 3);
        Inc(RemoveDirCount);
      end;
    until X = 0;

    repeat
      X := Pos('./', fNewURL);
      if X > 0 then
        Delete(fNewURL, X, 2);
    until X = 0;

    Result := '';
    for X := 0 to ParentDirs.Count-RemoveDirCount-1 do
      Result := Result + ParentDirs[X] + '/';

    Result := Result+fNewURL;

    repeat
      X := Pos('//', Result);
      if X > 0 then
        Delete(Result, X, 1);
    until X = 0;

  finally
    ParentDirs.Free;
  end;
  //DebugLn('CD BuildURL() Url: ', Result);
end;

function TIpChmDataProvider.GetDirsParents(ADir: String): TStringList;
var
  LastName: String;
begin
  Result := TStringList.Create;
  Result.Delimiter := '/';
  Result.StrictDelimiter := true;
  Result.DelimitedText := ADir;

  LastName := ExtractFileName(ADir);
  if LastName <> '' then
    Result.Delete(Result.Count-1);
  if Result[Result.Count-1] = '' then
    Result.Delete(Result.Count-1);
end;

function TIpChmDataProvider.DoGetStream(const URL: string): TStream;
var
 NewURL: String;
begin
  //DebugLn('CD DoGetStream() Url: ', URL);
  Result := nil;
  if Length(URL) = 0 then
    Exit;
  if not (URL[1] in ['/']) then
    NewURL := BuildUrl(fCurrentPath,URL)
  else
    NewURL := URL;
  Result := fChms.GetObject(NewURL);
  if Result <> nil then Result.Position:= 0;
  //if Result = nil then DebugLn('CD Err DoGetStream URL: '+URL);
end;

constructor TIpChmDataProvider.Create ( AOwner: TComponent ) ;
begin
  inherited Create(AOwner);
  fChms := TCHMFileListPublic.Create('');
end;

destructor TIpChmDataProvider.Destroy;
begin
  DoCloseChms;
  FreeAndnil(fChms);
  inherited Destroy;
end;

end.

