{ Copyright (C) <2005> <Andrew Haines> chmdataprovider.pas

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the copyright.
}
unit ChmDataProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IpHtml, iputils, IpMsg, Graphics, chmreader,
  LCLType, Controls,
  FPImage,
  {fpreadgif,} // doesn't exist yet!
  FPReadbmp,
  FPReadxpm,
  FPReadJPEg,
  FPReadpng,
  FPWritebmp,
  FPWritePNG,
  IntFGraphics;


type

  THelpPopupEvent = procedure(HelpFile: String; URL: String);
  THtmlPageLoadStreamEvent = procedure (var AStream: TStream) of object;

  { TIpChmDataProvider }

  TIpChmDataProvider = class(TIpAbstractHtmlDataProvider)
  private
    fChm: TChmFileList;
    fCurrentPage: String;
    fCurrentPath: String;
    FOnGetHtmlPage: THtmlPageLoadStreamEvent;
    fOnHelpPopup: THelpPopupEvent;
    function StripInPageLink(AURL: String): String;
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
    function GetDirsParents(ADir: String): TStringList;
    function DoGetStream(const URL: string): TStream; override;
  public
    constructor Create(AOwner: TComponent; AChm: TChmFileList);
    destructor Destroy; override;
    property Chm: TChmFileList read fChm write fChm;
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
    Result := Copy(Result, 1, i-1);
end;

function TIpChmDataProvider.DoGetHtmlStream(const URL: string;
  PostData: TIpFormDataEntity): TStream;
begin
  Result := fChm.GetObject(StripInPageLink(URL));
  // If for some reason we were not able to get the page return something so that
  // we don't cause an AV
  if Result = nil then begin
    Result := TMemoryStream.Create;
    Result.Write('<HTML>Page cannot be found!</HTML>',33);
  end;
  if Assigned(FOnGetHtmlPage) then
      FOnGetHtmlPage(Result);
end;

function TIpChmDataProvider.DoCheckURL(const URL: string;
  var ContentType: string): Boolean;
var
Reader: TChmReader = nil;
begin
  //DebugLn('RequestedUrl: ',URL);
  Result := fChm.ObjectExists(StripInPageLink(Url), Reader) > 0;
  if Result then begin
    ContentType := 'text/html';
    fCurrentPath := ExtractFilePath(Url);
    Result := True;
    fCurrentPage := URL;
  end;
end;

procedure TIpChmDataProvider.DoLeave(Html: TIpHtml);
begin
  //
//  //DebugLn('Left: ');
end;

procedure TIpChmDataProvider.DoReference(const URL: string);
begin
  //
  ////DebugLn('Reference=',URL);
end;

procedure TIpChmDataProvider.DoGetImage(Sender: TIpHtmlNode; const URL: string;
  var Picture: TPicture);
var
  Stream: TMemoryStream;
  FileExt: String;
begin
  //DebugLn('Getting Image ',(Url));
  Picture := nil;

  FileExt := ExtractFileExt(URL);

  Picture := TPicture.Create;
  Stream := fChm.GetObject('/'+URL);
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
  HelpFile: String;
  Reader: TChmReader = nil;
begin
  Result := True;
  if Pos('Java', URL) =1  then Result := False;
  if  (fChm.ObjectExists(StripInPageLink(url), Reader)= 0)
  and (fChm.ObjectExists(StripInPageLink(BuildUrl(fCurrentPath,Url)), Reader) = 0) then Result := False;
  //DebugLn('CanHandle ',Url,' = ', Result);
  //if not Result then if fChm.ObjectExists(BuildURL('', URL)) > 0 Then result := true;
  if Pos('javascript:helppopup(''', LowerCase(URL)) = 1 then begin
    HelpFile := Copy(URL, 23, Length(URL) - (23-1));
    HelpFile := Copy(HelpFile, 1, Pos('''', HelpFile)-1);
    //DebugLn('HelpFile = ', HelpFile);
  end;
  if (not Result) and (Pos('#', URL) = 1) then Result := True;
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
  if Pos('ms-its:', NewURL) = 1 then
    Exit;

  ParentDirs := GetDirsParents(OldURL);
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

  ParentDirs.Free;
  //WriteLn('res = ', Result);
end;

function TIpChmDataProvider.GetDirsParents(ADir: String): TStringList;
var
  LastName: String;
begin
  Result := TStringList.Create;
  Result.Delimiter := '/';
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
  Result := nil;
  if Length(URL) = 0 then
    Exit;
  if not (URL[1] in ['/']) then
    NewURL := BuildUrl(fCurrentPath,URL)
  else
    NewURL := URL;

  Result := fChm.GetObject(NewURL);
end;

constructor TIpChmDataProvider.Create(AOwner: TComponent; AChm: TChmFileList);
begin
  inherited Create(AOwner);
  fChm := AChm;
end;

destructor TIpChmDataProvider.Destroy;
begin
  inherited Destroy;
end;

end.

