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
  See the file COPYING.LCL, included in this distribution,
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
  IntFGraphics;
  

type

  THelpPopupEvent = procedure(HelpFile: String; URL: String);

  { TIpChmDataProvider }

  TIpChmDataProvider = class(TIpAbstractHtmlDataProvider)
  private
    fChm: TChmreader;
    fCurrentPath: String;
    fOnHelpPopup: THelpPopupEvent;
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
  public
    constructor Create(var AChm: TChmReader);
    destructor Destroy; override;
    property Chm: TChmReader read fChm write fChm;
    property OnHelpPopup: THelpPopupEvent read fOnHelpPopup write fOnHelpPopup;
    property CurrentPath: String read fCurrentPath write fCurrentPath;
  end;

implementation

{ TIpChmDataProvider }

function TIpChmDataProvider.DoGetHtmlStream(const URL: string;
  PostData: TIpFormDataEntity): TStream;
begin
  //DebugLn('Getting: ', URL);
  Result := fChm.GetObject(URL);
  
  // If for some reason we were not able to get the page return something so that
  // we don't cause an AV
  if Result = nil then begin
    Result := TMemoryStream.Create;
    Result.Write('<HTML>Page cannot be found!</HTML>',33);
  end;
end;

function TIpChmDataProvider.DoCheckURL(const URL: string;
  var ContentType: string): Boolean;

begin
  //DebugLn('RequestedUrl: ',URL);
  Result := fChm.ObjectExists(Url) > 0;
  if Result then begin
    ContentType := 'text/html';
    fCurrentPath := ExtractFilePath(Url);
    Result := True;
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
Stream: TMemoryStream = nil;
OrigImage: TFPCustomImageReader;
OutImage: TFPWriterBMP= nil;
Img : TFPMemoryImage = nil;
FileExt: String;
begin
  //DebugLn('Getting Image ',(Url));

  FileExt := ExtractFileExt(URL);
  if FileExt = '.jpg' then
    OrigImage := TFPReaderJPEG.Create
  else if FileExt = '.png' then
    OrigImage := TFPReaderPNG.Create
  else if FileExt = '.xpm' then
    OrigImage := TFPReaderXPM.Create
  else if FileExt = '.bmp' then
    OrigImage := TFPReaderBMP.Create
  else if FileExt = '.ico' then
    OrigImage := TFPReaderBMP.Create
  else begin
    OrigImage := nil;
  end;

  if OrigImage <> nil then begin
    try
      Picture := TPicture.Create;
      Picture.Graphic := TBitmap.Create;
      Stream := TMemoryStream(fChm.GetObject('/'+URL));
      if Stream = nil then exit;
      Img := TFPMemoryImage.Create(0,0);
      Img.UsePalette:=False;
      Img.LoadFromStream(Stream, OrigImage);
      Stream.Free;
      Stream := TMemoryStream.Create;
      OutImage := TFPWriterBMP.Create;

      Img.SaveToStream(Stream, OutImage);

      Stream.Position := 0;
      Picture.Graphic.LoadFromStream(Stream);

    finally
      if Assigned(OutImage) then OutImage.Free;
      if Assigned(Img) then Img.Free;
      if Assigned(OrigImage) then OrigImage.Free;
      if Assigned(Stream) then Stream.Free;
    end;
  end
  else begin
    // Couldn't find the picture we wanted.
    Picture := nil;
  end;
end;

function TIpChmDataProvider.CanHandle(const URL: string): Boolean;
var
  HelpFile,
  Link: String;
begin
  Result := True;
  if Pos('Java', URL) =1  then Result := False;
  if (fChm.ObjectExists(url)= 0) and (fChm.ObjectExists(BuildUrl(fCurrentPath,Url)) = 0) then Result := False;
  //DebugLn('CanHandle ',Url,' = ', Result);
  //if not Result then if fChm.ObjectExists(BuildURL('', URL)) > 0 Then result := true;
  if Pos('javascript:helppopup(''', LowerCase(URL)) = 1 then begin
    HelpFile := Copy(URL, 23, Length(URL) - (23-1));
    HelpFile := Copy(HelpFile, 1, Pos('''', HelpFile)-1);
    //DebugLn('HelpFile = ', HelpFile);
  end;
end;

function TIpChmDataProvider.BuildURL(const OldURL, NewURL: string): string;
var
tmp: String;
X: LongInt;
begin
  //Result := NewUrl;
  //DebugLn('OldURl=',OldUrl, ' NewURL=',NewUrl);
  //DebugLn('Current path = ', fcurrentpath);
  Result:=iputils.BuildURL(Oldurl,NewUrl);
  //DebugLn('NEWURL=',Result);
  X := Pos('#', Result);
  if X > 0 then Result := Copy(Result, 1 ,X-1);

  if NewURL[1] <> '/' then
  begin
    if fChm.ObjectExists(Result) > 0 then Tmp := Result
    else if fChm.ObjectExists('/'+Result) > 0 then begin
      Tmp := '/'+Result;
    end
    else if fChm.ObjectExists(fCurrentPath+Result) > 0 then begin
      Tmp := fCurrentPath+Result;
    end
    else if fChm.ObjectExists(fCurrentPath+NewUrl) > 0 then begin
      Tmp := fCurrentPath+NewURL;
    end;
    Result := Tmp;
  end;
  X := Pos('//', Result);
  while X > 0 do begin
    Delete(Result, X ,1);
    X := Pos('//', Result);
  end;

   // if not fChmfile.ObjectExists(Result) then Result := TryToLocateObject(Result);



end;

function TIpChmDataProvider.GetDirsParents(ADir: String): TStringList;
var
 X: Integer;
 Tmp: String;
begin
  Result := TStringList.Create;
  //Result.Add(ADir);
  for X := Length(ADir) downto 1 do begin
    if ADir[X] = '/' then begin
      Tmp := Copy(ADir, 1, X);
      Result.Add(Tmp);
    end;
  end;

end;

constructor TIpChmDataProvider.Create(var AChm: TChmReader);
begin
  inherited Create(nil);
  fChm := AChm;
end;

destructor TIpChmDataProvider.Destroy;
begin
  inherited Destroy;
end;

end.

