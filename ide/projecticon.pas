{
 /***************************************************************************
                        projecticon.pas  -  Lazarus IDE unit
                        ---------------------------------------
               TProjectIcon is responsible for the inclusion of the 
             icon in windows executables as rc file and others as .lrs.


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit ProjectIcon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Process, LCLProc, Controls, Forms,
  CodeToolManager, CodeCache, CodeAtom, LazConf, LResources, base64,
  ProjectResourcesIntf;
   
type
  { TProjectIcon }

  TProjectIcon = class(TAbstractProjectResource)
  private
    FIconText: String;
    icoFileName: string;
    procedure SetIconText(const AValue: String);
    procedure SetFileNames(const MainFilename: string);
  protected
    function GetAsHex: String;
  public
    constructor Create; override;

    function GetStream: TStream;
    procedure SetStream(AStream: TStream);

    function HasAnyLazarusResource: Boolean; override;
    function HasAnySystemResource: Boolean; override;
    function UpdateResources(AResources: TAbstractProjectResources; const MainFilename: string): Boolean; override;
    function CreateIconFile: Boolean;

    property IconText: String read FIconText write SetIconText;
  end;

implementation

const
  sIcon: String =
    'MAINICON ICON';


function TProjectIcon.GetStream: TStream;
var
  S: TStringStream;
  BS: TBase64DecodingStream;
begin
  if IconText <> '' then
  begin
    S := TStringStream.Create(IconText);
    S.Position := 0;
    BS := TBase64DecodingStream.Create(S);
    Result := TMemoryStream.Create;
    try
      Result.CopyFrom(BS, BS.Size);
      Result.Position := 0;
    except
      FreeAndNil(Result);
    end;
    BS.Free;
    S.Free;
  end
  else
    Result := nil;
end;

procedure TProjectIcon.SetStream(AStream: TStream);
var
  S: TStringStream;
  BS: TBase64EncodingStream;
  NewIconText: String;
begin
  NewIconText := '';
  if (AStream <> nil) then
  begin
    S := TStringStream.Create('');
    BS := TBase64EncodingStream.Create(S);
    BS.CopyFrom(AStream, AStream.Size);
    BS.Free;
    NewIconText := S.DataString;
    S.Free;
  end;
  IconText := NewIconText;
end;

function TProjectIcon.HasAnyLazarusResource: Boolean;
begin
  Result := IconText <> '';
end;

function TProjectIcon.HasAnySystemResource: Boolean;
begin
  Result := IconText <> '';
end;

function TProjectIcon.UpdateResources(AResources: TAbstractProjectResources;
  const MainFilename: string): Boolean;
var
  AResource: TStream;
begin
  Result := True;

  //debugln(['TProjectIcon.UpdateResources ',IconText = '']);
  if IconText = '' then
    Exit;

  SetFileNames(MainFilename);

  AResource := GetStream;
  try
    AResources.AddLazarusResource(AResource, 'MAINICON', 'ICO');
  finally
    AResource.Free;
  end;

  // the preferred way is this:
  // RCIcon := sIcon + #$D#$A + GetAsHex;
  // but it does not work

  if CreateIconFile then
    AResources.AddSystemResource(sIcon + Format(' "%s"', [StringReplace(icoFileName, '\', '\\', [rfReplaceAll])]))
  else
    Result := False;
end;

function TProjectIcon.CreateIconFile: Boolean;
var
  FileStream, AStream: TStream;
begin
  Result := False;
  AStream := GetStream;
  FileStream := nil;
  try
    FileStream := TFileStream.Create(UTF8ToSys(icoFileName), fmCreate);
    FileStream.CopyFrom(AStream, AStream.Size);
    Result := True;
  finally
    FileStream.Free;
  end;
  AStream.Free;
end;

{-----------------------------------------------------------------------------
 TProjectIcon SetFileNames
-----------------------------------------------------------------------------}
procedure TProjectIcon.SetFileNames(const MainFilename: string);
begin
  icoFileName := ExtractFilePath(MainFilename) + 'icon.ico';
end;

constructor TProjectIcon.Create;
var
  DefaultRes: TLResource;
  ResStream: TLazarusResourceStream;
begin
  inherited Create;

  FIconText := '';

  // Load default icon
  DefaultRes := LazarusResources.Find('LazarusProject', 'ICO');
  if DefaultRes <> nil then
  begin
    ResStream := TLazarusResourceStream.CreateFromHandle(DefaultRes);
    SetStream(ResStream);
    ResStream.Free;
  end;
end;

procedure TProjectIcon.SetIconText(const AValue: String);
begin
  if FIconText = AValue then Exit;
  FIconText := AValue;
  Modified := True;
end;

function TProjectIcon.GetAsHex: String;
var
  AStream: TStream;
  i, l: integer;
  b: PByte;
begin
  Result := '';
  AStream := GetStream;
  b := TMemoryStream(AStream).Memory;
  l := AStream.Size - 1;
  for i := 0 to l do
  begin
    if (i mod 16) = 0 then
      Result := Result + '''';
    Result := Result + IntToHex(b^, 2);
    if (i <> l) then
    begin
      Result := Result + ' ';
      if ((succ(i) mod 16) = 0) then
        Result := Result + ''''#$D#$A;
    end;
    inc(b);
  end;
  if l > 0 then
    Result := Result + '''';
  Result := '{'#$D#$A + Result + #$D#$A'}';
  AStream.Free;
end;

end.

