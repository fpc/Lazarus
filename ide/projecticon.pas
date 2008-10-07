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
  CodeToolManager, CodeCache, CodeAtom, LazConf, LResources, base64;
   
type
  { TProjectIcon }

  TProjectIcon = class(TObject)
  private
    FIconText: String;
    FMessages: TStrings;
    FModified: boolean;
    rcFileName: string;
    icoFileName: string;
    FOnModified: TNotifyEvent;
    procedure SetIconText(const AValue: String);
    procedure SetFileNames(const MainFilename: string);
    procedure SetModified(const AValue: Boolean);
  protected
    function GetAsHex: String;
  public
    constructor Create;
    destructor Destroy; override;
    
    function GetStream: TStream;
    procedure SetStream(AStream: TStream);

    function CreateRCFile(const MainFilename, TargetOS: string): TModalResult;
    function CreateIconFile: Boolean;
    function CreateResource: Boolean;
    function UpdateMainSourceFile(const AFilename: string): TModalResult;

    property IconText: String read FIconText write SetIconText;
    property Messages: TStrings read FMessages;
    property Modified: boolean read FModified write SetModified;

    property OnModified: TNotifyEvent read FOnModified write FOnModified;
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

{-----------------------------------------------------------------------------
 TProjectIcon CreateRCFile
-----------------------------------------------------------------------------}
function TProjectIcon.CreateRCFile(const MainFilename, TargetOS: string): TModalResult;
begin
  // in future we will compile manifest from rc, but now we just add our template
  Result := mrOk;
  SetFileNames(MainFilename);
  if ((TargetOS = 'win32') or (TargetOS = 'wince')) and (IconText <> '') then
  begin
    if not CreateResource then
      Result := mrCancel;
  end;
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

function TProjectIcon.CreateResource: Boolean;
var
  Stream: TStream;
  RCIcon: String;
begin
  Result := CreateIconFile;
  if not Result then
    Exit;
  Stream := nil;
  try
    Stream := TFileStream.Create(UTF8ToSys(rcFileName), fmCreate);
    // the preferred way is this:
    // RCIcon := sIcon + #$D#$A + GetAsHex;
    // but it does not work
    RCIcon := sIcon + Format(' "%s"', [StringReplace(icoFileName, '\', '\\', [rfReplaceAll])]);
    Stream.Write(RCIcon[1], length(RCIcon));
    Result := True;
  finally
    Stream.Free;
  end;
end;

{-----------------------------------------------------------------------------
 TProjectIcon UpdateMainSourceFile
-----------------------------------------------------------------------------}
function TProjectIcon.UpdateMainSourceFile(const AFilename: string): TModalResult;
var
  NewX, NewY, NewTopLine: integer;
  IconCodeBuf, NewCode: TCodeBuffer;
  Filename: String;
begin
  Result := mrCancel;
  IconCodeBuf := CodeToolBoss.LoadFile(AFilename,false,false);
  if IconCodeBuf <> nil then
  begin
    SetFileNames(AFilename);
    Filename:=ExtractFileName(rcFileName);
    // DebugLn(['TProjectIcon.UpdateMainSourceFile ',Filename]);
    if CodeToolBoss.FindResourceDirective(IconCodeBuf, 1, 1,
                               NewCode, NewX, NewY,
                               NewTopLine, Filename, false) then
    begin
      // there is a resource directive in the source
      if IconText = '' then 
      begin
        if not CodeToolBoss.RemoveDirective(NewCode, NewX,NewY,true) then
        begin
          Messages.Add('Could not remove "{$R'+ Filename +'"} from main source!');
          exit;
        end;
      end;
    end else 
    if IconText <> '' then
    begin
      if not CodeToolBoss.AddResourceDirective(IconCodeBuf,
        Filename,false,'{$IFDEF WINDOWS}{$R '+Filename+'}{$ENDIF}') then
      begin
        Messages.Add('Could not add "{$R'+ Filename +'"} to main source!');
        exit;
      end;
    end;
  end;
  //DebugLn(['TProjectIcon.UpdateMainSourceFile END ', IconCodeBuf.Source]);
  Result := mrOk;
end;

{-----------------------------------------------------------------------------
 TProjectIcon SetFileNames
-----------------------------------------------------------------------------}
procedure TProjectIcon.SetFileNames(const MainFilename: string);
begin
  rcFileName := ExtractFilePath(MainFilename) + 'icon.rc';
  icoFileName := ExtractFilePath(MainFilename) + 'icon.ico';
end;

constructor TProjectIcon.Create;
begin
  // TODO: default icon
  FIconText := '';
  FMessages := TStringList.Create;
end;

destructor TProjectIcon.Destroy;
begin
  FMessages.Free;
  inherited Destroy;
end;

procedure TProjectIcon.SetIconText(const AValue: String);
begin
  if FIconText = AValue then Exit;
  FIconText := AValue;
  Modified := True;
end;

procedure TProjectIcon.SetModified(const AValue: Boolean);
begin
  if FModified = AValue then
    Exit;
  FModified := AValue;
  if Assigned(OnModified) then 
    OnModified(Self);
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

