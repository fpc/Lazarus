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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit ProjectIcon;

{$mode objfpc}{$H+}

interface

uses
  // RTL + LCL
  Classes, SysUtils, resource, groupiconresource,
  // LCL
  LCLProc, Graphics,
  // LazUtils
  FileUtil, LazFileUtils, LazFileCache, Laz2_XMLCfg, lazutf8classes,
  // Codetools
  FileProcs,
  // IdeIntf
  ProjectResourcesIntf;
   
type
  TIconData = array of byte;

  { TProjectIcon }

  TProjectIcon = class(TAbstractProjectResource)
  private
    FData: TIconData;
    fFileAge: LongInt;
    fFileAgeValid: Boolean;
    FIcoFileName: string;
    function GetIsEmpry: Boolean;
    procedure SetIcoFileName(AValue: String);
    procedure SetIconData(const AValue: TIconData);
    procedure SetIsEmpty(const AValue: Boolean);
  public
    constructor Create; override;

    function GetStream: TStream;
    procedure SetStream(AStream: TStream);
    procedure LoadDefaultIcon;

    function UpdateResources(AResources: TAbstractProjectResources;
                             const MainFilename: string): Boolean; override;
    procedure WriteToProjectFile(AConfig: {TXMLConfig}TObject; const Path: String); override;
    procedure ReadFromProjectFile(AConfig: {TXMLConfig}TObject; const Path: String); override;

    function SaveIconFile: Boolean;

    property IconData: TIconData read FData write SetIconData;
    property IsEmpty: Boolean read GetIsEmpry write SetIsEmpty;
    property IcoFileName: String read FIcoFileName write SetIcoFileName;
  end;

implementation

function TProjectIcon.GetStream: TStream;
begin
  if length(FData)>0 then
  begin
    Result := TMemoryStream.Create;
    Result.WriteBuffer(FData[0], Length(FData));
    Result.Position := 0;
  end
  else
    Result := nil;
end;

procedure TProjectIcon.SetStream(AStream: TStream);
var
  NewIconData: TIconData;
begin
  NewIconData := nil;
  if (AStream <> nil) then
  begin
    SetLength(NewIconData, AStream.Size);
    AStream.ReadBuffer(NewIconData[0], AStream.Size);
  end;
  IconData := NewIconData;
end;

procedure TProjectIcon.LoadDefaultIcon;
var
  ResStream: TMemoryStream;
  Icon: TIcon;
begin
  // Load default icon
  Icon := TIcon.Create;
  ResStream := TMemoryStream.Create;
  try
    Icon.LoadFromResourceName(HInstance, 'MAINICONPROJECT');
    Icon.SaveToStream(ResStream);
    ResStream.Position := 0;
    SetStream(ResStream);
  finally
    ResStream.Free;
    Icon.Free;
  end;
end;

function TProjectIcon.UpdateResources(AResources: TAbstractProjectResources;
  const MainFilename: string): Boolean;
var
  AResource: TStream;
  AName: TResourceDesc;
  ARes: TGroupIconResource;
  ItemStream: TStream;
begin
  Result := True;
  if FData = nil then
    Exit;

  IcoFileName := ExtractFilePath(MainFilename)+ExtractFileNameOnly(MainFileName)+'.ico';
  if FilenameIsAbsolute(FIcoFileName) then
    if not SaveIconFile then begin
      debugln(['TProjectIcon.UpdateResources CreateIconFile "'+FIcoFileName+'" failed']);
      exit(false);
    end;

  AName := TResourceDesc.Create('MAINICON');
  ARes := TGroupIconResource.Create(nil, AName); //type is always RT_GROUP_ICON
  aName.Free; //not needed anymore
  AResource := GetStream;
  if AResource<>nil then
    try
      ItemStream:=nil;
      try
        ItemStream:=ARes.ItemData;
      except
        on E: Exception do begin
          DebugLn(['TProjectIcon.UpdateResources ignoring bug in fcl: ',E.Message]);
        end;
      end;
      if ItemStream<>nil then
        ItemStream.CopyFrom(AResource, AResource.Size);
    finally
      AResource.Free;
    end
  else
    ARes.ItemData.Size:=0;

  AResources.AddSystemResource(ARes);
end;

procedure TProjectIcon.WriteToProjectFile(AConfig: TObject; const Path: String);
begin
  TXMLConfig(AConfig).SetDeleteValue(Path+'General/Icon/Value', BoolToStr(IsEmpty), BoolToStr(true));
end;

procedure TProjectIcon.ReadFromProjectFile(AConfig: TObject; const Path: String);
begin
  with TXMLConfig(AConfig) do
  begin
    IcoFileName := ChangeFileExt(FileName, '.ico');
    IsEmpty := StrToBoolDef(GetValue(Path+'General/Icon/Value', BoolToStr(true)), False);
  end;
end;

function TProjectIcon.SaveIconFile: Boolean;
var
  fs: TFileStreamUTF8;
begin
  Result := False;
  if IsEmpty then exit;
  if fFileAgeValid and (FileAgeCached(IcoFileName)=fFileAge) then
    exit(true);
  // write ico file
  try
    if FileExistsUTF8(FIcoFileName) then
      fs:=TFileStreamUTF8.Create(IcoFileName,fmOpenWrite)
    else
      fs:=TFileStreamUTF8.Create(IcoFileName,fmCreate);
    try
      fs.Write(FData[0],length(FData));
      InvalidateFileStateCache(IcoFileName);
      fFileAge:=FileAgeCached(IcoFileName);
      fFileAgeValid:=true;
      Result:=true;
    finally
      fs.Free;
    end;
  except
    on E: Exception do
      debugln(['TProjectIcon.CreateIconFile "'+FIcoFileName+'": '+E.Message]);
  end;
end;

procedure TProjectIcon.SetIsEmpty(const AValue: Boolean);
var
  NewData: TIconData;
  fs: TFileStreamUTF8;
begin
  if IsEmpty=AValue then exit;
  if AValue then
  begin
    IconData := nil;
    Modified := True;
    fFileAgeValid := false;
  end
  else
  begin
    // We need to restore data from the .ico file
    try
      fs:=TFileStreamUTF8.Create(IcoFileName,fmOpenRead);
      try
        SetLength(NewData, fs.Size);
        if length(NewData)>0 then
          fs.Read(NewData[0],length(NewData));
        IconData := NewData;
        fFileAge:=FileAgeCached(IcoFileName);
        fFileAgeValid:=true;
        Modified := true;
      finally
        fs.Free
      end;
    except
    end;
  end;
end;

constructor TProjectIcon.Create;
begin
  inherited Create;
  FData := nil;
end;

procedure TProjectIcon.SetIconData(const AValue: TIconData);
begin
  if (Length(AValue) = Length(FData)) and
     (FData <> nil) and
     (CompareByte(AValue[0], FData[0], Length(FData)) = 0)
  then
    Exit;
  FData := AValue;
  fFileAgeValid := false;
  {$IFDEF VerboseIDEModified}
  debugln(['TProjectIcon.SetIconData ']);
  {$ENDIF}
  Modified := True;
end;

function TProjectIcon.GetIsEmpry: Boolean;
begin
  Result := FData = nil;
end;

procedure TProjectIcon.SetIcoFileName(AValue: String);
begin
  if FIcoFileName=AValue then Exit;
  FIcoFileName:=AValue;
  fFileAgeValid:=false;
end;

initialization
  RegisterProjectResource(TProjectIcon);

end.

