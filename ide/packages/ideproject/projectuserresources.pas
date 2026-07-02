{
 /***************************************************************************
                projectuserresources.pas  -  Lazarus IDE unit
                  ---------------------------------------
          TProjectUserResources is responsible for the inclusion of the 
           custom resources in executables as res file


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
unit ProjectUserResources;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL
  Classes, SysUtils, Contnrs,
  resource, bitmapresource, groupresource, groupiconresource, groupcursorresource,
  // LazUtils
  LazFileUtils, LazFileCache, LazUTF8, Laz2_XMLCfg, LazLoggerBase,
  // BuildIntf
  ProjectResourcesIntf, MacroIntf, IDEExternToolIntf,
  // IDE
  IdeProjectStrConsts;

type
  TAddIDEMessageEvent = procedure(Urgency: TMessageLineUrgency; Msg: string);

  TUserResourceType = (
    rtIcon,    // maps to RT_GROUP_ICON
    rtCursor,  // maps to RT_GROUP_CURSOR
    rtBitmap,  // maps to RT_BITMAP
    rtHTML,    // maps to RT_HTML
    rtRCData   // maps to RT_RCDATA
  );

  { TResourceItem }

  TResourceItem = class
  private
    // Cache of the file content, so that unchanged files are not re-read on
    // every build. Keyed on the resolved filename and its cached file age.
    FCacheData: TBytes;
    FCacheRealFileName: String;
    FCacheFileAge: Int64;
    FCacheValid: Boolean;
    // Returns a stream with the (cached) file content, or nil if the file does
    // not exist. The caller is responsible for freeing the stream.
    function GetFileStream(const RealFileName: String): TStream;
  public
    FileName: String;
    ResType: TUserResourceType;
    ResName: String;
    procedure ReadFromProjectFile(AConfig: TXMLConfig; const Path: String);
    procedure WriteToProjectFile(AConfig: TXMLConfig; const Path: String);
    function CreateResource(const ProjectDirectory: String): TAbstractResource;
    function GetRealFileName(const ProjectDirectory: String): String;
  end;

  { TResourceList }

  TResourceList = class(TFPObjectList)
  private
    function GetItem(AIndex: Integer): TResourceItem;
  public
    function IndexOfFileName(const AFileName: String): Integer;
    function AddItem: TResourceItem;
    procedure AddResource(const FileName: String; ResType: TUserResourceType; const ResName: String);
    property Items[AIndex: Integer]: TResourceItem read GetItem; default;
  end;

  { TProjectUserResources }

  TProjectUserResources = class(TAbstractProjectResource)
  private
    FList: TResourceList;
  public
    constructor Create; override;
    destructor Destroy; override;

    function UpdateResources(AResources: TAbstractProjectResources;
                             const MainFilename: string): Boolean; override;
    procedure WriteToProjectFile(AConfig: TXMLConfig; const Path: String); override;
    procedure ReadFromProjectFile(AConfig: TXMLConfig; const Path: String); override;
    property List: TResourceList read FList;
  end;

const
  ResourceTypeToStr: array[TUserResourceType] of String = (
 { rtIcon   } 'ICON',
 { rtCursor } 'CURSOR',
 { rtBitmap } 'BITMAP',
 { rtHTML   } 'HTML',
 { rtRCData } 'RCDATA'
  );

var
  OnAddIDEMessage: TAddIDEMessageEvent;

function StrToResourceType(const AStr: String): TUserResourceType;


implementation

function StrToResourceType(const AStr: String): TUserResourceType;
begin
  case AStr of
    'ICON': Result := rtIcon;
    'CURSOR': Result := rtCursor;
    'BITMAP': Result := rtBitmap;
    'HTML': Result := rtHTML;
  else
    Result := rtRCData;
  end;
end;

{ TResourceItem }

procedure TResourceItem.ReadFromProjectFile(AConfig: TXMLConfig; const Path: String);
begin
  FileName := AConfig.GetValue(Path + 'FileName', '');
  ResType := StrToResourceType(AConfig.GetValue(Path + 'Type', ''));
  ResName := AConfig.GetValue(Path + 'ResourceName', '');
end;

procedure TResourceItem.WriteToProjectFile(AConfig: TXMLConfig; const Path: String);
begin
  AConfig.SetValue(Path + 'FileName', FileName);
  AConfig.SetValue(Path + 'Type', ResourceTypeToStr[ResType]);
  AConfig.SetValue(Path + 'ResourceName', ResName);
end;

function TResourceItem.GetFileStream(const RealFileName: String): TStream;
var
  Age: Int64;
  FileStream: TFileStream;
begin
  Age := FileAgeCached(RealFileName);
  if Age = -1 then
  begin
    // File does not exist -> drop cache and signal "no data".
    FCacheValid := False;
    Exit(nil);
  end;
  if (not FCacheValid) or (FCacheRealFileName <> RealFileName)
  or (FCacheFileAge <> Age) then
  begin
    // The file is new or has changed since last read -> (re)load it.
    FileStream := TFileStream.Create(UTF8ToSys(RealFileName), fmOpenRead or fmShareDenyWrite);
    try
      SetLength(FCacheData, FileStream.Size);
      if Length(FCacheData) > 0 then
        FileStream.ReadBuffer(FCacheData[0], Length(FCacheData));
    finally
      FileStream.Free;
    end;
    FCacheRealFileName := RealFileName;
    FCacheFileAge := Age;
    FCacheValid := True;
  end;
  // Shares the cached byte array (reference counted, no copy).
  Result := TBytesStream.Create(FCacheData);
end;

function TResourceItem.CreateResource(const ProjectDirectory: String): TAbstractResource;
var
  Stream: TStream;
  TypeDesc, NameDesc: TResourceDesc;
  RealFileName: String;
begin
  Result := nil;

  RealFileName := GetRealFileName(ProjectDirectory);

  Stream := GetFileStream(RealFileName);
  if Stream <> nil then
  begin
    try
      NameDesc := TResourceDesc.Create(ResName);
      case ResType of
        rtIcon:
          begin
            Result := TGroupIconResource.Create(nil, NameDesc);
            TGroupResource(Result).ItemData.CopyFrom(Stream, Stream.Size)
          end;
        rtCursor:
          begin
            Result := TGroupCursorResource.Create(nil, NameDesc);
            TGroupResource(Result).ItemData.CopyFrom(Stream, Stream.Size)
          end;
        rtBitmap:
          begin
            Result := TBitmapResource.Create(nil, NameDesc);
            TBitmapResource(Result).BitmapData.CopyFrom(Stream, Stream.Size);
          end;
        rtHTML:
          begin
            TypeDesc := TResourceDesc.Create(RT_HTML);
            Result := TGenericResource.Create(TypeDesc, NameDesc);
            TypeDesc.Free;
            TGenericResource(Result).RawData.CopyFrom(Stream, Stream.Size)
          end;
        rtRCData:
          begin
            TypeDesc := TResourceDesc.Create(RT_RCDATA);
            Result := TGenericResource.Create(TypeDesc, NameDesc);
            TypeDesc.Free;
            TGenericResource(Result).RawData.CopyFrom(Stream, Stream.Size)
          end;
      end;
      NameDesc.Free;
    finally
      Stream.Free;
    end;
  end
  else if Assigned(OnAddIDEMessage) then
    OnAddIDEMessage(mluError, Format(lisFileNotFound2,[FileName]));
end;

function TResourceItem.GetRealFileName(const ProjectDirectory: String): String;
begin
  Result := FileName;
  if not IDEMacros.SubstituteMacros(Result) then
    debugln(['TResourceItem.GetRealFileName failed FileName="', FileName, '"']);
  Result := TrimFilename(Result);
  ForcePathDelims(Result);
  if not FilenameIsAbsolute(Result) then
    Result := TrimFilename(AppendPathDelim(ProjectDirectory) + Result);
end;

{ TResourceList }

function TResourceList.GetItem(AIndex: Integer): TResourceItem;
begin
  Result := TResourceItem(inherited Items[AIndex]);
end;

function TResourceList.IndexOfFileName(const AFileName: String): Integer;
begin
  for Result := 0 to Count - 1 do
    if CompareFilenames(Items[Result].FileName, AFileName) = 0 then
      Exit;
  Result := -1;
end;

function TResourceList.AddItem: TResourceItem;
begin
  Result := TResourceItem.Create;
  Add(Result);
end;

procedure TResourceList.AddResource(const FileName: String;
  ResType: TUserResourceType; const ResName: String);
var
  Data: TResourceItem;
begin
  Data := AddItem;
  Data.FileName := FileName;
  Data.ResType := ResType;
  Data.ResName := ResName;
end;

function TProjectUserResources.UpdateResources(AResources: TAbstractProjectResources; const MainFilename: string): Boolean;
var
  I: Integer;
  ARes: TAbstractResource;
  ProjectDirectory: String;
begin
  Result := True;
  ProjectDirectory := ExtractFilePath(MainFileName);
  for I := 0 to List.Count - 1 do
  begin
    ARes := List[I].CreateResource(ProjectDirectory);
    if Assigned(ARes) then
      AResources.AddSystemResource(ARes);
  end;
end;

procedure TProjectUserResources.WriteToProjectFile(AConfig: TXMLConfig; const Path: String);
var
  I: Integer;
begin
  AConfig.SetDeleteValue(Path+'General/Resources/Count', List.Count, 0);
  for I := 0 to List.Count - 1 do
    List[I].WriteToProjectFile(TXMLConfig(AConfig), Path + 'General/Resources/Resource_' + IntToStr(I) + '/')
end;

procedure TProjectUserResources.ReadFromProjectFile(AConfig: TXMLConfig; const Path: String);
var
  I, Count: Integer;
begin
  List.Clear;
  Count := AConfig.GetValue(Path+'General/Resources/Count', 0);
  for I := 0 to Count - 1 do
    List.AddItem.ReadFromProjectFile(TXMLConfig(AConfig), Path + 'General/Resources/Resource_' + IntToStr(I) + '/')
end;

constructor TProjectUserResources.Create;
begin
  inherited Create;
  FList := TResourceList.Create;
end;

destructor TProjectUserResources.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

initialization
  RegisterProjectResource(TProjectUserResources);

end.

