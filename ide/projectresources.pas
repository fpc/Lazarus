{
 /***************************************************************************
                    projectresources.pas  -  Lazarus IDE unit
                    -----------------------------------------

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

 Abstract: Project Resources - is a list of System and Lazarus resources.
 Currently it contains:
   - Version information
   - XP manifest
   - Project icon
}
unit ProjectResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LCLProc, LResources, FileUtil, Laz_XMLCfg,
  Dialogs, ProjectIntf, ProjectResourcesIntf, LazarusIDEStrConsts, AvgLvlTree,
  KeywordFuncLists, BasicCodeTools,
  W32VersionInfo, W32Manifest, ProjectIcon, IDEProcs, DialogProcs,
  CodeToolManager, CodeCache;

type
  TLFMResourceType = (
    lfmrtLRS,
    lfmrtRes
    );

  { TProjectResources }

  TProjectResources = class(TAbstractProjectResources)
  private
    FLFMResourceType: TLFMResourceType;
    FModified: Boolean;
    FOnModified: TNotifyEvent;
    FInModified: Boolean;
    FLrsIncludeAllowed: Boolean;

    FSystemResources: TStringList;
    FLazarusResources: TStringList;

    rcFileName: String;
    lrsFileName: String;
    LastrcFilename: String;
    LastLrsFileName: String;

    FVersionInfo: TProjectVersionInfo;
    FXPManifest: TProjectXPManifest;
    FProjectIcon: TProjectIcon;

    procedure SetFileNames(const MainFileName, TestDir: String);
    procedure SetLFMResourceType(const AValue: TLFMResourceType);
    procedure SetModified(const AValue: Boolean);
    procedure EmbeddedObjectModified(Sender: TObject);
    function Update: Boolean;
    function UpdateMainSourceFile(const AFileName: string): Boolean;
    procedure UpdateFlagLrsIncludeAllowed(const AFileName: string);
    function Save(SaveToTestDir: string): Boolean;
    procedure UpdateCodeBuffers;
    procedure DeleteLastCodeBuffers;
  public
    constructor Create(AProject: TLazProject); override;
    destructor Destroy; override;

    procedure AddSystemResource(const AResource: String); override;
    procedure AddLazarusResource(AResource: TStream; const ResourceName, ResourceType: String); override;

    procedure DoBeforeBuild(SaveToTestDir: boolean);
    procedure Clear;
    function Regenerate(const MainFileName: String;
                        UpdateSource, PerformSave: boolean;
                        SaveToTestDir: string): Boolean;
    function RenameDirectives(const CurFileName, NewFileName: String): Boolean;
    procedure DeleteResourceBuffers;

    function HasSystemResources: Boolean;
    function HasLazarusResources: Boolean;

    procedure WriteToProjectFile(AConfig: TXMLConfig; Path: String);
    procedure ReadFromProjectFile(AConfig: TXMLConfig; Path: String);

    property VersionInfo: TProjectVersionInfo read FVersionInfo;
    property XPManifest: TProjectXPManifest read FXPManifest;
    property ProjectIcon: TProjectIcon read FProjectIcon;

    property Modified: Boolean read FModified write SetModified;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;

    property LFMResourceType: TLFMResourceType read FLFMResourceType write SetLFMResourceType;
  end;

procedure ParseLFMResourceType(const Src: string; NestedComments: boolean;
  out HasLRSIncludeDirective, HasRDirective: boolean);
function GuessLFMResourceType(Code: TCodeBuffer; out Typ: TLFMResourceType
  ): boolean;

const
  LFMResourceTypeNames: array[TLFMResourceType] of string = (
    'lrs',
    'res'
    );
function StrToLFMResourceType(const s: string): TLFMResourceType;

implementation

const
  LazResourcesUnit = 'LResources';

function StrToLFMResourceType(const s: string): TLFMResourceType;
var
  t: TLFMResourceType;
begin
  for t:=Low(TLFMResourceType) to high(TLFMResourceType) do
    if SysUtils.CompareText(LFMResourceTypeNames[t],s)=0 then exit(t);
  Result:=lfmrtLRS;
end;

procedure ParseLFMResourceType(const Src: string; NestedComments: boolean;
  out HasLRSIncludeDirective, HasRDirective: boolean);
var
  p: Integer;
  d: PChar;
  PointPos: PChar;
begin
  HasLRSIncludeDirective:=false;
  HasRDirective:=false;
  p:=1;
  while p<length(Src) do begin
    p:=FindNextCompilerDirective(Src,p,NestedComments);
    if p>length(Src) then break;
    d:=@Src[p];
    if (d[0]='{') and (d[1]='$') then begin
      inc(d,2);
      if (d[0] in ['r','R']) and (not IsIdentChar[d[1]]) then begin
        // using resources
        HasRDirective:=true;
      end else if (d[0] in ['i','I'])
        and ((d[1] in [' ',#9]) or (CompareIdentifiers(@d[0],'include')=0))
      then begin
        PointPos:=nil;
        while not (d^ in [#0,#10,#13,'}']) do begin
          if d^='.' then
            PointPos:=d;
          inc(d);
        end;
        if (PointPos<>nil) and (d-PointPos=4)
        and (PointPos[1]='l') and (PointPos[2]='r') and (PointPos[3]='s') then
        begin
          // using include directive with lrs file
          HasLRSIncludeDirective:=true;
        end;
      end;
    end;
    p:=FindCommentEnd(Src,p,NestedComments);
  end;
end;

type
  TLFMResourceTypesCacheItem = class
  public
    Code: TCodeBuffer;
    CodeStamp: integer;
    HasLRSIncludeDirective: boolean;
    HasRDirective: boolean;
  end;

function CompareLFMResTypCacheItems(Data1, Data2: Pointer): integer;
var
  Item1: TLFMResourceTypesCacheItem absolute Data1;
  Item2: TLFMResourceTypesCacheItem absolute Data2;
begin
  Result:=CompareFilenames(Item1.Code.Filename,Item2.Code.Filename);
end;

function CompareCodeWithLFMResTypCacheItem(CodeBuf, CacheItem: Pointer): integer;
var
  Code: TCodeBuffer absolute CodeBuf;
  Item: TLFMResourceTypesCacheItem absolute CacheItem;
begin
  Result:=CompareFilenames(Code.Filename,Item.Code.Filename);
end;

type

  { TLFMResourceTypesCache }

  TLFMResourceTypesCache = class
  public
    Tree: TAvgLvlTree; //
    constructor Create;
    destructor Destroy; override;
    procedure Parse(Code: TCodeBuffer;
                    out HasLRSIncludeDirective, HasRDirective: boolean);
  end;

{ TLFMResourceTypesCache }

constructor TLFMResourceTypesCache.Create;
begin
  Tree:=TAvgLvlTree.Create(@CompareLFMResTypCacheItems);
end;

destructor TLFMResourceTypesCache.Destroy;
begin
  Tree.FreeAndClear;
  FreeAndNil(Tree);
  inherited Destroy;
end;

procedure TLFMResourceTypesCache.Parse(Code: TCodeBuffer; out
  HasLRSIncludeDirective, HasRDirective: boolean);
var
  Node: TAvgLvlTreeNode;
  Item: TLFMResourceTypesCacheItem;
begin
  Node:=Tree.FindKey(Code,@CompareCodeWithLFMResTypCacheItem);
  if (Node<>nil) then begin
    Item:=TLFMResourceTypesCacheItem(Node.Data);
    if (Item.CodeStamp=Item.Code.ChangeStep) then begin
      // cache valid
      HasLRSIncludeDirective:=Item.HasLRSIncludeDirective;
      HasRDirective:=Item.HasRDirective;
      exit;
    end;
  end else
    Item:=nil;
  // update
  if Item=nil then begin
    Item:=TLFMResourceTypesCacheItem.Create;
    Item.Code:=Code;
    Tree.Add(Item);
  end;
  Item.CodeStamp:=Code.ChangeStep;
  ParseLFMResourceType(Code.Source,
    CodeToolBoss.GetNestedCommentsFlagForFile(Code.Filename),
    Item.HasLRSIncludeDirective,Item.HasRDirective);
end;

var
  LFMResourceTypesCache: TLFMResourceTypesCache = nil;

function GuessLFMResourceType(Code: TCodeBuffer; out Typ: TLFMResourceType
  ): boolean;
var
  HasLRSIncludeDirective,HasRDirective: Boolean;
begin
  if LFMResourceTypesCache=nil then
    LFMResourceTypesCache:=TLFMResourceTypesCache.Create;
  LFMResourceTypesCache.Parse(Code,HasLRSIncludeDirective,HasRDirective);
  if HasLRSIncludeDirective then begin
    Typ:=lfmrtLRS;
    Result:=true;
  end
  else if HasRDirective then begin
    Typ:=lfmrtRes;
    Result:=true;
  end
  else begin
    Typ:=lfmrtLRS;
    Result:=false;
  end;
end;

{ TProjectResources }

procedure TProjectResources.SetFileNames(const MainFileName, TestDir: String);
begin
  // rc is in the exectable dir
  //rcFileName := TestDir + ExtractFileNameOnly(MainFileName) + '.rc';

  // rc is in the project dir for now because {$R project1.rc} searches only in unit dir
  // lrs is in the project dir also
  if FileNameIsAbsolute(MainFileName) then
  begin
    rcFileName := ChangeFileExt(MainFileName, '.rc');
    lrsFileName := ChangeFileExt(MainFileName, '.lrs');
  end
  else
  begin
    rcFileName := TestDir + ExtractFileNameOnly(MainFileName) + '.rc';
    lrsFileName := TestDir + ExtractFileNameOnly(MainFileName) + '.lrs';
  end;
end;

procedure TProjectResources.SetLFMResourceType(const AValue: TLFMResourceType);
begin
  if FLFMResourceType=AValue then exit;
  FLFMResourceType:=AValue;
  Modified:=true;
end;

procedure TProjectResources.SetModified(const AValue: Boolean);
begin
  if FInModified then
    Exit;
  FInModified := True;
  if FModified <> AValue then
  begin
    FModified := AValue;
    if not FModified then
    begin
      VersionInfo.Modified := False;
      XPManifest.Modified := False;
      ProjectIcon.Modified := False;
    end;
    if Assigned(FOnModified) then
      OnModified(Self);
  end;
  FInModified := False;
end;

function TProjectResources.Update: Boolean;
begin
  Clear;
  // handle versioninfo
  Result := VersionInfo.UpdateResources(Self, rcFileName);
  if not Result then
    Exit;

  // handle manifest
  Result := XPManifest.UpdateResources(Self, rcFileName);
  if not Result then
    Exit;

  // handle project icon
  Result := ProjectIcon.UpdateResources(Self, rcFileName);
end;

procedure TProjectResources.EmbeddedObjectModified(Sender: TObject);
begin
  Modified :=
    VersionInfo.Modified or
    XPManifest.Modified or
    ProjectIcon.Modified;
end;

constructor TProjectResources.Create(AProject: TLazProject);
begin
  inherited Create(AProject);

  FInModified := False;
  FLrsIncludeAllowed := False;

  FSystemResources := TStringList.Create;
  FLazarusResources := TStringList.Create;

  FVersionInfo := TProjectVersionInfo.Create;
  FVersionInfo.OnModified := @EmbeddedObjectModified;

  FXPManifest := TProjectXPManifest.Create;
  FXPManifest.UseManifest := True;
  FXPManifest.OnModified := @EmbeddedObjectModified;

  FProjectIcon := TProjectIcon.Create;
  FProjectIcon.OnModified := @EmbeddedObjectModified;
end;

destructor TProjectResources.Destroy;
begin
  DeleteResourceBuffers;

  FreeAndNil(FVersionInfo);
  FreeAndNil(FXPManifest);
  FreeAndNil(FProjectIcon);

  FreeAndNil(FSystemResources);
  FreeAndNil(FLazarusResources);

  inherited Destroy;
end;

procedure TProjectResources.AddSystemResource(const AResource: String);
begin
  FSystemResources.Add(AResource);
end;

procedure TProjectResources.AddLazarusResource(AResource: TStream;
  const ResourceName, ResourceType: String);
var
  OutStream: TStringStream;
begin
  OutStream := TStringStream.Create('');
  try
    BinaryToLazarusResourceCode(AResource, OutStream, ResourceName, ResourceType);
    FLazarusResources.Add(OutStream.DataString);
  finally
    OutStream.Free;
  end;
end;

procedure TProjectResources.DoBeforeBuild(SaveToTestDir: boolean);
begin
  VersionInfo.DoBeforeBuild(Self,SaveToTestDir);
  XPManifest.DoBeforeBuild(Self,SaveToTestDir);
  ProjectIcon.DoBeforeBuild(Self,SaveToTestDir);
end;

procedure TProjectResources.Clear;
begin
  FSystemResources.Clear;
  FLazarusResources.Clear;
  FMessages.Clear;
end;

function TProjectResources.Regenerate(const MainFileName: String;
  UpdateSource, PerformSave: boolean; SaveToTestDir: string): Boolean;
begin
  //DebugLn(['TProjectResources.Regenerate MainFilename=',MainFilename,' UpdateSource=',UpdateSource,' PerformSave=',PerformSave]);
  //DumpStack;
  Result := False;

  if (MainFileName = '') then
    Exit;

  // remember old codebuffer filenames
  LastrcFilename := rcFileName;
  LastLrsFileName := lrsFileName;
  SetFileNames(MainFileName, SaveToTestDir);

  UpdateFlagLrsIncludeAllowed(MainFileName);

  try
    // update resources (FLazarusResources, FSystemResources, ...)
    if not Update then
      Exit;
    // create codebuffers of new .lrs and .rc files
    UpdateCodeBuffers;
    // update .lpr file (old and new include files exist, so parsing should work without errors)
    if UpdateSource and not UpdateMainSourceFile(MainFileName) then
      Exit;

    if PerformSave and not Save(SaveToTestDir) then
      Exit;
  finally
    DeleteLastCodeBuffers;
  end;

  Result := True;
end;

function TProjectResources.HasSystemResources: Boolean;
begin
  Result := FSystemResources.Count > 0;
end;

function TProjectResources.HasLazarusResources: Boolean;
begin
  Result := FLazarusResources.Count > 0;
end;

procedure TProjectResources.WriteToProjectFile(AConfig: TXMLConfig; Path: String);
begin
  // todo: further split by classes
  with AConfig do
  begin
    SetDeleteValue(Path+'General/LFMResourceType/Value', LFMResourceTypeNames[LFMResourceType], LFMResourceTypeNames[lfmrtLRS]);
    SetDeleteValue(Path+'General/Icon/Value', BoolToStr(ProjectIcon.IsEmpty), '-1');
    SetDeleteValue(Path+'General/UseXPManifest/Value', XPManifest.UseManifest, False);
    SetDeleteValue(Path+'VersionInfo/UseVersionInfo/Value', VersionInfo.UseVersionInfo,false);
    SetDeleteValue(Path+'VersionInfo/AutoIncrementBuild/Value', VersionInfo.AutoIncrementBuild,false);
    SetDeleteValue(Path+'VersionInfo/CurrentVersionNr/Value', VersionInfo.VersionNr,0);
    SetDeleteValue(Path+'VersionInfo/CurrentMajorRevNr/Value', VersionInfo.MajorRevNr,0);
    SetDeleteValue(Path+'VersionInfo/CurrentMinorRevNr/Value', VersionInfo.MinorRevNr,0);
    SetDeleteValue(Path+'VersionInfo/CurrentBuildNr/Value', VersionInfo.BuildNr,0);
    SetDeleteValue(Path+'VersionInfo/ProjectVersion/Value', VersionInfo.ProductVersionString,'1.0.0.0');
    SetDeleteValue(Path+'VersionInfo/Language/Value', VersionInfo.HexLang,'0409');
    SetDeleteValue(Path+'VersionInfo/CharSet/Value', VersionInfo.HexCharSet,'04E4');
    SetDeleteValue(Path+'VersionInfo/Comments/Value', VersionInfo.CommentsString,'');
    SetDeleteValue(Path+'VersionInfo/CompanyName/Value', VersionInfo.CompanyString,'');
    SetDeleteValue(Path+'VersionInfo/FileDescription/Value', VersionInfo.DescriptionString,'');
    SetDeleteValue(Path+'VersionInfo/InternalName/Value', VersionInfo.InternalNameString,'');
    SetDeleteValue(Path+'VersionInfo/LegalCopyright/Value', VersionInfo.CopyrightString,'');
    SetDeleteValue(Path+'VersionInfo/LegalTrademarks/Value', VersionInfo.TrademarksString,'');
    SetDeleteValue(Path+'VersionInfo/OriginalFilename/Value', VersionInfo.OriginalFilenameString,'');
    SetDeleteValue(Path+'VersionInfo/ProductName/Value', VersionInfo.ProdNameString,'');
  end;
end;

procedure TProjectResources.ReadFromProjectFile(AConfig: TXMLConfig; Path: String);
begin
  // todo: further split by classes
  with AConfig do
  begin
    ProjectIcon.IcoFileName := ChangeFileExt(FileName, '.ico');

    LFMResourceType := StrToLFMResourceType(GetValue(Path+'General/LFMResourceType/Value', LFMResourceTypeNames[lfmrtLRS]));
    ProjectIcon.IsEmpty := StrToBoolDef(GetValue(Path+'General/Icon/Value', '-1'), False);
    XPManifest.UseManifest := GetValue(Path+'General/UseXPManifest/Value', False);
    VersionInfo.UseVersionInfo := GetValue(Path+'VersionInfo/UseVersionInfo/Value', False);
    VersionInfo.AutoIncrementBuild := GetValue(Path+'VersionInfo/AutoIncrementBuild/Value', False);
    VersionInfo.VersionNr := GetValue(Path+'VersionInfo/CurrentVersionNr/Value', 0);
    VersionInfo.MajorRevNr := GetValue(Path+'VersionInfo/CurrentMajorRevNr/Value', 0);
    VersionInfo.MinorRevNr := GetValue(Path+'VersionInfo/CurrentMinorRevNr/Value', 0);
    VersionInfo.BuildNr := GetValue(Path+'VersionInfo/CurrentBuildNr/Value', 0);
    VersionInfo.ProductVersionString := GetValue(Path+'VersionInfo/ProjectVersion/Value', '1.0.0.0');
    VersionInfo.HexLang := GetValue(Path+'VersionInfo/Language/Value', '0409');
    VersionInfo.HexCharSet := GetValue(Path+'VersionInfo/CharSet/Value', '04E4');
    VersionInfo.CommentsString := LineBreaksToSystemLineBreaks(GetValue(Path+'VersionInfo/Comments/Value', ''));
    VersionInfo.CompanyString := LineBreaksToSystemLineBreaks(GetValue(Path+'VersionInfo/CompanyName/Value', ''));
    VersionInfo.DescriptionString := LineBreaksToSystemLineBreaks(GetValue(Path+'VersionInfo/FileDescription/Value', ''));
    VersionInfo.InternalNameString := LineBreaksToSystemLineBreaks(GetValue(Path+'VersionInfo/InternalName/Value', ''));
    VersionInfo.CopyrightString := LineBreaksToSystemLineBreaks(GetValue(Path+'VersionInfo/LegalCopyright/Value', ''));
    VersionInfo.TrademarksString := LineBreaksToSystemLineBreaks(GetValue(Path+'VersionInfo/LegalTrademarks/Value', ''));
    VersionInfo.OriginalFilenameString := GetValue(Path+'VersionInfo/OriginalFilename/Value', '');
    VersionInfo.ProdNameString := LineBreaksToSystemLineBreaks(GetValue(Path+'VersionInfo/ProductName/Value', ''));
  end;
end;

function TProjectResources.UpdateMainSourceFile(const AFileName: string): Boolean;
var
  NewX, NewY, NewTopLine: integer;
  CodeBuf, NewCode: TCodeBuffer;
  Filename: String;
  NamePos, InPos: integer;
begin
  Result := True;

  CodeBuf := CodeToolBoss.LoadFile(AFilename, False, False);
  if CodeBuf <> nil then
  begin
    SetFileNames(AFileName, '');
    Filename := ExtractFileName(rcFileName);
    //debugln(['TProjectResources.UpdateMainSourceFile HasSystemResources=',HasSystemResources,' Filename=',Filename,' HasLazarusResources=',HasLazarusResources]);

    // update LResources uses
    if CodeToolBoss.FindUnitInAllUsesSections(CodeBuf, LazResourcesUnit, NamePos, InPos) then
    begin
      if not (FLrsIncludeAllowed and HasLazarusResources) then
      begin
        if not CodeToolBoss.RemoveUnitFromAllUsesSections(CodeBuf, LazResourcesUnit) then
        begin
          Result := False;
          Messages.Add(Format(lisCouldNotRemoveFromMainSource, ['"',
            LazResourcesUnit, '"']));
          debugln(['TProjectResources.UpdateMainSourceFile adding LResources to main source failed']);
        end;
      end;
    end
    else
    if FLrsIncludeAllowed and HasLazarusResources then
    begin
      if not CodeToolBoss.AddUnitToMainUsesSection(CodeBuf, LazResourcesUnit,'') then
      begin
        Result := False;
        Messages.Add(Format(lisCouldNotAddToMainSource, ['"', LazResourcesUnit,
          '"']));
        debugln(['TProjectResources.UpdateMainSourceFile adding LResources to main source failed']);
      end;
    end;

    // update {$R filename} directive
    if CodeToolBoss.FindResourceDirective(CodeBuf, 1, 1,
                               NewCode, NewX, NewY,
                               NewTopLine, Filename, false) then
    begin
      // there is a resource directive in the source
      if not HasSystemResources then
      begin
        if not CodeToolBoss.RemoveDirective(NewCode, NewX, NewY, true) then
        begin
          Result := False;
          Messages.Add(Format(lisCouldNotRemoveRFromMainSource, ['"', Filename,
            '"']));
          debugln(['TProjectResources.UpdateMainSourceFile failed: removing resource directive']);
        end;
      end;
    end
    else
    if HasSystemResources then
    begin
      if not CodeToolBoss.AddResourceDirective(CodeBuf,
        Filename, false, '{$IFDEF WINDOWS}{$R '+Filename+'}{$ENDIF}') then
      begin
        Result := False;
        Messages.Add(Format(lisCouldNotAddRToMainSource, ['"', Filename, '"']));
        debugln(['TProjectResources.UpdateMainSourceFile failed: adding resource directive']);
      end;
    end;

    // update {$I filename} directive
    Filename := ExtractFileName(lrsFileName);
    if CodeToolBoss.FindIncludeDirective(CodeBuf, 1, 1,
                               NewCode, NewX, NewY,
                               NewTopLine, Filename, false) then
    begin
      // there is a resource directive in the source
      //debugln(['TProjectResources.UpdateMainSourceFile include directive found: FCanHaveLrsInclude=',FLrsIncludeAllowed,' HasLazarusResources=',HasLazarusResources]);
      if not (FLrsIncludeAllowed and HasLazarusResources) then
      begin
        if not CodeToolBoss.RemoveDirective(NewCode, NewX, NewY, true) then
        begin
          Result := False;
          Messages.Add(Format(lisCouldNotRemoveIFromMainSource, ['"', Filename,
            '"']));
          debugln(['TProjectResources.UpdateMainSourceFile removing include directive from main source failed']);
          Exit;
        end;
      end;
    end
    else
    if FLrsIncludeAllowed and HasLazarusResources then
    begin
      //debugln(['TProjectResources.UpdateMainSourceFile include directive not found: FCanHaveLrsInclude=',FLrsIncludeAllowed,' HasLazarusResources=',HasLazarusResources]);
      if not CodeToolBoss.AddIncludeDirective(CodeBuf,
        Filename,'') then
      begin
        Result := False;
        Messages.Add(Format(lisCouldNotAddIToMainSource, ['"', Filename, '"']));
        debugln(['TProjectResources.UpdateMainSourceFile adding include directive to main source failed']);
        Exit;
      end;
    end;
  end;
end;

procedure TProjectResources.UpdateFlagLrsIncludeAllowed(const AFileName: string);
var
  CodeBuf: TCodeBuffer;
  NamePos, InPos: Integer;
begin
  FLrsIncludeAllowed := False;

  CodeBuf := CodeToolBoss.LoadFile(AFileName, False, False);
  if CodeBuf = nil then
    Exit;

  // Check that .lpr contains Forms and Interfaces in the uses section. If it does not
  // we cannot add LResources (it is not a lazarus application)
  CodeToolBoss.ActivateWriteLock;
  try
    FLrsIncludeAllowed :=
      CodeToolBoss.FindUnitInAllUsesSections(CodeBuf, 'Forms', NamePos, InPos, True) and
      CodeToolBoss.FindUnitInAllUsesSections(CodeBuf, 'Interfaces', NamePos, InPos, True);
  finally
    CodeToolBoss.DeactivateWriteLock;
  end;
end;

function TProjectResources.RenameDirectives(const CurFileName,
  NewFileName: String): Boolean;
var
  NewX, NewY, NewTopLine: integer;
  CodeBuf, NewCode: TCodeBuffer;

  oldRcFileName, oldLrsFileName,
  newRcFilename, newLrsFileName: String;
begin
  //DebugLn(['TProjectResources.RenameDirectives CurFileName="',CurFileName,'" NewFileName="',NewFileName,'"']);
  Result := True;

  CodeBuf := CodeToolBoss.LoadFile(CurFileName, False, False);
  if CodeBuf = nil then
    Exit;

  LastRcFilename := rcFileName;
  LastLrsFileName := lrsFileName;
  try
    SetFileNames(CurFileName, '');
    oldRcFilename := ExtractFileName(rcFileName);
    oldLrsFileName := ExtractFileName(lrsFileName);
    SetFileNames(NewFileName, '');
    newRcFilename := ExtractFileName(rcFileName);
    newLrsFileName := ExtractFileName(lrsFileName);

    // update resources (FLazarusResources, FSystemResources, ...)
    UpdateFlagLrsIncludeAllowed(CurFileName);
    if not Update then
      Exit;
    // update codebuffers of new .lrs and .rc files
    UpdateCodeBuffers;

    // update {$R filename} directive
    if CodeToolBoss.FindResourceDirective(CodeBuf, 1, 1,
                               NewCode, NewX, NewY,
                               NewTopLine, oldRcFileName, false) then
    begin
      // there is a resource directive in the source
      if not CodeToolBoss.RemoveDirective(NewCode, NewX, NewY, true) then
      begin
        Result := False;
        debugln(['TProjectResources.RenameDirectives failed: removing resource directive']);
        Messages.Add('Could not remove "{$R '+ oldRcFileName +'"} from main source!');
      end;
      if not CodeToolBoss.AddResourceDirective(CodeBuf,
        newRcFileName, false, '{$IFDEF WINDOWS}{$R '+ newRcFileName +'}{$ENDIF}') then
      begin
        Result := False;
        debugln(['TProjectResources.RenameDirectives failed: adding resource directive']);
        Messages.Add('Could not add "{$R '+ newRcFileName +'"} to main source!');
      end;
    end;

    // update {$I filename} directive
    if CodeToolBoss.FindIncludeDirective(CodeBuf, 1, 1,
                               NewCode, NewX, NewY,
                               NewTopLine, oldLrsFileName, false) then
    begin
      // there is a resource directive in the source
      if not CodeToolBoss.RemoveDirective(NewCode, NewX, NewY, true) then
      begin
        Result := False;
        debugln(['TProjectResources.RenameDirectives removing include directive from main source failed']);
        Messages.Add('Could not remove "{$I '+ oldLrsFileName +'"} from main source!');
        Exit;
      end;
      if not CodeToolBoss.AddIncludeDirective(CodeBuf, newLrsFileName, '') then
      begin
        Result := False;
        debugln(['TProjectResources.RenameDirectives adding include directive to main source failed']);
        Messages.Add('Could not add "{$I '+ newLrsFileName +'"} to main source!');
        Exit;
      end;
    end;
  finally
    DeleteLastCodeBuffers;
  end;
end;

procedure TProjectResources.DeleteResourceBuffers;

  procedure DeleteBuffer(Filename: string);
  var
    CodeBuf: TCodeBuffer;
  begin
    if Filename = '' then 
      Exit;
    CodeBuf := CodeToolBoss.FindFile(Filename);
    if CodeBuf <> nil then
      CodeBuf.IsDeleted := true;
  end;

begin
  DeleteLastCodeBuffers;
  DeleteBuffer(rcFileName);
  DeleteBuffer(lrsFileName);
end;

function TProjectResources.Save(SaveToTestDir: string): Boolean;

  function SaveCodeBuf(Filename: string): boolean;
  var
    CodeBuf: TCodeBuffer;
    TestFilename: String;
  begin
    CodeBuf := CodeToolBoss.FindFile(Filename);
    if (CodeBuf = nil) or CodeBuf.IsDeleted then
      Exit(true);
    if not CodeBuf.IsVirtual then
    begin
      Result := SaveCodeBuffer(CodeBuf) in [mrOk,mrIgnore];
    end else if SaveToTestDir<>'' then
    begin
      TestFilename:=AppendPathDelim(SaveToTestDir)+CodeBuf.Filename;
      Result := SaveCodeBufferToFile(CodeBuf,TestFilename) in [mrOk,mrIgnore];
    end;
  end;

begin
  Result := False;
  if not SaveCodeBuf(rcFileName) then Exit;
  if not SaveCodeBuf(lrsFileName) then Exit;
  Result := True;
end;

procedure TProjectResources.UpdateCodeBuffers;

  procedure UpdateCodeBuffer(NewFilename, Source: string);
  var
    CodeBuf: TCodeBuffer;
  begin
    CodeBuf := CodeToolBoss.CreateFile(NewFileName);
    CodeBuf.Source := Source;
  end;

begin
  if HasSystemResources then
    UpdateCodeBuffer(rcFileName, FSystemResources.Text)
  else if FilenameIsAbsolute(rcFileName) and FileExistsUTF8(rcFileName) then
    DeleteFileInteractive(rcFileName,[mbRetry]);
  if FLrsIncludeAllowed and HasLazarusResources then
    UpdateCodeBuffer(lrsFileName, FLazarusResources.Text);
end;

procedure TProjectResources.DeleteLastCodeBuffers;

  procedure CleanCodeBuffer(var OldFilename: string; const NewFilename: string);
  var
    CodeBuf: TCodeBuffer;
  begin
    if (OldFileName <> '') and (OldFilename <> NewFilename) then 
    begin
      // file was renamed => mark old file as deleted
      CodeBuf := CodeToolBoss.FindFile(OldFileName);
      if (CodeBuf <> nil) then
        CodeBuf.IsDeleted := true;
      OldFileName := '';
    end;
  end;

begin
  CleanCodeBuffer(LastrcFilename, rcFileName);
  CleanCodeBuffer(LastlrsFilename, lrsFileName);
end;

finalization
  LFMResourceTypesCache.Free;

end.

