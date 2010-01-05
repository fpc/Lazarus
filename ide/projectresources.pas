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
  CodeToolManager, CodeCache, resource, reswriter;

type
  { TProjectResources }

  TProjectResources = class(TAbstractProjectResources)
  private
    FModified: Boolean;
    FOnModified: TNotifyEvent;
    FInModified: Boolean;
    FLrsIncludeAllowed: Boolean;

    FSystemResources: TResources;
    FLazarusResources: TStringList;

    resFileName: String;
    lrsFileName: String;
    LastResFilename: String;
    LastLrsFileName: String;

    FVersionInfo: TProjectVersionInfo;
    FXPManifest: TProjectXPManifest;
    FProjectIcon: TProjectIcon;

    procedure SetFileNames(const MainFileName, TestDir: String);
    procedure SetModified(const AValue: Boolean);
    procedure EmbeddedObjectModified(Sender: TObject);
    function Update: Boolean;
    function UpdateMainSourceFile(const AFileName: string): Boolean;
    procedure UpdateFlagLrsIncludeAllowed(const AFileName: string);
    function Save(SaveToTestDir: string): Boolean;
    procedure UpdateCodeBuffers;
    procedure DeleteLastCodeBuffers;
  protected
    procedure SetResourceType(const AValue: TResourceType); override;
  public
    constructor Create(AProject: TLazProject); override;
    destructor Destroy; override;

    procedure AddSystemResource(AResource: TAbstractResource); override;
    procedure AddLazarusResource(AResource: TStream; const AResourceName, AResourceType: String); override;

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
  end;

procedure ParseResourceType(const Src: string; NestedComments: boolean;
  out HasLRSIncludeDirective, HasRDirective: boolean);
function GuessResourceType(Code: TCodeBuffer; out Typ: TResourceType): boolean;

const
  ResourceTypeNames: array[TResourceType] of string = (
    'lrs',
    'res'
  );
function StrToResourceType(const s: string): TResourceType;

implementation

const
  LazResourcesUnit = 'LResources';

function StrToResourceType(const s: string): TResourceType;
var
  t: TResourceType;
begin
  for t := Low(TResourceType) to High(TResourceType) do
    if SysUtils.CompareText(ResourceTypeNames[t], s) = 0 then exit(t);
  Result := rtLRS;
end;

procedure ParseResourceType(const Src: string; NestedComments: boolean;
  out HasLRSIncludeDirective, HasRDirective: boolean);
var
  p: Integer;
  d: PChar;
  PointPos: PChar;
begin
  HasLRSIncludeDirective := False;
  HasRDirective := False;
  p:=1;
  while p < length(Src) do
  begin
    p := FindNextCompilerDirective(Src, p, NestedComments);
    if p > length(Src) then break;
    d := @Src[p];
    if (d[0]='{') and (d[1]='$') then
    begin
      inc(d, 2);
      if (d[0] in ['r','R']) and (not IsIdentChar[d[1]]) then
      begin
        // using resources
        HasRDirective := True;
      end
      else
      if (d[0] in ['i','I']) and ((d[1] in [' ',#9]) or
         (CompareIdentifiers(@d[0],'include')=0)) then
      begin
        PointPos := nil;
        while not (d^ in [#0,#10,#13,'}']) do
        begin
          if d^ = '.' then
            PointPos := d;
          inc(d);
        end;
        if (PointPos<>nil) and (d-PointPos=4) and (PointPos[1]='l') and
           (PointPos[2]='r') and (PointPos[3]='s') then
        begin
          // using include directive with lrs file
          HasLRSIncludeDirective := True;
        end;
      end;
    end;
    p := FindCommentEnd(Src, p, NestedComments);
  end;
end;

type
  TResourceTypesCacheItem = class
  public
    Code: TCodeBuffer;
    CodeStamp: integer;
    HasLRSIncludeDirective: boolean;
    HasRDirective: boolean;
  end;

function CompareResTypCacheItems(Data1, Data2: Pointer): integer;
var
  Item1: TResourceTypesCacheItem absolute Data1;
  Item2: TResourceTypesCacheItem absolute Data2;
begin
  Result:=CompareFilenames(Item1.Code.Filename,Item2.Code.Filename);
end;

function CompareCodeWithResTypCacheItem(CodeBuf, CacheItem: Pointer): integer;
var
  Code: TCodeBuffer absolute CodeBuf;
  Item: TResourceTypesCacheItem absolute CacheItem;
begin
  Result:=CompareFilenames(Code.Filename,Item.Code.Filename);
end;

type

  { TResourceTypesCache }

  TResourceTypesCache = class
  public
    Tree: TAvgLvlTree; //
    constructor Create;
    destructor Destroy; override;
    procedure Parse(Code: TCodeBuffer;
                    out HasLRSIncludeDirective, HasRDirective: boolean);
  end;

{ TResourceTypesCache }

constructor TResourceTypesCache.Create;
begin
  Tree:=TAvgLvlTree.Create(@CompareResTypCacheItems);
end;

destructor TResourceTypesCache.Destroy;
begin
  Tree.FreeAndClear;
  FreeAndNil(Tree);
  inherited Destroy;
end;

procedure TResourceTypesCache.Parse(Code: TCodeBuffer; out
  HasLRSIncludeDirective, HasRDirective: boolean);
var
  Node: TAvgLvlTreeNode;
  Item: TResourceTypesCacheItem;
begin
  Node := Tree.FindKey(Code, @CompareCodeWithResTypCacheItem);
  if (Node <> nil) then
  begin
    Item := TResourceTypesCacheItem(Node.Data);
    if (Item.CodeStamp = Item.Code.ChangeStep) then
    begin
      // cache valid
      HasLRSIncludeDirective := Item.HasLRSIncludeDirective;
      HasRDirective := Item.HasRDirective;
      exit;
    end;
  end
  else
    Item := nil;
  // update
  if Item = nil then
  begin
    Item := TResourceTypesCacheItem.Create;
    Item.Code := Code;
    Tree.Add(Item);
  end;
  Item.CodeStamp := Code.ChangeStep;
  ParseResourceType(Code.Source,
    CodeToolBoss.GetNestedCommentsFlagForFile(Code.Filename),
    Item.HasLRSIncludeDirective, Item.HasRDirective);
end;

var
  ResourceTypesCache: TResourceTypesCache = nil;

function GuessResourceType(Code: TCodeBuffer; out Typ: TResourceType): boolean;
var
  HasLRSIncludeDirective, HasRDirective: Boolean;
begin
  if ResourceTypesCache = nil then
    ResourceTypesCache := TResourceTypesCache.Create;
  ResourceTypesCache.Parse(Code, HasLRSIncludeDirective, HasRDirective);
  if HasLRSIncludeDirective then
  begin
    Typ := rtLRS;
    Result := True;
  end
  else
  if HasRDirective then
  begin
    Typ := rtRes;
    Result := True;
  end
  else
  begin
    Typ := rtLRS;
    Result := False;
  end;
end;

{ TProjectResources }

procedure TProjectResources.SetFileNames(const MainFileName, TestDir: String);
begin
  // rc is in the executable dir
  //resFileName := TestDir + ExtractFileNameOnly(MainFileName) + '.rc';

  // res is in the project dir for now because {$R project1.res} searches only in unit dir
  // lrs is in the project dir also
  if FileNameIsAbsolute(MainFileName) then
  begin
    resFileName := ChangeFileExt(MainFileName, '.res');
    lrsFileName := ChangeFileExt(MainFileName, '.lrs');
  end
  else
  begin
    resFileName := TestDir + ExtractFileNameOnly(MainFileName) + '.res';
    lrsFileName := TestDir + ExtractFileNameOnly(MainFileName) + '.lrs';
  end;
end;

procedure TProjectResources.SetResourceType(const AValue: TResourceType);
begin
  if ResourceType <> AValue then
  begin
    inherited SetResourceType(AValue);
    Modified := True;
  end;
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
  Result := VersionInfo.UpdateResources(Self, resFileName);
  if not Result then
    Exit;

  // handle manifest
  Result := XPManifest.UpdateResources(Self, resFileName);
  if not Result then
    Exit;

  // handle project icon
  Result := ProjectIcon.UpdateResources(Self, resFileName);
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

  FSystemResources := TResources.Create;
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

procedure TProjectResources.AddSystemResource(AResource: TAbstractResource);
begin
  FSystemResources.Add(AResource);
end;

procedure TProjectResources.AddLazarusResource(AResource: TStream;
  const AResourceName, AResourceType: String);
var
  OutStream: TStringStream;
begin
  OutStream := TStringStream.Create('');
  try
    BinaryToLazarusResourceCode(AResource, OutStream, AResourceName, AResourceType);
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
  LastResFilename := resFileName;
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
    SetDeleteValue(Path+'General/ResourceType/Value', ResourceTypeNames[ResourceType], ResourceTypeNames[rtLRS]);
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

    ResourceType := StrToResourceType(GetValue(Path+'General/ResourceType/Value', ResourceTypeNames[rtLRS]));
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
  Filename, Directive: String;
  NamePos, InPos: integer;
begin
  Result := True;

  CodeBuf := CodeToolBoss.LoadFile(AFilename, False, False);
  if CodeBuf <> nil then
  begin
    SetFileNames(AFileName, '');
    Filename := ExtractFileName(resFileName);
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
      Directive := '{$R '+Filename+'}';
      if not CodeToolBoss.AddResourceDirective(CodeBuf, Filename, false, Directive) then
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

function TProjectResources.RenameDirectives(const CurFileName, NewFileName: String): Boolean;
var
  NewX, NewY, NewTopLine: integer;
  CodeBuf, NewCode: TCodeBuffer;

  Directive,
  oldResFileName, oldLrsFileName,
  newResFilename, newLrsFileName: String;
begin
  //DebugLn(['TProjectResources.RenameDirectives CurFileName="',CurFileName,'" NewFileName="',NewFileName,'"']);
  Result := True;

  CodeBuf := CodeToolBoss.LoadFile(CurFileName, False, False);
  if CodeBuf = nil then
    Exit;

  LastResFilename := resFileName;
  LastLrsFileName := lrsFileName;
  try
    SetFileNames(CurFileName, '');
    oldResFilename := ExtractFileName(resFileName);
    oldLrsFileName := ExtractFileName(lrsFileName);
    SetFileNames(NewFileName, '');
    newResFilename := ExtractFileName(resFileName);
    newLrsFileName := ExtractFileName(lrsFileName);

    // update resources (FLazarusResources, FSystemResources, ...)
    UpdateFlagLrsIncludeAllowed(CurFileName);
    if not Update then
      Exit;
    // update codebuffers of new .lrs and .res files
    UpdateCodeBuffers;

    // update {$R filename} directive
    if CodeToolBoss.FindResourceDirective(CodeBuf, 1, 1,
                               NewCode, NewX, NewY,
                               NewTopLine, oldResFileName, false) then
    begin
      // there is a resource directive in the source
      if not CodeToolBoss.RemoveDirective(NewCode, NewX, NewY, true) then
      begin
        Result := False;
        debugln(['TProjectResources.RenameDirectives failed: removing resource directive']);
        Messages.Add('Could not remove "{$R '+ oldResFileName +'"} from main source!');
      end;
      Directive := '{$R '+ newResFileName +'}';
      if not CodeToolBoss.AddResourceDirective(CodeBuf, newResFileName, false, Directive) then
      begin
        Result := False;
        debugln(['TProjectResources.RenameDirectives failed: adding resource directive']);
        Messages.Add('Could not add "{$R '+ newResFileName +'"} to main source!');
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
  DeleteBuffer(resFileName);
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
      Exit(True);
    if not CodeBuf.IsVirtual then
    begin
      Result := SaveCodeBuffer(CodeBuf) in [mrOk,mrIgnore];
    end else if SaveToTestDir<>'' then
    begin
      TestFilename := AppendPathDelim(SaveToTestDir) + CodeBuf.Filename;
      Result := SaveCodeBufferToFile(CodeBuf, TestFilename) in [mrOk, mrIgnore];
    end;
  end;

begin
  Result := False;
  if not SaveCodeBuf(resFileName) then Exit;
  if not SaveCodeBuf(lrsFileName) then Exit;
  Result := True;
end;

procedure TProjectResources.UpdateCodeBuffers;
var
  CodeBuf: TCodeBuffer;
  S: TStream;
  Writer: TAbstractResourceWriter;
begin
  if HasSystemResources then
  begin
    CodeBuf := CodeToolBoss.CreateFile(resFileName);
    S := TMemoryStream.Create;
    Writer := TResResourceWriter.Create;
    try
      FSystemResources.WriteToStream(S, Writer);
      S.Position := 0;
      CodeBuf.LoadFromStream(S);
    finally
      Writer.Free;
      S.Free;
    end;
  end
  else
  if FilenameIsAbsolute(resFileName) and FileExistsUTF8(resFileName) then
    DeleteFileInteractive(resFileName,[mbRetry]);
  if FLrsIncludeAllowed and HasLazarusResources then
  begin
    CodeBuf := CodeToolBoss.CreateFile(lrsFileName);
    CodeBuf.Source := FLazarusResources.Text;
  end;
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
  CleanCodeBuffer(LastResFilename, resFileName);
  CleanCodeBuffer(LastLrsFileName, lrsFileName);
end;

finalization
  ResourceTypesCache.Free;

end.

