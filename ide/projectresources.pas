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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
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
  // RTL + LCL
  Classes, SysUtils, resource, reswriter, fgl, Laz_AVL_Tree,
  // LCL
  Controls, LCLProc, LResources,
  // LazUtils
  LazFileUtils, Laz2_XMLCfg,
  // Codetools
  KeywordFuncLists, BasicCodeTools, CodeToolManager, CodeCache,
  // IdeIntf
  ProjectIntf, ProjectResourcesIntf, CompOptsIntf,
  // IDE
  LazarusIDEStrConsts, DialogProcs,
  W32Manifest, W32VersionInfo, ProjectIcon, ProjectUserResources;

type
  TResourceList = specialize TFPGObjectList<TAbstractProjectResource>;

  { TProjectResources }

  TProjectResources = class(TAbstractProjectResources)
  private
    FModified: Boolean;
    FOnModified: TNotifyEvent;
    FInModified: Boolean;
    FLrsIncludeAllowed: Boolean;

    FResources: TResourceList;
    FSystemResources: TResources;
    FLazarusResources: TStringList;

    resFileName: String;
    lrsFileName: String;
    LastResFileName: String;
    LastLrsFileName: String;

    function GetProjectIcon: TProjectIcon;
    function GetProjectUserResources: TProjectUserResources;
    function GetVersionInfo: TProjectVersionInfo;
    function GetXPManifest: TProjectXPManifest;
    procedure SetFileNames(const MainFileName, TestDir: String);
    procedure SetModified(const AValue: Boolean);
    function Update: Boolean;
    function UpdateMainSourceFile(const AFileName: string): Boolean;
    procedure UpdateFlagLrsIncludeAllowed(const AFileName: string);
    function Save(SaveToTestDir: string): Boolean;
    procedure UpdateCodeBuffers;
    procedure DeleteLastCodeBuffers;

    procedure OnResourceModified(Sender: TObject);
  protected
    procedure SetResourceType(const AValue: TResourceType); override;
    function GetProjectResource(AIndex: TAbstractProjectResourceClass): TAbstractProjectResource; override;
  public
    constructor Create(AProject: TLazProject); override;
    destructor Destroy; override;

    procedure AddSystemResource(AResource: TAbstractResource); override;
    procedure AddLazarusResource(AResource: TStream; const AResourceName, AResourceType: String); override;

    procedure DoAfterBuild(AReason: TCompileReason; SaveToTestDir: boolean);
    procedure DoBeforeBuild(AReason: TCompileReason; SaveToTestDir: boolean);
    procedure Clear;
    function Regenerate(const MainFileName: String;
                        UpdateSource, PerformSave: boolean;
                        const SaveToTestDir: string): Boolean;
    function RenameDirectives(const CurFileName, NewFileName: String): Boolean;
    procedure DeleteResourceBuffers;

    function HasSystemResources: Boolean;
    function HasLazarusResources: Boolean;

    procedure WriteToProjectFile(AConfig: TXMLConfig; const Path: String);
    procedure ReadFromProjectFile(AConfig: TXMLConfig; const Path: String; ReadAll: Boolean);

    property Modified: Boolean read FModified write SetModified;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;

    property XPManifest: TProjectXPManifest read GetXPManifest;
    property VersionInfo: TProjectVersionInfo read GetVersionInfo;
    property ProjectIcon: TProjectIcon read GetProjectIcon;
    property UserResources: TProjectUserResources read GetProjectUserResources;
  end;

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

procedure ParseResourceType(Code: TCodeBuffer; NestedComments: boolean;
  out HasLRSIncludeDirective, HasRDirective: boolean);

  function ExtractDirectiveFileName(ds: PChar): string;
  var i: Integer;
  begin
    while IsIdentChar[ds^] do Inc(ds);
    while ds^ in [' ',#9] do Inc(ds);
    if ds^ = '''' then
    begin
      Inc(ds);
      i := IndexChar(ds^, -1, '''');
      SetLength(Result, i);
      if i>0 then
        Move(ds^, Result[1], i);
    end else begin
      i := IndexChar(ds^, -1, '}');
      SetLength(Result, i);
      if i>0 then
        Move(ds^, Result[1], i);
      Result := TrimRight(Result);
    end;
  end;

var
  p: Integer;
  d: PChar;
  Src, dFileName: string;
begin
  Src := Code.Source;
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
      if (d[0] in ['r','R']) and not (HasRDirective or IsIdentChar[d[1]]) then
      begin
        // using resources
        dFileName := ExtractDirectiveFileName(d);
        HasRDirective := SameText(dFileName, '*.lfm') or
          SameText(dFileName, ExtractFileNameOnly(Code.Filename) + '.lfm');
      end
      else
      if (d[0] in ['i','I']) and not HasLRSIncludeDirective
      and ((d[1] in [' ',#9]) or (CompareIdentifiers(@d[0],'include')=0)) then
      begin
        // using include directive with lrs file
        dFileName := ExtractDirectiveFileName(d);
        HasLRSIncludeDirective :=
          SameText(dFileName, ExtractFileNameOnly(Code.Filename) + '.lrs') or
          SameText(dFileName, '*.lrs');
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
    Tree: TAvlTree; //
    constructor Create;
    destructor Destroy; override;
    procedure Parse(Code: TCodeBuffer;
                    out HasLRSIncludeDirective, HasRDirective: boolean);
  end;

{ TResourceTypesCache }

constructor TResourceTypesCache.Create;
begin
  Tree:=TAvlTree.Create(@CompareResTypCacheItems);
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
  Node: TAvlTreeNode;
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
  ParseResourceType(Code,
    CodeToolBoss.GetNestedCommentsFlagForFile(Code.Filename),
    Item.HasLRSIncludeDirective, Item.HasRDirective);
  HasLRSIncludeDirective := Item.HasLRSIncludeDirective;
  HasRDirective := Item.HasRDirective;
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
  //DebugLn(['GuessResourceType ',Code.Filename,' HasLRS=',HasLRSIncludeDirective,' HasR=',HasRDirective]);
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

function TProjectResources.GetProjectIcon: TProjectIcon;
begin
  Result := TProjectIcon(GetProjectResource(TProjectIcon));
end;

function TProjectResources.GetProjectUserResources: TProjectUserResources;
begin
  Result := TProjectUserResources(GetProjectResource(TProjectUserResources));
end;

function TProjectResources.GetVersionInfo: TProjectVersionInfo;
begin
  Result := TProjectVersionInfo(GetProjectResource(TProjectVersionInfo));
end;

function TProjectResources.GetXPManifest: TProjectXPManifest;
begin
  Result := TProjectXPManifest(GetProjectResource(TProjectXPManifest));
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
var
  i: integer;
begin
  if FInModified then
    Exit;
  FInModified := True;
  if FModified <> AValue then
  begin
    FModified := AValue;
    if not FModified then
      for i := 0 to FResources.Count - 1 do
        FResources[i].Modified := False;
    if Assigned(FOnModified) then
      OnModified(Self);
  end;
  FInModified := False;
end;

function TProjectResources.Update: Boolean;
var
  i: integer;
begin
  Result:=true;
  Clear;
  for i := 0 to FResources.Count - 1 do
  begin
    Result := FResources[i].UpdateResources(Self, resFileName);
    if not Result then begin
      debugln(['TProjectResources.Update UpdateResources of ',DbgSName(FResources[i]),' failed']);
      Exit;
    end;
  end;
end;

procedure TProjectResources.OnResourceModified(Sender: TObject);
begin
  Modified := Modified or TAbstractProjectResource(Sender).Modified;
end;

constructor TProjectResources.Create(AProject: TLazProject);
var
  i: integer;
  L: TList;
  R: TAbstractProjectResource;
begin
  inherited Create(AProject);
  inherited SetResourceType(rtRes); // set fpc resources by default

  FInModified := False;
  FLrsIncludeAllowed := False;

  FSystemResources := TResources.Create;
  FLazarusResources := TStringList.Create;

  FResources := TResourceList.Create;
  L := GetRegisteredResources;
  for i := 0 to L.Count - 1 do
  begin
    R := TAbstractProjectResourceClass(L[i]).Create;
    R.Modified := False;
    R.OnModified := @OnResourceModified;
    FResources.Add(R);
  end;
end;

destructor TProjectResources.Destroy;
begin
  DeleteResourceBuffers;

  FreeAndNil(FResources);
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

function TProjectResources.GetProjectResource(AIndex: TAbstractProjectResourceClass): TAbstractProjectResource;
var
  i: integer;
begin
  for i := 0 to FResources.Count - 1 do
  begin
    Result := FResources[i];
    if Result.InheritsFrom(AIndex) then
      Exit;
  end;
  Result := nil;
end;

procedure TProjectResources.DoAfterBuild(AReason: TCompileReason; SaveToTestDir: boolean);
var
  i: integer;
begin
  for i := 0 to FResources.Count - 1 do
    FResources[i].DoAfterBuild(Self, AReason, SaveToTestDir);
end;

procedure TProjectResources.DoBeforeBuild(AReason: TCompileReason; SaveToTestDir: boolean);
var
  i: integer;
begin
  for i := 0 to FResources.Count - 1 do
    FResources[i].DoBeforeBuild(Self, AReason, SaveToTestDir);
end;

procedure TProjectResources.Clear;
begin
  FSystemResources.Clear;
  FLazarusResources.Clear;
  FMessages.Clear;
end;

function TProjectResources.Regenerate(const MainFileName: String; UpdateSource,
  PerformSave: boolean; const SaveToTestDir: string): Boolean;
begin
  //DebugLn(['TProjectResources.Regenerate MainFilename=',MainFilename,
  //         ' UpdateSource=',UpdateSource,' PerformSave=',PerformSave]);
  //DumpStack;
  Result := False;

  if (MainFileName = '') then
    Exit(true);

  // remember old codebuffer filenames
  LastResFileName := resFileName;
  LastLrsFileName := lrsFileName;
  SetFileNames(MainFileName, SaveToTestDir);

  UpdateFlagLrsIncludeAllowed(MainFileName);

  try
    // update resources (FLazarusResources, FSystemResources, ...)
    if not Update then begin
      debugln(['TProjectResources.Regenerate Update failed']);
      Exit;
    end;
    // create codebuffers of new .lrs and .rc files
    UpdateCodeBuffers;
    // update .lpr file (old and new include files exist, so parsing should work without errors)
    if UpdateSource and not UpdateMainSourceFile(MainFileName) then begin
      debugln(['TProjectResources.Regenerate UpdateMainSourceFile failed']);
      exit;
    end;

    if PerformSave and not Save(SaveToTestDir) then begin
      debugln(['TProjectResources.Regenerate Save failed']);
      Exit;
    end;
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

procedure TProjectResources.WriteToProjectFile(AConfig: TXMLConfig;
  const Path: String);
var
  i: integer;
begin
  AConfig.SetDeleteValue(Path+'General/ResourceType/Value', ResourceTypeNames[ResourceType], ResourceTypeNames[rtLRS]);
  for i := 0 to FResources.Count - 1 do
    FResources[i].WriteToProjectFile(AConfig, Path);
end;

procedure TProjectResources.ReadFromProjectFile(AConfig: TXMLConfig;
  const Path: String; ReadAll: Boolean);
var
  i: integer;
begin
  ResourceType := StrToResourceType(AConfig.GetValue(Path+'General/ResourceType/Value', ResourceTypeNames[rtLRS]));
  for i := 0 to FResources.Count - 1 do
    if ReadAll or FResources[i].IsDefaultOption then
      FResources[i].ReadFromProjectFile(AConfig, Path);
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
          Messages.Add(Format(lisCouldNotRemoveFromMainSource, [LazResourcesUnit]));
          debugln(['TProjectResources.UpdateMainSourceFile removing LResources from all uses sections failed']);
        end;
      end;
    end
    else
    if FLrsIncludeAllowed and HasLazarusResources then
    begin
      if not CodeToolBoss.AddUnitToMainUsesSection(CodeBuf, LazResourcesUnit,'') then
      begin
        Result := False;
        Messages.Add(Format(lisCouldNotAddToMainSource, [LazResourcesUnit]));
        debugln(['TProjectResources.UpdateMainSourceFile adding LResources to main source failed']);
      end;
    end;

    // update {$R filename} directive
    if CodeToolBoss.FindResourceDirective(CodeBuf, 1, 1,
                               NewCode, NewX, NewY,
                               NewTopLine, '*.res', false) then
    begin
      // there is a resource directive in the source
      if not HasSystemResources then
      begin
        if not CodeToolBoss.RemoveDirective(NewCode, NewX, NewY, true) then
        begin
          Result := False;
          Messages.Add(Format(lisCouldNotRemoveRFromMainSource, [Filename]));
          debugln(['TProjectResources.UpdateMainSourceFile failed: removing resource directive']);
        end;
      end;
    end
    else
    if HasSystemResources then
    begin
      Directive := '{$R *.res}';
      if not CodeToolBoss.AddResourceDirective(CodeBuf, Filename, false, Directive) then
      begin
        Result := False;
        Messages.Add(Format(lisCouldNotAddRToMainSource, [Filename]));
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
          Messages.Add(Format(lisCouldNotRemoveIFromMainSource, [Filename]));
          debugln(['TProjectResources.UpdateMainSourceFile removing include directive from main source failed']);
          Exit;
        end;
      end;
    end
    else
    if FLrsIncludeAllowed and HasLazarusResources then
    begin
      //debugln(['TProjectResources.UpdateMainSourceFile include directive not found: FCanHaveLrsInclude=',FLrsIncludeAllowed,' HasLazarusResources=',HasLazarusResources]);
      if not CodeToolBoss.AddIncludeDirectiveForInit(CodeBuf,Filename,'') then
      begin
        Result := False;
        Messages.Add(Format(lisCouldNotAddIToMainSource, [Filename]));
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

  oldLrsFileName, newLrsFileName: String;
begin
  //DebugLn(['TProjectResources.RenameDirectives CurFileName="',CurFileName,'" NewFileName="',NewFileName,'"']);
  Result := True;

  CodeBuf := CodeToolBoss.LoadFile(CurFileName, False, False);
  if CodeBuf = nil then
    Exit;

  LastResFileName := resFileName;
  LastLrsFileName := lrsFileName;
  try
    SetFileNames(CurFileName, '');
    oldLrsFileName := ExtractFileName(lrsFileName);
    SetFileNames(NewFileName, '');
    newLrsFileName := ExtractFileName(lrsFileName);

    // update resources (FLazarusResources, FSystemResources, ...)
    UpdateFlagLrsIncludeAllowed(CurFileName);
    if not Update then
      Exit;
    // update codebuffers of new .lrs and .res files
    UpdateCodeBuffers;

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
      if not CodeToolBoss.AddIncludeDirectiveForInit(CodeBuf, newLrsFileName, '') then
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
    Result := True;
    CodeBuf := CodeToolBoss.FindFile(Filename);
    if (CodeBuf = nil) or CodeBuf.IsDeleted then Exit;
    if not CodeBuf.IsVirtual then
      Result := SaveCodeBuffer(CodeBuf) in [mrOk,mrIgnore]
    else if SaveToTestDir<>'' then
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
  end;
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
  CleanCodeBuffer(LastResFileName, resFileName);
  CleanCodeBuffer(LastLrsFileName, lrsFileName);
end;

finalization
  ResourceTypesCache.Free;

end.

