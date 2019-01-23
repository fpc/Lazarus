{
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

  Author: Mattias Gaertner

  Abstract:
    Functions and classes to build dependency graphs for pascal units.
}
unit CTUnitGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  // LazUtils
  LazFileUtils, LazStringUtils,
  // Codetools
  FileProcs, FindDeclarationTool, CodeBeautifier, CodeCache, StdCodeTools,
  DirectoryCacher, LinkScanner, CustomCodeTool, CodeTree, CodeToolsStructs;

type

  { TFindIdentifierReferenceCache }

  TFindIdentifierReferenceCache = class
  public
    IdentifierCode: TCodeBuffer;
    X, Y: integer;

    SourcesChangeStep: int64;
    FilesChangeStep: int64;
    InitValuesChangeStep: integer;
    NewTool: TFindDeclarationTool;
    NewNode: TCodeTreeNode;
    NewPos: TCodeXYPosition;
    IsPrivate: boolean;
    procedure Clear;
  end;

type
  TUGUnitFlag = (
    ugufReached,
    ugufLoadError,
    ugufIsIncludeFile,
    ugufHasSyntaxErrors
    );
  TUGUnitFlags = set of TUGUnitFlag;

  { TUGUnit }

  TUGUnit = class
  public
    Flags: TUGUnitFlags;
    TheUnitName: string;
    Filename: string;
    Code: TCodeBuffer;
    Tool: TStandardCodeTool;
    UsesUnits: TFPList; // list of TUGUses, can be nil
    UsedByUnits: TFPList; // list of TUGUses, can be nil
    constructor Create(const aFilename: string);
    destructor Destroy; override;
    procedure Clear;
    function IndexOfUses(const aFilename: string): integer; // slow linear search
  end;
  TUGUnitClass = class of TUGUnit;

  { TUGUses }

  TUGUses = class
  public
    Owner: TUGUnit;
    UsesUnit: TUGUnit;
    InImplementation: boolean;
    constructor Create(TheOwner, TheUses: TUGUnit);
    destructor Destroy; override;
  end;
  TUGUsesClass = class of TUGUses;

  { TUsesGraph }

  TUsesGraph = class
  private
    FFiles: TAVLTree; // tree of TUGUnit sorted for Filename
    FIgnoreFiles: TAVLTree; // tree of TUGUnit sorted for Filename
    FQueuedFiles: TAVLTree; // tree of TUGUnit sorted for Filename
    FTargetAll: boolean;
    FTargetFiles: TAVLTree; // tree of TUGUnit sorted for Filename
    FTargetDirsValid: boolean;
    FTargetDirs: string;
    FTargetInFPCSrc: boolean;
    FUnitClass: TUGUnitClass;
    FUsesClass: TUGUsesClass;
  public
    DirectoryCachePool: TCTDirectoryCachePool;
    OnGetCodeToolForBuffer: TOnGetCodeToolForBuffer;
    OnLoadFile: TOnLoadCTFile;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ConsistencyCheck;
    function GetUnit(const ExpFilename: string; CreateIfNotExists: boolean): TUGUnit;
    function FindUnit(const AnUnitName: string): TUGUnit; // slow

    procedure AddStartUnit(ExpFilename: string);
    procedure AddTargetUnit(ExpFilename: string);
    procedure AddIgnoreUnit(ExpFilename: string);
    procedure AddSystemUnitAsTarget;
    function Parse(IgnoreErrors: boolean; out Completed: boolean;
                   StopAfterMs: integer = -1): boolean;
    function GetUnitsTreeUsingTargets: TAVLTree; // tree of TUGUnit sorted for filename
    function GetCodeTreeUsingTargets: TAVLTree; // tree of TCodeBuffer sorted for filename
    function UnitCanFindTarget(ExpFilename: string): boolean;
    function IsTargetDir(ExpDir: string): boolean;

    function FindShortestPath(StartUnit, EndUnit: TUGUnit): TFPList; // list of TUGUnit, nil if no path exists
    function InsertMissingLinks(UGUnitList: TFPList): boolean;

    property FilesTree: TAVLTree read FFiles; // tree of TUGUnit sorted for Filename (all parsed)
    property IgnoreFilesTree: TAVLTree read FIgnoreFiles; // tree of TUGUnit sorted for Filename
    property QueuedFilesTree: TAVLTree read FQueuedFiles; // tree of TUGUnit sorted for Filename
    property TargetFilesTree: TAVLTree read FTargetFiles; // tree of TUGUnit sorted for Filename
    property TargetAll: boolean read FTargetAll write FTargetAll;

    property UnitClass: TUGUnitClass read FUnitClass write FUnitClass;
    property UsesClass: TUGUsesClass read FUsesClass write FUsesClass;
  end;

function CompareUGUnitFilenames(UGUnit1, UGUnit2: Pointer): integer;
function CompareFilenameAndUGUnit(FileAnsistring, UGUnit: Pointer): integer;

implementation

function CompareUGUnitFilenames(UGUnit1, UGUnit2: Pointer): integer;
var
  Unit1: TUGUnit absolute UGUnit1;
  Unit2: TUGUnit absolute UGUnit2;
begin
  Result:=CompareFilenames(Unit1.Filename,Unit2.Filename);
end;

function CompareFilenameAndUGUnit(FileAnsistring, UGUnit: Pointer): integer;
var
  AnUnit: TUGUnit absolute UGUnit;
  Filename: String;
begin
  Filename:=AnsiString(FileAnsistring);
  Result:=CompareFilenames(Filename,AnUnit.Filename);
end;

{ TFindIdentifierReferenceCache }

procedure TFindIdentifierReferenceCache.Clear;
begin
  SourcesChangeStep:=CTInvalidChangeStamp64;
  FilesChangeStep:=CTInvalidChangeStamp64;
  InitValuesChangeStep:=CTInvalidChangeStamp;
  NewTool:=nil;
  NewNode:=nil;
  NewPos:=CleanCodeXYPosition;
  IsPrivate:=false;
end;

{ TUGUses }

constructor TUGUses.Create(TheOwner, TheUses: TUGUnit);
begin
  Owner:=TheOwner;
  UsesUnit:=TheUses;
end;

destructor TUGUses.Destroy;
begin
  if Owner<>nil then begin
    Owner.UsesUnits.Remove(Self);
    Owner:=nil;
  end;
  if UsesUnit<>nil then begin
    UsesUnit.UsedByUnits.Remove(Self);
    UsesUnit:=nil;
  end;
  inherited Destroy;
end;

{ TUGUnit }

constructor TUGUnit.Create(const aFilename: string);
begin
  Filename:=aFilename;
  TheUnitName:=ExtractFileNameOnly(Filename);
end;

destructor TUGUnit.Destroy;
begin
  Clear;
  FreeAndNil(UsesUnits);
  FreeAndNil(UsedByUnits);
  inherited Destroy;
end;

procedure TUGUnit.Clear;

  procedure FreeUsesList(var List: TFPList);
  begin
    if List=nil then exit;
    while List.Count>0 do TObject(List[0]).Free;
    FreeAndNil(List);
  end;

begin
  FreeUsesList(UsesUnits);
  FreeUsesList(UsedByUnits);
  Flags:=Flags-[ugufHasSyntaxErrors,ugufReached];
end;

function TUGUnit.IndexOfUses(const aFilename: string): integer;
begin
  if UsesUnits=nil then exit(-1);
  Result:=UsesUnits.Count-1;
  while (Result>=0)
  and (CompareFilenames(aFilename,TUGUses(UsesUnits[Result]).UsesUnit.Filename)<>0) do
    dec(Result);
end;

{ TUsesGraph }

constructor TUsesGraph.Create;
begin
  FUnitClass:=TUGUnit;
  FUsesClass:=TUGUses;
  FFiles:=TAVLTree.Create(@CompareUGUnitFilenames);
  FIgnoreFiles:=TAVLTree.Create(@CompareUGUnitFilenames);
  FQueuedFiles:=TAVLTree.Create(@CompareUGUnitFilenames);
  FTargetFiles:=TAVLTree.Create(@CompareUGUnitFilenames);
end;

destructor TUsesGraph.Destroy;
begin
  Clear;
  FreeAndNil(FIgnoreFiles);
  FreeAndNil(FQueuedFiles);
  FreeAndNil(FTargetFiles);
  FreeAndNil(FFiles);
  inherited Destroy;
end;

procedure TUsesGraph.Clear;
begin
  FQueuedFiles.Clear; // all files of StartFiles are in Files too
  FTargetFiles.Clear; // all files of TargetFiles are in Files too
  FFiles.FreeAndClear;
end;

procedure TUsesGraph.ConsistencyCheck;
var
  AVLNode: TAVLTreeNode;
  AnUnit: TUGUnit;
begin
  FFiles.ConsistencyCheck;
  FQueuedFiles.ConsistencyCheck;

  AVLNode:=FQueuedFiles.FindLowest;
  while AVLNode<>nil do begin
    AnUnit:=TUGUnit(AVLNode.Data);
    if AnUnit.Filename='' then
      raise Exception.Create('AnUnit without filename');
    if FFiles.FindKey(PChar(AnUnit.Filename),@CompareFilenameAndUGUnit)=nil then
      raise Exception.Create('startfile not in files: '+AnUnit.Filename);
    AVLNode:=FQueuedFiles.FindSuccessor(AVLNode);
  end;
end;

function TUsesGraph.GetUnit(const ExpFilename: string;
  CreateIfNotExists: boolean): TUGUnit;
var
  AVLNode: TAVLTreeNode;
begin
  if ExpFilename='' then begin
    Result:=nil;
    if CreateIfNotExists then
      raise Exception.Create('TUsesGraph.GetUnit missing filename');
    exit;
  end;
  AVLNode:=FFiles.FindKey(PChar(ExpFilename),@CompareFilenameAndUGUnit);
  if AVLNode<>nil then begin
    Result:=TUGUnit(AVLNode.Data);
  end else if CreateIfNotExists then begin
    Result:=UnitClass.Create(ExpFilename);
    FFiles.Add(Result);
  end else
    Result:=nil;
end;

function TUsesGraph.FindUnit(const AnUnitName: string): TUGUnit;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=FFiles.FindLowest;
  while AVLNode<>nil do begin
    Result:=TUGUnit(AVLNode.Data);
    if CompareText(ExtractFileNameOnly(Result.Filename),AnUnitName)=0 then
      exit;
    AVLNode:=FFiles.FindSuccessor(AVLNode);
  end;
end;

procedure TUsesGraph.AddStartUnit(ExpFilename: string);
var
  NewUnit: TUGUnit;
begin
  if ExpFilename='' then exit;
  if FQueuedFiles.FindKey(PChar(ExpFilename),@CompareFilenameAndUGUnit)<>nil then
    exit; // already a start file
  NewUnit:=GetUnit(ExpFilename,true);
  if ugufReached in NewUnit.Flags then exit; // already parsed
  // add to FFiles and FQueuedFiles
  //debugln(['TUsesGraph.AddStartUnit ',ExpFilename]);
  FQueuedFiles.Add(NewUnit);
end;

procedure TUsesGraph.AddTargetUnit(ExpFilename: string);
var
  NewUnit: TUGUnit;
begin
  if ExpFilename='' then exit;
  if FTargetFiles.FindKey(PChar(ExpFilename),@CompareFilenameAndUGUnit)<>nil then
    exit; // already a start file
  // add to FFiles and FTargetFiles
  //debugln(['TUsesGraph.AddTargetUnit ',ExpFilename]);
  NewUnit:=GetUnit(ExpFilename,true);
  if FTargetFiles.Find(NewUnit)=nil then
    FTargetFiles.Add(NewUnit);
  FTargetDirsValid:=false;
end;

procedure TUsesGraph.AddIgnoreUnit(ExpFilename: string);
var
  NewUnit: TUGUnit;
begin
  NewUnit:=GetUnit(ExpFilename,true);
  if FIgnoreFiles.Find(NewUnit)=nil then
    FIgnoreFiles.Add(NewUnit);
end;

procedure TUsesGraph.AddSystemUnitAsTarget;
begin
  AddTargetUnit(DirectoryCachePool.FindUnitInUnitSet('','system'));
end;

function TUsesGraph.Parse(IgnoreErrors: boolean; out Completed: boolean;
  StopAfterMs: integer): boolean;

  procedure AddUses(CurUnit: TUGUnit; UsedFiles: TStrings;
    InImplementation: boolean);
  var
    i: Integer;
    Filename: string;
    NewUnit: TUGUnit;
    NewUses: TUGUses;
  begin
    if UsedFiles=nil then exit;
    for i:=0 to UsedFiles.Count-1 do begin
      Filename:=UsedFiles[i];
      if not FilenameIsPascalUnit(Filename) then continue;
      // check if already used
      if CurUnit.IndexOfUses(Filename)>=0 then continue;
      if not UnitCanFindTarget(Filename) then continue;
      // add connection
      NewUnit:=GetUnit(Filename,true);
      if CurUnit.UsesUnits=nil then
        CurUnit.UsesUnits:=TFPList.Create;
      NewUses:=UsesClass.Create(CurUnit,NewUnit);
      NewUses.InImplementation:=InImplementation;
      CurUnit.UsesUnits.Add(NewUses);
      if NewUnit.UsedByUnits=nil then
        NewUnit.UsedByUnits:=TFPList.Create;
      NewUnit.UsedByUnits.Add(NewUses);
      // put new file on queue
      AddStartUnit(Filename);
    end;
  end;

  function ParseUnit(CurUnit: TUGUnit): boolean;
  // returns true to continue
  var
    Abort: boolean;
    MainUsesSection: TStrings;
    ImplementationUsesSection: TStrings;
  begin
    Result:=false;
    //debugln(['ParseUnit ',CurUnit.Filename,' ',Pos('tcfiler',CurUnit.Filename)]);
    Include(CurUnit.Flags,ugufLoadError);
    // load file
    Abort:=false;
    OnLoadFile(Self,CurUnit.Filename,CurUnit.Code,Abort);
    if Abort then exit;
    if CurUnit.Code=nil then begin
      debugln(['TUsesGraph.Parse failed loading file ',CurUnit.Filename]);
      Result:=IgnoreErrors;
      exit;
    end;
    try
      MainUsesSection:=nil;
      ImplementationUsesSection:=nil;
      try
        // create tool
        CurUnit.Tool:=OnGetCodeToolForBuffer(Self,CurUnit.Code,true) as TStandardCodeTool;
        if CurUnit.Tool=nil then begin
          debugln(['TUsesGraph.Parse failed getting tool for file ',CurUnit.Code.Filename]);
          Result:=IgnoreErrors;
          exit;
        end;
        // check if include file
        if CompareFilenames(CurUnit.Tool.MainFilename,CurUnit.Code.Filename)<>0 then
        begin
          Include(CurUnit.Flags,ugufIsIncludeFile);
          exit(true);
        end;
        Exclude(CurUnit.Flags,ugufLoadError);
        // parse both uses sections
        Include(CurUnit.Flags,ugufHasSyntaxErrors);
        CurUnit.Tool.BuildTree(lsrImplementationUsesSectionEnd);
        Exclude(CurUnit.Flags,ugufHasSyntaxErrors);
        // locate used units
        if not CurUnit.Tool.FindUsedUnitFiles(MainUsesSection,
                                              ImplementationUsesSection)
        then begin
          Result:=IgnoreErrors;
          exit;
        end;
        AddUses(CurUnit,MainUsesSection,false);
        AddUses(CurUnit,ImplementationUsesSection,true);
        Result:=true;
      finally
        MainUsesSection.Free;
        ImplementationUsesSection.Free;
      end;
    except
      on E: ECodeToolError do begin
        if not IgnoreErrors then raise;
      end;
      on E: ELinkScannerError do begin
        if not IgnoreErrors then raise;
      end;
    end;
  end;

var
  StartTime: TDateTime;
  AVLNode: TAVLTreeNode;
  CurUnit: TUGUnit;
begin
  Result:=false;
  Completed:=false;
  if StopAfterMs>=0 then
    StartTime:=Now
  else
    StartTime:=0;
  while FQueuedFiles.Count>0 do begin
    AVLNode:=FQueuedFiles.FindLowest;
    CurUnit:=TUGUnit(AVLNode.Data);
    FQueuedFiles.Delete(AVLNode);
    Include(CurUnit.Flags,ugufReached);
    if FIgnoreFiles.Find(CurUnit)<>nil then continue;
    //debugln(['TUsesGraph.Parse Unit=',CurUnit.Filename,' UnitCanFindTarget=',UnitCanFindTarget(CurUnit.Filename)]);
    if UnitCanFindTarget(CurUnit.Filename) then begin
      ParseUnit(CurUnit);
    end;

    if (StopAfterMs>=0) and (Abs(Now-StartTime)*86400000>=StopAfterMs) then
      exit(true);
  end;

  Completed:=true;
  Result:=true;
end;

function TUsesGraph.GetUnitsTreeUsingTargets: TAVLTree;

  procedure Add(Units: TAVLTree; NewUnit: TUGUnit);
  var
    i: Integer;
    CurUses: TUGUses;
  begin
    if NewUnit=nil then exit;
    if not (ugufReached in NewUnit.Flags) then exit; // this unit was not reached
    if ugufIsIncludeFile in NewUnit.Flags then exit;
    if Units.Find(NewUnit)<>nil then exit; // already added
    Units.Add(NewUnit);
    if NewUnit.UsedByUnits=nil then exit;
    for i:=0 to NewUnit.UsedByUnits.Count-1 do begin
      CurUses:=TUGUses(NewUnit.UsedByUnits[i]);
      Add(Units,CurUses.Owner);
    end;
  end;

var
  AVLNode: TAVLTreeNode;
begin
  Result:=TAVLTree.Create(@CompareUGUnitFilenames);
  AVLNode:=FTargetFiles.FindLowest;
  while AVLNode<>nil do begin
    Add(Result,TUGUnit(AVLNode.Data));
    AVLNode:=FTargetFiles.FindSuccessor(AVLNode);
  end;
end;

function TUsesGraph.GetCodeTreeUsingTargets: TAVLTree;
var
  Units: TAVLTree;
  AVLNode: TAVLTreeNode;
  CurUnit: TUGUnit;
begin
  Result:=TAVLTree.Create(@CompareCodeBuffers);
  Units:=GetUnitsTreeUsingTargets;
  try
    AVLNode:=Units.FindLowest;
    while AVLNode<>nil do begin
      CurUnit:=TUGUnit(AVLNode.Data);
      if not (ugufIsIncludeFile in CurUnit.Flags)
      and (Result.Find(CurUnit.Code)=nil) then
        Result.Add(CurUnit.Code);
      AVLNode:=Units.FindSuccessor(AVLNode);
    end;
  finally
    Units.Free;
  end;
end;

function TUsesGraph.UnitCanFindTarget(ExpFilename: string): boolean;
// returns true if ExpFilename can find one of the targets via the search paths
var
  BaseDir: String;
  SrcPath: String;
  p: integer;
  ReachableDir: String;
begin
  Result:=true;
  if FTargetInFPCSrc or TargetAll then exit; // standard units can always be found

  BaseDir:=ExtractFilePath(ExpFilename);
  if IsTargetDir(BaseDir) then exit;

  // check complete search path, including SrcPath, UnitPath
  // and resolved compiled unit paths
  SrcPath:=DirectoryCachePool.GetString(BaseDir,ctdcsCompleteSrcPath);
  p:=1;
  repeat
    ReachableDir:=GetNextDelimitedItem(SrcPath,';',p);
    if ReachableDir<>'' then begin
      if not FilenameIsAbsolute(ReachableDir) then
        ReachableDir:=BaseDir+ReachableDir;
      if IsTargetDir(ReachableDir) then exit;
    end;
  until p>length(SrcPath);

  Result:=false;
end;

function TUsesGraph.IsTargetDir(ExpDir: string): boolean;
var
  AVLNode: TAVLTreeNode;
  CurUnit: TUGUnit;
  Dir: String;
begin
  if FTargetFiles.Count=0 then exit(TargetAll);

  if not FTargetDirsValid then begin
    FTargetDirsValid:=true;
    FTargetInFPCSrc:=TargetAll;
    // build list of target directories for quick lookup
    AVLNode:=FTargetFiles.FindLowest;
    while AVLNode<>nil do begin
      CurUnit:=TUGUnit(AVLNode.Data);
      Dir:=ExtractFilePath(CurUnit.Filename);
      if FilenameIsAbsolute(Dir)
      and (CompareFilenames(DirectoryCachePool.FindUnitInUnitSet(Dir,CurUnit.TheUnitName),
             CurUnit.Filename)=0)
      then begin
        // this is a standard unit (e.g. in FPC sources)
        // they are not reachable via search paths, but via the UnitSet
        FTargetInFPCSrc:=true;
      end else if Dir='' then begin
        // in virtual directory
        if (FTargetDirs='') or (FTargetDirs[1]<>';') then
          FTargetDirs:=';'+FTargetDirs;
      end else if FindPathInSearchPath(Dir,FTargetDirs)<1 then begin
        // normal source directory
        if FTargetDirs='' then
          FTargetDirs:=Dir
        else
          FTargetDirs:=FTargetDirs+';'+Dir;
      end;
      AVLNode:=FTargetFiles.FindSuccessor(AVLNode);
    end;
  end;

  Result:=true;
  if TargetAll then exit;
  if (ExpDir='') and (FTargetDirs[1]=';') then
    exit; // virtual directory
  Result:=FindPathInSearchPath(ExpDir,FTargetDirs)>0;
end;

function TUsesGraph.FindShortestPath(StartUnit, EndUnit: TUGUnit): TFPList;
// broad search first
var
  Queue: TFPList;
  NodeToPrevNode: TPointerToPointerTree;
  CurUnit: TUGUnit;
  i: Integer;
  CurUses: TUGUses;
  UsesUnit: TUGUnit;
  PrevUnit: TUGUnit;
begin
  Result:=nil;
  if (StartUnit=nil) or (EndUnit=nil) then exit;
  Queue:=TFPList.Create;
  NodeToPrevNode:=TPointerToPointerTree.Create;
  try
    Queue.Add(EndUnit);
    NodeToPrevNode[EndUnit]:=EndUnit; // set end marker
    while Queue.Count>0 do begin
      CurUnit:=TUGUnit(Queue[0]);
      Queue.Delete(0);
      if CurUnit.UsedByUnits=nil then continue;
      for i:=0 to CurUnit.UsedByUnits.Count-1 do begin
        CurUses:=TUGUses(CurUnit.UsedByUnits[i]);
        if CurUses.InImplementation then continue;
        UsesUnit:=CurUses.Owner;
        if NodeToPrevNode.Contains(UsesUnit) then
          continue; // already visited
        NodeToPrevNode[UsesUnit]:=CurUnit;
        if UsesUnit=StartUnit then begin
          // found StartUnit
          // => create list from StartUnit to EndUnit
          Result:=TFPList.Create;
          CurUnit:=StartUnit;
          repeat
            Result.Add(CurUnit);
            PrevUnit:=TUGUnit(NodeToPrevNode[CurUnit]);
            if PrevUnit=CurUnit then exit; // end marker found
            CurUnit:=PrevUnit;
          until false;
          exit;
        end;
        Queue.Add(UsesUnit);
      end;
    end;
  finally
    NodeToPrevNode.Free;
    Queue.Free;
  end;
end;

function TUsesGraph.InsertMissingLinks(UGUnitList: TFPList): boolean;
var
  i,j: Integer;
  StartUnit: TUGUnit;
  EndUnit: TUGUnit;
  CurList: TFPList;
begin
  Result:=true;
  for i:=UGUnitList.Count-2 downto 0 do begin
    StartUnit:=TUGUnit(UGUnitList[i]);
    EndUnit:=TUGUnit(UGUnitList[i+1]);
    CurList:=FindShortestPath(StartUnit,EndUnit);
    if (CurList=nil) then begin
      Result:=false;
      continue;
    end;
    if CurList.Count>2 then begin
      for j:=1 to CurList.Count-2 do
        UGUnitList.Insert(i+j,CurList[j]);
    end;
    CurList.Free;
  end;
end;

end.

