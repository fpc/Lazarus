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
    Functions and classes to build dependency graphs for ppu files.
}
unit PPUGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  {$IFnDEF HASAMIGA}
  dynlibs,
  {$ENDIF}
  // LazUtils
  LazStringUtils,
  // Codetools
  PPUParser, CodeTree, FileProcs, LazFileUtils, BasicCodeTools, CodeGraph,
  CodeToolManager, CodeToolsStructs;

const
  FPCPPUGroupPrefix = 'fpc_';
  {$IFDEF HASAMIGA}
  SharedSuffix = 'library';
  {$ENDIF}

type
  TPPUGroup = class;

  TPPUMemberFlag = (
    pmfDisabled,
    pmfAutoDisabled
    );
  TPPUMemberFlags = set of TPPUMemberFlag;
  
  { TPPUMember }

  TPPUMember = class
  public
    Unit_Name: string;
    PPUFilename: string;
    KeyNode: TCodeTreeNode;
    InitializationMangledName: string;
    FinalizationMangledName: string;
    MainUses: TStrings;
    ImplementationUses: TStrings;
    Group: TPPUGroup;
    PPU: TPPU;
    Flags: TPPUMemberFlags;
    constructor Create;
    destructor Destroy; override;
    function UpdatePPU: boolean;
    procedure GetMissingUnits(var List: TStrings);
  end;

  TPPUGroups = class;

  { TPPUGroup }

  TPPUGroup = class
  private
    FMembers: TAVLTree;// tree of TPPUMember sorted for AUnitName
    FUnitGraph: TCodeGraph;
    FSortedUnits: TFPList;// list of TPPUMember
    function FindAVLNodeOfMemberWithUnitName(const AName: string): TAVLTreeNode;
    function GetSortedUnits(Index: integer): TPPUMember;
    procedure InternalRemoveMember(AMember: TPPUMember);
    procedure UpdateTopologicalSortedList;
  public
    Name: string;
    KeyNode: TCodeTreeNode;
    Groups: TPPUGroups;
    LibName: string;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddMember(const NewUnitName: string): TPPUMember;
    function FindMemberWithUnitName(const AName: string): TPPUMember;
    function UpdatePPUs: boolean;
    function UpdateDependencies: boolean;
    function UpdateLoader: boolean;
    procedure GetMissingUnits(var List: TStrings);
    property Members: TAVLTree read FMembers;
    property UnitGraph: TCodeGraph read FUnitGraph;
    property SortedUnits[Index: integer]: TPPUMember read GetSortedUnits;
  end;

  { TPPUGroups }

  TPPUGroups = class
  private
    FGroups: TAVLTree;// tree of TPPUGroup sorted for name
    FMembers: TAVLTree;// tree of TPPUMember sorted for AUnitName
    FGroupGraph: TCodeGraph;
    FUnitGraph: TCodeGraph;
    FSortedGroups: TFPList; // list of TPPUGroup
    function FindAVLNodeOfGroupWithName(const AName: string): TAVLTreeNode;
    function FindAVLNodeOfMemberWithName(const AName: string): TAVLTreeNode;
    function GetSortedGroups(Index: integer): TPPUGroup;
    procedure InternalRemoveMember(AMember: TPPUMember);
    procedure InternalRemoveGroup(AGroup: TPPUGroup);
    procedure UpdateTopologicalSortedList;
  public
    Name: string;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearAutoDisableFlags;
    function AddGroup(const NewName: string): TPPUGroup;
    procedure AddFPCGroupsForCurrentCompiler(const BaseDirectory: string);
    procedure AddFPCGroups(const FPCPPUBaseDir: string); // for example: /usr/lib/fpc/2.2.3/units/i386-linux/
    procedure AddFPCGroup(const BaseGroupname, Directory: string);
    function FindGroupWithName(const AName: string): TPPUGroup;
    function FindMemberWithUnitName(const AName: string): TPPUMember;
    function UpdateDependencies: boolean;
    function UpdateLoaders: boolean;
    procedure AutoDisableUnitsWithBrokenDependencies;
    procedure AutoDisableMember(Member: TPPUMember);
    procedure GetMissingUnits(var List: TStrings);
    property GroupGraph: TCodeGraph read FGroupGraph;
    property UnitGraph: TCodeGraph read FUnitGraph;
    property SortedGroups[Index: integer]: TPPUGroup read GetSortedGroups;
  end;
  
function ComparePPUMembersByUnitName(Member1, Member2: Pointer): integer;
function CompareNameWithPPUMemberName(NamePChar, Member: Pointer): integer;

function ComparePPUGroupsByName(Group1, Group2: Pointer): integer;
function CompareNameWithPPUGroupName(NamePChar, Group: Pointer): integer;

function PPUGroupObjectAsString(Obj: TObject): string;

implementation

function ComparePPUMembersByUnitName(Member1, Member2: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(Pointer(TPPUMember(Member1).Unit_Name),
                                Pointer(TPPUMember(Member2).Unit_Name));
end;

function CompareNameWithPPUMemberName(NamePChar, Member: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(NamePChar,Pointer(TPPUMember(Member).Unit_Name));
end;

function ComparePPUGroupsByName(Group1, Group2: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(Pointer(TPPUGroup(Group1).Name),
                                Pointer(TPPUGroup(Group2).Name));
end;

function CompareNameWithPPUGroupName(NamePChar, Group: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(NamePChar,Pointer(TPPUGroup(Group).Name));
end;

function PPUGroupObjectAsString(Obj: TObject): string;
begin
  if Obj is TPPUMember then
    Result:='unit '+TPPUMember(Obj).Unit_Name
  else if Obj is TPPUGroup then
    Result:='group '+TPPUGroup(Obj).Name
  else
    Result:=dbgs(Obj);
end;

{ TPPUMember }

constructor TPPUMember.Create;
begin
  KeyNode:=TCodeTreeNode.Create;
  MainUses:=TStringList.Create;
  ImplementationUses:=TStringList.Create;
end;

destructor TPPUMember.Destroy;
begin
  FreeAndNil(PPU);
  FreeAndNil(MainUses);
  FreeAndNil(ImplementationUses);
  FreeAndNil(KeyNode);
  if Group<>nil then
    Group.InternalRemoveMember(Self);
  inherited Destroy;
end;

function TPPUMember.UpdatePPU: boolean;
begin
  Result:=false;
  MainUses.Clear;
  ImplementationUses.Clear;
  InitializationMangledName:='';
  FinalizationMangledName:='';
  if PPU=nil then PPU:=TPPU.Create(Self);
  PPU.LoadFromFile(PPUFilename);
  debugln('================================================================');
  DebugLn(['TPPUMember.UpdatePPU Group=',Group.Name,' AUnitName=',Unit_Name,' Filename=',PPUFilename]);
  //PPU.Dump('');
  PPU.GetMainUsesSectionNames(MainUses);
  if MainUses.Count>0 then
    debugln('Main used units: ',MainUses.DelimitedText);
  PPU.GetImplementationUsesSectionNames(ImplementationUses);
  if ImplementationUses.Count>0 then
    debugln('Implementation used units: ',ImplementationUses.DelimitedText);
  InitializationMangledName:=PPU.GetInitProcName;
  //debugln('Initialization proc: ',InitializationMangledName);
  FinalizationMangledName:=PPU.GetFinalProcName;
  //debugln('Finalization proc: ',FinalizationMangledName);
  
  Result:=true;
end;

procedure TPPUMember.GetMissingUnits(var List: TStrings);

  procedure GetMissing(UsesList: TStrings);
  var
    i: Integer;
    CurUnitName: string;
  begin
    if UsesList=nil then exit;
    for i:=0 to UsesList.Count-1 do begin
      CurUnitName:=UsesList[i];
      if Group.Groups.FindMemberWithUnitName(CurUnitName)=nil then begin
        if List=nil then
          List:=TStringList.Create;
        if List.IndexOf(CurUnitName)<0 then
          List.Add(CurUnitName);
      end;
    end;
  end;

begin
  GetMissing(MainUses);
  GetMissing(ImplementationUses);
end;

{ TPPUGroup }

function TPPUGroup.FindAVLNodeOfMemberWithUnitName(const AName: string
  ): TAVLTreeNode;
begin
  Result:=FMembers.FindKey(PChar(AName),@CompareNameWithPPUMemberName);
end;

function TPPUGroup.GetSortedUnits(Index: integer): TPPUMember;
begin
  Result:=TPPUMember(TCodeGraphNode(FSortedUnits[Index]).Data);
end;

procedure TPPUGroup.InternalRemoveMember(AMember: TPPUMember);
begin
  FUnitGraph.DeleteGraphNode(AMember.KeyNode);
  AVLRemovePointer(FMembers,AMember);
  if Groups<>nil then
    Groups.InternalRemoveMember(AMember);
end;

procedure TPPUGroup.UpdateTopologicalSortedList;
begin
  FreeAndNil(FSortedUnits);
  UnitGraph.GetTopologicalSortedList(FSortedUnits,true,false,false);
  if FSortedUnits=nil then
    FSortedUnits:=TFPList.Create;
  //DebugLn(['TPPUGroup.UpdateTopologicalSortedList ',Name,' ',FMembers.Count,' ',FSortedUnits.Count]);
end;

constructor TPPUGroup.Create;
begin
  FMembers:=TAVLTree.Create(@ComparePPUMembersByUnitName);
  KeyNode:=TCodeTreeNode.Create;
  FUnitGraph:=TCodeGraph.Create;
end;

destructor TPPUGroup.Destroy;
begin
  Clear;
  FreeAndNil(FUnitGraph);
  FreeAndNil(FMembers);
  FreeAndNil(KeyNode);
  if Groups<>nil then
    Groups.InternalRemoveGroup(Self);
  inherited Destroy;
end;

procedure TPPUGroup.Clear;
begin
  FreeAndNil(FSortedUnits);
  FUnitGraph.Clear;
  while FMembers.Count>0 do
    TPPUMember(FMembers.Root.Data).Free;
end;

function TPPUGroup.AddMember(const NewUnitName: string): TPPUMember;
begin
  Result:=FindMemberWithUnitName(NewUnitName);
  if Result<>nil then exit;
  Result:=TPPUMember.Create;
  Result.Unit_Name:=NewUnitName;
  FMembers.Add(Result);
  Result.Group:=Self;
  Groups.FMembers.Add(Result);
end;

function TPPUGroup.FindMemberWithUnitName(const AName: string): TPPUMember;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=FindAVLNodeOfMemberWithUnitName(AName);
  if AVLNode<>nil then
    Result:=TPPUMember(AVLNode.Data)
  else
    Result:=nil;
end;

function TPPUGroup.UpdatePPUs: boolean;
var
  AVLNode: TAVLTreeNode;
  Member: TPPUMember;
begin
  Result:=true;
  // load all PPU
  AVLNode:=FMembers.FindLowest;
  while AVLNode<>nil do begin
    Member:=TPPUMember(AVLNode.Data);
    if not Member.UpdatePPU then exit(false);
    AVLNode:=FMembers.FindSuccessor(AVLNode);
  end;
end;

function TPPUGroup.UpdateDependencies: boolean;

  procedure AddUnitDependency(Member: TPPUMember; const UsedUnit: string);
  var
    Graph: TCodeGraph;
    UsedMember: TPPUMember;
  begin
    UsedMember:=Groups.FindMemberWithUnitName(UsedUnit);
    if UsedMember=nil then begin
      DebugLn(['AddUnitDependency ',Member.Unit_Name,' misses an unit: ',UsedUnit]);
      exit;
    end;
    // add to 'global' unit graph
    Graph:=Groups.UnitGraph;
    if not Graph.PathExists(UsedMember.KeyNode,Member.KeyNode) then
      Graph.AddEdge(Member.KeyNode,UsedMember.KeyNode)
    else
      DebugLn(['AddUnitDependency Unit circle found: ',Member.Unit_Name,' to ',UsedMember.Unit_Name]);
    if Member.Group=UsedMember.Group then begin
      // add to unit graph of group
      Graph:=Member.Group.UnitGraph;
      if not Graph.PathExists(UsedMember.KeyNode,Member.KeyNode) then
        Graph.AddEdge(Member.KeyNode,UsedMember.KeyNode)
      else
        DebugLn(['AddUnitDependency Unit circle found: ',Member.Unit_Name,' to ',UsedMember.Unit_Name]);
    end else begin
      // add to 'global' package graph
      if not Groups.GroupGraph.PathExists(UsedMember.Group.KeyNode,Member.Group.KeyNode) then
        Groups.GroupGraph.AddEdge(Member.Group.KeyNode,UsedMember.Group.KeyNode)
      else
        DebugLn(['AddUnitDependency Group circle found: ',Member.Group.Name,' to ',UsedMember.Group.Name]);
    end;
  end;

  procedure AddSectionDependencies(Member: TPPUMember; UsesList: TStrings);
  var
    i: Integer;
  begin
    if UsesList=nil then exit;
    for i:=0 to UsesList.Count-1 do
      AddUnitDependency(Member,UsesList[i]);
  end;
  
  procedure AddDependencies(Main: boolean);
  var
    AVLNode: TAVLTreeNode;
    Member: TPPUMember;
  begin
    AVLNode:=FMembers.FindLowest;
    while AVLNode<>nil do begin
      Member:=TPPUMember(AVLNode.Data);
      if Main then
        AddSectionDependencies(Member,Member.MainUses)
      else
        AddSectionDependencies(Member,Member.ImplementationUses);
      AVLNode:=FMembers.FindSuccessor(AVLNode);
    end;
  end;

var
  AVLNode: TAVLTreeNode;
  Member: TPPUMember;
  GraphNode: TCodeGraphNode;
begin
  Result:=false;
  FUnitGraph.Clear;

  // create graph nodes
  AVLNode:=FMembers.FindLowest;
  while AVLNode<>nil do begin
    Member:=TPPUMember(AVLNode.Data);
    GraphNode:=UnitGraph.AddGraphNode(Member.KeyNode);
    GraphNode.Data:=Member;
    AVLNode:=FMembers.FindSuccessor(AVLNode);
  end;
  
  // add primary dependencies
  AddDependencies(true);
  // add secondary dependencies
  AddDependencies(false);

  // sort topological
  UpdateTopologicalSortedList;

  Result:=true;
end;

function TPPUGroup.UpdateLoader: boolean;

  function StringToParagraph(Code: string): string;
  const
    MaxLineLen=80;
  var
    p: Integer;
    LineLen: Integer;
    BreakPos: Integer;
    Indent: String;
    InsertStr: String;
  begin
    Result:=Code;
    p:=1;
    LineLen:=0;
    BreakPos:=0;
    Indent:='      ';
    while (p<length(Result)) do begin
      if (LineLen>=MaxLineLen) and (BreakPos>0) then begin
        if Result[BreakPos]=',' then begin
          InsertStr:=LineEnding+Indent;
          LineLen:=length(Indent);
        end else begin
          InsertStr:=''''+LineEnding+Indent+'+''';
          LineLen:=length(Indent)+2;
        end;
        Result:=copy(Result,1,BreakPos)+InsertStr+copy(Result,BreakPos+1,length(Result));
        inc(p,length(InsertStr));
        BreakPos:=0;
      end else begin
        if Result[p] in [',',';'] then
          BreakPos:=p;
        inc(p);
        inc(LineLen);
      end;
    end;
  end;

var
  i: Integer;
  GraphNode: TCodeGraphNode;
  Member: TPPUMember;
  Group: TPPUGroup;
  NeededLibs: String;
  InitProcs: String;
  FinalProcs: String;
  s: String;
  RegisterFPLibProcName: String;
begin
  Result:=true;
  LibName:=Name+'.'+SharedSuffix;
  // needed groups in topological order
  if Groups.GroupGraph.GetGraphNode(KeyNode,false)=nil then
    raise Exception.Create('inconsistency');
    
    
  NeededLibs:='';
  for i:=0 to Groups.FSortedGroups.Count-1 do begin
    Group:=Groups.SortedGroups[i];
    if Groups.GroupGraph.GetGraphNode(Group.KeyNode,false)=nil then
      raise Exception.Create('inconsistency');
    if Groups.GroupGraph.GetEdge(KeyNode,Group.KeyNode,false)<>nil then begin
      if NeededLibs<>'' then NeededLibs:=NeededLibs+';';
      NeededLibs:=NeededLibs+Group.Name;
    end;
  end;
  // initialize units
  InitProcs:='';
  for i:=FSortedUnits.Count-1 downto 0 do begin
    GraphNode:=TCodeGraphNode(FSortedUnits[i]);
    Member:=TPPUMember(GraphNode.Data);
    if Member.InitializationMangledName<>'' then begin
      if InitProcs<>'' then InitProcs:=InitProcs+';';
      InitProcs:=InitProcs+Member.InitializationMangledName;
    end;
  end;
  // finalize units
  FinalProcs:='';
  for i:=0 to FSortedUnits.Count-1 do begin
    GraphNode:=TCodeGraphNode(FSortedUnits[i]);
    Member:=TPPUMember(GraphNode.Data);
    if Member.FinalizationMangledName<>'' then begin
      if FinalProcs<>'' then FinalProcs:=FinalProcs+';';
      FinalProcs:=FinalProcs+Member.FinalizationMangledName;
    end;
  end;
  RegisterFPLibProcName:='REGISTER_FPLIBRARY_'+UpperCase(Name);
  s:=  'procedure '+RegisterFPLibProcName+';[public, alias : '''+RegisterFPLibProcName+'''];'+LineEnding;
  s:=s+'begin'+LineEnding;
  s:=s+StringToParagraph('  RegisterFPDynLib('''+Name+''','''+NeededLibs+''','''+InitProcs+''','''+FinalProcs+''');')+LineEnding;
  s:=s+'end;'+LineEnding;
  Debugln(s);
end;

procedure TPPUGroup.GetMissingUnits(var List: TStrings);
var
  Member: TPPUMember;
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=FMembers.FindLowest;
  while AVLNode<>nil do begin
    Member:=TPPUMember(AVLNode.Data);
    Member.GetMissingUnits(List);
    AVLNode:=FMembers.FindSuccessor(AVLNode);
  end;
end;

{ TPPUGroups }

function TPPUGroups.FindAVLNodeOfGroupWithName(const AName: string
  ): TAVLTreeNode;
begin
  Result:=FGroups.FindKey(PChar(AName),@CompareNameWithPPUGroupName);
end;

function TPPUGroups.FindAVLNodeOfMemberWithName(const AName: string
  ): TAVLTreeNode;
begin
  Result:=FMembers.FindKey(PChar(AName),@CompareNameWithPPUMemberName);
end;

function TPPUGroups.GetSortedGroups(Index: integer): TPPUGroup;
begin
  Result:=TPPUGroup(TCodeGraphNode(FSortedGroups[Index]).Data);
end;

procedure TPPUGroups.InternalRemoveMember(AMember: TPPUMember);
begin
  AVLRemovePointer(FMembers,AMember);
end;

procedure TPPUGroups.InternalRemoveGroup(AGroup: TPPUGroup);
begin
  AVLRemovePointer(FGroups,AGroup);
end;

procedure TPPUGroups.UpdateTopologicalSortedList;
begin
  FreeAndNil(FSortedGroups);
  GroupGraph.GetTopologicalSortedList(FSortedGroups,false,false,false);
  if FSortedGroups=nil then
    FSortedGroups:=TFPList.Create;
  //DebugLn(['TPPUGroups.UpdateTopologicalSortedList ',FGroups.Count,' ',FSortedGroups.Count]);
end;

constructor TPPUGroups.Create;
begin
  FGroups:=TAVLTree.Create(@ComparePPUGroupsByName);
  FMembers:=TAVLTree.Create(@ComparePPUMembersByUnitName);
  FGroupGraph:=TCodeGraph.Create;
  FUnitGraph:=TCodeGraph.Create;
end;

destructor TPPUGroups.Destroy;
begin
  Clear;
  FreeAndNil(FUnitGraph);
  FreeAndNil(FGroupGraph);
  FreeAndNil(FGroups);
  FreeAndNil(FMembers);
  inherited Destroy;
end;

procedure TPPUGroups.Clear;
begin
  FreeAndNil(FSortedGroups);
  FGroupGraph.Clear;
  FUnitGraph.Clear;
  while FGroups.Count>0 do
    TPPUGroup(FGroups.Root.Data).Free;
end;

procedure TPPUGroups.ClearAutoDisableFlags;
var
  AVLNode: TAVLTreeNode;
  Member: TPPUMember;
begin
  AVLNode:=FMembers.FindLowest;
  while AVLNode<>nil do begin
    Member:=TPPUMember(AVLNode.Data);
    Exclude(Member.Flags,pmfAutoDisabled);
    AVLNode:=FMembers.FindSuccessor(AVLNode);
  end;
end;

function TPPUGroups.AddGroup(const NewName: string): TPPUGroup;
begin
  Result:=FindGroupWithName(NewName);
  if Result<>nil then exit;
  Result:=TPPUGroup.Create;
  Result.Name:=NewName;
  FGroups.Add(Result);
  Result.Groups:=Self;
end;

procedure TPPUGroups.AddFPCGroupsForCurrentCompiler(const BaseDirectory: string);
var
  FPCSearchPath: String;
  SystemPPUFilename: String;
  RTLPPUDirectory: String; // directory containing the system.ppu
  FPCPPUBaseDir: String; // directory containing all FPC ppu directories
begin
  FPCSearchPath:=CodeToolBoss.GetFPCUnitPathForDirectory(BaseDirectory);
  // search system.ppu
  SystemPPUFilename:=SearchFileInPath('system.ppu',BaseDirectory,FPCSearchPath,
                                      ';',ctsfcDefault);
  if SystemPPUFilename='' then begin
    debugln(['TPPUGroups.AddFPCGroupsForCurrentCompiler BaseDir="',BaseDirectory,'" FPCSearchPath="',FPCSearchPath,'"']);
    raise Exception.Create('TPPUGroups.AddFPCGroupsForCurrentCompiler: system.ppu is not in the FPC search paths');
  end;
  RTLPPUDirectory:=ExtractFilePath(SystemPPUFilename);
  FPCPPUBaseDir:=ExtractFilePath(ChompPathDelim(RTLPPUDirectory));
  AddFPCGroups(FPCPPUBaseDir);
end;

procedure TPPUGroups.AddFPCGroups(const FPCPPUBaseDir: string);
var
  FileInfo: TSearchRec;
  GroupName: String;
  i: Integer;
begin
  DebugLn(['TPPUGroups.AddFPCGroups ',FPCPPUBaseDir]);
  if FindFirstUTF8(AppendPathDelim(FPCPPUBaseDir)+FileMask,faAnyFile,FileInfo)=0
  then begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
        continue;
      if (faDirectory and FileInfo.Attr)<>0 then begin
        GroupName:=FileInfo.Name;
        for i:=length(GroupName) downto 1 do
          if not (Groupname[i] in ['a'..'z','A'..'Z','0'..'9','_']) then
            System.Delete(GroupName,i,1);
        if (Groupname='') then continue;
        Groupname:=FPCPPUGroupPrefix+LowerCase(Groupname);
        if (not LazIsValidIdent(Groupname,true,true)) then continue;
        AddFPCGroup(GroupName,AppendPathDelim(FPCPPUBaseDir)+FileInfo.Name);
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
end;

procedure TPPUGroups.AddFPCGroup(const BaseGroupname, Directory: string);
var
  FileInfo: TSearchRec;
  Filename: String;
  AUnitName: String;
  Group: TPPUGroup;
  Member: TPPUMember;
  GroupName: String;
begin
  //DebugLn(['TPPUGroups.AddFPCGroup ',Groupname,' ',Directory]);
  Group:=nil;
  if FindFirstUTF8(AppendPathDelim(Directory)+FileMask,faAnyFile,FileInfo)=0
  then begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
        continue;
      Filename:=FileInfo.Name;
      if (CompareFileExt(Filename,'ppu',false)<>0) then continue;
      AUnitName:=ExtractFileNameOnly(Filename);
      Filename:=AppendPathDelim(Directory)+Filename;
      if not LazIsValidIdent(AUnitName,true,true) then begin
        DebugLn(['TPPUGroups.AddFPCGroup NOTE: invalid ppu name: ',Filename]);
        continue;
      end;
      GroupName:=BaseGroupName;
      if BaseGroupname=FPCPPUGroupPrefix+'rtl' then begin
        if (copy(FileInfo.Name,1,3)='si_') then begin
          // the si_* units are program loaders => not for libraries
          continue;
        end;

        if (CompareFilenames(FileInfo.Name,'system.ppu')=0)
        or (CompareFilenames(FileInfo.Name,'dl.ppu')=0)
        then begin
          // the RTL should only contain the minimum for dynamic libs.
          // It looks strange to exclude the dynlibs.ppu, but
          // the dynlibs.ppu uses objpas.ppu, which might not be needed.
          // But: do they hurt?
          GroupName:=BaseGroupName+'_system';
        end else begin
          // all other ppu of the rtl directory need to be loaded separately
          // => put them into separate groups
          GroupName:=BaseGroupName+'_'+lowercase(ExtractFileNameOnly(FileInfo.Name));
        end;
      end;
      if FindGroupWithName(GroupName)=nil then
        DebugLn(['TPPUGroups.AddFPCGroup Creating group ',GroupName]);
      Group:=AddGroup(GroupName);
      Member:=Group.AddMember(AUnitName);
      Member.PPUFilename:=Filename;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
end;

function TPPUGroups.FindGroupWithName(const AName: string): TPPUGroup;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=FindAVLNodeOfGroupWithName(AName);
  if AVLNode<>nil then
    Result:=TPPUGroup(AVLNode.Data)
  else
    Result:=nil;
end;

function TPPUGroups.FindMemberWithUnitName(const AName: string): TPPUMember;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=FindAVLNodeOfMemberWithName(AName);
  if AVLNode<>nil then
    Result:=TPPUMember(AVLNode.Data)
  else
    Result:=nil;
end;

function TPPUGroups.UpdateDependencies: boolean;
var
  AVLNode: TAVLTreeNode;
  Group: TPPUGroup;
  GraphNode: TCodeGraphNode;
begin
  Result:=false;
  FGroupGraph.Clear;
  FUnitGraph.Clear;
  FreeAndNil(FSortedGroups);
  ClearAutoDisableFlags;

  // add nodes to GroupGraph
  AVLNode:=FGroups.FindLowest;
  while AVLNode<>nil do begin
    Group:=TPPUGroup(AVLNode.Data);
    GraphNode:=GroupGraph.AddGraphNode(Group.KeyNode);
    GraphNode.Data:=Group;
    AVLNode:=FGroups.FindSuccessor(AVLNode);
  end;
  // parse PPU
  AVLNode:=FGroups.FindLowest;
  while AVLNode<>nil do begin
    Group:=TPPUGroup(AVLNode.Data);
    if not Group.UpdatePPUs then exit;
    AVLNode:=FGroups.FindSuccessor(AVLNode);
  end;
  // update dependencies
  AVLNode:=FGroups.FindLowest;
  while AVLNode<>nil do begin
    Group:=TPPUGroup(AVLNode.Data);
    if not Group.UpdateDependencies then exit;
    AVLNode:=FGroups.FindSuccessor(AVLNode);
  end;
  // auto disable units with broken dependencies
  AutoDisableUnitsWithBrokenDependencies;
  // sort topologically
  UpdateTopologicalSortedList;
  // update loader units
  if not UpdateLoaders then exit;
  
  Result:=true;
end;

function TPPUGroups.UpdateLoaders: boolean;
var
  i: Integer;
begin
  Result:=true;
  for i:=0 to FSortedGroups.Count-1 do
    if not SortedGroups[i].UpdateLoader then exit(false);
end;

procedure TPPUGroups.AutoDisableUnitsWithBrokenDependencies;
var
  AVLNode: TAVLTreeNode;
  Member: TPPUMember;
  List: TStringList;
begin
  AVLNode:=FMembers.FindLowest;
  List:=TStringList.Create;
  while AVLNode<>nil do begin
    Member:=TPPUMember(AVLNode.Data);
    if not (pmfAutoDisabled in Member.Flags) then begin
      List.Clear;
      Member.GetMissingUnits(TStrings(List));
      if List.Count>0 then begin
        DebugLn(['TPPUGroups.AutoDisableUnitsWithBrokenDependencies auto disabling unit ',Member.Unit_Name,' due to missing units: ',List.DelimitedText]);
        AutoDisableMember(Member);
      end;
    end;
    AVLNode:=FMembers.FindSuccessor(AVLNode);
  end;
  List.Free;
end;

procedure TPPUGroups.AutoDisableMember(Member: TPPUMember);
var
  GraphNode: TCodeGraphNode;
  AVLNode: TAVLTreeNode;
  GraphEdge: TCodeGraphEdge;
  DependingMember: TPPUMember;
begin
  if pmfAutoDisabled in Member.Flags then exit;
  Include(Member.Flags,pmfAutoDisabled);
  GraphNode:=FUnitGraph.GetGraphNode(Member.KeyNode,false);
  if (GraphNode=nil) or (GraphNode.InTree=nil) then exit;
  AVLNode:=GraphNode.InTree.FindLowest;
  while AVLNode<>nil do begin
    GraphEdge:=TCodeGraphEdge(AVLNode.Data);
    DependingMember:=TPPUMember(GraphEdge.FromNode.Data);
    if not (pmfAutoDisabled in DependingMember.Flags) then begin
      DebugLn(['TPPUGroups.AutoDisableMember auto disabling unit ',DependingMember.Unit_Name,' because it uses auto disabled unit ',Member.Unit_Name]);
      AutoDisableMember(DependingMember);
    end;
    AVLNode:=GraphNode.InTree.FindSuccessor(AVLNode);
  end;
end;

procedure TPPUGroups.GetMissingUnits(var List: TStrings);
var
  AVLNode: TAVLTreeNode;
  Group: TPPUGroup;
begin
  AVLNode:=FGroups.FindLowest;
  while AVLNode<>nil do begin
    Group:=TPPUGroup(AVLNode.Data);
    Group.GetMissingUnits(List);
    AVLNode:=FGroups.FindSuccessor(AVLNode);
  end;
end;

end.

