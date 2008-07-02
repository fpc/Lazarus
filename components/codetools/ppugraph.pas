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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
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
  Classes, SysUtils, PPUParser, CodeTree, AVL_Tree, FileProcs, BasicCodeTools,
  CodeGraph;
  
type
  TPPUGroup = class;

  { TPPUMember }

  TPPUMember = class
  public
    Unitname: string;
    PPUFilename: string;
    KeyNode: TCodeTreeNode;
    InitializationMangledName: string;
    FinalizationMangledName: string;
    MainUses: TStrings;
    ImplementationUses: TStrings;
    Group: TPPUGroup;
    PPU: TPPU;
    constructor Create;
    destructor Destroy; override;
    function UpdatePPU: boolean;
    procedure GetMissingUnits(var List: TStrings);
  end;

  TPPUGroups = class;

  { TPPUGroup }

  TPPUGroup = class
  private
    FMembers: TAVLTree;// tree of TPPUMember sorted for unitname
    FUnitGraph: TCodeGraph;
    function FindAVLNodeOfMemberWithUnitName(const AName: string): TAVLTreeNode;
    procedure InternalRemoveMember(AMember: TPPUMember);
  public
    Name: string;
    KeyNode: TCodeTreeNode;
    Groups: TPPUGroups;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddMember(const NewUnitName: string): TPPUMember;
    function FindMemberWithUnitName(const AName: string): TPPUMember;
    function UpdateDependencies: boolean;
    procedure GetMissingUnits(var List: TStrings);
    property UnitGraph: TCodeGraph read FUnitGraph;
  end;

  { TPPUGroups }

  TPPUGroups = class
  private
    FGroups: TAVLTree;// tree of TPPUGroup sorted for name
    FMembers: TAVLTree;// tree of TPPUMember sorted for unitname
    FGroupGraph: TCodeGraph;
    function FindAVLNodeOfGroupWithName(const AName: string): TAVLTreeNode;
    function FindAVLNodeOfMemberWithName(const AName: string): TAVLTreeNode;
    procedure InternalRemoveMember(AMember: TPPUMember);
    procedure InternalRemoveGroup(AGroup: TPPUGroup);
    procedure AddDependency(Member: TPPUMember; const UsedUnit: string);
  public
    Name: string;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddGroup(const NewName: string): TPPUGroup;
    function FindGroupWithName(const AName: string): TPPUGroup;
    function FindMemberWithUnitName(const AName: string): TPPUMember;
    function UpdateDependencies: boolean;
    procedure GetMissingUnits(var List: TStrings);
    property GroupGraph: TCodeGraph read FGroupGraph;
  end;
  
function ComparePPUMembersByUnitName(Member1, Member2: Pointer): integer;
function CompareNameWithPPUMemberName(NamePChar, Member: Pointer): integer;

function ComparePPUGroupsByName(Group1, Group2: Pointer): integer;
function CompareNameWithPPUGroupName(NamePChar, Group: Pointer): integer;


implementation

function ComparePPUMembersByUnitName(Member1, Member2: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(Pointer(TPPUMember(Member1).Unitname),
                                Pointer(TPPUMember(Member2).Unitname));
end;

function CompareNameWithPPUMemberName(NamePChar, Member: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(NamePChar,Pointer(TPPUMember(Member).Unitname));
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

{ TPPUMember }

constructor TPPUMember.Create;
begin
  KeyNode:=NodeMemManager.NewNode;
  MainUses:=TStringList.Create;
  ImplementationUses:=TStringList.Create;
end;

destructor TPPUMember.Destroy;
begin
  FreeAndNil(PPU);
  FreeAndNil(MainUses);
  FreeAndNil(ImplementationUses);
  if KeyNode<>nil then
    NodeMemManager.DisposeNode(KeyNode);
  KeyNode:=nil;
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
  if PPU=nil then PPU:=TPPU.Create;
  PPU.LoadFromFile(PPUFilename);
  debugln('================================================================');
  DebugLn(['TPPUMember.UpdateDependencies UnitName=',Unitname,' Filename=',PPUFilename]);
  //PPU.Dump('');
  PPU.GetMainUsesSectionNames(MainUses);
  debugln('Main used units: ',MainUses.DelimitedText);
  PPU.GetImplementationUsesSectionNames(ImplementationUses);
  debugln('Implementation used units: ',ImplementationUses.DelimitedText);
  InitializationMangledName:=PPU.GetInitProcName;
  debugln('Initialization proc: ',InitializationMangledName);
  FinalizationMangledName:=PPU.GetFinalProcName;
  debugln('Finalization proc: ',FinalizationMangledName);
  
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

procedure TPPUGroup.InternalRemoveMember(AMember: TPPUMember);
begin
  FUnitGraph.DeleteGraphNode(AMember.KeyNode);
  FMembers.RemovePointer(AMember);
  if Groups<>nil then
    Groups.InternalRemoveMember(AMember);
end;

constructor TPPUGroup.Create;
begin
  FMembers:=TAVLTree.Create(@ComparePPUMembersByUnitName);
  KeyNode:=NodeMemManager.NewNode;
  FUnitGraph:=TCodeGraph.Create;
end;

destructor TPPUGroup.Destroy;
begin
  Clear;
  FreeAndNil(FUnitGraph);
  FreeAndNil(FMembers);
  if KeyNode<>nil then
    NodeMemManager.DisposeNode(KeyNode);
  KeyNode:=nil;
  if Groups<>nil then
    Groups.InternalRemoveGroup(Self);
  inherited Destroy;
end;

procedure TPPUGroup.Clear;
begin
  FUnitGraph.Clear;
  while FMembers.Count>0 do
    TPPUMember(FMembers.Root.Data).Free;
end;

function TPPUGroup.AddMember(const NewUnitName: string): TPPUMember;
begin
  Result:=FindMemberWithUnitName(NewUnitName);
  if Result<>nil then exit;
  Result:=TPPUMember.Create;
  Result.Unitname:=NewUnitName;
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

function TPPUGroup.UpdateDependencies: boolean;

  procedure AddDependencies(Member: TPPUMember; UsesList: TStrings);
  var
    i: Integer;
  begin
    if UsesList=nil then exit;
    for i:=0 to UsesList.Count-1 do
      Groups.AddDependency(Member,UsesList[i]);
  end;

var
  AVLNode: TAVLTreeNode;
  Member: TPPUMember;
begin
  Result:=false;
  FUnitGraph.Clear;

  // create nodes in the UnitGraph
  AVLNode:=FMembers.FindLowest;
  while AVLNode<>nil do begin
    Member:=TPPUMember(AVLNode.Data);
    UnitGraph.AddGraphNode(Member.KeyNode);
    AVLNode:=FMembers.FindSuccessor(AVLNode);
  end;

  // load all PPU
  AVLNode:=FMembers.FindLowest;
  while AVLNode<>nil do begin
    Member:=TPPUMember(AVLNode.Data);
    if not Member.UpdatePPU then exit;
    AddDependencies(Member,Member.MainUses);
    AddDependencies(Member,Member.ImplementationUses);
    AVLNode:=FMembers.FindSuccessor(AVLNode);
  end;

  Result:=true;
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

procedure TPPUGroups.InternalRemoveMember(AMember: TPPUMember);
begin
  FMembers.RemovePointer(AMember);
end;

procedure TPPUGroups.InternalRemoveGroup(AGroup: TPPUGroup);
begin
  FGroups.RemovePointer(AGroup);
end;

procedure TPPUGroups.AddDependency(Member: TPPUMember; const UsedUnit: string);
var
  UsedMember: TPPUMember;
begin
  UsedMember:=FindMemberWithUnitName(UsedUnit);
  if UsedMember=nil then exit;
  if Member.Group=UsedMember.Group then begin
    Member.Group.UnitGraph.AddEdge(Member.KeyNode,UsedMember.KeyNode);
  end else begin
    GroupGraph.AddEdge(Member.Group.KeyNode,UsedMember.Group.KeyNode);
  end;
end;

constructor TPPUGroups.Create;
begin
  FGroups:=TAVLTree.Create(@ComparePPUGroupsByName);
  FMembers:=TAVLTree.Create(@ComparePPUMembersByUnitName);
  FGroupGraph:=TCodeGraph.Create;
end;

destructor TPPUGroups.Destroy;
begin
  Clear;
  FreeAndNil(FGroupGraph);
  FreeAndNil(FGroups);
  FreeAndNil(FMembers);
  inherited Destroy;
end;

procedure TPPUGroups.Clear;
begin
  FGroupGraph.Clear;
  while FGroups.Count>0 do
    TPPUGroup(FGroups.Root.Data).Free;
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
begin
  FGroupGraph.Clear;
  Result:=false;
  
  // add nodes to GroupGraph
  AVLNode:=FGroups.FindLowest;
  while AVLNode<>nil do begin
    Group:=TPPUGroup(AVLNode.Data);
    GroupGraph.AddGraphNode(Group.KeyNode);
    AVLNode:=FGroups.FindSuccessor(AVLNode);
  end;
  // parse PPU
  AVLNode:=FGroups.FindLowest;
  while AVLNode<>nil do begin
    Group:=TPPUGroup(AVLNode.Data);
    if not Group.UpdateDependencies then exit;
    AVLNode:=FGroups.FindSuccessor(AVLNode);
  end;
  Result:=true;
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

