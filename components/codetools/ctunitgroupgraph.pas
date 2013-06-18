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
    Functions and classes to build dependency graphs for groups of pascal units.
}
unit CTUnitGroupGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree, CTUnitGraph;

type
  TUGGroup = class;
  TUGGroups = class;

  { TUGGroupUnit }

  TUGGroupUnit = class(TUGUnit)
  public
    Group: TUGGroup;
  end;

  TUGGroup = class
  private
    FBaseDir: string;
    FGroups: TUGGroups;
    FName: string;
    FUnits: TAVLTree;
    procedure SetName(AValue: string);
  public
    constructor Create(aName: string; TheGroups: TUGGroups);
    destructor Destroy; override;
    procedure Clear;
    procedure AddUnit(anUnit: TUGGroupUnit);
    procedure RemoveUnit(anUnit: TUGGroupUnit);
    property Name: string read FName write SetName;
    property BaseDir: string read FBaseDir write FBaseDir;
    property Groups: TUGGroups read FGroups;
    property Units: TAVLTree read FUnits; // tree of TUGGroupUnit sorted for Filename
  end;

  { TUGGroups }

  TUGGroups = class
  private
    fClearing: boolean;
    FGroups: TAVLTree;
  public
    constructor Create(Graph: TUsesGraph);
    destructor Destroy; override;
    procedure Clear;
    function GetGroup(Name: string; CreateIfNotExists: boolean): TUGGroup;
    property Groups: TAVLTree read FGroups; // tree of TUGGroup sorted for Name
  end;

function CompareUGGroupNames(UGGroup1, UGGroup2: Pointer): integer;
function CompareNameAndUGGroup(NameAnsistring, UGGroup: Pointer): integer;

implementation

function CompareUGGroupNames(UGGroup1, UGGroup2: Pointer): integer;
var
  Group1: TUGGroup absolute UGGroup1;
  Group2: TUGGroup absolute UGGroup2;
begin
  Result:=SysUtils.CompareText(Group1.Name,Group2.Name);
end;

function CompareNameAndUGGroup(NameAnsistring, UGGroup: Pointer): integer;
var
  Group: TUGGroup absolute UGGroup;
  Name: String;
begin
  Name:=AnsiString(NameAnsistring);
  Result:=SysUtils.CompareText(Name,Group.Name);
end;

{ TUGGroups }

constructor TUGGroups.Create(Graph: TUsesGraph);
begin
  if (not Graph.UnitClass.InheritsFrom(TUGGroup))
  and ((Graph.FilesTree.Count>0) or (Graph.QueuedFilesTree.Count>0)
     or (Graph.TargetFilesTree.Count>0))
  then
    raise Exception.Create('TUGGroups.Create You must create TUGGroups before adding units');
  FGroups:=TAVLTree.Create(@CompareUGGroupNames);
  Graph.UnitClass:=TUGGroupUnit;
end;

destructor TUGGroups.Destroy;
begin
  Clear;
  FreeAndNil(FGroups);
  inherited Destroy;
end;

procedure TUGGroups.Clear;
begin
  if FGroups=nil then exit;
  fClearing:=true;
  try
    FGroups.FreeAndClear;
  finally
    fClearing:=false;
  end;
end;

function TUGGroups.GetGroup(Name: string; CreateIfNotExists: boolean): TUGGroup;
var
  Node: TAVLTreeNode;
begin
  Node:=FGroups.FindKey(Pointer(Name),@CompareNameAndUGGroup);
  if Node<>nil then begin
    Result:=TUGGroup(Node.Data);
  end else if CreateIfNotExists then begin
    Result:=TUGGroup.Create(Name,Self);
    FGroups.Add(Result);
  end else
   Result:=nil;
end;

{ TUGGroup }

procedure TUGGroup.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  if Groups.GetGroup(AValue,false)<>nil then
    raise Exception.Create('TUGGroup.SetName name already exists');
  Groups.fGroups.Remove(Self);
  FName:=AValue;
  Groups.fGroups.Add(Self);
end;

constructor TUGGroup.Create(aName: string; TheGroups: TUGGroups);
begin
  FName:=aName;
  FGroups:=TheGroups;
  FUnits:=TAVLTree.Create(@CompareUGUnitFilenames);
end;

destructor TUGGroup.Destroy;
begin
  Clear;
  if not Groups.fClearing then
    Groups.FGroups.Remove(Self);
  fGroups:=nil;
  FreeAndNil(FUnits);
  inherited Destroy;
end;

procedure TUGGroup.Clear;
var
  Node: TAVLTreeNode;
begin
  Node:=FUnits.FindLowest;
  while Node<>nil do begin
    TUGGroupUnit(Node.Data).Group:=nil;
    Node:=FUnits.FindSuccessor(Node);
  end;
  FUnits.Clear;
end;

procedure TUGGroup.AddUnit(anUnit: TUGGroupUnit);
begin
  if anUnit.Group<>nil then begin
    anUnit.Group.FUnits.Remove(anUnit);
    anUnit.Group:=nil;
  end;
  FUnits.Add(anUnit);
  anUnit.Group:=Self;
end;

procedure TUGGroup.RemoveUnit(anUnit: TUGGroupUnit);
begin
  if (anUnit.Group<>nil) and (anUnit.Group<>Self) then
    raise Exception.Create('TUGGroup.RemoveUnit inconsistency');
  FUnits.Remove(anUnit);
  anUnit.Group:=nil;
end;

end.

