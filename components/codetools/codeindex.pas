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
    Functions and classes to list identifiers of groups of units.
}
unit CodeIndex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree, CodeAtom, CodeTree, CodeCache,
  FileProcs, StdCodeTools;

type
  TCodeBrowserUnit = class;
  TCodeBrowserUnitList = class;


  { TCodeBrowserNode }

  TCodeBrowserNode = class
  private
    FCBUnit: TCodeBrowserUnit;
    FChildNodes: TAVLTree;
    FCodePos: TCodePosition;
    FDesc: TCodeTreeNodeDesc;
    FDescription: string;
    FIdentifier: string;
    FParentNode: TCodeBrowserNode;
  public
    constructor Create(TheUnit: TCodeBrowserUnit;
                       TheParent: TCodeBrowserNode;
                       const TheDescription, TheIdentifier: string);
    destructor Destroy; override;
    procedure Clear;
    function AddNode(const Description, Identifier: string): TCodeBrowserNode;
    property CBUnit: TCodeBrowserUnit read FCBUnit;
    property Desc: TCodeTreeNodeDesc read FDesc write FDesc;
    property CodePos: TCodePosition read FCodePos write FCodePos;
    property ParentNode: TCodeBrowserNode read FParentNode;
    property ChildNodes: TAVLTree read FChildNodes;
    property Description: string read FDescription;
    property Identifier: string read FIdentifier;
  end;


  { TCodeBrowserUnit }

  TCodeBrowserUnit = class
  private
    FChildNodes: TAVLTree;
    FCodeBuffer: TCodeBuffer;
    FCodeTool: TStandardCodeTool;
    FCodeTreeChangeStep: integer;
    FFilename: string;
    FScanned: boolean;
    FScannedBytes: integer;
    FScannedIdentifiers: integer;
    FScannedLines: integer;
    FUnitList: TCodeBrowserUnitList;
    procedure SetCodeBuffer(const AValue: TCodeBuffer);
    procedure SetCodeTool(const AValue: TStandardCodeTool);
    procedure SetScanned(const AValue: boolean);
  public
    constructor Create(const TheFilename: string);
    destructor Destroy; override;
    procedure Clear;
    function AddNode(const Description, Identifier: string): TCodeBrowserNode;
    function ChildNodeCount: integer;
    procedure DeleteNode(var Node: TCodeBrowserNode);
    property Filename: string read FFilename;
    property CodeBuffer: TCodeBuffer read FCodeBuffer write SetCodeBuffer;
    property CodeTool: TStandardCodeTool read FCodeTool write SetCodeTool;
    property CodeTreeChangeStep: integer read FCodeTreeChangeStep;
    property UnitList: TCodeBrowserUnitList read FUnitList;
    property ChildNodes: TAVLTree read FChildNodes;
    property ScannedLines: integer read FScannedLines write FScannedLines;
    property ScannedBytes: integer read FScannedBytes write FScannedBytes;
    property ScannedIdentifiers: integer read FScannedIdentifiers write FScannedIdentifiers;
    property Scanned: boolean read FScanned write SetScanned;
  end;


  { TCodeBrowserUnitList }

  TCodeBrowserUnitList = class
  private
    FOwner: string;
    FParentList: TCodeBrowserUnitList;
    FScannedUnits: integer;
    FUnitLists: TAVLTree;
    FUnits: TAVLTree;
    FUnitsValid: boolean;
    fClearing: boolean;
    procedure SetOwner(const AValue: string);
    procedure InternalAddUnitList(List: TCodeBrowserUnitList);
    procedure InternalRemoveUnitList(List: TCodeBrowserUnitList);
    procedure InternalAddUnit(AnUnit: TCodeBrowserUnit);
    procedure InternalRemoveUnit(AnUnit: TCodeBrowserUnit);
  public
    constructor Create(TheOwner: string; TheParent: TCodeBrowserUnitList);
    destructor Destroy; override;
    procedure Clear;
    function FindUnit(const Filename: string): TCodeBrowserUnit;
    function FindUnitList(const OwnerName: string): TCodeBrowserUnitList;
    function UnitCount: integer;
    function UnitListCount: integer;
    function IsEmpty: boolean;
    procedure DeleteUnit(AnUnit: TCodeBrowserUnit);
    function AddUnit(const Filename: string): TCodeBrowserUnit;
    procedure AddUnit(AnUnit: TCodeBrowserUnit);
    property Owner: string read FOwner write SetOwner;// IDE, project, package
    property ParentList: TCodeBrowserUnitList read FParentList;
    property Units: TAVLTree read FUnits;
    property UnitLists: TAVLTree read FUnitLists;
    property UnitsValid: boolean read FUnitsValid write FUnitsValid;
    property ScannedUnits: integer read FScannedUnits write FScannedUnits;
  end;

function CompareUnitListOwners(Data1, Data2: Pointer): integer;
function ComparePAnsiStringWithUnitListOwner(Data1, Data2: Pointer): integer;
function CompareUnitFilenames(Data1, Data2: Pointer): integer;
function ComparePAnsiStringWithUnitFilename(Data1, Data2: Pointer): integer;
function CompareNodeIdentifiers(Data1, Data2: Pointer): integer;
function ComparePAnsiStringWithNodeIdentifier(Data1, Data2: Pointer): integer;

implementation

function CompareUnitListOwners(Data1, Data2: Pointer): integer;
begin
  Result:=SysUtils.CompareText(TCodeBrowserUnitList(Data1).Owner,
                               TCodeBrowserUnitList(Data2).Owner);
end;

function ComparePAnsiStringWithUnitListOwner(Data1, Data2: Pointer): integer;
begin
  Result:=SysUtils.CompareText(PAnsiString(Data1)^,
                               TCodeBrowserUnitList(Data2).Owner);
end;

function CompareUnitFilenames(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenames(TCodeBrowserUnit(Data1).Filename,
                           TCodeBrowserUnit(Data2).Filename);
end;

function ComparePAnsiStringWithUnitFilename(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenames(PAnsiString(Data1)^,
                           TCodeBrowserUnit(Data2).Filename);
end;

function CompareNodeIdentifiers(Data1, Data2: Pointer): integer;
begin
  Result:=SysUtils.CompareText(TCodeBrowserNode(Data1).Identifier,
                               TCodeBrowserNode(Data2).Identifier);
end;

function ComparePAnsiStringWithNodeIdentifier(Data1, Data2: Pointer): integer;
begin
  Result:=SysUtils.CompareText(PAnsiString(Data1)^,
                               TCodeBrowserNode(Data2).Identifier);
end;

{ TCodeBrowserNode }

constructor TCodeBrowserNode.Create(TheUnit: TCodeBrowserUnit;
  TheParent: TCodeBrowserNode; const TheDescription, TheIdentifier: string);
begin
  FCBUnit:=TheUnit;
  FParentNode:=TheParent;
  FDescription:=TheDescription;
  FIdentifier:=TheIdentifier;
end;

destructor TCodeBrowserNode.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCodeBrowserNode.Clear;
begin
  if FChildNodes<>nil then
    FChildNodes.FreeAndClear;
  FreeAndNil(FChildNodes);
end;

function TCodeBrowserNode.AddNode(const Description,
  Identifier: string): TCodeBrowserNode;
begin
  Result:=TCodeBrowserNode.Create(nil,Self,Description,Identifier);
  if FChildNodes=nil then
    FChildNodes:=TAVLTree.Create(@CompareNodeIdentifiers);
  FChildNodes.Add(Result);
end;

{ TCodeBrowserUnit }

procedure TCodeBrowserUnit.SetScanned(const AValue: boolean);
begin
  if FScanned=AValue then exit;
  FScanned:=AValue;
  FScannedBytes:=0;
  FScannedLines:=0;
  FScannedIdentifiers:=0;
  if UnitList<>nil then begin
    if FScanned then
      inc(UnitList.FScannedUnits)
    else
      dec(UnitList.FScannedUnits);
  end;
end;

procedure TCodeBrowserUnit.SetCodeTool(const AValue: TStandardCodeTool);
begin
  if FCodeTool=nil then exit;
  FCodeTool:=AValue;
end;

procedure TCodeBrowserUnit.SetCodeBuffer(const AValue: TCodeBuffer);
begin
  if FCodeBuffer=AValue then exit;
  FCodeBuffer:=AValue;
end;

constructor TCodeBrowserUnit.Create(const TheFilename: string);
begin
  FFilename:=TheFilename;
end;

destructor TCodeBrowserUnit.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCodeBrowserUnit.Clear;
begin
  if FChildNodes<>nil then
    FChildNodes.FreeAndClear;
  FreeAndNil(FChildNodes);
end;

function TCodeBrowserUnit.AddNode(const Description,
  Identifier: string): TCodeBrowserNode;
begin
  Result:=TCodeBrowserNode.Create(Self,nil,Description,Identifier);
  if FChildNodes=nil then
    FChildNodes:=TAVLTree.Create(@CompareNodeIdentifiers);
  FChildNodes.Add(Result);
end;

function TCodeBrowserUnit.ChildNodeCount: integer;
begin
  if FChildNodes=nil then
    Result:=0
  else
    Result:=FChildNodes.Count;
end;

procedure TCodeBrowserUnit.DeleteNode(var Node: TCodeBrowserNode);
begin
  if Node=nil then exit;
  if ChildNodes<>nil then
    FChildNodes.RemovePointer(Node);
  FreeAndNil(Node);
end;

{ TCodeBrowserUnitList }

procedure TCodeBrowserUnitList.SetOwner(const AValue: string);
begin
  if Owner=AValue then exit;
  if ParentList<>nil then raise Exception.Create('not allowed');
  FOwner:=AValue;
  FUnitsValid:=false;
end;

procedure TCodeBrowserUnitList.InternalAddUnitList(List: TCodeBrowserUnitList);
begin
  if FUnitLists=nil then
    FUnitLists:=TAVLTree.Create(@CompareUnitListOwners);
  FUnitLists.Add(List);
end;

procedure TCodeBrowserUnitList.InternalRemoveUnitList(List: TCodeBrowserUnitList
  );
begin
  if FUnitLists<>nil then
    FUnitLists.Remove(List);
end;

procedure TCodeBrowserUnitList.InternalAddUnit(AnUnit: TCodeBrowserUnit);
begin
  if FUnits=nil then
    FUnits:=TAVLTree.Create(@CompareUnitFilenames);
  FUnits.Add(AnUnit);
  AnUnit.FUnitList:=Self;
end;

procedure TCodeBrowserUnitList.InternalRemoveUnit(AnUnit: TCodeBrowserUnit);
begin
  if (not fClearing) and (FUnits<>nil) then
    FUnits.Remove(AnUnit);
  AnUnit.FUnitList:=nil;
end;

constructor TCodeBrowserUnitList.Create(TheOwner: string;
  TheParent: TCodeBrowserUnitList);
begin
  //DebugLn(['TCodeBrowserUnitList.Create ',TheOwner]);
  //DumpStack;
  FOwner:=TheOwner;
  FParentList:=TheParent;
  if FParentList<>nil then
    FParentList.InternalAddUnitList(Self);
end;

destructor TCodeBrowserUnitList.Destroy;
begin
  Clear;
  if FParentList<>nil then begin
    FParentList.InternalRemoveUnitList(Self);
    FParentList:=nil;
  end;
  inherited Destroy;
end;

procedure TCodeBrowserUnitList.Clear;

  procedure FreeTree(var Tree: TAVLTree);
  var
    TmpTree: TAVLTree;
  begin
    if Tree=nil then exit;
    TmpTree:=Tree;
    Tree:=nil;
    TmpTree.FreeAndClear;
    TmpTree.Free;
  end;

begin
  fClearing:=true;
  try
    FreeTree(FUnits);
    FreeTree(FUnitLists);
    FUnitsValid:=false;
  finally
    fClearing:=false;
  end;
end;

function TCodeBrowserUnitList.FindUnit(const Filename: string
  ): TCodeBrowserUnit;
var
  Node: TAVLTreeNode;
begin
  Result:=nil;
  if Filename='' then exit;
  if FUnits=nil then exit;
  Node:=FUnits.FindKey(@Filename,@ComparePAnsiStringWithUnitFilename);
  if Node=nil then exit;
  Result:=TCodeBrowserUnit(Node.Data);
end;

function TCodeBrowserUnitList.FindUnitList(const OwnerName: string
  ): TCodeBrowserUnitList;
var
  Node: TAVLTreeNode;
begin
  Result:=nil;
  if FUnitLists=nil then exit;
  if OwnerName='' then exit;
  Node:=FUnitLists.FindKey(@OwnerName,@ComparePAnsiStringWithUnitListOwner);
  if Node=nil then exit;
  Result:=TCodeBrowserUnitList(Node.Data);
end;

function TCodeBrowserUnitList.UnitCount: integer;
begin
  if FUnits=nil then
    Result:=0
  else
    Result:=FUnits.Count;
end;

function TCodeBrowserUnitList.UnitListCount: integer;
begin
  if FUnitLists=nil then
    Result:=0
  else
    Result:=FUnitLists.Count;
end;

function TCodeBrowserUnitList.IsEmpty: boolean;
begin
  Result:=(UnitCount=0) and (UnitListCount=0);
end;

procedure TCodeBrowserUnitList.DeleteUnit(AnUnit: TCodeBrowserUnit);
begin
  if AnUnit=nil then exit;
  if FUnits=nil then exit;
  FUnits.Remove(AnUnit);
  AnUnit.Free;
end;

function TCodeBrowserUnitList.AddUnit(const Filename: string
  ): TCodeBrowserUnit;
begin
  Result:=TCodeBrowserUnit.Create(Filename);
  InternalAddUnit(Result);
end;

procedure TCodeBrowserUnitList.AddUnit(AnUnit: TCodeBrowserUnit);
begin
  if (AnUnit.UnitList=Self) then exit;
  if AnUnit.UnitList<>nil then
    AnUnit.UnitList.InternalRemoveUnit(AnUnit);
  InternalAddUnit(AnUnit);
end;

end.

