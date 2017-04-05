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
    Most codetools returns simple values like a single code position or a
    string. But some creates lists of data.
    This unit provides structures for complex results.
}
unit CodeToolsStructs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  // LazUtils
  LazUtilities, LazDbgLog, AvgLvlTree,
  // Codetools
  BasicCodeTools;

type
  TResourcestringInsertPolicy = (
    rsipNone,          // do not add/insert
    rsipAppend,        // append at end
    rsipAlphabetically,// insert alphabetically
    rsipContext        // insert context sensitive
    );

  TPascalClassSection = (
    pcsPrivate,
    pcsProtected,
    pcsPublic,
    pcsPublished
    );
  TPascalClassSections = set of TPascalClassSection;
  
const
  AllPascalClassSections = [low(TPascalClassSection)..high(TPascalClassSection)];
  
const
  PascalClassSectionKeywords: array[TPascalClassSection] of string = (
    'private',
    'protected',
    'public',
    'published'
    );

type

  { TMTAVLTreeNodeMemManager }

  TMTAVLTreeNodeMemManager = class(TAVLTreeNodeMemManager)
  public
    procedure DisposeNode(ANode: TAVLTreeNode); override;
    function NewNode: TAVLTreeNode; override;
  end;

  { TMTAVLTree - TAVLTree with a multithreaded node manager }

  TMTAVLTree = class(TAVLTree)
  protected
    fNodeManager: TAVLTreeNodeMemManager;
  public
    constructor Create(OnCompareMethod: TListSortCompare);
    destructor Destroy; override;
  end;

  TPointerToPointerItem = record
    Key, Value: Pointer;
  end;
  PPointerToPointerItem = ^TPointerToPointerItem;

  { TPointerToPointerTree }

  TPointerToPointerTree = class
  private
    FTree: TAVLTree;// tree of PPointerToPointerItem
    function GetItems(Key: Pointer): Pointer;
    procedure SetItems(Key: Pointer; AValue: Pointer);
  protected
    procedure DisposeItem(p: PPointerToPointerItem); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    function Contains(Key: Pointer): boolean;
    procedure Remove(Key: Pointer); virtual;
    property Tree: TAVLTree read FTree; // tree of PPointerToPointerItem
    function GetNodeData(AVLNode: TAVLTreeNode): PPointerToPointerItem; inline;
    function Count: integer;
    function FindNode(Key: Pointer): TAVLTreeNode;
    procedure Add(Key, Value: Pointer); virtual;
    property Items[Key: Pointer]: Pointer read GetItems write SetItems; default;
  end;

  { TIdentStringToStringTree }

  TIdentStringToStringTree = class(TStringToStringTree)
  private
  protected
  public
    function FindNodeWithIdentifierAsPrefix(P: PChar): TAVLTreeNode;
  end;

  TStringTree = class;

  { TStringTreeEnumerator }

  TStringTreeEnumerator = class
  private
    FTree: TStringTree;
    FCurrent: TAVLTreeNode;
    function GetCurrent: string;
  public
    constructor Create(Tree: TStringTree);
    function MoveNext: boolean;
    property Current: string read GetCurrent;
  end;

  { TStringTree }

  TStringTree = class
  public
    Tree: TAVLTree;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function FindNode(const s: string): TAVLTreeNode; inline;
    procedure ReplaceString(var s: string);
    function CalcMemSize: PtrUInt;
    function GetEnumerator: TStringTreeEnumerator;
  end;

type
  TCTComponentAccess = class(TComponent);

  { TComponentChildCollector }

  TComponentChildCollector = class
  private
    FChildren: TFPList;
    FRoot: TComponent;
    procedure AddChildComponent(Child: TComponent);
  public
    constructor Create;
    destructor Destroy; override;
    function GetComponents(RootComponent: TComponent; AddRoot: boolean = true): TFPList;
    property Children: TFPList read FChildren;
    property Root: TComponent read FRoot;
  end;


function ComparePointerToPointerItems(Data1, Data2: Pointer): integer;
function ComparePointerAndP2PItem(Key, Data: Pointer): integer;

// case sensitive
//function CompareStringToStringItems(Data1, Data2: Pointer): integer;
//function CompareStringAndStringToStringTreeItem(Key, Data: Pointer): integer;
//function CompareIdentifierAndStringToStringTreeItem(Identifier, Data: Pointer): integer;
function CompareIdentifierPrefixAndStringToStringTreeItem(Identifier, Data: Pointer): integer;

// case insensitive
//function CompareStringToStringItemsI(Data1, Data2: Pointer): integer;
//function CompareStringAndStringToStringTreeItemI(Key, Data: Pointer): integer;
//function CompareIdentifierAndStringToStringTreeItemI(Identifier, Data: Pointer): integer;
function CompareIdentifierPrefixAndStringToStringTreeItemI(Identifier, Data: Pointer): integer;

//function CompareFilenameToStringItems(Data1, Data2: Pointer): integer;
//function CompareFilenameAndFilenameToStringTreeItem(Key, Data: Pointer): integer;

//function CompareFilenameToStringItemsI(Data1, Data2: Pointer): integer;
//function CompareFilenameAndFilenameToStringTreeItemI(Key, Data: Pointer): integer;

function CompareAnsiStringPtrs(Data1, Data2: Pointer): integer;

function AVLFindPointer(Tree: TAVLTree; Data: Pointer): TAVLTreeNode; inline;
procedure AVLRemovePointer(Tree: TAVLTree; Data: Pointer); inline;

implementation

function ComparePointerToPointerItems(Data1, Data2: Pointer): integer;
var
  P2PItem1: PPointerToPointerItem absolute Data1;
  P2PItem2: PPointerToPointerItem absolute Data2;
begin
  Result:=ComparePointers(P2PItem1^.Key,P2PItem2^.Key);
end;

function ComparePointerAndP2PItem(Key, Data: Pointer): integer;
var
  P2PItem: PPointerToPointerItem absolute Data;
begin
  Result:=ComparePointers(Key,P2PItem^.Key);
end;
{
function CompareStringToStringItems(Data1, Data2: Pointer): integer;
begin
  Result:=CompareStr(PStringToStringTreeItem(Data1)^.Name,
                     PStringToStringTreeItem(Data2)^.Name);
end;

function CompareStringToStringItemsI(Data1, Data2: Pointer): integer;
begin
  Result:=CompareText(PStringToStringTreeItem(Data1)^.Name,
                      PStringToStringTreeItem(Data2)^.Name);
end;

function CompareFilenameToStringItems(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenames(PStringToStringTreeItem(Data1)^.Name,
                           PStringToStringTreeItem(Data2)^.Name);
end;

function CompareStringAndStringToStringTreeItem(Key, Data: Pointer): integer;
begin
  Result:=CompareStr(String(Key),PStringToStringTreeItem(Data)^.Name);
end;

function CompareIdentifierAndStringToStringTreeItem(Identifier, Data: Pointer
  ): integer;
var
  Id: PChar absolute Identifier;
  Item: PStringToStringTreeItem absolute Data;
  IdLen: LongInt;
  ItemLen: PtrInt;
begin
  Result:=-CompareIdentifiersCaseSensitive(Id,PChar(Item^.Name));
  if Result=0 then begin
    IdLen:=GetIdentLen(Id);
    ItemLen:=length(Item^.Name);
    if IdLen=Itemlen then
      Result:=0
    else if IdLen>ItemLen then
      Result:=1
    else
      Result:=-1;
  end;
end;
}
function CompareIdentifierPrefixAndStringToStringTreeItem(Identifier, Data: Pointer): integer;
var
  Id: PChar absolute Identifier;
  Item: PStringToStringItem absolute Data;
begin
  Result:=-CompareIdentifiersCaseSensitive(Id,PChar(Item^.Name));
end;
{
function CompareStringAndStringToStringTreeItemI(Key, Data: Pointer): integer;
begin
  Result:=CompareText(String(Key),PStringToStringTreeItem(Data)^.Name);
end;

function CompareIdentifierAndStringToStringTreeItemI(Identifier, Data: Pointer
  ): integer;
var
  Id: PChar absolute Identifier;
  Item: PStringToStringTreeItem absolute Data;
  IdLen: LongInt;
  ItemLen: PtrInt;
begin
  Result:=-CompareIdentifiers(Id,PChar(Item^.Name));
  if Result=0 then begin
    IdLen:=GetIdentLen(Id);
    ItemLen:=length(Item^.Name);
    if IdLen=Itemlen then
      Result:=0
    else if IdLen>ItemLen then
      Result:=1
    else
      Result:=-1;
  end;
end;
}
function CompareIdentifierPrefixAndStringToStringTreeItemI(Identifier, Data: Pointer): integer;
var
  Id: PChar absolute Identifier;
  Item: PStringToStringItem absolute Data;
begin
  Result:=-CompareIdentifiers(Id,PChar(Item^.Name));
end;
{
function CompareFilenameAndFilenameToStringTreeItem(Key, Data: Pointer): integer;
begin
  Result:=CompareFilenames(String(Key),PStringToStringTreeItem(Data)^.Name);
end;

function CompareFilenameToStringItemsI(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenamesIgnoreCase(PStringToStringTreeItem(Data1)^.Name,
                                     PStringToStringTreeItem(Data2)^.Name);
end;

function CompareFilenameAndFilenameToStringTreeItemI(Key, Data: Pointer): integer;
begin
  Result:=CompareFilenamesIgnoreCase(String(Key),
                                     PStringToStringTreeItem(Data)^.Name);
end;
}
function CompareAnsiStringPtrs(Data1, Data2: Pointer): integer;
begin
  Result:=CompareStr(AnsiString(Data1),AnsiString(Data2));
end;

function AVLFindPointer(Tree: TAVLTree; Data: Pointer): TAVLTreeNode;
begin
  Result:=Tree.FindPointer(Data);
end;

procedure AVLRemovePointer(Tree: TAVLTree; Data: Pointer);
begin
  Tree.RemovePointer(Data);
end;

{ TPointerToPointerTree }

function TPointerToPointerTree.GetItems(Key: Pointer): Pointer;
var
  Node: TAVLTreeNode;
begin
  Node:=FindNode(Key);
  if Node<>nil then
    Result:=PPointerToPointerItem(Node.Data)^.Value
  else
    Result:=nil;
end;

procedure TPointerToPointerTree.SetItems(Key: Pointer; AValue: Pointer);
var
  Node: TAVLTreeNode;
  NewItem: PPointerToPointerItem;
begin
  Node:=FindNode(Key);
  if Node<>nil then begin
    PPointerToPointerItem(Node.Data)^.Value:=AValue;
  end else begin
    New(NewItem);
    NewItem^.Key:=Key;
    NewItem^.Value:=AValue;
    FTree.Add(NewItem);
  end;
end;

procedure TPointerToPointerTree.DisposeItem(p: PPointerToPointerItem);
begin
  Dispose(p);
end;

constructor TPointerToPointerTree.Create;
begin
  FTree:=TMTAVLTree.Create(@ComparePointerToPointerItems);
end;

destructor TPointerToPointerTree.Destroy;
begin
  Clear;
  FreeAndNil(FTree);
  inherited Destroy;
end;

procedure TPointerToPointerTree.Clear;
var
  Node: TAVLTreeNode;
begin
  Node:=FTree.FindLowest;
  while Node<>nil do begin
    DisposeItem(PPointerToPointerItem(Node.Data));
    Node:=FTree.FindSuccessor(Node);
  end;
  FTree.Clear;
end;

function TPointerToPointerTree.Contains(Key: Pointer): boolean;
begin
  Result:=FindNode(Key)<>nil;
end;

procedure TPointerToPointerTree.Remove(Key: Pointer);
var
  Node: TAVLTreeNode;
  Item: PPointerToPointerItem;
begin
  Node:=FindNode(Key);
  if Node<>nil then begin
    Item:=PPointerToPointerItem(Node.Data);
    FTree.Delete(Node);
    DisposeItem(Item);
  end;
end;

function TPointerToPointerTree.GetNodeData(AVLNode: TAVLTreeNode): PPointerToPointerItem;
begin
  Result:=PPointerToPointerItem(AVLNode.Data);
end;

function TPointerToPointerTree.Count: integer;
begin
  Result:=FTree.Count;
end;

function TPointerToPointerTree.FindNode(Key: Pointer): TAVLTreeNode;
begin
  Result:=FTree.FindKey(Key,@ComparePointerAndP2PItem);
end;

procedure TPointerToPointerTree.Add(Key, Value: Pointer);
begin
  Items[Key]:=Value;
end;

{ TMTAVLTree }

constructor TMTAVLTree.Create(OnCompareMethod: TListSortCompare);
begin
  inherited Create(OnCompareMethod);
  fNodeManager:=TMTAVLTreeNodeMemManager.Create;
  SetNodeManager(fNodeManager);
end;

destructor TMTAVLTree.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fNodeManager);
end;

{ TMTAVLTreeNodeMemManager }

procedure TMTAVLTreeNodeMemManager.DisposeNode(ANode: TAVLTreeNode);
begin
  ANode.Free;
end;

function TMTAVLTreeNodeMemManager.NewNode: TAVLTreeNode;
begin
  Result:=TAVLTreeNode.Create;
end;

{ TIdentStringToStringTree }

function TIdentStringToStringTree.FindNodeWithIdentifierAsPrefix(P: PChar): TAVLTreeNode;
begin
  if CaseSensitive then
    Result:=Tree.FindKey(p,@CompareIdentifierPrefixAndStringToStringTreeItem)
  else
    Result:=Tree.FindKey(p,@CompareIdentifierPrefixAndStringToStringTreeItemI);
end;

{ TStringTreeEnumerator }

function TStringTreeEnumerator.GetCurrent: string;
begin
  Result:=AnsiString(FCurrent.Data);
end;

constructor TStringTreeEnumerator.Create(Tree: TStringTree);
begin
  FTree:=Tree;
end;

function TStringTreeEnumerator.MoveNext: boolean;
begin
  if FCurrent=nil then
    FCurrent:=FTree.Tree.FindLowest
  else
    FCurrent:=FTree.Tree.FindSuccessor(FCurrent);
  Result:=FCurrent<>nil;
end;

{ TStringTree }

constructor TStringTree.Create;
begin
  Tree:=TMTAVLTree.Create(@CompareAnsiStringPtrs);
end;

destructor TStringTree.Destroy;
begin
  Clear;
  FreeAndNil(Tree);
  inherited Destroy;
end;

procedure TStringTree.Clear;
var
  Node: TAVLTreeNode;
begin
  Node:=Tree.FindLowest;
  while Node<>nil do begin
    AnsiString(Node.Data):='';
    Node:=Tree.FindSuccessor(Node);
  end;
  Tree.Clear;
end;

function TStringTree.FindNode(const s: string): TAVLTreeNode;
begin
  Result:=Tree.Find(Pointer(s));
end;

procedure TStringTree.ReplaceString(var s: string);
var
  Node: TAVLTreeNode;
  h: String;
begin
  if GetStringRefCount(s)<=0 then exit;
  Node:=FindNode(s);
  if Node=nil then begin
    // increase refcount
    h:=s;
    Tree.Add(Pointer(h));
    Pointer(h):=nil; // keep refcount
    //debugln(['TStringTree.ReplaceString new string: refcount=',GetStringRefCount(s)]);
    //debugln(['TStringTree.ReplaceString NewString="',dbgstr(s),'"']);
  end else begin
    s:=AnsiString(Node.Data);
    //debugln(['TStringTree.ReplaceString old string: refcount=',GetStringRefCount(s)]);
    //debugln(['TStringTree.ReplaceString OldString="',dbgstr(s),'"']);
  end;
  //debugln(['TStringTree.ReplaceString ',GetStringRefCount(s),' ',Node<>nil]);
end;

function TStringTree.CalcMemSize: PtrUInt;
var
  Node: TAVLTreeNode;
begin
  Result:=PtrUInt(InstanceSize)
    +PtrUInt(Tree.InstanceSize)
    +PtrUInt(TAVLTreeNode.InstanceSize)*PtrUInt(Tree.Count);
  Node:=Tree.FindLowest;
  while Node<>nil do begin
    inc(Result,MemSizeString(AnsiString(Node.Data)));
    Node:=Tree.FindSuccessor(Node);
  end;
end;

function TStringTree.GetEnumerator: TStringTreeEnumerator;
begin
  Result:=TStringTreeEnumerator.Create(Self);
end;

{ TComponentChildCollector }

procedure TComponentChildCollector.AddChildComponent(Child: TComponent);
var
  OldRoot: TComponent;
begin
  //debugln(['TComponentChildCollector.AddChildComponent ',DbgSName(Child)]);
  Children.Add(Child);
  OldRoot := Root;
  try
    if csInline in Child.ComponentState then
      FRoot := Child;
    TCTComponentAccess(Child).GetChildren(@AddChildComponent,Root);
  finally
    FRoot := OldRoot;
  end;
end;

constructor TComponentChildCollector.Create;
begin
  FChildren:=TFPList.Create;
end;

destructor TComponentChildCollector.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;

function TComponentChildCollector.GetComponents(RootComponent: TComponent;
  AddRoot: boolean): TFPList;
begin
  Children.Clear;
  if AddRoot then
    Children.Add(RootComponent);
  FRoot:=RootComponent;
  TCTComponentAccess(RootComponent).GetChildren(@AddChildComponent,FRoot);
  Result:=Children;
end;

end.

