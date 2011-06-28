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
    Most codetools returns simple values like a single code position or a
    string. But some creates lists of data.
    This unit provides structures for complex results.
  
    TCodeXYPositions - a list of PCodeXYPosition

}
unit CodeToolsStructs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs, AVL_Tree, CodeCache, CodeAtom;
  
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
  
type

  { TCodeXYPositions - a list of PCodeXYPosition }

  TCodeXYPositions = class
  private
    FItems: TFPList; // list of PCodeXYPosition, can be nil
    function GetCaretsXY(Index: integer): TPoint;
    function GetCodes(Index: integer): TCodeBuffer;
    function GetItems(Index: integer): PCodeXYPosition;
    procedure SetCaretsXY(Index: integer; const AValue: TPoint);
    procedure SetCodes(Index: integer; const AValue: TCodeBuffer);
    procedure SetItems(Index: integer; const AValue: PCodeXYPosition);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(const Position: TCodeXYPosition): integer;
    function Add(X,Y: integer; Code: TCodeBuffer): integer;
    procedure Assign(Source: TCodeXYPositions);
    function IsEqual(Source: TCodeXYPositions): boolean;
    function Count: integer;
    procedure Delete(Index: integer);
    function CreateCopy: TCodeXYPositions;
    function CalcMemSize: PtrUint;
  public
    property Items[Index: integer]: PCodeXYPosition
                                          read GetItems write SetItems; default;
    property CaretsXY[Index: integer]: TPoint read GetCaretsXY write SetCaretsXY;
    property Codes[Index: integer]: TCodeBuffer read GetCodes write SetCodes;
  end;
  
const
  PascalClassSectionKeywords: array[TPascalClassSection] of string = (
    'private',
    'protected',
    'public',
    'published'
    );


type
  TStringToStringTreeItem = record
    Name: string;
    Value: string;
  end;
  PStringToStringTreeItem = ^TStringToStringTreeItem;

  { TStringToStringTree }

  TStringToStringTree = class
  private
    FCompareKeyItemFunc: TListSortCompare;
    FTree: TAVLTree;// tree of PStringToStringTreeItem
    FCaseSensitive: boolean;
    function GetCompareItemsFunc: TListSortCompare;
    function GetStrings(const s: string): string;
    procedure SetStrings(const s: string; const AValue: string);
    function FindNode(const s: string): TAVLTreeNode;
  public
    constructor Create(TheCaseSensitive: boolean);
    destructor Destroy; override;
    procedure Clear;
    function Contains(const s: string): boolean;
    function GetString(const Name: string; out Value: string): boolean;
    procedure Add(const Name, Value: string);
    procedure GetNames(List: TStrings);
    procedure Remove(const Name: string);
    property Strings[const s: string]: string read GetStrings write SetStrings; default;
    property CaseSensitive: boolean read FCaseSensitive;
    property Tree: TAVLTree read FTree;
    function AsText: string;
    function Equals(OtherTree: TStringToStringTree): boolean; reintroduce;
    procedure Assign(Source: TStringToStringTree);
    procedure WriteDebugReport;
    function CalcMemSize: PtrUint;
    property CompareItemsFunc: TListSortCompare read GetCompareItemsFunc;
    property CompareKeyItemFunc: TListSortCompare read FCompareKeyItemFunc;
    procedure SetCompareFuncs(
            const NewCompareItemsFunc, NewCompareKeyItemFunc: TListSortCompare);
  end;

  { TFilenameToStringTree }

  TFilenameToStringTree = class(TStringToStringTree)
  public
    constructor Create(CaseInsensitive: boolean);
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
  end;

type
  TCTComponentAccess = class(TComponent);

  { TComponentChildCollector }

  TComponentChildCollector = class
  private
    FChilds: TFPList;
    FRoot: TComponent;
    procedure AddChildComponent(Child: TComponent);
  public
    constructor Create;
    destructor Destroy; override;
    function GetComponents(RootComponent: TComponent; AddRoot: boolean = true): TFPList;
    property Children: TFPList read FChilds;
    property Root: TComponent read FRoot;
  end;
  
function CompareStringToStringItems(Data1, Data2: Pointer): integer;
function CompareStringAndStringToStringTreeItem(Key, Data: Pointer): integer;

function CompareStringToStringItemsI(Data1, Data2: Pointer): integer;
function CompareStringAndStringToStringTreeItemI(Key, Data: Pointer): integer;

function CompareFilenameToStringItems(Data1, Data2: Pointer): integer;
function CompareFilenameAndFilenameToStringTreeItem(Key, Data: Pointer): integer;

function CompareFilenameToStringItemsI(Data1, Data2: Pointer): integer;
function CompareFilenameAndFilenameToStringTreeItemI(Key, Data: Pointer): integer;

function CompareAnsiStringPtrs(Data1, Data2: Pointer): integer;

implementation

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

function CompareStringAndStringToStringTreeItemI(Key, Data: Pointer): integer;
begin
  Result:=CompareText(String(Key),PStringToStringTreeItem(Data)^.Name);
end;

function CompareFilenameAndFilenameToStringTreeItem(Key, Data: Pointer
  ): integer;
begin
  Result:=CompareFilenames(String(Key),PStringToStringTreeItem(Data)^.Name);
end;

function CompareFilenameToStringItemsI(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenamesIgnoreCase(PStringToStringTreeItem(Data1)^.Name,
                                     PStringToStringTreeItem(Data2)^.Name);
end;

function CompareFilenameAndFilenameToStringTreeItemI(Key, Data: Pointer
  ): integer;
begin
  Result:=CompareFilenamesIgnoreCase(String(Key),
                                     PStringToStringTreeItem(Data)^.Name);
end;

function CompareAnsiStringPtrs(Data1, Data2: Pointer): integer;
begin
  Result:=CompareStr(AnsiString(Data1),AnsiString(Data2));
end;

{ TCodeXYPositions }

function TCodeXYPositions.GetItems(Index: integer): PCodeXYPosition;
begin
  Result:=PCodeXYPosition(FItems[Index]);
end;

function TCodeXYPositions.GetCaretsXY(Index: integer): TPoint;
var
  Item: PCodeXYPosition;
begin
  Item:=Items[Index];
  Result:=Point(Item^.X,Item^.Y);
end;

function TCodeXYPositions.GetCodes(Index: integer): TCodeBuffer;
var
  Item: PCodeXYPosition;
begin
  Item:=Items[Index];
  Result:=Item^.Code;
end;

procedure TCodeXYPositions.SetCaretsXY(Index: integer; const AValue: TPoint);
var
  Item: PCodeXYPosition;
begin
  Item:=Items[Index];
  Item^.X:=AValue.X;
  Item^.Y:=AValue.Y;
end;

procedure TCodeXYPositions.SetCodes(Index: integer; const AValue: TCodeBuffer);
var
  Item: PCodeXYPosition;
begin
  Item:=Items[Index];
  Item^.Code:=AValue;
end;

procedure TCodeXYPositions.SetItems(Index: integer;
  const AValue: PCodeXYPosition);
begin
  FItems[Index]:=AValue;
end;

constructor TCodeXYPositions.Create;
begin

end;

destructor TCodeXYPositions.Destroy;
begin
  Clear;
  FItems.Free;
  FItems:=nil;
  inherited Destroy;
end;

procedure TCodeXYPositions.Clear;
var
  i: Integer;
  Item: PCodeXYPosition;
begin
  if FItems<>nil then begin
    for i:=0 to FItems.Count-1 do begin
      Item:=Items[i];
      Dispose(Item);
    end;
    FItems.Clear;
  end;
end;

function TCodeXYPositions.Add(const Position: TCodeXYPosition): integer;
var
  NewItem: PCodeXYPosition;
begin
  New(NewItem);
  NewItem^:=Position;
  if FItems=nil then FItems:=TFPList.Create;
  Result:=FItems.Add(NewItem);
end;

function TCodeXYPositions.Add(X, Y: integer; Code: TCodeBuffer): integer;
var
  NewItem: TCodeXYPosition;
begin
  NewItem.X:=X;
  NewItem.Y:=Y;
  NewItem.Code:=Code;
  Result:=Add(NewItem);
end;

procedure TCodeXYPositions.Assign(Source: TCodeXYPositions);
var
  i: Integer;
begin
  if IsEqual(Source) then exit;
  Clear;
  for i:=0 to Source.Count-1 do
    Add(Source[i]^);
end;

function TCodeXYPositions.IsEqual(Source: TCodeXYPositions): boolean;
var
  SrcItem: TCodeXYPosition;
  CurItem: TCodeXYPosition;
  i: Integer;
begin
  if Source=Self then
    Result:=true
  else if (Source=nil) or (Source.Count<>Count) then
    Result:=false
  else begin
    for i:=0 to Count-1 do begin
      SrcItem:=Source[i]^;
      CurItem:=Items[i]^;
      if (SrcItem.X<>CurItem.X)
      or (SrcItem.Y<>CurItem.Y)
      or (SrcItem.Code<>CurItem.Code)
      then begin
        Result:=false;
        exit;
      end;
    end;
    Result:=true;
  end;
end;

function TCodeXYPositions.Count: integer;
begin
  if FItems<>nil then
    Result:=FItems.Count
  else
    Result:=0;
end;

procedure TCodeXYPositions.Delete(Index: integer);
var
  Item: PCodeXYPosition;
begin
  Item:=Items[Index];
  Dispose(Item);
  FItems.Delete(Index);
end;

function TCodeXYPositions.CreateCopy: TCodeXYPositions;
begin
  Result:=TCodeXYPositions.Create;
  Result.Assign(Self);
end;

function TCodeXYPositions.CalcMemSize: PtrUint;
begin
  Result:=PtrUInt(InstanceSize);
  if FItems<>nil then
    inc(Result,PtrUInt(FItems.InstanceSize)
      +PtrUInt(FItems.Capacity)*SizeOf(TCodeXYPosition));
end;

{ TStringToStringTree }

function TStringToStringTree.GetStrings(const s: string): string;
var
  Node: TAVLTreeNode;
begin
  Node:=FindNode(s);
  if Node<>nil then
    Result:=PStringToStringTreeItem(Node.Data)^.Value
  else
    Result:=''
end;

function TStringToStringTree.GetCompareItemsFunc: TListSortCompare;
begin
  Result:=Tree.OnCompare;
end;

procedure TStringToStringTree.SetStrings(const s: string; const AValue: string);
var
  Node: TAVLTreeNode;
  NewItem: PStringToStringTreeItem;
begin
  Node:=FindNode(s);
  if Node<>nil then begin
    PStringToStringTreeItem(Node.Data)^.Value:=AValue;
  end else begin
    New(NewItem);
    NewItem^.Name:=s;
    NewItem^.Value:=AValue;
    FTree.Add(NewItem);
  end;
end;

function TStringToStringTree.FindNode(const s: string): TAVLTreeNode;
begin
  Result:=FTree.FindKey(Pointer(s),FCompareKeyItemFunc)
end;

constructor TStringToStringTree.Create(TheCaseSensitive: boolean);
begin
  FCaseSensitive:=TheCaseSensitive;
  if CaseSensitive then begin
    FCompareKeyItemFunc:=@CompareStringAndStringToStringTreeItem;
    FTree:=TAVLTree.Create(@CompareStringToStringItems);
  end else begin
    FCompareKeyItemFunc:=@CompareStringAndStringToStringTreeItemI;
    FTree:=TAVLTree.Create(@CompareStringToStringItemsI);
  end;
end;

destructor TStringToStringTree.Destroy;
begin
  Clear;
  FTree.Free;
  FTree:=nil;
  inherited Destroy;
end;

procedure TStringToStringTree.Clear;
var
  Node: TAVLTreeNode;
  Item: PStringToStringTreeItem;
begin
  Node:=FTree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringTreeItem(Node.Data);
    Dispose(Item);
    Node:=FTree.FindSuccessor(Node);
  end;
  FTree.Clear;
end;

function TStringToStringTree.Contains(const s: string): boolean;
begin
  Result:=FindNode(s)<>nil;
end;

function TStringToStringTree.GetString(const Name: string; out Value: string
  ): boolean;
var
  Node: TAVLTreeNode;
begin
  Node:=FindNode(Name);
  if Node<>nil then begin
    Value:=PStringToStringTreeItem(Node.Data)^.Value;
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

procedure TStringToStringTree.Add(const Name, Value: string);
begin
  Strings[Name]:=Value;
end;

procedure TStringToStringTree.GetNames(List: TStrings);
var
  Node: TAVLTreeNode;
  Item: PStringToStringTreeItem;
begin
  Node:=Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringTreeItem(Node.Data);
    List.Add(Item^.Name);
    Node:=Tree.FindSuccessor(Node);
  end;
end;

procedure TStringToStringTree.Remove(const Name: string);
var
  Node: TAVLTreeNode;
  Item: PStringToStringTreeItem;
begin
  Node:=FindNode(Name);
  if Node<>nil then begin
    Item:=PStringToStringTreeItem(Node.Data);
    FTree.Delete(Node);
    Dispose(Item);
  end;
end;

function TStringToStringTree.AsText: string;
var
  Node: TAVLTreeNode;
  Item: PStringToStringTreeItem;
begin
  Result:='';
  Node:=Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringTreeItem(Node.Data);
    Result:=Result+Item^.Name+'='+Item^.Value+LineEnding;
    Node:=Tree.FindSuccessor(Node);
  end;
end;

function TStringToStringTree.Equals(OtherTree: TStringToStringTree): boolean;
var
  Node: TAVLTreeNode;
  OtherNode: TAVLTreeNode;
  OtherItem: PStringToStringTreeItem;
  Item: PStringToStringTreeItem;
begin
  Result:=false;
  if OtherTree=nil then exit;
  if Tree.Count<>OtherTree.Tree.Count then exit;
  Node:=Tree.FindLowest;
  OtherNode:=OtherTree.Tree.FindLowest;
  while Node<>nil do begin
    if OtherNode=nil then exit;
    Item:=PStringToStringTreeItem(Node.Data);
    OtherItem:=PStringToStringTreeItem(OtherNode.Data);
    if (Item^.Name<>OtherItem^.Name)
    or (Item^.Value<>OtherItem^.Value) then exit;
    OtherNode:=OtherTree.Tree.FindSuccessor(OtherNode);
    Node:=Tree.FindSuccessor(Node);
  end;
  if OtherNode<>nil then exit;
  Result:=true;
end;

procedure TStringToStringTree.Assign(Source: TStringToStringTree);
var
  Node: TAVLTreeNode;
  Item: PStringToStringTreeItem;
begin
  Clear;
  Node:=Source.Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringTreeItem(Node.Data);
    Strings[Item^.Name]:=Item^.Value;
    Node:=Source.Tree.FindSuccessor(Node);
  end;
end;

procedure TStringToStringTree.WriteDebugReport;
var
  Node: TAVLTreeNode;
  Item: PStringToStringTreeItem;
begin
  DebugLn(['TStringToStringTree.WriteDebugReport ',Tree.Count]);
  Node:=Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringTreeItem(Node.Data);
    DebugLn([Item^.Name,'=',Item^.Value]);
    Node:=Tree.FindSuccessor(Node);
  end;
end;

function TStringToStringTree.CalcMemSize: PtrUint;
var
  Node: TAVLTreeNode;
  Item: PStringToStringTreeItem;
begin
  Result:=PtrUInt(InstanceSize)
    +PtrUInt(FTree.InstanceSize)
    +PtrUint(FTree.Count)*SizeOf(TAVLTreeNode);
  Node:=FTree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringTreeItem(Node.Data);
    inc(Result,MemSizeString(Item^.Name)
       +MemSizeString(Item^.Value)
       +SizeOf(TStringToStringTreeItem));
    Node:=FTree.FindSuccessor(Node);
  end;
end;

procedure TStringToStringTree.SetCompareFuncs(const NewCompareItemsFunc,
  NewCompareKeyItemFunc: TListSortCompare);
begin
  FCompareKeyItemFunc:=NewCompareKeyItemFunc;
  Tree.OnCompare:=NewCompareItemsFunc;
end;

{ TFilenameToStringTree }

constructor TFilenameToStringTree.Create(CaseInsensitive: boolean);
begin
  inherited Create(true);
  if CaseInsensitive then
    SetCompareFuncs(@CompareFilenameToStringItemsI,
                    @CompareFilenameAndFilenameToStringTreeItemI)
  else
    SetCompareFuncs(@CompareFilenameToStringItems,
                    @CompareFilenameAndFilenameToStringTreeItem);
end;

{ TStringTree }

constructor TStringTree.Create;
begin
  Tree:=TAVLTree.Create(@CompareAnsiStringPtrs);
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
  Node:=FindNode(s);
  if Node=nil then begin
    // increase refcount
    h:=s;
    Tree.Add(Pointer(h));
    Pointer(h):=nil; // keep refcount
  end else
    s:=AnsiString(Node.Data);
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
  FChilds:=TFPList.Create;
end;

destructor TComponentChildCollector.Destroy;
begin
  FreeAndNil(FChilds);
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

