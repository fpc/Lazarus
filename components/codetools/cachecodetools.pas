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
    High level caches.
}
unit CacheCodeTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FileProcs, CodeAtom, CodeCache, KeywordFuncLists, CustomCodeTool,
  BasicCodeTools, FindDeclarationTool, AVL_Tree;

type

  { TDeclarationInheritanceCacheItem }

  TDeclarationInheritanceCacheItem = class
  public
    CodePos: TCodePosition;
    ListOfPCodeXYPosition: TFPList;
    destructor Destroy; override;
  end;

  { TDeclarationInheritanceCacheTree
    Tree of TDeclarationInheritanceCacheItem sorted by CompareDeclInhCacheItems }

  TDeclarationInheritanceCacheTree = class(TAVLTree)
  public
    CodeToolsChangeStep: integer;
    constructor CreateDeclInhTree;
    destructor Destroy; override;
  end;
  
  TOnFindDeclarations = function(Code: TCodeBuffer; X,Y: integer;
          out ListOfPCodeXYPosition: TFPList;
          Flags: TFindDeclarationListFlags): boolean of object;

  TDeclarationInheritanceCache = class
  private
    FCurrent: TDeclarationInheritanceCacheTree;
    FOldTrees: TFPList; // list of TDeclarationInheritanceCacheTree
    FOnFindDeclarations: TOnFindDeclarations;
    FOnGetNodesDeletedStep: TGetChangeStepEvent;
    procedure CheckCurrentIsValid;
    procedure CleanCache(FreeItemCount: integer);
  public
    constructor Create(const TheOnFindDeclarations: TOnFindDeclarations;
                       const TheOnGetNodesDeletedStep: TGetChangeStepEvent);
    destructor Destroy; override;
    procedure Clear;
    function FindDeclarations(Code: TCodeBuffer; X,Y: integer;
          out ListOfPCodeXYPosition: TFPList;
          out CacheWasUsed: boolean): boolean;
    property OnFindDeclarations: TOnFindDeclarations read FOnFindDeclarations
                                                     write FOnFindDeclarations;
    property OnGetNodesDeletedStep: TGetChangeStepEvent read FOnGetNodesDeletedStep
                                                   write FOnGetNodesDeletedStep;
  end;

function CompareDeclInhCacheItems(Data1, Data2: Pointer): integer;
function ComparePCodePosWithDeclInhCacheItem(CodePosition, DeclInhItem: Pointer): integer;

implementation

function CompareDeclInhCacheItems(Data1, Data2: Pointer): integer;
var
  Item1: TDeclarationInheritanceCacheItem;
  Item2: TDeclarationInheritanceCacheItem;
begin
  Item1:=TDeclarationInheritanceCacheItem(Data1);
  Item2:=TDeclarationInheritanceCacheItem(Data2);
  Result:=CompareCodePositions(@Item1.CodePos,@Item2.CodePos);
end;

function ComparePCodePosWithDeclInhCacheItem(CodePosition, DeclInhItem: Pointer): integer;
begin
  Result:=CompareCodePositions(PCodePosition(CodePosition),
                               @TDeclarationInheritanceCacheItem(DeclInhItem).CodePos);
end;

procedure TDeclarationInheritanceCache.CheckCurrentIsValid;
var
  NodesDeletedStep: integer;
begin
  if FCurrent=nil then exit;
  OnGetNodesDeletedStep(NodesDeletedStep);
  if (FCurrent.CodeToolsChangeStep=NodesDeletedStep) then exit;
  // the current cache is invalid => move to old
  if FOldTrees=nil then FOldTrees:=TFPList.Create;
  FOldTrees.Add(FCurrent);
  FCurrent:=nil;
end;

procedure TDeclarationInheritanceCache.CleanCache(FreeItemCount: integer);
// free some old cache items
var
  i: Integer;
  OldTree: TDeclarationInheritanceCacheTree;
begin
  for i:=1 to FreeItemCount do begin
    if FOldTrees=nil then exit;
    if FOldTrees.Count=0 then begin
      FreeAndNil(FOldTrees);
    end else begin
      OldTree:=TDeclarationInheritanceCacheTree(FOldTrees[FOldTrees.Count-1]);
      if OldTree.Count=0 then begin
        OldTree.Free;
        FOldTrees.Delete(FOldTrees.Count-1);
      end else begin
        OldTree.FreeAndDelete(OldTree.Root);
      end;
    end;
  end;
end;

constructor TDeclarationInheritanceCache.Create(
  const TheOnFindDeclarations: TOnFindDeclarations;
  const TheOnGetNodesDeletedStep: TGetChangeStepEvent);
begin
  OnFindDeclarations:=TheOnFindDeclarations;
  OnGetNodesDeletedStep:=TheOnGetNodesDeletedStep;
end;

destructor TDeclarationInheritanceCache.Destroy;
begin
  Clear;
  FreeAndNil(FCurrent);
  FreeAndNil(FOldTrees);
  inherited Destroy;
end;

procedure TDeclarationInheritanceCache.Clear;
var
  i: LongInt;
begin
  if FOldTrees<>nil then begin
    for i:=FOldTrees.Count-1 downto 0 do
      TDeclarationInheritanceCacheTree(FOldTrees[i]).Free;
    FreeAndNil(FOldTrees);
  end;
end;

function TDeclarationInheritanceCache.FindDeclarations(Code: TCodeBuffer; X,
  Y: integer; out ListOfPCodeXYPosition: TFPList; out CacheWasUsed: boolean
  ): boolean;
var
  CodePos: TCodePosition;
  AVLNode: TAVLTreeNode;
  Item: TDeclarationInheritanceCacheItem;
begin
  Result:=false;
  ListOfPCodeXYPosition:=nil;
  CacheWasUsed:=true;
  if Code=nil then exit;
  CodePos.Code:=Code;
  Code.LineColToPosition(Y,X,CodePos.P);
  if (CodePos.P<1) or (CodePos.P>Code.SourceLength) then exit;

  // move cursor to start of atom (needed to find CodePos in cache)
  CodePos.P:=FindStartOfAtom(Code.Source,CodePos.P);

  // search in cache
  CheckCurrentIsValid;
  if FCurrent<>nil then begin
    // the current cache is valid
    AVLNode:=FCurrent.FindKey(@CodePos,@ComparePCodePosWithDeclInhCacheItem);
    if AVLNode<>nil then begin
      Item:=TDeclarationInheritanceCacheItem(AVLNode.Data);
      ListOfPCodeXYPosition:=Item.ListOfPCodeXYPosition;
      Result:=ListOfPCodeXYPosition<>nil;
      exit;
    end;
  end;

  CacheWasUsed:=false;

  //DebugLn(['TDeclarationInheritanceCache.FindDeclarations searching ',Code.Filename,'(X=',X,',Y=',Y,')']);

  // ask the codetools
  if OnFindDeclarations(Code,X,Y,ListOfPCodeXYPosition,[])
  and (ListOfPCodeXYPosition<>nil)
  and (ListOfPCodeXYPosition.Count>0) then begin
    Result:=true;
  end else begin
    FreeAndNil(ListOfPCodeXYPosition);
    Result:=false;
  end;

  // save to cache
  Item:=TDeclarationInheritanceCacheItem.Create;
  Item.CodePos:=CodePos;
  Item.ListOfPCodeXYPosition:=ListOfPCodeXYPosition;
  CheckCurrentIsValid;
  if FCurrent=nil then begin
    FCurrent:=TDeclarationInheritanceCacheTree.CreateDeclInhTree;
    OnGetNodesDeletedStep(FCurrent.CodeToolsChangeStep);
  end;
  FCurrent.Add(Item);
  
  //if ListOfPCodeXYPosition<>nil then DebugLn(['TDeclarationInheritanceCache.FindDeclarations ',ListOfPCodeXYPositionToStr(ListOfPCodeXYPosition)]);

  // clean up cache a bit
  CleanCache(5);
  
  // consistency check
  AVLNode:=FCurrent.FindKey(@CodePos,@ComparePCodePosWithDeclInhCacheItem);
  if Item<>TDeclarationInheritanceCacheItem(AVLNode.Data) then raise Exception.Create('');
end;

constructor TDeclarationInheritanceCacheTree.CreateDeclInhTree;
begin
  Create(@CompareDeclInhCacheItems);
end;

destructor TDeclarationInheritanceCacheTree.Destroy;
begin
  FreeAndClear;
  inherited Destroy;
end;

{ TDeclarationInheritanceCacheItem }

destructor TDeclarationInheritanceCacheItem.Destroy;
begin
  FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
  ListOfPCodeXYPosition:=nil;
  inherited Destroy;
end;

end.

