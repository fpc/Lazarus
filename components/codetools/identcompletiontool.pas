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
    TIdentCompletionTool enhances the TFindDeclarationTool with the ability
    to create lists of valid identifiers at a specific code position.

}
unit IdentCompletionTool;

{$mode objfpc}{$H+}

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

// activate for debug:

// mem check
{ $DEFINE MEM_CHECK}

// verbosity
{$DEFINE CTDEBUG}
{ $DEFINE ShowFoundIdents}
{ $DEFINE ShowFilteredIdents}

// new features
{ $DEFINE IgnoreErrorAfterCursor}


uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeToolsStrConsts, CodeTree, CodeAtom, CustomCodeTool,
  SourceLog, KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, AVL_Tree,
  FindDeclarationTool, PascalParserTool;
  

type
  TIdentCompletionTool = class;

  TIdentifierCompatibility = (
    icompExact,
    icompCompatible,
    icompUnknown,
    icompIncompatible
    );
  TIdentifierCompatibilities = set of TIdentifierCompatibility;
  
  TIdentifierListItem = class
  public
    Compatibility: TIdentifierCompatibility;
    HasChilds: boolean;   // identifier can contain childs (class, record)
    HistoryIndex: integer;
    Identifier: PChar;
    Level: integer;
    Node: TCodeTreeNode;
    Tool: TFindDeclarationTool;
    function AsString: string;
  end;
  
  TIdentifierListFlag = (ilfFilteredListNeedsUpdate);
  TIdentifierListFlags = set of TIdentifierListFlag;
  
  TIdentifierList = class
  private
    FFilteredList: TList;
    FFlags: TIdentifierListFlags;
    FItems: TAVLTree;
    FPrefix: string;
    procedure UpdateFilteredList;
    function GetFilteredItems(Index: integer): TIdentifierListItem;
    procedure SetPrefix(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(NewItem: TIdentifierListItem);
    function Count: integer;
    function GetFilteredCount: integer;
  public
    property Prefix: string read FPrefix write SetPrefix;
    property FilteredItems[Index: integer]: TIdentifierListItem
      read GetFilteredItems;
  end;

  TIdentCompletionTool = class(TFindDeclarationTool)
  private
    LastGatheredIdentParent: TCodeTreeNode;
    LastGatheredIdentLevel: integer;
  protected
    CurrentIdentifierList: TIdentifierList;
    function CollectAllIdentifiers(Params: TFindDeclarationParams;
      const FoundContext: TFindContext): TIdentifierFoundResult;
    procedure GatherPredefinedIdentifiers;
  public
    function GatherIdentifiers(const CursorPos: TCodeXYPosition;
      var IdentifierList: TIdentifierList): boolean;
  end;
  
const
  IdentifierCompatibilityNames: array[TIdentifierCompatibility] of string = (
    'icompExact',
    'icompCompatible',
    'icompUnknown',
    'icompIncompatible'
    );

implementation

function CompareIdentListItems(Data1, Data2: Pointer): integer;
var
  Item1: TIdentifierListItem;
  Item2: TIdentifierListItem;
begin
  Item1:=TIdentifierListItem(Data1);
  Item2:=TIdentifierListItem(Data2);
  
  // first sort for Compatibility  (lower is better)
  if ord(Item1.Compatibility)<ord(Item2.Compatibility) then begin
    Result:=-1;
    exit;
  end else if ord(Item1.Compatibility)>ord(Item2.Compatibility) then begin
    Result:=1;
    exit;
  end;
  
  // then sort for History (lower is better)
  if Item1.HistoryIndex<Item2.HistoryIndex then begin
    Result:=-1;
    exit;
  end else if Item1.HistoryIndex>Item2.HistoryIndex then begin
    Result:=1;
    exit;
  end;

  // then sort for Level (lower is better)
  if Item1.Level<Item2.Level then begin
    Result:=-1;
    exit;
  end else if Item1.Level>Item2.Level then begin
    Result:=1;
    exit;
  end;

  // then sort alpabetically (lower is better)
  Result:=CompareIdentifiers(Item1.Identifier,Item2.Identifier);
  if Result<>0 then exit;

  // no difference found
  Result:=0;
end;

{ TIdentifierList }

procedure TIdentifierList.SetPrefix(const AValue: string);
begin
  if FPrefix=AValue then exit;
  FPrefix:=AValue;
  Include(FFlags,ilfFilteredListNeedsUpdate);
end;

procedure TIdentifierList.UpdateFilteredList;
var
  AnAVLNode: TAVLTreeNode;
  CurItem: TIdentifierListItem;
begin
  if not (ilfFilteredListNeedsUpdate in FFlags) then exit;
  if FFilteredList=nil then FFilteredList:=TList.Create;
  FFilteredList.Clear;
  AnAVLNode:=FItems.FindLowest;
  while AnAVLNode<>nil do begin
    CurItem:=TIdentifierListItem(AnAVLNode.Data);
    if (CurItem.Identifier<>nil)
    and ComparePrefixIdent(PChar(Prefix),CurItem.Identifier) then begin
      {$IFDEF ShowFilteredIdents}
      writeln('::: FILTERED ITEM ',FFilteredList.Count,' ',GetIdentifier(CurItem.Identifier));
      {$ENDIF}
      FFilteredList.Add(CurItem);
    end;
    AnAVLNode:=FItems.FindSuccessor(AnAVLNode);
  end;
  {$IFDEF CTDEBUG}
  writeln('TIdentifierList.UpdateFilteredList ',FFilteredList.Count,' of ',FItems.Count);
  {$ENDIF}
  Exclude(FFlags,ilfFilteredListNeedsUpdate);
end;

function TIdentifierList.GetFilteredItems(Index: integer): TIdentifierListItem;
begin
  UpdateFilteredList;
  if (Index<0) or (Index>=FFilteredList.Count) then
    Result:=nil
  else
    Result:=TIdentifierListItem(FFilteredList[Index]);
end;

constructor TIdentifierList.Create;
begin
  FFlags:=[ilfFilteredListNeedsUpdate];
  FItems:=TAVLTree.Create(@CompareIdentListItems);
end;

destructor TIdentifierList.Destroy;
begin
  Clear;
  FItems.Free;
  FFilteredList.Free;
  inherited Destroy;
end;

procedure TIdentifierList.Clear;
begin
  FItems.FreeAndClear;
  Include(FFlags,ilfFilteredListNeedsUpdate);
end;

procedure TIdentifierList.Add(NewItem: TIdentifierListItem);
begin
  FItems.Add(NewItem);
  Include(FFlags,ilfFilteredListNeedsUpdate);
end;

function TIdentifierList.Count: integer;
begin
  Result:=FItems.Count;
end;

function TIdentifierList.GetFilteredCount: integer;
begin
  UpdateFilteredList;
  Result:=FFilteredList.Count;
end;

{ TIdentCompletionTool }

function TIdentCompletionTool.CollectAllIdentifiers(
  Params: TFindDeclarationParams; const FoundContext: TFindContext
  ): TIdentifierFoundResult;
var
  NewItem: TIdentifierListItem;
  Ident: PChar;
begin
  // proceed searching ...
  Result:=ifrProceedSearch;

  {$IFDEF ShowFoundIdents}
  writeln('::: COLLECT IDENT ',FoundContext.Node.DescAsString,
    ' "',StringToPascalConst(copy(FoundContext.Tool.Src,FoundContext.Node.StartPos,50)),'"'
    ,' ',fdfIgnoreUsedUnits in Params.Flags);
  {$ENDIF}
    
  if LastGatheredIdentParent<>FoundContext.Node.Parent then begin
    // new context level
    LastGatheredIdentParent:=FoundContext.Node.Parent;
    inc(LastGatheredIdentLevel);
  end;

  Ident:=nil;
  case FoundContext.Node.Desc of
  
  ctnTypeDefinition:
    begin
      if (FoundContext.Node.FirstChild<>nil)
      and (FoundContext.Node.FirstChild.Desc in [ctnClass,ctnClassInterface])
      and ((ctnsForwardDeclaration and FoundContext.Node.FirstChild.SubDesc)>0)
      then
        // skip forward definition
        exit;
      Ident:=@FoundContext.Tool.Src[FoundContext.Node.StartPos];
    end;
  
  ctnVarDefinition,ctnConstDefinition:
    Ident:=@FoundContext.Tool.Src[FoundContext.Node.StartPos];
    
  ctnProcedure,ctnProcedureHead:
    Ident:=FoundContext.Tool.GetProcNameIdentifier(FoundContext.Node);
    
  ctnProperty:
    Ident:=FoundContext.Tool.GetPropertyNameIdentifier(FoundContext.Node);
    
  end;
  if Ident=nil then exit;

  NewItem:=TIdentifierListItem.Create;
  NewItem.Compatibility:=icompUnknown;
  NewItem.HasChilds:=false;
  NewItem.HistoryIndex:=0;
  NewItem.Identifier:=Ident;
  NewItem.Level:=LastGatheredIdentLevel;
  NewItem.Node:=FoundContext.Node;
  NewItem.Tool:=FoundContext.Tool;
  
  {$IFDEF ShowFoundIdents}
  writeln('  IDENT COLLECTED: ',NewItem.AsString);
  {$ENDIF}
  
  CurrentIdentifierList.Add(NewItem);
end;

procedure TIdentCompletionTool.GatherPredefinedIdentifiers;
begin
  // ToDo:
end;

function TIdentCompletionTool.GatherIdentifiers(
  const CursorPos: TCodeXYPosition;
  var IdentifierList: TIdentifierList): boolean;
var
  CleanCursorPos, IdentStartPos, IdentEndPos: integer;
  CursorNode: TCodeTreeNode;
  Params: TFindDeclarationParams;
  GatherContext: TFindContext;
  ExprType: TExpressionType;
  ContextExprStartPos: Integer;
begin
  Result:=false;
  if IdentifierList=nil then IdentifierList:=TIdentifierList.Create;
  CurrentIdentifierList:=IdentifierList;
  CurrentIdentifierList.Clear;
  LastGatheredIdentParent:=nil;
  LastGatheredIdentLevel:=0;
  
  ActivateGlobalWriteLock;
  Params:=TFindDeclarationParams.Create;
  try
    // build code tree
    {$IFDEF CTDEBUG}
    writeln('TIdentCompletionTool.GatherIdentifiers A CursorPos=',CursorPos.X,',',CursorPos.Y);
    {$ENDIF}
    BuildTreeAndGetCleanPos(trTillCursor,CursorPos,CleanCursorPos,
                  [{$IFDEF IgnoreErrorAfterCursor}btSetIgnoreErrorPos{$ENDIF}]);
    CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
    GetIdentStartEndAtPosition(Src,CleanCursorPos,IdentStartPos,IdentEndPos);

    // find context
    {$IFDEF CTDEBUG}
    writeln('TIdentCompletionTool.GatherIdentifiers B',
      ' CleanCursorPos=',CleanCursorPos,
      ' IdentStartPos=',IdentStartPos,' IdentEndPos=',IdentEndPos,
      ' Ident=',copy(Src,IdentStartPos,IdentEndPos-IdentStartPos));
    {$ENDIF}
    GatherContext:=CreateFindContext(Self,CursorNode);
    ContextExprStartPos:=FindStartOfVariable(IdentStartPos);
    {$IFDEF CTDEBUG}
    writeln('TIdentCompletionTool.GatherIdentifiers C',
      ' ContextExprStartPos=',ContextExprStartPos,
      ' Expr=',StringToPascalConst(copy(Src,ContextExprStartPos,
                    IdentStartPos-ContextExprStartPos)));
    {$ENDIF}
    if ContextExprStartPos<IdentStartPos then begin
      MoveCursorToCleanPos(IdentStartPos);
      Params.ContextNode:=CursorNode;
      Params.SetIdentifier(Self,nil,nil);
      Params.Flags:=[fdfExceptionOnNotFound,
                     fdfSearchInParentNodes,fdfSearchInAncestors]
                    +fdfAllClassVisibilities;
      ExprType:=FindExpressionTypeOfVariable(ContextExprStartPos,IdentStartPos,
                                             Params);
      if (ExprType.Desc=xtContext) then
        GatherContext:=ExprType.Context;
    end;
    if (GatherContext.Tool<>nil) and (GatherContext.Node<>nil) then begin
      {$IFDEF CTDEBUG}
      writeln('TIdentCompletionTool.GatherIdentifiers D CONTEXT: ',
        GatherContext.Tool.MainFilename,
        ' ',GatherContext.Node.DescAsString,
        ' "',StringToPascalConst(copy(GatherContext.Tool.Src,GatherContext.Node.StartPos,50)),'"');
      {$ENDIF}
      // gather all identifiers in context
      Params.ContextNode:=GatherContext.Node;
      Params.SetIdentifier(Self,nil,@CollectAllIdentifiers);
      Params.Flags:=[fdfSearchInParentNodes,fdfSearchInAncestors,
                     fdfCollect,fdfFindVariable,fdfIgnoreCurContextNode]
                    +fdfAllClassVisibilities;
      {$IFDEF CTDEBUG}
      writeln('TIdentCompletionTool.GatherIdentifiers F');
      {$ENDIF}
      GatherContext.Tool.FindIdentifierInContext(Params);
    end;
    // add predefined identifiers
    {$IFDEF CTDEBUG}
    writeln('TIdentCompletionTool.GatherIdentifiers G');
    {$ENDIF}
    GatherPredefinedIdentifiers;
    
    Result:=true;
  finally
    Params.Free;
    ClearIgnoreErrorAfter;
    DeactivateGlobalWriteLock;
  end;
  {$IFDEF CTDEBUG}
  writeln('TIdentCompletionTool.GatherIdentifiers END');
  {$ENDIF}
end;

{ TIdentifierListItem }

function TIdentifierListItem.AsString: string;
begin
  Result:=IdentifierCompatibilityNames[Compatibility];
  if HasChilds then
    Result:=Result+' HasChilds'
  else
    Result:=Result+' HasNoChilds';
  Result:=Result+' History='+IntToStr(HistoryIndex);
  Result:=Result+' Ident='+GetIdentifier(Identifier);
  Result:=Result+' Lvl='+IntToStr(Level);
  Result:=Result+' File='+Tool.MainFilename;
  Result:=Result+' Node='+Node.DescAsString
    +' "'+StringToPascalConst(copy(Tool.Src,Node.StartPos,50))+'"';
end;

end.

