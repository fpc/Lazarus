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

interface

{$I codetools.inc}

// activate for debug:

// mem check
{ $DEFINE MEM_CHECK}

// verbosity
{ $DEFINE CTDEBUG}
{ $DEFINE ShowFoundIdents}
{ $DEFINE ShowFilteredIdents}
{ $DEFINE ShowHistory}

// new features
{ $DEFINE DisableIgnoreErrorAfter}


uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, FileProcs, CodeTree, CodeAtom, CustomCodeTool,
  KeywordFuncLists, BasicCodeTools, LinkScanner,
  AVL_Tree, CodeToolMemManager, DefineTemplates,
  SourceChanger, FindDeclarationTool, PascalParserTool;
  

type
  TIdentCompletionTool = class;
  TIdentifierHistoryList = class;

  //----------------------------------------------------------------------------
  // gathered identifier list

  TIdentifierCompatibility = (
    icompExact,
    icompCompatible,
    icompUnknown,
    icompIncompatible
    );
  TIdentifierCompatibilities = set of TIdentifierCompatibility;
  
  TIdentListItemFlag = (
    iliHasChilds,
    iliBaseExprTypeValid,
    iliIsFunction,
    iliIsFunctionValid,
    iliIsAbstractMethod,
    iliIsAbstractMethodValid,
    iliParamListValid
    );
  TIdentListItemFlags = set of TIdentListItemFlag;
  
  { TIdentifierListItem }

  TIdentifierListItem = class
  private
    FNext: TIdentifierListItem;
    FParamList: string;
    function GetParamList: string;
    procedure SetParamList(const AValue: string);
  public
    Compatibility: TIdentifierCompatibility;
    HistoryIndex: integer;
    Identifier: PChar;
    Level: integer;
    Node: TCodeTreeNode;
    Tool: TFindDeclarationTool;
    DefaultDesc: TCodeTreeNodeDesc;
    Flags: TIdentListItemFlags;
    BaseExprType: TExpressionType;
    function AsString: string;
    function GetDesc: TCodeTreeNodeDesc;
    constructor Create(NewCompatibility: TIdentifierCompatibility;
      NewHasChilds: boolean; NewHistoryIndex: integer;
      NewIdentifier: PChar; NewLevel: integer;
      NewNode: TCodeTreeNode; NewTool: TFindDeclarationTool;
      NewDefaultDesc: TCodeTreeNodeDesc);
    function IsProcNodeWithParams: boolean;
    function IsPropertyWithParams: boolean;
    function CheckHasChilds: boolean;
    function CanBeAssigned: boolean;
    procedure UpdateBaseContext;
    function HasChilds: boolean;
    function IsFunction: boolean;
    function IsAbstractMethod: boolean;
    procedure Clear;
    function CompareParamList(CompareItem: TIdentifierListItem): integer;
  public
    property ParamList: string read GetParamList write SetParamList;
  end;
  
  TIdentifierListFlag = (ilfFilteredListNeedsUpdate);
  TIdentifierListFlags = set of TIdentifierListFlag;
  
  TIdentifierListContextFlag = (
    ilcfStartInStatement,  // context starts in statements. e.g. between begin..end
    ilcfStartIsLValue,     // position is start of one statement. e.g. 'A:=', does not check if A can be assigned
    ilcfNeedsEndSemicolon, // after context a semicolon is needed. e.g. 'A end'
    ilcfIsExpression,      // is expression part of statement. e.g. 'if expr'
    ilcfCanProcDeclaration // context allows to declare a procedure/method
    );
  TIdentifierListContextFlags = set of TIdentifierListContextFlag;
  
  TIdentifierList = class
  private
    FContext: TFindContext;
    FContextFlags: TIdentifierListContextFlags;
    FStartAtomBehind: TAtomPosition;
    FStartAtomInFront: TAtomPosition;
    FStartBracketLvl: integer;
    FStartContextPos: TCodeXYPosition;
    FCreatedIdentifiers: TFPList; // list of PChar
    FFilteredList: TFPList; // list of TIdentifierListItem
    FFlags: TIdentifierListFlags;
    FHistory: TIdentifierHistoryList;
    FItems: TAVLTree; // tree of TIdentifierListItem (completely sorted)
    FIdentView: TAVLTree; // tree of TIdentHistListItem sorted for identifiers
    FIdentSearchItem: TIdentifierListItem;
    FPrefix: string;
    FStartContext: TFindContext;
    procedure SetContextFlags(const AValue: TIdentifierListContextFlags);
    procedure SetHistory(const AValue: TIdentifierHistoryList);
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
    function HasIdentifier(Identifier: PChar; const ParamList: string): boolean;
    function FindCreatedIdentifier(const Ident: string): integer;
    function CreateIdentifier(const Ident: string): PChar;
    function StartUpAtomInFrontIs(const s: string): boolean;
    function StartUpAtomBehindIs(const s: string): boolean;
    function CompletePrefix(const OldPrefix: string): string;
  public
    property Context: TFindContext read FContext write FContext;
    property ContextFlags: TIdentifierListContextFlags
                                       read FContextFlags write SetContextFlags;
    property FilteredItems[Index: integer]: TIdentifierListItem
                                                          read GetFilteredItems;
    property History: TIdentifierHistoryList read FHistory write SetHistory;
    property Prefix: string read FPrefix write SetPrefix;
    property StartAtomInFront: TAtomPosition
                                 read FStartAtomInFront write FStartAtomInFront;
    property StartAtomBehind: TAtomPosition
                                   read FStartAtomBehind write FStartAtomBehind;
    property StartBracketLvl: integer
                                   read FStartBracketLvl write FStartBracketLvl;
    property StartContext: TFindContext read FStartContext write FStartContext;
    property StartContextPos: TCodeXYPosition
                                   read FStartContextPos write FStartContextPos;
  end;
  
  //----------------------------------------------------------------------------
  // history list

  TIdentHistListItem = class
  public
    Identifier: string;
    NodeDesc: TCodeTreeNodeDesc;
    ParamList: string;
    HistoryIndex: integer;
  end;

  TIdentifierHistoryList = class
  private
    FCapacity: integer;
    FItems: TAVLTree; // tree of TIdentHistListItem
    procedure SetCapacity(const AValue: integer);
    function FindItem(NewItem: TIdentifierListItem): TAVLTreeNode;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(NewItem: TIdentifierListItem);
    function GetHistoryIndex(AnItem: TIdentifierListItem): integer;
    function Count: integer;
  public
    property Capacity: integer read FCapacity write SetCapacity;
  end;


  //----------------------------------------------------------------------------
  { TCodeContextInfo }

  TCodeContextInfo = class
  private
    FEndPos: integer;
    FItems: PExpressionType;
    FCount: integer;
    FParameterIndex: integer;
    FProcName: string;
    FProcNameAtom: TAtomPosition;
    FStartPos: integer;
    FTool: TFindDeclarationTool;
    function GetItems(Index: integer): TExpressionType;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    property Items[Index: integer]: TExpressionType read GetItems; default;
    function Add(const Context: TExpressionType): integer;
    procedure Clear;
    property Tool: TFindDeclarationTool read FTool write FTool;
    property ParameterIndex: integer read FParameterIndex write FParameterIndex;// 1 based
    property ProcName: string read FProcName write FProcName;
    property ProcNameAtom: TAtomPosition read FProcNameAtom write FProcNameAtom;
    property StartPos: integer read FStartPos write FStartPos;// context is valid from StartPos to EndPos
    property EndPos: integer read FEndPos write FEndPos;
  end;

  //----------------------------------------------------------------------------
  // TIdentCompletionTool

  TIdentCompletionTool = class(TFindDeclarationTool)
  private
    LastGatheredIdentParent: TCodeTreeNode;
    LastGatheredIdentLevel: integer;
    ClassAndAncestors: TFPList;// list of PCodeXYPosition
    FoundPublicProperties: TAVLTree;// tree of PChar (pointing to the
                                    // property names in source)
  protected
    CurrentIdentifierList: TIdentifierList;
    CurrentContexts: TCodeContextInfo;
    function CollectAllIdentifiers(Params: TFindDeclarationParams;
      const FoundContext: TFindContext): TIdentifierFoundResult;
    procedure GatherPredefinedIdentifiers(CleanPos: integer;
      const Context: TFindContext; BeautifyCodeOptions: TBeautifyCodeOptions);
    procedure GatherUsefulIdentifiers(CleanPos: integer;
      const Context: TFindContext; BeautifyCodeOptions: TBeautifyCodeOptions);
    procedure GatherUnitnames(CleanPos: integer;
      const Context: TFindContext; BeautifyCodeOptions: TBeautifyCodeOptions);
    procedure GatherSourceNames(const Context: TFindContext);
    procedure InitCollectIdentifiers(const CursorPos: TCodeXYPosition;
      var IdentifierList: TIdentifierList);
    procedure ParseSourceTillCollectionStart(const CursorPos: TCodeXYPosition;
      out CleanCursorPos: integer; out CursorNode: TCodeTreeNode;
      out IdentStartPos, IdentEndPos: integer);
    procedure FindCollectionContext(Params: TFindDeclarationParams;
      IdentStartPos: integer; CursorNode: TCodeTreeNode;
      out GatherContext: TFindContext; out ContextExprStartPos: LongInt;
      out StartInSubContext: Boolean);
    function CollectAllContexts(Params: TFindDeclarationParams;
      const FoundContext: TFindContext): TIdentifierFoundResult;
    procedure AddCollectionContext(Tool: TFindDeclarationTool;
                                   Node: TCodeTreeNode);
  public
    function GatherIdentifiers(const CursorPos: TCodeXYPosition;
                            var IdentifierList: TIdentifierList;
                            BeautifyCodeOptions: TBeautifyCodeOptions): boolean;
    function FindCodeContext(const CursorPos: TCodeXYPosition;
                             out CodeContexts: TCodeContextInfo): boolean;
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
  Result:=CompareIdentifiers(Item2.Identifier,Item1.Identifier);
  if Result<>0 then exit;
  
  // then sort for ParamList (lower is better)
  Result:=Item2.CompareParamList(Item1);
end;

function CompareIdentListItemsForIdents(Data1, Data2: Pointer): integer;
var
  Item1: TIdentifierListItem;
  Item2: TIdentifierListItem;
begin
  Item1:=TIdentifierListItem(Data1);
  Item2:=TIdentifierListItem(Data2);

  // sort alpabetically (lower is better)
  Result:=CompareIdentifiers(Item2.Identifier,Item1.Identifier);
  if Result<>0 then exit;

  // then sort for ParamList (lower is better)
  Result:=Item2.CompareParamList(Item1);
end;

function CompareIdentHistListItem(Data1, Data2: Pointer): integer;
var
  Item1: TIdentHistListItem;
  Item2: TIdentHistListItem;
begin
  Item1:=TIdentHistListItem(Data1);
  Item2:=TIdentHistListItem(Data2);

  Result:=CompareIdentifiers(PChar(Pointer(Item2.Identifier)),
                             PChar(Pointer(Item1.Identifier)));
  if Result<>0 then exit;

  //debugln('CompareIdentHistListItem ',Item2.Identifier,'=',Item1.Identifier);
  Result:=CompareIdentifiers(PChar(Pointer(Item2.ParamList)),
                             PChar(Pointer(Item1.ParamList)));
end;

function CompareIdentItemWithHistListItem(Data1, Data2: Pointer): integer;
var
  IdentItem: TIdentifierListItem;
  HistItem: TIdentHistListItem;
begin
  IdentItem:=TIdentifierListItem(Data1);
  HistItem:=TIdentHistListItem(Data2);

  Result:=CompareIdentifiers(PChar(Pointer(HistItem.Identifier)),
                             IdentItem.Identifier);
  if Result<>0 then exit;

  //debugln('CompareIdentItemWithHistListItem ',HistItem.Identifier,'=',GetIdentifier(IdentItem.Identifier));
  Result:=SysUtils.CompareText(HistItem.ParamList,IdentItem.ParamList);
end;

type
  TIdentifierListItemMemManager = class(TCodeToolMemManager)
  protected
    procedure FreeFirstItem; override;
  public
    procedure DisposeIdentListItem(IdentListItem: TIdentifierListItem);
    function NewIdentListItem(NewCompatibility: TIdentifierCompatibility;
      NewHasChilds: boolean; NewHistoryIndex: integer;
      NewIdentifier: PChar; NewLevel: integer;
      NewNode: TCodeTreeNode; NewTool: TFindDeclarationTool;
      NewDefaultDesc: TCodeTreeNodeDesc): TIdentifierListItem;
  end;
  
var
  IdentifierListItemMemManager: TIdentifierListItemMemManager;

{ TIdentifierListItemMemManager }

procedure TIdentifierListItemMemManager.FreeFirstItem;
var Item: TIdentifierListItem;
begin
  Item:=TIdentifierListItem(FFirstFree);
  TIdentifierListItem(FFirstFree):=Item.FNext;
  Item.Free;
end;

procedure TIdentifierListItemMemManager.DisposeIdentListItem(
  IdentListItem: TIdentifierListItem);
begin
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add IdentListItem to Free list
    IdentListItem.FNext:=TIdentifierListItem(FFirstFree);
    TIdentifierListItem(FFirstFree):=IdentListItem;
    inc(FFreeCount);
  end else begin
    // free list full -> free IdentListItem
    IdentListItem.Free;
    {$IFDEF DebugCTMemManager}
    inc(FFreedCount);
    {$ENDIF}
  end;
  dec(FCount);
end;

function TIdentifierListItemMemManager.NewIdentListItem(
  NewCompatibility: TIdentifierCompatibility;
  NewHasChilds: boolean; NewHistoryIndex: integer;
  NewIdentifier: PChar; NewLevel: integer;
  NewNode: TCodeTreeNode; NewTool: TFindDeclarationTool;
  NewDefaultDesc: TCodeTreeNodeDesc): TIdentifierListItem;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=TIdentifierListItem(FFirstFree);
    // ToDo: set values
    TIdentifierListItem(FFirstFree):=Result.FNext;
    Result.FNext:=nil;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new node
    Result:=TIdentifierListItem.Create(NewCompatibility,
      NewHasChilds,NewHistoryIndex,NewIdentifier,NewLevel,
      NewNode,NewTool,
      NewDefaultDesc);
    {$IFDEF DebugCTMemManager}
    inc(FAllocatedCount);
    {$ENDIF}
  end;
  inc(FCount);
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
  if FFilteredList=nil then FFilteredList:=TFPList.Create;
  FFilteredList.Count:=0;
  FFilteredList.Capacity:=FItems.Count;
  {$IFDEF CTDEBUG}
  DebugLn('TIdentifierList.UpdateFilteredList Prefix="',Prefix,'"');
  {$ENDIF}
  AnAVLNode:=FItems.FindLowest;
  while AnAVLNode<>nil do begin
    CurItem:=TIdentifierListItem(AnAVLNode.Data);
    if (CurItem.Identifier<>nil)
    and ComparePrefixIdent(PChar(Pointer(Prefix)),CurItem.Identifier) then begin
      {$IFDEF ShowFilteredIdents}
      DebugLn('::: FILTERED ITEM ',FFilteredList.Count,' ',GetIdentifier(CurItem.Identifier));
      {$ENDIF}
      FFilteredList.Add(CurItem);
    end;
    AnAVLNode:=FItems.FindSuccessor(AnAVLNode);
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TIdentifierList.UpdateFilteredList ',dbgs(FFilteredList.Count),' of ',dbgs(FItems.Count));
  {$ENDIF}
  Exclude(FFlags,ilfFilteredListNeedsUpdate);
end;

procedure TIdentifierList.SetHistory(const AValue: TIdentifierHistoryList);
begin
  if FHistory=AValue then exit;
  FHistory:=AValue;
end;

procedure TIdentifierList.SetContextFlags(
  const AValue: TIdentifierListContextFlags);
begin
  if FContextFlags=AValue then exit;
  FContextFlags:=AValue;
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
  FIdentView:=TAVLTree.Create(@CompareIdentListItemsForIdents);
  FIdentSearchItem:=TIdentifierListItem.Create(icompUnknown,
      false,0,nil,0,nil,nil,ctnNone);
  FCreatedIdentifiers:=TFPList.Create;
end;

destructor TIdentifierList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  FreeAndNil(FIdentView);
  FreeAndNil(FFilteredList);
  FreeAndNil(FIdentSearchItem);
  FreeAndNil(FCreatedIdentifiers);
  inherited Destroy;
end;

procedure TIdentifierList.Clear;
var
  i: Integer;
  p: Pointer;
begin
  fContextFlags:=[];
  fContext:=CleanFindContext;
  FStartBracketLvl:=0;
  fStartContext:=CleanFindContext;
  fStartContextPos.Code:=nil;
  fStartContextPos.X:=1;
  fStartContextPos.Y:=1;
  for i:=0 to FCreatedIdentifiers.Count-1 do begin
    p:=FCreatedIdentifiers[i];
    FreeMem(p);
  end;
  FCreatedIdentifiers.Clear;
  FItems.FreeAndClear;
  FIdentView.Clear;
  Include(FFlags,ilfFilteredListNeedsUpdate);
end;

procedure TIdentifierList.Add(NewItem: TIdentifierListItem);
var
  AnAVLNode: TAVLTreeNode;
begin
  AnAVLNode:=FIdentView.FindKey(NewItem,@CompareIdentListItemsForIdents);
  if AnAVLNode=nil then begin
    if History<>nil then
      NewItem.HistoryIndex:=History.GetHistoryIndex(NewItem);
    FItems.Add(NewItem);
    FIdentView.Add(NewItem);
    Include(FFlags,ilfFilteredListNeedsUpdate);
  end else begin
    // redefined identifier -> ignore
    //DebugLn('TIdentifierList.Add redefined: ',NewItem.AsString);
    NewItem.Free;
  end;
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

function TIdentifierList.HasIdentifier(Identifier: PChar;
  const ParamList: string): boolean;
begin
  FIdentSearchItem.Identifier:=Identifier;
  FIdentSearchItem.ParamList:='';
  Result:=FIdentView.FindKey(FIdentSearchItem,
                             @CompareIdentListItemsForIdents)<>nil;
end;

function TIdentifierList.FindCreatedIdentifier(const Ident: string): integer;
begin
  if Ident<>'' then begin
    Result:=FCreatedIdentifiers.Count-1;
    while (Result>=0)
    and (CompareIdentifiers(PChar(Pointer(Ident)),
                            PChar(Pointer(FCreatedIdentifiers[Result])))<>0)
    do
      dec(Result);
  end else begin
    Result:=-1;
  end;
end;

function TIdentifierList.CreateIdentifier(const Ident: string): PChar;
var
  i: Integer;
begin
  if Ident<>'' then begin
    i:=FindCreatedIdentifier(Ident);
    if i>=0 then
      Result:=PChar(Pointer(FCreatedIdentifiers[i]))
    else begin
      GetMem(Result,length(Ident)+1);
      Move(Ident[1],Result^,length(Ident)+1);
      FCreatedIdentifiers.Add(Result);
    end;
  end else
    Result:=nil;
end;

function TIdentifierList.StartUpAtomInFrontIs(const s: string): boolean;
begin
  Result:=StartContext.Tool.FreeUpAtomIs(StartAtomInFront,s);
end;

function TIdentifierList.StartUpAtomBehindIs(const s: string): boolean;
begin
  Result:=StartContext.Tool.FreeUpAtomIs(StartAtomBehind,s);
end;

function TIdentifierList.CompletePrefix(const OldPrefix: string): string;
// search all identifiers beginning with Prefix
// and return the biggest prefix of all of them
var
  AnAVLNode: TAVLTreeNode;
  CurItem: TIdentifierListItem;
  FoundFirst: Boolean;
  SamePos: Integer;
begin
  Result:=Prefix;
  FoundFirst:=false;
  AnAVLNode:=FItems.FindLowest;
  while AnAVLNode<>nil do begin
    CurItem:=TIdentifierListItem(AnAVLNode.Data);
    if (CurItem.Identifier<>nil)
    and ComparePrefixIdent(PChar(Pointer(Prefix)),CurItem.Identifier) then begin
      if not FoundFirst then begin
        Result:=GetIdentifier(CurItem.Identifier);
        FoundFirst:=true;
      end else begin
        SamePos:=length(Prefix);
        while (SamePos<length(Result))
        and (UpChars[CurItem.Identifier[SamePos]]=UpChars[Result[SamePos+1]])
        do
          inc(SamePos);
        if SamePos<length(Result) then begin
          Result:=copy(Result,1,SamePos);
          if length(Result)=length(Prefix) then exit;
        end;
      end;
    end;
    AnAVLNode:=FItems.FindSuccessor(AnAVLNode);
  end;
end;

{ TIdentCompletionTool }

function TIdentCompletionTool.CollectAllIdentifiers(
  Params: TFindDeclarationParams; const FoundContext: TFindContext
  ): TIdentifierFoundResult;
var
  Ident: PChar;
  CurContextParent: TCodeTreeNode;

  function ProtectedNodeIsInAllowedClass: boolean;
  var
    CurClassNode: TCodeTreeNode;
    p: TFindContext;
  begin
    if ClassAndAncestors<>nil then begin
      // start of the identifier completion is in a method or class
      // => all protected ancestor classes are allowed as well.
      CurClassNode:=FoundContext.Node;
      while (CurClassNode<>nil)
      and (not (CurClassNode.Desc in [ctnClass,ctnClassInterface])) do
        CurClassNode:=CurClassNode.Parent;
      if CurClassNode=nil then exit;
      p:=CreateFindContext(Params.NewCodeTool,CurClassNode);
      if IndexOfFindContext(ClassAndAncestors,@p)>=0 then begin
        // this class node is the class or one of the ancestors of the class
        // of the start context of the identifier completion
        exit(true);
      end;
    end;

    Result:=false;
  end;
  
  function PropertyIsOverridenPublicPublish: boolean;
  begin
    // protected properties can be made public in child classes.
    //debugln('PropertyIsOverridenPublicPublish Identifier=',GetIdentifier(Ident),' Find=',dbgs((FoundPublicProperties<>nil) and (FoundPublicProperties.Find(Ident)<>nil)));
    if FoundPublicProperties<>nil then begin
      if FoundPublicProperties.Find(Ident)<>nil then begin
        // there is a public/published property with the same name
        exit(true);
      end;
    end;
    Result:=false;
  end;
  
  procedure SavePublicPublishedProperty;
  begin
    if FoundPublicProperties=nil then begin
      // create tree
      FoundPublicProperties:=
                         TAVLTree.Create(TListSortCompare(@CompareIdentifiers))
    end else if FoundPublicProperties.Find(Ident)<>nil then begin
      // identifier is already public
    end;
    FoundPublicProperties.Add(Ident);
    //debugln('SavePublicPublishedProperty Identifier=',GetIdentifier(Ident),' Find=',dbgs(FoundPublicProperties.Find(Ident)<>nil));
  end;
  
var
  NewItem: TIdentifierListItem;
  Node: TCodeTreeNode;
begin
  // proceed searching ...
  Result:=ifrProceedSearch;

  {$IFDEF ShowFoundIdents}
  if FoundContext.Tool=Self then
  DebugLn('::: COLLECT IDENT ',FoundContext.Node.DescAsString,
    ' "',StringToPascalConst(copy(FoundContext.Tool.Src,FoundContext.Node.StartPos,50)),'"'
    ,' '+dbgs(fdfIgnoreUsedUnits in Params.Flags));
  {$ENDIF}

  CurContextParent:=FoundContext.Node.GetFindContextParent;
  if LastGatheredIdentParent<>CurContextParent then begin
    // new context level
    LastGatheredIdentParent:=CurContextParent;
    inc(LastGatheredIdentLevel);
  end;
  
  if FoundContext.Tool=Self then begin
    // identifier is in the same unit
    //DebugLn('::: COLLECT IDENT in SELF ',FoundContext.Node.DescAsString,
    //  ' "',StringToPascalConst(copy(FoundContext.Tool.Src,FoundContext.Node.StartPos,50)),'"'
    //  ,' '+dbgs(fdfIgnoreUsedUnits in Params.Flags));
  end else begin
    // identifier is in another unit
    if (FoundContext.Node.Parent<>nil) then begin
      if (FoundContext.Node.Parent.Desc=ctnClassPrivate) then begin
        // skip private definitions in other units
        exit;
      end;
      if (FoundContext.Node.Parent.Desc=ctnClassProtected) then begin
        // protected defnitions are only accessible from descendants
        if ProtectedNodeIsInAllowedClass then begin
          //debugln('TIdentCompletionTool.CollectAllIdentifiers ALLOWED Protected in ANCESTOR '+StringToPascalConst(copy(FoundContext.Tool.Src,FoundContext.Node.StartPos,50)));
        end else if (FoundContext.Node.Desc=ctnProperty) then begin
          //debugln('TIdentCompletionTool.CollectAllIdentifiers MAYBE Protected made Public '+StringToPascalConst(copy(FoundContext.Tool.Src,FoundContext.Node.StartPos,50)));
        end else begin
          //debugln('TIdentCompletionTool.CollectAllIdentifiers FORBIDDEN Protected '+StringToPascalConst(copy(FoundContext.Tool.Src,FoundContext.Node.StartPos,50)));
          exit;
        end;
      end;
    end;
  end;

  Ident:=nil;
  case FoundContext.Node.Desc of
  
  ctnTypeDefinition,ctnGenericType:
    begin
      Node:=FoundContext.Tool.FindTypeNodeOfDefinition(FoundContext.Node);
      if (Node<>nil)
      and (Node.Desc in [ctnClass,ctnClassInterface])
      and ((ctnsForwardDeclaration and Node.SubDesc)>0)
      then
        // skip forward definition
        exit;
      if FoundContext.Node.Desc=ctnTypeDefinition then
        Ident:=@FoundContext.Tool.Src[FoundContext.Node.StartPos]
      else begin
        // generic
        if FoundContext.Node.FirstChild=nil then exit;
        Ident:=@FoundContext.Tool.Src[FoundContext.Node.FirstChild.StartPos];
      end;
    end;
  
  ctnVarDefinition,ctnConstDefinition,ctnEnumIdentifier:
    Ident:=@FoundContext.Tool.Src[FoundContext.Node.StartPos];
    
  ctnProcedure,ctnProcedureHead:
    Ident:=FoundContext.Tool.GetProcNameIdentifier(FoundContext.Node);
    
  ctnProperty:
    begin
      Ident:=FoundContext.Tool.GetPropertyNameIdentifier(FoundContext.Node);
      if FoundContext.Tool.PropNodeIsTypeLess(FoundContext.Node) then begin
        if FoundContext.Node.Parent.Desc in [ctnClassPublic,ctnClassPublished]
        then
          SavePublicPublishedProperty;
        exit;
      end;
      if (FoundContext.Node.Parent.Desc in [ctnClassPrivate,ctnClassProtected])
      and (not PropertyIsOverridenPublicPublish) then begin
        exit;
      end;
    end;
    
  ctnRecordCase:
    Ident:=@FoundContext.Tool.Src[Params.NewCleanPos];

  end;
  if Ident=nil then exit;

  NewItem:=TIdentifierListItem.Create(
                            icompUnknown,
                            false,
                            0,
                            Ident,
                            LastGatheredIdentLevel,
                            FoundContext.Node,
                            FoundContext.Tool,
                            ctnNone);
  
  {$IFDEF ShowFoundIdents}
  DebugLn('  IDENT COLLECTED: ',NewItem.AsString);
  {$ENDIF}
  
  CurrentIdentifierList.Add(NewItem);
end;

procedure TIdentCompletionTool.GatherPredefinedIdentifiers(CleanPos: integer;
  const Context: TFindContext; BeautifyCodeOptions: TBeautifyCodeOptions);
// Add predefined identifiers
const
  CompilerFuncHistoryIndex = 10;
  CompilerFuncLevel = 10;

  function StatementLevel: integer;
  var
    ANode: TCodeTreeNode;
  begin
    Result:=0;
    ANode:=Context.Node;
    while (ANode<>nil) and (not (ANode.Desc in [ctnBeginBlock,ctnAsmBlock])) do
    begin
      ANode:=ANode.Parent;
      inc(Result);
    end;
    if ANode=nil then Result:=0;
  end;
  
  procedure AddCompilerProcedure(const AProcName, AParameterList: PChar);
  var
    NewItem: TIdentifierListItem;
  begin
    NewItem:=TIdentifierListItem.Create(
        icompUnknown,
        false,
        CompilerFuncHistoryIndex,
        AProcName,
        CompilerFuncLevel,
        nil,
        nil,
        ctnProcedure);
    NewItem.ParamList:=AParameterList;
    CurrentIdentifierList.Add(NewItem);
  end;
  
  procedure AddCompilerFunction(const AProcName, AParameterList,
    AResultType: PChar);
  var
    NewItem: TIdentifierListItem;
  begin
    NewItem:=TIdentifierListItem.Create(
        icompUnknown,
        false,
        CompilerFuncHistoryIndex,
        AProcName,
        CompilerFuncLevel,
        nil,
        nil,
        ctnProcedure);
    NewItem.ParamList:=AParameterList;
    NewItem.Flags:=NewItem.Flags+[iliIsFunction,iliIsFunctionValid];
    CurrentIdentifierList.Add(NewItem);
  end;

var
  NewItem: TIdentifierListItem;
  ProcNode: TCodeTreeNode;
begin
  if Context.Node.Desc in AllPascalStatements then begin
    AddCompilerProcedure('SetLength','array of type; NewLength: integer');
    AddCompilerProcedure('Copy','const s: string; FromPosition, Count: integer');
    AddCompilerProcedure('Write','Args : Arguments');
    AddCompilerProcedure('WriteLn','Args : Arguments');
    AddCompilerProcedure('Read','');
    AddCompilerProcedure('ReadLn','');
    AddCompilerFunction('Length','array of type','ordinal');
    AddCompilerFunction('High','Argument','ordinal');
    AddCompilerFunction('Low','Argument','ordinal');
    AddCompilerProcedure('Include','set of enum; enum');
    AddCompilerProcedure('Exclude','set of enum; enum');

    if Context.Tool.NodeIsInAMethod(Context.Node)
    and (not CurrentIdentifierList.HasIdentifier('Self','')) then begin
      // method body -> add 'Self'
      NewItem:=TIdentifierListItem.Create(
          icompUnknown,
          true,
          1,
          'Self',
          StatementLevel,
          nil,
          nil,
          ctnVarDefinition);
      CurrentIdentifierList.Add(NewItem);
    end;
    ProcNode:=Context.Node.GetNodeOfType(ctnProcedure);
    if Context.Tool.NodeIsFunction(ProcNode)
    and (not CurrentIdentifierList.HasIdentifier('Result','')) then begin
      // function body -> add 'Result'
      NewItem:=TIdentifierListItem.Create(
          icompUnknown,
          true,
          1,
          'Result',
          StatementLevel,
          nil,
          nil,
          ctnVarDefinition);
      CurrentIdentifierList.Add(NewItem);
    end;
  end;
end;

procedure TIdentCompletionTool.GatherUsefulIdentifiers(CleanPos: integer;
  const Context: TFindContext; BeautifyCodeOptions: TBeautifyCodeOptions);
var
  NewItem: TIdentifierListItem;
  PropertyName: String;
begin
  while (CleanPos>1) and (IsIdentChar[Src[CleanPos-1]]) do dec(CleanPos);
  GatherPredefinedIdentifiers(CleanPos,Context,BeautifyCodeOptions);
  if Context.Node.Desc=ctnProperty then begin
    PropertyName:=ExtractPropName(Context.Node,false);
    //debugln('TIdentCompletionTool.GatherUsefulIdentifiers Property ',PropertyName);
    MoveCursorToCleanPos(CleanPos);
    ReadPriorAtom;
    if UpAtomIs('READ') then begin
      // add the default class completion 'read' specifier function
      NewItem:=TIdentifierListItem.Create(
          icompUnknown,true,0,
          CurrentIdentifierList.CreateIdentifier(
            BeautifyCodeOptions.PropertyReadIdentPrefix+PropertyName),
          0,nil,nil,ctnProcedure);
      CurrentIdentifierList.Add(NewItem);
    end;
    if UpAtomIs('WRITE') then begin
      // add the default class completion 'write' specifier function
      NewItem:=TIdentifierListItem.Create(
          icompUnknown,true,0,
          CurrentIdentifierList.CreateIdentifier(
            BeautifyCodeOptions.PropertyWriteIdentPrefix+PropertyName),
          0,nil,nil,ctnProcedure);
      CurrentIdentifierList.Add(NewItem);
    end;
    if UpAtomIs('READ') or UpAtomIs('WRITE') then begin
      // add the default class completion 'read'/'write' specifier variable
      NewItem:=TIdentifierListItem.Create(
          icompUnknown,true,0,
          CurrentIdentifierList.CreateIdentifier(
            BeautifyCodeOptions.PrivateVariablePrefix+PropertyName),
          0,nil,nil,ctnVarDefinition);
      CurrentIdentifierList.Add(NewItem);
    end;
    if UpAtomIs('STORED') then begin
      // add the default class completion 'stored' specifier function
      NewItem:=TIdentifierListItem.Create(
          icompUnknown,true,0,
          CurrentIdentifierList.CreateIdentifier(
            PropertyName+BeautifyCodeOptions.PropertyStoredIdentPostfix),
          0,nil,nil,ctnProcedure);
      CurrentIdentifierList.Add(NewItem);
    end;
  end;
end;

procedure TIdentCompletionTool.GatherUnitnames(CleanPos: integer;
  const Context: TFindContext; BeautifyCodeOptions: TBeautifyCodeOptions);
var
  TreeOfUnitFiles: TAVLTree;

  procedure GatherUnitsFromUnitLinks;
  var
    UnitLinks: string;
    UnitLinkStart: Integer;
    UnitLinkEnd: LongInt;
    UnitLinkLen: Integer;
    Filename: String;
  begin
    UnitLinks:=Scanner.Values[ExternalMacroStart+'UnitLinks'];
    UnitLinkStart:=1;
    while UnitLinkStart<=length(UnitLinks) do begin
      while (UnitLinkStart<=length(UnitLinks))
      and (UnitLinks[UnitLinkStart] in [#10,#13]) do
        inc(UnitLinkStart);
      UnitLinkEnd:=UnitLinkStart;
      while (UnitLinkEnd<=length(UnitLinks)) and (UnitLinks[UnitLinkEnd]<>' ')
      do
        inc(UnitLinkEnd);
      UnitLinkLen:=UnitLinkEnd-UnitLinkStart;
      if UnitLinkLen>0 then begin
        Filename:=copy(UnitLinks,UnitLinkStart,UnitLinkEnd-UnitLinkStart);
        AddToTreeOfUnitFiles(TreeOfUnitFiles,Filename,false);
      end;
      UnitLinkStart:=UnitLinkEnd+1;
      while (UnitLinkStart<=length(UnitLinks))
      and (not (UnitLinks[UnitLinkStart] in [#10,#13])) do
        inc(UnitLinkStart);
    end;
  end;
  
var
  UnitPath, SrcPath: string;
  BaseDir: String;
  ANode: TAVLTreeNode;
  UnitFileInfo: TUnitFileInfo;
  NewItem: TIdentifierListItem;
  UnitExt: String;
  SrcExt: String;
  CurSourceName: String;
begin
  UnitPath:='';
  SrcPath:='';
  GatherUnitAndSrcPath(UnitPath,SrcPath);
  //DebugLn('TIdentCompletionTool.GatherUnitnames UnitPath="',UnitPath,'" SrcPath="',SrcPath,'"');
  BaseDir:=ExtractFilePath(MainFilename);
  TreeOfUnitFiles:=nil;
  try
    // search in unitpath
    UnitExt:='pp;pas;ppu';
    GatherUnitFiles(BaseDir,UnitPath,UnitExt,false,true,TreeOfUnitFiles);
    // search in srcpath
    SrcExt:='pp;pas';
    GatherUnitFiles(BaseDir,SrcPath,SrcExt,false,true,TreeOfUnitFiles);
    // add unitlinks
    GatherUnitsFromUnitLinks;
    // create list
    CurSourceName:=GetSourceName;
    ANode:=TreeOfUnitFiles.FindLowest;
    while ANode<>nil do begin
      UnitFileInfo:=TUnitFileInfo(ANode.Data);
      if CompareIdentifiers(PChar(Pointer(UnitFileInfo.UnitName)),
                            PChar(Pointer(CurSourceName)))<>0
      then begin
        NewItem:=TIdentifierListItem.Create(
            icompCompatible,true,0,
            CurrentIdentifierList.CreateIdentifier(UnitFileInfo.UnitName),
            0,nil,nil,ctnUnit);
        CurrentIdentifierList.Add(NewItem);
      end;
      ANode:=TreeOfUnitFiles.FindSuccessor(ANode);
    end;
  finally
    FreeTreeOfUnitFiles(TreeOfUnitFiles);
  end;
end;

procedure TIdentCompletionTool.GatherSourceNames(const Context: TFindContext);

  procedure Add(const SrcName: string);
  var
    NewItem: TIdentifierListItem;
  begin
    NewItem:=TIdentifierListItem.Create(
        icompExact,true,0,
        CurrentIdentifierList.CreateIdentifier(SrcName),
        0,nil,nil,Context.Node.Desc);
    CurrentIdentifierList.Add(NewItem);
  end;

var
  NewSourceName: String;
  FileSourceName: String;
begin
  // add the unitname as in the filename and as in the source
  FileSourceName:=ExtractFilenameOnly(MainFilename);
  NewSourceName:=GetSourceName(false);
  //DebugLn('TIdentCompletionTool.GatherSourceNames FileSourceName=',FileSourceName,' NewSourceName=',NewSourceName);
  if (FileSourceName<>lowercase(FileSourceName)) then begin
    // the file is not written lowercase => case is important, ignore source name
    Add(FileSourceName);
  end else if (SysUtils.CompareText(NewSourceName,FileSourceName)<>0) then begin
    // source name is not correct => only use file name
    Add(FileSourceName);
  end else if NewSourceName=FileSourceName then begin
    // both are the same => add only one
    Add(FileSourceName);
  end else begin
    // both are valid, just different in case
    // the filename is written lowercase
    // => prefer the source name
    Add(NewSourceName);
  end;
end;

procedure TIdentCompletionTool.InitCollectIdentifiers(
  const CursorPos: TCodeXYPosition; var IdentifierList: TIdentifierList);
var
  StartContext: TFindContext;
begin
  if IdentifierList=nil then IdentifierList:=TIdentifierList.Create;
  CurrentIdentifierList:=IdentifierList;
  CurrentIdentifierList.Clear;
  LastGatheredIdentParent:=nil;
  LastGatheredIdentLevel:=0;
  CurrentIdentifierList.StartContextPos:=CursorPos;
  StartContext := CurrentIdentifierList.StartContext;
  StartContext.Tool := Self;
  CurrentIdentifierList.StartContext:=StartContext;
end;

procedure TIdentCompletionTool.ParseSourceTillCollectionStart(
  const CursorPos: TCodeXYPosition; out CleanCursorPos: integer;
  out CursorNode: TCodeTreeNode; out IdentStartPos, IdentEndPos: integer);
var
  StartContext: TFindContext;
begin
  CleanCursorPos:=0;
  CursorNode:=nil;
  IdentStartPos:=0;
  IdentEndPos:=0;

  // build code tree
  {$IFDEF CTDEBUG}
  DebugLn('TIdentCompletionTool.GatherIdentifiers A CursorPos=',dbgs(CursorPos.X),',',dbgs(CursorPos.Y));
  {$ENDIF}
  BuildTreeAndGetCleanPos(trTillCursor,CursorPos,CleanCursorPos,
                [{$IFNDEF DisableIgnoreErrorAfter}btSetIgnoreErrorPos{$ENDIF}]);

  // find node at position
  CursorNode:=FindDeepestExpandedNodeAtPos(CleanCursorPos,true);
  if CurrentIdentifierList<>nil then begin
    StartContext:=CurrentIdentifierList.StartContext;
    StartContext.Node:=CursorNode;
    CurrentIdentifierList.StartContext:=StartContext;
  end;
  
  // get identifier position
  GetIdentStartEndAtPosition(Src,CleanCursorPos,IdentStartPos,IdentEndPos);
end;

procedure TIdentCompletionTool.FindCollectionContext(
  Params: TFindDeclarationParams; IdentStartPos: integer;
  CursorNode: TCodeTreeNode;
  out GatherContext: TFindContext;
  out ContextExprStartPos: LongInt;
  out StartInSubContext: Boolean);

  function GetContextExprStartPos(IdentStartPos: integer;
    ContextNode: TCodeTreeNode): integer;
  begin
    Result:=FindStartOfVariable(IdentStartPos);
    if Result<ContextNode.StartPos then
      Result:=ContextNode.StartPos;
    MoveCursorToCleanPos(Result);
    ReadNextAtom;
    case ContextNode.Desc of
    ctnProperty:
      // check for special property keywords
      if WordIsPropertySpecifier.DoItUpperCase(UpperSrc,
          CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
      then
        // do not resolve property specifiers
        Result:=IdentStartPos;
    end;
  end;

var
  ExprType: TExpressionType;
begin
  GatherContext:=CreateFindContext(Self,CursorNode);

  ContextExprStartPos:=GetContextExprStartPos(IdentStartPos,CursorNode);
  if GatherContext.Node.Desc=ctnWithVariable then
    GatherContext.Node:=GatherContext.Node.Parent;

  StartInSubContext:=false;
  //DebugLn(['TIdentCompletionTool.FindCollectionContext ContextExprStartPos=',ContextExprStartPos,' "',dbgstr(copy(Src,ContextExprStartPos,20)),'" IdentStartPos="',dbgstr(copy(Src,IdentStartPos,20)),'"']);
  if ContextExprStartPos<IdentStartPos then begin
    MoveCursorToCleanPos(IdentStartPos);
    Params.ContextNode:=CursorNode;
    Params.SetIdentifier(Self,nil,nil);
    Params.Flags:=[fdfExceptionOnNotFound,
                   fdfSearchInParentNodes,fdfSearchInAncestors];
    ExprType:=FindExpressionTypeOfVariable(ContextExprStartPos,IdentStartPos,
                                           Params);
    //DebugLn(['TIdentCompletionTool.FindCollectionContext ',ExprTypeToString(ExprType)]);
    if (ExprType.Desc=xtContext) then begin
      GatherContext:=ExprType.Context;
      StartInSubContext:=true;
    end;
  end;
end;

function TIdentCompletionTool.CollectAllContexts(
  Params: TFindDeclarationParams; const FoundContext: TFindContext
  ): TIdentifierFoundResult;
begin
  Result:=ifrProceedSearch;
  if FoundContext.Node=nil then exit;
  case FoundContext.Node.Desc of
  ctnProcedure:
    begin
      //DebugLn('TIdentCompletionTool.CollectAllContexts CurrentContexts.ProcNameAtom.StartPos=',dbgs(CurrentContexts.ProcNameAtom.StartPos));
      if (CurrentContexts.ProcName='') then exit;
      FoundContext.Tool.MoveCursorToProcName(FoundContext.Node,true);
      //DebugLn(['TIdentCompletionTool.CollectAllContexts ProcName=',GetIdentifier(@FoundContext.Tool.Src[FoundContext.Tool.CurPos.StartPos])]);
      if not FoundContext.Tool.CompareSrcIdentifier(
        FoundContext.Tool.CurPos.StartPos,
        CurrentContexts.ProcName)
      then exit;
    end;
  else
    exit;
  end;
  AddCollectionContext(FoundContext.Tool,FoundContext.Node);
end;

procedure TIdentCompletionTool.AddCollectionContext(Tool: TFindDeclarationTool;
  Node: TCodeTreeNode);
begin
  if CurrentContexts=nil then
    CurrentContexts:=TCodeContextInfo.Create;
  CurrentContexts.Add(CreateExpressionType(xtContext,xtNone,
                                           CreateFindContext(Tool,Node)));
  //DebugLn('TIdentCompletionTool.AddCollectionContext ',Node.DescAsString,' ',ExtractNode(Node,[]));
end;

function TIdentCompletionTool.GatherIdentifiers(
  const CursorPos: TCodeXYPosition; var IdentifierList: TIdentifierList;
  BeautifyCodeOptions: TBeautifyCodeOptions): boolean;
var
  CleanCursorPos, IdentStartPos, IdentEndPos: integer;
  CursorNode: TCodeTreeNode;
  Params: TFindDeclarationParams;
  GatherContext: TFindContext;
  ContextExprStartPos: Integer;
  StartInSubContext: Boolean;
  StartPosOfVariable: LongInt;
  
  procedure CheckProcedureDeclarationContext;
  var
    Node: TCodeTreeNode;
    Can: Boolean;
  begin
    //DebugLn(['CheckProcedureDeclarationContext ',CursorNode.DescAsString]);
    Node:=CursorNode;
    Can:=false;
    if (Node.Parent<>nil)
    and (Node.Parent.Desc in (AllClassBaseSections+AllClassVarSections))
    and (Node.Desc=ctnVarDefinition)
    and (CurrentIdentifierList.StartAtomBehind.Flag<>cafColon) then begin
      { cursor is at a class variable definition without type
        for example:
        
        public
          MouseM|
        end;
      }
      Can:=true;
    end
    else if (Node.Desc=ctnProcedure) and (not NodeIsMethodBody(Node))
    and (not (CurrentIdentifierList.StartAtomBehind.Flag
              in [cafEdgedBracketOpen,cafRoundBracketOpen]))
    then begin
      // for example: procedure DoSomething|
      Can:=true;
    end
    else if Node.Desc in (AllClassBaseSections+AllSourceTypes
                     +[ctnInterface,ctnImplementation])
    then begin
      //DebugLn(['TIdentCompletionTool.CheckProcedureDeclarationContext ilcfCanProcDeclaration']);
      Can:=true;
    end;
    if Can then
      CurrentIdentifierList.ContextFlags:=
        CurrentIdentifierList.ContextFlags+[ilcfCanProcDeclaration];
  end;
  
begin
  Result:=false;

  ActivateGlobalWriteLock;
  Params:=TFindDeclarationParams.Create;
  try
    InitCollectIdentifiers(CursorPos,IdentifierList);
    ParseSourceTillCollectionStart(CursorPos,CleanCursorPos,CursorNode,
                                   IdentStartPos,IdentEndPos);
    if CleanCursorPos=0 then ;

    // find context
    {$IFDEF CTDEBUG}
    DebugLn('TIdentCompletionTool.GatherIdentifiers B',
      ' CleanCursorPos=',dbgs(CleanCursorPos),
      ' IdentStartPos=',dbgs(IdentStartPos),' IdentEndPos=',dbgs(IdentEndPos),
      ' Ident=',copy(Src,IdentStartPos,IdentEndPos-IdentStartPos));
    {$ENDIF}
    GatherContext:=CreateFindContext(Self,CursorNode);
    if CursorNode.Desc=ctnUsesSection then begin
      GatherUnitNames(IdentStartPos,GatherContext,BeautifyCodeOptions);
    end else if CursorNode.Desc in AllSourceTypes then begin
      GatherSourceNames(GatherContext);
    end else begin
      // find class and ancestors if existing (needed for protected identifiers)
      FindContextClassAndAncestors(CursorPos,ClassAndAncestors);

      FindCollectionContext(Params,IdentStartPos,CursorNode,
                           GatherContext,ContextExprStartPos,StartInSubContext);
      if ContextExprStartPos=0 then ;

      // search and gather identifiers in context
      if (GatherContext.Tool<>nil) and (GatherContext.Node<>nil) then begin
        {$IFDEF CTDEBUG}
        DebugLn('TIdentCompletionTool.GatherIdentifiers D CONTEXT: ',
          GatherContext.Tool.MainFilename,
          ' ',GatherContext.Node.DescAsString,
          ' "',StringToPascalConst(copy(GatherContext.Tool.Src,GatherContext.Node.StartPos,50)),'"');
        {$ENDIF}
        // gather all identifiers in context
        Params.ContextNode:=GatherContext.Node;
        Params.SetIdentifier(Self,nil,@CollectAllIdentifiers);
        Params.Flags:=[fdfSearchInAncestors,fdfCollect,fdfFindVariable];
        if not StartInSubContext then
          Include(Params.Flags,fdfSearchInParentNodes);
        if Params.ContextNode.Desc in [ctnClass,ctnClassInterface] then
          Exclude(Params.Flags,fdfSearchInParentNodes);
        {$IFDEF CTDEBUG}
        DebugLn('TIdentCompletionTool.GatherIdentifiers F');
        {$ENDIF}
        CurrentIdentifierList.Context:=GatherContext;
        GatherContext.Tool.FindIdentifierInContext(Params);
      end;

      // add useful identifiers without context
      {$IFDEF CTDEBUG}
      DebugLn('TIdentCompletionTool.GatherIdentifiers G');
      {$ENDIF}
      GatherUsefulIdentifiers(IdentStartPos,GatherContext,BeautifyCodeOptions);

      // check for incomplete context
      
      // context bracket level
      CurrentIdentifierList.StartBracketLvl:=
        GetBracketLvl(Src,CursorNode.StartPos,IdentStartPos,
                      Scanner.NestedComments);
      if CursorNode.Desc in AllPascalStatements then begin
        CurrentIdentifierList.ContextFlags:=
          CurrentIdentifierList.ContextFlags+[ilcfStartInStatement];
      end;
      // context in front of
      StartPosOfVariable:=FindStartOfVariable(IdentStartPos);
      if StartPosOfVariable>0 then begin
        MoveCursorToCleanPos(StartPosOfVariable);
        ReadPriorAtom;
        CurrentIdentifierList.StartAtomInFront:=CurPos;
        // check if LValue
        if (ilcfStartInStatement in CurrentIdentifierList.ContextFlags) then
        begin
          if (CurPos.Flag in [cafSemicolon,cafBegin,cafEnd])
          or UpAtomIs('TRY') or UpAtomIs('FOR') or UpAtomIs('DO')
          then begin
            CurrentIdentifierList.ContextFlags:=
              CurrentIdentifierList.ContextFlags+[ilcfStartIsLValue];
          end;
          if UpAtomIs('IF') or UpAtomIs('FOR') or UpAtomIs('DO')
          or UpAtomIs('CASE') or UpAtomIs('OF') or UpAtomIs('WHILE') then begin
            CurrentIdentifierList.ContextFlags:=
              CurrentIdentifierList.ContextFlags+[ilcfIsExpression];
          end;
        end;
      end;
      // context behind
      if IdentEndPos<SrcLen then begin
        MoveCursorToCleanPos(IdentEndPos);
        ReadNextAtom;
        CurrentIdentifierList.StartAtomBehind:=CurPos;
        // check if in statement
        if (ilcfStartInStatement in CurrentIdentifierList.ContextFlags) then
        begin
          if (CurrentIdentifierList.StartBracketLvl=0) then begin
            // check if end needs semicolon
            if (CurPos.Flag in [cafEnd,cafBegin])
            or WordIsBlockKeyWord.DoItUpperCase(UpperSrc,
                                  CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
            or ((CurPos.Flag=cafWord)
                and (not UpAtomIs('ELSE'))
                and (not PositionsInSameLine(Src,IdentEndPos,CurPos.StartPos)))
            then begin
              // add semicolon
              CurrentIdentifierList.ContextFlags:=
                CurrentIdentifierList.ContextFlags+[ilcfNeedsEndSemicolon];
            end;
          end;
        end;
      end;

      // check for procedure/method declaration context
      CheckProcedureDeclarationContext;
    end;

    Result:=true;
  finally
    FreeListOfPFindContext(ClassAndAncestors);
    FreeAndNil(FoundPublicProperties);
    Params.Free;
    ClearIgnoreErrorAfter;
    DeactivateGlobalWriteLock;
    CurrentIdentifierList:=nil;
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TIdentCompletionTool.GatherIdentifiers END');
  {$ENDIF}
end;

function TIdentCompletionTool.FindCodeContext(const CursorPos: TCodeXYPosition;
  out CodeContexts: TCodeContextInfo): boolean;
var
  CleanCursorPos: integer;
  CursorNode: TCodeTreeNode;
  Params: TFindDeclarationParams;

  function CheckContextIsParameter(var Ok: boolean): boolean;
  // returns true, on error or context is parameter
  var
    VarNameAtom, ProcNameAtom: TAtomPosition;
    ParameterIndex: integer;
    GatherContext: TFindContext;
    ContextExprStartPos: LongInt;
    StartInSubContext: Boolean;
  begin
    Result:=false;
    // check if in a begin..end block
    if (CursorNode.Desc<>ctnBeginBlock)
    and (not CursorNode.HasParentOfType(ctnBeginBlock)) then begin
      DebugLn(['TIdentCompletionTool.FindCodeContext.CheckContextIsParameter not in a begin block']);
      exit;
    end;
    // check if cursor is in a parameter list
    if not CheckParameterSyntax(CursorNode, CleanCursorPos,
                                VarNameAtom, ProcNameAtom, ParameterIndex)
    then begin
      if VarNameAtom.StartPos=0 then ;
      DebugLn(['TIdentCompletionTool.FindCodeContext.CheckContextIsParameter not in a parameter list']);
      exit;
    end;
    //DebugLn('CheckContextIsParameter Variable=',GetAtom(VarNameAtom),' Proc=',GetAtom(ProcNameAtom),' ParameterIndex=',dbgs(ParameterIndex));
    
    // it is a parameter -> create context
    Result:=true;
    if CurrentContexts=nil then
      CurrentContexts:=TCodeContextInfo.Create;
    CurrentContexts.Tool:=Self;
    CurrentContexts.ParameterIndex:=ParameterIndex+1;
    CurrentContexts.ProcNameAtom:=ProcNameAtom;
    CurrentContexts.ProcName:=GetAtom(ProcNameAtom);
    MoveCursorToAtomPos(ProcNameAtom);
    ReadNextAtom; // read opening bracket
    CurrentContexts.StartPos:=CurPos.EndPos;
    // read closing bracket
    if ReadTilBracketClose(false) then
      CurrentContexts.EndPos:=CurPos.StartPos
    else
      CurrentContexts.EndPos:=SrcLen+1;

    FindCollectionContext(Params,ProcNameAtom.StartPos,CursorNode,
                          GatherContext,ContextExprStartPos,StartInSubContext);
    if ContextExprStartPos=0 then ;
    //DebugLn(['CheckContextIsParameter StartInSubContext=',StartInSubContext,' ',GatherContext.Node.DescAsString,' "',copy(GatherContext.Tool.Src,GatherContext.Node.StartPos-20,25),'"']);

    // gather declarations of all parameter lists
    Params.ContextNode:=GatherContext.Node;
    Params.SetIdentifier(Self,@Src[ProcNameAtom.StartPos],@CollectAllContexts);
    Params.Flags:=[fdfSearchInAncestors,fdfCollect,fdfFindVariable];
    if not StartInSubContext then
      Include(Params.Flags,fdfSearchInParentNodes);
    CurrentIdentifierList.Context:=GatherContext;
    //DebugLn('CheckContextIsParameter searching procedure ...');
    GatherContext.Tool.FindIdentifierInContext(Params);
    //DebugLn('CheckContextIsParameter END');
    Ok:=true;
  end;

var
  IdentifierList: TIdentifierList;
  IdentStartPos, IdentEndPos: integer;
begin
  CodeContexts:=nil;
  Result:=false;

  IdentifierList:=nil;
  CurrentContexts:=CodeContexts;

  ActivateGlobalWriteLock;
  Params:=TFindDeclarationParams.Create;
  try
    InitCollectIdentifiers(CursorPos,IdentifierList);
    ParseSourceTillCollectionStart(CursorPos,CleanCursorPos,CursorNode,
                                   IdentStartPos,IdentEndPos);
    if IdentStartPos=0 then ;
    if IdentEndPos=0 then ;

    // find class and ancestors if existing (needed for protected identifiers)
    FindContextClassAndAncestors(CursorPos,ClassAndAncestors);

    if CursorNode<>nil then begin
      if not CheckContextIsParameter(Result) then begin
        DebugLn(['TIdentCompletionTool.FindCodeContext cursor not at parameter']);
        exit;
      end;
    end;

    if CurrentContexts=nil then begin
      // create default
      AddCollectionContext(Self,CursorNode);
    end;

    Result:=true;
  finally
    if Result then begin
      CodeContexts:=CurrentContexts;
      CurrentContexts:=nil;
    end else begin
      FreeAndNil(CurrentContexts);
    end;
    FreeListOfPFindContext(ClassAndAncestors);
    FreeAndNil(FoundPublicProperties);
    Params.Free;
    ClearIgnoreErrorAfter;
    DeactivateGlobalWriteLock;
    FreeAndNil(CurrentIdentifierList);
  end;
end;


{ TIdentifierListItem }

function TIdentifierListItem.GetParamList: string;
begin
  if not (iliParamListValid in Flags) then begin
    // Note: if you implement param lists for other than ctnProcedure, check
    //       CompareParamList
    if (Node<>nil) and (Node.Desc=ctnProcedure) then begin
      FParamList:=Tool.ExtractProcHead(Node,
         [phpWithoutClassKeyword,phpWithoutClassName,
          phpWithoutName,phpInUpperCase]);
      //debugln('TIdentifierListItem.GetParamList A ',GetIdentifier(Identifier),' ',Tool.MainFilename,' ',dbgs(Node.StartPos));
    end else
      FParamList:='';
    Include(Flags,iliParamListValid);
  end;
  Result:=FParamList;
end;

procedure TIdentifierListItem.SetParamList(const AValue: string);
begin
  FParamList:=AValue;
  Include(Flags,iliParamListValid);
end;

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
  if Tool<>nil then
    Result:=Result+' File='+Tool.MainFilename;
  if Node<>nil then
    Result:=Result+' Node='+Node.DescAsString
      +' "'+StringToPascalConst(copy(Tool.Src,Node.StartPos,50))+'"';
end;

function TIdentifierListItem.GetDesc: TCodeTreeNodeDesc;
begin
  if Node<>nil then
    Result:=Node.Desc
  else
    Result:=DefaultDesc;
end;

constructor TIdentifierListItem.Create(
  NewCompatibility: TIdentifierCompatibility; NewHasChilds: boolean;
  NewHistoryIndex: integer; NewIdentifier: PChar; NewLevel: integer;
  NewNode: TCodeTreeNode; NewTool: TFindDeclarationTool;
  NewDefaultDesc: TCodeTreeNodeDesc);
begin
  Compatibility:=NewCompatibility;
  if NewHasChilds then Include(FLags,iliHasChilds);
  HistoryIndex:=NewHistoryIndex;
  Identifier:=NewIdentifier;
  Level:=NewLevel;
  Node:=NewNode;
  Tool:=NewTool;
  DefaultDesc:=NewDefaultDesc;
  BaseExprType:=CleanExpressionType;
end;

function TIdentifierListItem.IsProcNodeWithParams: boolean;
begin
  Result:=(Node<>nil) and Tool.ProcNodeHasParamList(Node);
end;

function TIdentifierListItem.IsPropertyWithParams: boolean;
begin
  Result:=(Node<>nil) and Tool.PropertyNodeHasParamList(Node);
end;

function TIdentifierListItem.CheckHasChilds: boolean;
// returns true if test was successful
begin
  Result:=false;
  if GetDesc in [ctnClass,ctnRecordType,ctnClassInterface] then begin
    Result:=true;
    exit;
  end;
  if Node=nil then exit;
  UpdateBaseContext;
  if (BaseExprType.Desc=xtContext)
    and (BaseExprType.Context.Node<>nil)
    and (BaseExprType.Context.Node.Desc
      in [ctnClass,ctnRecordType,ctnClassInterface])
  then
    Include(Flags,iliHasChilds);
end;

function TIdentifierListItem.CanBeAssigned: boolean;
begin
  Result:=false;
  if (Node=nil) then exit;
  if (GetDesc=ctnVarDefinition) then
    Result:=true;
  if (Node.Desc in [ctnProperty,ctnGlobalProperty]) then begin
    if Tool.PropertyHasSpecifier(Node,'write') then exit(true);
    if Tool.PropNodeIsTypeLess(Node) then begin
      exit(true);// ToDo: search the real property definition
    end;
  end;
end;

procedure TIdentifierListItem.UpdateBaseContext;
var
  Params: TFindDeclarationParams;
begin
  if (iliBaseExprTypeValid in Flags) then exit;
  BaseExprType:=CleanExpressionType;
  BaseExprType.Desc:=xtNone;
  if (Node<>nil) and (Tool<>nil) then begin
    Tool.ActivateGlobalWriteLock;
    Params:=TFindDeclarationParams.Create;
    try
      BaseExprType.Context:=Tool.FindBaseTypeOfNode(Params,Node);
      if (BaseExprType.Context.Node<>nil) then
        BaseExprType.Desc:=xtContext;
    finally
      Params.Free;
      Tool.DeactivateGlobalWriteLock;
    end;
  end;
  Include(Flags,iliBaseExprTypeValid);
end;

function TIdentifierListItem.HasChilds: boolean;
begin
  Result:=iliHasChilds in Flags;
end;

function TIdentifierListItem.IsFunction: boolean;
begin
  if not (iliIsFunctionValid in Flags) then begin
    if (Node<>nil)
    and Tool.NodeIsFunction(Node) then
      Include(Flags,iliIsFunction);
    Include(Flags,iliIsFunctionValid);
  end;
  Result:=iliIsFunction in Flags;
end;

function TIdentifierListItem.IsAbstractMethod: boolean;
begin
  if not (iliIsAbstractMethodValid in Flags) then begin
    if (Node<>nil)
    and Tool.MoveCursorToProcSpecifier(Node,psABSTRACT) then
      Include(Flags,iliIsAbstractMethod);
    Include(Flags,iliIsAbstractMethodValid);
  end;
  Result:=iliIsAbstractMethod in Flags;
end;

procedure TIdentifierListItem.Clear;
begin
  FParamList:='';
  Compatibility:=icompUnknown;
  HistoryIndex:=0;
  Identifier:=nil;
  Level:=0;
  Node:=nil;
  Tool:=nil;
  DefaultDesc:=ctnNone;
  Flags:=[];
  BaseExprType:=CleanExpressionType;
end;

function TIdentifierListItem.CompareParamList(CompareItem: TIdentifierListItem
  ): integer;
begin
  Result:=0;
  if Self=CompareItem then exit;
  if (Node=CompareItem.Node) then exit;
  if (Node=nil) or (CompareItem.Node=nil) then exit;
  if (Node.Desc<>ctnProcedure) or (CompareItem.Node.Desc<>ctnProcedure) then
    exit;
  {DbgOut('TIdentifierListItem.CompareParamList ',GetIdentifier(Identifier),'=',GetIdentifier(CompareItem.Identifier));
  if Node<>nil then
    DbgOut(' Self=',Tool.MainFilename,' ',dbgs(Node.StartPos));
  if CompareItem.Node<>nil then
    DbgOut(' Other=',CompareItem.Tool.MainFilename,' ',dbgs(CompareItem.Node.StartPos));
  debugln('');}
  Result:=SysUtils.CompareText(ParamList,CompareItem.ParamList);
end;

{ TIdentifierHistoryList }

procedure TIdentifierHistoryList.SetCapacity(const AValue: integer);
begin
  if FCapacity=AValue then exit;
  FCapacity:=AValue;
  if FCapacity<1 then FCapacity:=1;
  while (FItems.Count>0) and (FItems.Count>=FCapacity) do
    FItems.FreeAndDelete(FItems.FindHighest);
end;

function TIdentifierHistoryList.FindItem(NewItem: TIdentifierListItem
  ): TAVLTreeNode;
begin
  if NewItem<>nil then
    Result:=FItems.FindKey(NewItem,@CompareIdentItemWithHistListItem)
  else
    Result:=nil;
end;

constructor TIdentifierHistoryList.Create;
begin
  FItems:=TAVLTree.Create(@CompareIdentHistListItem);
  FCapacity:=30;
end;

destructor TIdentifierHistoryList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TIdentifierHistoryList.Clear;
begin
  FItems.FreeAndClear;
end;

procedure TIdentifierHistoryList.Add(NewItem: TIdentifierListItem);
var
  OldAVLNode: TAVLTreeNode;
  NewHistItem: TIdentHistListItem;
  AnAVLNode: TAVLTreeNode;
  AdjustIndex: Integer;
  AnHistItem: TIdentHistListItem;
begin
  if NewItem=nil then exit;
  OldAVLNode:=FindItem(NewItem);
  {$IFDEF ShowHistory}
  DebugLn('TIdentifierHistoryList.Add Count=',Count,' Found=',OldAVLNode<>nil,
    ' ITEM: ',NewItem.AsString);
  {$ENDIF}
  if OldAVLNode<>nil then begin
    // already in tree
    NewHistItem:=TIdentHistListItem(OldAVLNode.Data);
    if NewHistItem.HistoryIndex=0 then exit;
    // must be moved -> remove it from the tree
    AdjustIndex:=NewHistItem.HistoryIndex;
    FItems.Delete(OldAVLNode);
  end else begin
    // create a new history item
    NewHistItem:=TIdentHistListItem.Create;
    NewHistItem.Identifier:=GetIdentifier(NewItem.Identifier);
    NewHistItem.NodeDesc:=NewItem.GetDesc;
    NewHistItem.ParamList:=NewItem.ParamList;
    AdjustIndex:=0;
  end;
  NewHistItem.HistoryIndex:=0;
  // adjust all other HistoryIndex
  AnAVLNode:=Fitems.FindLowest;
  while AnAVLNode<>nil do begin
    AnHistItem:=TIdentHistListItem(AnAVLNode.Data);
    if AnHistItem.HistoryIndex>=AdjustIndex then
      inc(AnHistItem.HistoryIndex);
    AnAVLNode:=FItems.FindSuccessor(AnAVLNode);
  end;
  if (FItems.Count>0) and (FItems.Count>=FCapacity) then
    FItems.FreeAndDelete(FItems.FindHighest);
  FItems.Add(NewHistItem);
  {$IFDEF ShowHistory}
  DebugLn('TIdentifierHistoryList.Added Count=',Count);
  {$ENDIF}
end;

function TIdentifierHistoryList.GetHistoryIndex(AnItem: TIdentifierListItem
  ): integer;
var
  AnAVLNode: TAVLTreeNode;
begin
  AnAVLNode:=FindItem(AnItem);
  if AnAVLNode=nil then
    Result:=33333333  // a very high value
  else
    Result:=TIdentHistListItem(AnAVLNode.Data).HistoryIndex;
end;

function TIdentifierHistoryList.Count: integer;
begin
  Result:=FItems.Count;
end;

{ TCodeContextInfo }

function TCodeContextInfo.GetItems(Index: integer): TExpressionType;
begin
  Result:=FItems[Index];
end;

constructor TCodeContextInfo.Create;
begin

end;

destructor TCodeContextInfo.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TCodeContextInfo.Count: integer;
begin
  Result:=FCount;
end;

function TCodeContextInfo.Add(const Context: TExpressionType): integer;
begin
  inc(FCount);
  Result:=Count;
  ReAllocMem(FItems,SizeOf(TExpressionType)*FCount);
  FItems[FCount-1]:=Context;
end;

procedure TCodeContextInfo.Clear;
begin
  FCount:=0;
  ReAllocMem(FItems,0);
end;

initialization
  IdentifierListItemMemManager:=TIdentifierListItemMemManager.Create;
  
finalization
  IdentifierListItemMemManager.Free;
  IdentifierListItemMemManager:=nil;

end.

