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
{ $DEFINE ShowHistory}

// new features
{ $DEFINE IgnoreErrorAfterCursor}


uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, FileProcs, CodeTree, CodeAtom, CustomCodeTool,
  KeywordFuncLists, BasicCodeTools, LinkScanner, AVL_Tree, CodeToolMemManager,
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
    iliBaseExprTypeValid
    );
  TIdentListItemFlags = set of TIdentListItemFlag;
  
  TIdentifierListItem = class
  private
    FNext: TIdentifierListItem;
    FParamList: string;
    FParamListValid: boolean;
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
    procedure Clear;
  public
    property ParamList: string read GetParamList write SetParamList;
  end;
  
  TIdentifierListFlag = (ilfFilteredListNeedsUpdate);
  TIdentifierListFlags = set of TIdentifierListFlag;
  
  TIdentifierListContextFlag = (
    ilcfStartInStatement,
    ilcfStartIsLValue,
    ilcfContextNeedsEndSemicolon
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
    FCreatedIdentifiers: TList; // list of PChar
    FFilteredList: TList; // list of TIdentifierListItem
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
  // TIdentCompletionTool

  TIdentCompletionTool = class(TFindDeclarationTool)
  private
    LastGatheredIdentParent: TCodeTreeNode;
    LastGatheredIdentLevel: integer;
  protected
    CurrentIdentifierList: TIdentifierList;
    function CollectAllIdentifiers(Params: TFindDeclarationParams;
      const FoundContext: TFindContext): TIdentifierFoundResult;
    procedure GatherPredefinedIdentifiers(CleanPos: integer;
      const Context: TFindContext; BeautifyCodeOptions: TBeautifyCodeOptions);
    procedure GatherUsefulIdentifiers(CleanPos: integer;
      const Context: TFindContext; BeautifyCodeOptions: TBeautifyCodeOptions);
  public
    function GatherIdentifiers(const CursorPos: TCodeXYPosition;
      var IdentifierList: TIdentifierList;
      BeautifyCodeOptions: TBeautifyCodeOptions): boolean;
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
  Result:=AnsiCompareText(Item2.ParamList,Item1.ParamList);
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
  Result:=AnsiCompareText(Item2.ParamList,Item1.ParamList);
end;

function CompareIdentHistListItem(Data1, Data2: Pointer): integer;
var
  Item1: TIdentHistListItem;
  Item2: TIdentHistListItem;
begin
  Item1:=TIdentHistListItem(Data1);
  Item2:=TIdentHistListItem(Data2);

  Result:=CompareIdentifiers(PChar(Item2.Identifier),PChar(Item1.Identifier));
  if Result<>0 then exit;

  Result:=CompareIdentifiers(PChar(Item2.ParamList),PChar(Item1.ParamList));
end;

function CompareIdentItemWithHistListItem(Data1, Data2: Pointer): integer;
var
  IdentItem: TIdentifierListItem;
  HistItem: TIdentHistListItem;
begin
  IdentItem:=TIdentifierListItem(Data1);
  HistItem:=TIdentHistListItem(Data2);

  Result:=CompareIdentifiers(PChar(HistItem.Identifier),IdentItem.Identifier);
  if Result<>0 then exit;

  Result:=AnsiCompareText(HistItem.ParamList,IdentItem.ParamList);
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
  if FFilteredList=nil then FFilteredList:=TList.Create;
  FFilteredList.Clear;
  {$IFDEF CTDEBUG}
  DebugLn('TIdentifierList.UpdateFilteredList Prefix="',Prefix,'"');
  {$ENDIF}
  AnAVLNode:=FItems.FindLowest;
  while AnAVLNode<>nil do begin
    CurItem:=TIdentifierListItem(AnAVLNode.Data);
    if (CurItem.Identifier<>nil)
    and ComparePrefixIdent(PChar(Prefix),CurItem.Identifier) then begin
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
  FCreatedIdentifiers:=TList.Create;
end;

destructor TIdentifierList.Destroy;
begin
  Clear;
  FItems.Free;
  FIdentView.Free;
  FFilteredList.Free;
  FIdentSearchItem.Free;
  FCreatedIdentifiers.Free;
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
    and (CompareIdentifiers(PChar(Ident),PChar(FCreatedIdentifiers[Result]))<>0)
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
      Result:=PChar(FCreatedIdentifiers[i])
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
    and ComparePrefixIdent(PChar(Prefix),CurItem.Identifier) then begin
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
  NewItem: TIdentifierListItem;
  Ident: PChar;
  CurContextParent: TCodeTreeNode;
begin
  // proceed searching ...
  Result:=ifrProceedSearch;

  {$IFDEF ShowFoundIdents}
  DebugLn('::: COLLECT IDENT ',FoundContext.Node.DescAsString,
    ' "',StringToPascalConst(copy(FoundContext.Tool.Src,FoundContext.Node.StartPos,50)),'"'
    ,' ',fdfIgnoreUsedUnits in Params.Flags);
  {$ENDIF}

  CurContextParent:=FoundContext.Node.GetFindContextParent;
  if LastGatheredIdentParent<>CurContextParent then begin
    // new context level
    LastGatheredIdentParent:=CurContextParent;
    inc(LastGatheredIdentLevel);
  end;
  
  if FoundContext.Tool<>Self then begin
    // identifier is in another unit
    if (FoundContext.Node.Parent<>nil) then begin
      if (FoundContext.Node.Parent.Desc=ctnClassPrivate) then begin
        // skip private definitions in other units
        exit;
      end;
    end;
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
  
  ctnVarDefinition,ctnConstDefinition,ctnEnumIdentifier:
    Ident:=@FoundContext.Tool.Src[FoundContext.Node.StartPos];
    
  ctnProcedure,ctnProcedureHead:
    Ident:=FoundContext.Tool.GetProcNameIdentifier(FoundContext.Node);
    
  ctnProperty:
    begin
      if FoundContext.Tool.PropNodeIsTypeLess(FoundContext.Node) then exit;
      Ident:=FoundContext.Tool.GetPropertyNameIdentifier(FoundContext.Node);
    end;
    
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

var
  NewItem: TIdentifierListItem;
  ProcNode: TCodeTreeNode;
begin
  if Context.Node.Desc in AllPascalStatements then begin
    if Context.Tool.NodeIsInAMethod(Context.Node)
    and (not CurrentIdentifierList.HasIdentifier('Self','')) then begin
      // method body -> add 'Self'
      NewItem:=TIdentifierListItem.Create(
          icompUnknown,
          true,
          0,
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
          0,
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

function TIdentCompletionTool.GatherIdentifiers(
  const CursorPos: TCodeXYPosition; var IdentifierList: TIdentifierList;
  BeautifyCodeOptions: TBeautifyCodeOptions): boolean;
  
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
        // don't resolve property specifiers
        Result:=IdentStartPos;
    end;
  end;
  
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
  CurrentIdentifierList.StartContextPos:=CursorPos;
  CurrentIdentifierList.StartContext.Tool:=Self;
  
  ActivateGlobalWriteLock;
  Params:=TFindDeclarationParams.Create;
  try
    // build code tree
    {$IFDEF CTDEBUG}
    DebugLn('TIdentCompletionTool.GatherIdentifiers A CursorPos=',dbgs(CursorPos.X),',',dbgs(CursorPos.Y));
    {$ENDIF}
    BuildTreeAndGetCleanPos(trTillCursor,CursorPos,CleanCursorPos,
             [{$IFDEF IgnoreErrorAfterCursor}btSetIgnoreErrorPos,{$ENDIF}]);
                  
    // find node at position
    CursorNode:=FindDeepestExpandedNodeAtPos(CleanCursorPos,true);
    CurrentIdentifierList.StartContext.Node:=CursorNode;

    // get identifier position
    GetIdentStartEndAtPosition(Src,CleanCursorPos,IdentStartPos,IdentEndPos);
    
    // find context
    {$IFDEF CTDEBUG}
    DebugLn('TIdentCompletionTool.GatherIdentifiers B',
      ' CleanCursorPos=',dbgs(CleanCursorPos),
      ' IdentStartPos=',dbgs(IdentStartPos),' IdentEndPos=',dbgs(IdentEndPos),
      ' Ident=',copy(Src,IdentStartPos,IdentEndPos-IdentStartPos));
    {$ENDIF}
    GatherContext:=CreateFindContext(Self,CursorNode);
    ContextExprStartPos:=GetContextExprStartPos(IdentStartPos,CursorNode);

    {$IFDEF CTDEBUG}
    DebugLn('TIdentCompletionTool.GatherIdentifiers C',
      ' ContextExprStartPos=',dbgs(ContextExprStartPos),
      ' Expr=',StringToPascalConst(copy(Src,ContextExprStartPos,
                    IdentStartPos-ContextExprStartPos)));
    {$ENDIF}
    if ContextExprStartPos<IdentStartPos then begin
      MoveCursorToCleanPos(IdentStartPos);
      Params.ContextNode:=CursorNode;
      Params.SetIdentifier(Self,nil,nil);
      Params.Flags:=[fdfExceptionOnNotFound,
                     fdfSearchInParentNodes,fdfSearchInAncestors];
      ExprType:=FindExpressionTypeOfVariable(ContextExprStartPos,IdentStartPos,
                                             Params);
      if (ExprType.Desc=xtContext) then
        GatherContext:=ExprType.Context;
    end;
    
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
      Params.Flags:=[fdfSearchInParentNodes,fdfSearchInAncestors,
                     fdfCollect,fdfFindVariable];
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
    // context behind
    if IdentEndPos<SrcLen then begin
      MoveCursorToCleanPos(IdentEndPos);
      ReadNextAtom;
      CurrentIdentifierList.StartAtomBehind:=CurPos;
      // check if in statement
      if CursorNode.Desc in AllPascalStatements then begin
        CurrentIdentifierList.ContextFlags:=
          CurrentIdentifierList.ContextFlags+[ilcfStartInStatement];
        // check if at end of statement
        if (CurPos.Flag in [cafEnd,cafBegin])
        or ((not UpAtomIs('ELSE'))
            and (CurPos.Flag=cafWord)
            and (not PositionsInSameLine(Src,IdentEndPos,CurPos.StartPos)))
        then
          if CurrentIdentifierList.StartBracketLvl=0 then
            CurrentIdentifierList.ContextFlags:=
              CurrentIdentifierList.ContextFlags+[ilcfContextNeedsEndSemicolon];
      end;
    end;
    // context in front of
    MoveCursorToCleanPos(IdentStartPos);
    ReadPriorAtom;
    CurrentIdentifierList.StartAtomInFront:=CurPos;
    // check if LValue
    if (ilcfStartInStatement in CurrentIdentifierList.ContextFlags) then begin
      if (CurPos.Flag in [cafSemicolon,cafBegin,cafEnd])
         or WordIsBlockKeyWord.DoItUpperCase(UpperSrc,
                                  CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
      then
        CurrentIdentifierList.ContextFlags:=
          CurrentIdentifierList.ContextFlags+[ilcfStartIsLValue];
    end;

    Result:=true;
  finally
    Params.Free;
    ClearIgnoreErrorAfter;
    DeactivateGlobalWriteLock;
  end;
  {$IFDEF CTDEBUG}
  DebugLn('TIdentCompletionTool.GatherIdentifiers END');
  {$ENDIF}
end;

{ TIdentifierListItem }

function TIdentifierListItem.GetParamList: string;
begin
  if not FParamListValid then begin
    if (Node<>nil) and (Node.Desc=ctnProcedure) then
      FParamList:=Tool.ExtractProcHead(Node,
         [phpWithoutClassKeyword,phpWithoutClassName,
          phpWithoutName,phpInUpperCase])
    else
      FParamList:='';
    FParamListValid:=true;
  end;
  Result:=FParamList;
end;

procedure TIdentifierListItem.SetParamList(const AValue: string);
begin
  FParamList:=AValue;
  FParamListValid:=true;
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
end;

procedure TIdentifierListItem.UpdateBaseContext;
var
  Params: TFindDeclarationParams;
begin
  if (iliBaseExprTypeValid in Flags) then exit;
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

procedure TIdentifierListItem.Clear;
begin
  FParamList:='';
  FParamListValid:=false;
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

initialization
  IdentifierListItemMemManager:=TIdentifierListItemMemManager.Create;
  
finalization
  IdentifierListItemMemManager.Free;
  IdentifierListItemMemManager:=nil;

end.

