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
    TSourceChangeCache manages write operations to a Cleaned Code. A Cleaned
    Code is the product of a TLinkScanner and is a scanned source code with
    include files and erased non reachable code (e.g. compiler directives).
    TSourceChangeCache caches these operations, and can apply these changes all
    at once. It also supports gaps. A gap can be none, a space char, a new line
    or two new lines.
    The TBeautifyCodeOptions can beautify procedure heads and single statements.
    
  ToDo:
    - BeautifyStatement: support for line ends in dirty code
    - Beautify whole unit/ program
}
unit SourceChanger;

{$ifdef fpc}{$mode objfpc}{$endif}{$H+}

interface

{off $DEFINE CTDEBUG}

uses
  Classes, SysUtils, FileProcs, CodeToolsStrConsts, CodeCache, BasicCodeTools,
  LinkScanner, AVL_Tree, CodeBeautifier, KeywordFuncLists;
  
type
  // Insert policy types for class parts (properties, variables, method defs)
  TClassPartInsertPolicy = (
    cpipAlphabetically,
    cpipLast            // as last sibling
    );
    
  // Insert policy for method bodies (begin..end of methods, not procs)
  TMethodInsertPolicy = (
    mipAlphabetically,
    mipLast,           // behind all existing methods of the same class
    mipClassOrder      // try to copy the order of the class
    );
    
  TForwardProcBodyInsertPolicy = (
    fpipLast,
    fpipInFrontOfMethods,
    fpipBehindMethods
    );

  // where to add new units to a uses section
  TUsesInsertPolicy = (
    uipFirst,
    uipInFrontOfRelated, // related = shortest relative file path (#directory changes)
    uipBehindRelated,
    uipLast,
    uipAlphabetically
    );

  TWordPolicy = (wpNone, wpLowerCase, wpUpperCase, wpLowerCaseFirstLetterUp);
  TAtomType = (atNone, atKeyword, atIdentifier, atColon, atSemicolon, atComma,
               atPoint, atAt, atNumber, atStringConstant, atNewLine,
               atSpace, atCommentStart, atDirectiveStart, atCommentEnd,
               atSymbol, atBracket);
  TAtomTypes = set of TAtomType;
  
  TBeautifyCodeFlag = (
    bcfNoIndentOnBreakLine,
    bcfDoNotIndentFirstLine,
    bcfIndentExistingLineBreaks
    );
  TBeautifyCodeFlags = set of TBeautifyCodeFlag;

const
  DefaultUsesInsertPolicy = uipBehindRelated;

type
  { TBeautifyCodeOptions }

  TBeautifyCodeOptions = class(TPersistent)
  private
    CurLineLen: integer;
    LastSplitPos: integer; // last position where splitting is allowed
    LastSrcLineStart: integer;// last line start, not added by splitting
    CurAtomType, LastAtomType: TAtomType;
    CurPos, AtomStart, AtomEnd, SrcLen, CurIndent, HiddenIndent: integer;
    CommentLvl: integer;
    CommentStartPos: array of integer;
    Src: string;
    procedure AddAtom(var CurCode: string; NewAtom: string);
    procedure ReadNextAtom;
    procedure ReadTilDirectiveEnd;
    procedure StartComment(p: integer);
    procedure EndComment(CommentStart: char; p: integer);
  public
    LineLength: integer;
    LineEnd: string;
    Indent: integer;
    TabWidth: integer;
    KeyWordPolicy: TWordPolicy;
    IdentifierPolicy: TWordPolicy;
    DoNotSplitLineInFront: TAtomTypes;
    DoNotSplitLineAfter: TAtomTypes;
    DoInsertSpaceInFront: TAtomTypes;
    DoInsertSpaceAfter: TAtomTypes;
    DoNotInsertSpaceInFront: TAtomTypes;
    DoNotInsertSpaceAfter: TAtomTypes;
    // procedures
    ForwardProcBodyInsertPolicy: TForwardProcBodyInsertPolicy;
    KeepForwardProcOrder: boolean;
    // classes, methods, properties
    ClassHeaderComments: boolean;
    ClassImplementationComments: boolean;
    ClassPartInsertPolicy: TClassPartInsertPolicy;
    MixMethodsAndProperties: boolean;
    MethodInsertPolicy: TMethodInsertPolicy;
    PropertyReadIdentPrefix: string;
    PropertyWriteIdentPrefix: string;
    PropertyStoredIdentPostfix: string;
    PrivateVariablePrefix: string;
    // uses section
    UsesInsertPolicy: TUsesInsertPolicy;

    CurFlags: TBeautifyCodeFlags;
    
    NestedComments: boolean;

    function BeautifyProc(const AProcCode: string; IndentSize: integer;
        AddBeginEnd: boolean): string;
    function BeautifyStatement(const AStatement: string; IndentSize: integer
        ): string;
    function BeautifyStatementLeftAligned(const AStatement: string;
        IndentSize: integer): string;
    function BeautifyStatement(const AStatement: string; IndentSize: integer;
        BeautifyFlags: TBeautifyCodeFlags): string;
    function AddClassAndNameToProc(const AProcCode, AClassName,
        AMethodName: string): string;
    function BeautifyWord(const AWord: string; WordPolicy: TWordPolicy): string;
    function BeautifyKeyWord(const AWord: string): string;
    function BeautifyIdentifier(const AWord: string): string;
    procedure ConsistencyCheck;
    procedure WriteDebugReport;
    constructor Create;
  end;


  { TSourceChangeCache }

  //----------------------------------------------------------------------------
  // in front of and after a text change can a gap be set.
  // A Gap is for example a space char or a newline. TSourceChangeLog will add
  // the gap if it is not already in the code
  TGapTyp = (gtNone,     // no special gap
             gtSpace,    // at least a single space
             gtNewLine,  // at least a newline
             gtEmptyLine // at least two newlines
             );

  { TSourceChangeCacheEntry }

  TSourceChangeCacheEntry = class
  public
    FrontGap, AfterGap: TGapTyp;
    FromPos, ToPos: integer;
    Text: string;
    DirectCode: TCodeBuffer; // set if change of non cleaned source
    FromDirectPos, ToDirectPos: integer;
    IsDirectChange: boolean;
    constructor Create(aFrontGap, anAfterGap: TGapTyp; aFromPos,
        aToPos: integer; const aText: string; aDirectCode: TCodeBuffer;
        aFromDirectPos, AToDirectPos: integer; aIsDirectChange: boolean);
    function IsDeleteOperation: boolean;
    function IsDeleteOnlyOperation: boolean;
    function IsAtSamePos(AnEntry: TSourceChangeCacheEntry): boolean;
    function CalcMemSize: PtrUint;
  end;
  
  //----------------------------------------------------------------------------
  TOnBeforeApplyChanges = procedure(var Abort: boolean) of object;
  TOnAfterApplyChanges = procedure of object;

  TSourceChangeCache = class
  private
    FMainScanner: TLinkScanner;
    FEntries: TAVLTree; // tree of TSourceChangeCacheEntry
    FBuffersToModify: TFPList; // sorted list of TCodeBuffer
    FBuffersToModifyNeedsUpdate: boolean;
    FMainScannerNeeded: boolean;
    FOnBeforeApplyChanges: TOnBeforeApplyChanges;
    FOnAfterApplyChanges: TOnAfterApplyChanges;
    FUpdateLock: integer;
    Src: string; // current cleaned source
    SrcLen: integer; // same as length(Src)
    procedure DeleteCleanText(CleanFromPos,CleanToPos: integer);
    procedure DeleteDirectText(ACode: TCodeBuffer;
                               DirectFromPos,DirectToPos: integer);
    procedure InsertNewText(ACode: TCodeBuffer; DirectPos: integer;
                            const InsertText: string);
    procedure SetMainScanner(NewScanner: TLinkScanner);
    function GetBuffersToModify(Index: integer): TCodeBuffer;
    procedure UpdateBuffersToModify;
  protected
    procedure RaiseException(const AMessage: string);
  public
    BeautifyCodeOptions: TBeautifyCodeOptions;
    procedure BeginUpdate;
    function EndUpdate: boolean;
    property MainScanner: TLinkScanner read FMainScanner write SetMainScanner;
    property MainScannerNeeded: boolean read FMainScannerNeeded;
    function Replace(FrontGap, AfterGap: TGapTyp; FromPos, ToPos: integer;
                     const Text: string): boolean;
    function ReplaceEx(FrontGap, AfterGap: TGapTyp; FromPos, ToPos: integer;
                   DirectCode: TCodeBuffer; FromDirectPos, ToDirectPos: integer;
                   const Text: string): boolean;
    function Apply: boolean;
    function FindEntryInRange(FromPos, ToPos: integer): TSourceChangeCacheEntry;
    function FindEntryAtPos(APos: integer): TSourceChangeCacheEntry;
    property BuffersToModify[Index: integer]: TCodeBuffer
                                                        read GetBuffersToModify;
    function BuffersToModifyCount: integer;
    function BufferIsModified(ACode: TCodeBuffer): boolean;
    property OnBeforeApplyChanges: TOnBeforeApplyChanges
                         read FOnBeforeApplyChanges write FOnBeforeApplyChanges;
    property OnAfterApplyChanges: TOnAfterApplyChanges
                           read FOnAfterApplyChanges write FOnAfterApplyChanges;
    procedure Clear;
    procedure ConsistencyCheck;
    procedure WriteDebugReport;
    procedure CalcMemSize(Stats: TCTMemStats);
    constructor Create;
    destructor Destroy; override;
  end;
  
  { ESourceChangeCacheError }
  
  ESourceChangeCacheError = class(Exception)
  public
    Sender: TSourceChangeCache;
    constructor Create(ASender: TSourceChangeCache; const AMessage: string);
  end;


const
  AtomTypeNames: array[TAtomType] of shortstring = (
      'None',
      'Keyword',
      'Identifier',
      'Colon',
      'Semicolon',
      'Comma',
      'Point',
      'At',
      'Number',
      'StringConstant',
      'NewLine',
      'Space',
      'CommentStart',
      'DirectiveStart',
      'CommentEnd',
      'Symbol',
      'Bracket'
    );

  WordPolicyNames: array[TWordPolicy] of shortstring = (
      'None', 'LowerCase', 'UpperCase', 'LowerCaseFirstLetterUp'
    );

  ClassPartInsertPolicyNames: array[TClassPartInsertPolicy] of shortstring = (
      'Alphabetically', 'Last'
    );
    
  MethodInsertPolicyNames: array[TMethodInsertPolicy] of shortstring = (
      'Alphabetically', 'Last', 'ClassOrder'
    );
    
  ForwardProcBodyInsertPolicyNames: array[TForwardProcBodyInsertPolicy] of
    shortstring = (
      'Last',
      'InFrontOfMethods',
      'BehindMethods'
    );
    
  UsesInsertPolicyNames: array[TUsesInsertPolicy] of shortstring = (
      'First',
      'InFrontOfRelated',
      'BehindRelated',
      'Last',
      'Alphabetically'
    );

  DefaultDoNotSplitLineInFront: TAtomTypes =
    [atColon,atComma,atSemicolon,atPoint];
  DefaultDoNotSplitLineAfter: TAtomTypes = [atColon,atAt,atPoint,atKeyWord];
  DefaultDoInsertSpaceInFront: TAtomTypes = [];
  DefaultDoInsertSpaceAfter: TAtomTypes = [atColon,atComma,atSemicolon];
  DefaultDoNotInsertSpaceInFront: TAtomTypes = [];
  DefaultDoNotInsertSpaceAfter: TAtomTypes = [atDirectiveStart];

function AtomTypeNameToType(const s: string): TAtomType;
function AtomTypesToStr(const AtomTypes: TAtomTypes): string;
function WordPolicyNameToPolicy(const s: string): TWordPolicy;
function ClassPartPolicyNameToPolicy(const s: string): TClassPartInsertPolicy;
function MethodInsertPolicyNameToPolicy(const s: string): TMethodInsertPolicy;
function ForwardProcBodyInsertPolicyNameToPolicy(
  const s: string): TForwardProcBodyInsertPolicy;
function UsesInsertPolicyNameToPolicy(const s: string): TUsesInsertPolicy;


implementation


function AtomTypeNameToType(const s: string): TAtomType;
begin
  for Result:=Low(TAtomType) to High(TAtomType) do
    if SysUtils.CompareText(AtomTypeNames[Result],s)=0 then exit;
  Result:=atNone;
end;

function AtomTypesToStr(const AtomTypes: TAtomTypes): string;
var
  a: TAtomType;
begin
  Result:='';
  for a:=Low(TAtomType) to High(TAtomType) do begin
    if a in AtomTypes then begin
      if Result<>'' then Result:=Result+',';
      Result:=Result+AtomTypeNames[a];
    end;
  end;
  Result:='['+Result+']';
end;

function WordPolicyNameToPolicy(const s: string): TWordPolicy;
begin
  for Result:=Low(TWordPolicy) to High(TWordPolicy) do
    if SysUtils.CompareText(WordPolicyNames[Result],s)=0 then exit;
  Result:=wpNone;
end;

function ClassPartPolicyNameToPolicy(const s: string): TClassPartInsertPolicy;
begin
  for Result:=Low(TClassPartInsertPolicy) to High(TClassPartInsertPolicy) do
    if SysUtils.CompareText(ClassPartInsertPolicyNames[Result],s)=0 then exit;
  Result:=cpipLast;
end;

function MethodInsertPolicyNameToPolicy(
  const s: string): TMethodInsertPolicy;
begin
  for Result:=Low(TMethodInsertPolicy) to High(TMethodInsertPolicy) do
    if SysUtils.CompareText(MethodInsertPolicyNames[Result],s)=0 then exit;
  Result:=mipLast;
end;

function ForwardProcBodyInsertPolicyNameToPolicy(
  const s: string): TForwardProcBodyInsertPolicy;
begin
  for Result:=Low(TForwardProcBodyInsertPolicy)
  to High(TForwardProcBodyInsertPolicy) do
    if SysUtils.CompareText(ForwardProcBodyInsertPolicyNames[Result],s)=0 then
      exit;
  Result:=fpipBehindMethods;
end;

function UsesInsertPolicyNameToPolicy(const s: string): TUsesInsertPolicy;
begin
  for Result:=Low(TUsesInsertPolicy) to High(TUsesInsertPolicy) do
    if SysUtils.CompareText(UsesInsertPolicyNames[Result],s)=0 then exit;
  Result:=DefaultUsesInsertPolicy;
end;

function CompareSourceChangeCacheEntry(NodeData1, NodeData2: pointer): integer;
var
  Entry1, Entry2: TSourceChangeCacheEntry;
  IsEntry1Delete, IsEntry2Delete: boolean;
begin
  Entry1:=TSourceChangeCacheEntry(NodeData1);
  Entry2:=TSourceChangeCacheEntry(NodeData2);
  if Entry1.FromPos>Entry2.FromPos then
    Result:=1
  else if Entry1.FromPos<Entry2.FromPos then
    Result:=-1
  else begin
    IsEntry1Delete:=Entry1.IsDeleteOperation;
    IsEntry2Delete:=Entry2.IsDeleteOperation;
    if IsEntry1Delete=IsEntry2Delete then begin
      if Entry1.FromDirectPos>Entry2.FromDirectPos then
        Result:=1
      else if Entry1.FromDirectPos<Entry2.FromDirectPos then
        Result:=-1
      else
        Result:=0;
    end else begin
      if IsEntry1Delete then
        Result:=1
      else
        Result:=-1;
    end;
  end;
end;

{ TSourceChangeCacheEntry }

constructor TSourceChangeCacheEntry.Create(aFrontGap, anAfterGap: TGapTyp;
  aFromPos, aToPos: integer; const aText: string; aDirectCode: TCodeBuffer;
  aFromDirectPos, AToDirectPos: integer; aIsDirectChange: boolean);
begin
  inherited Create;
  FrontGap:=aFrontGap;
  AfterGap:=anAfterGap;
  FromPos:=aFromPos;
  ToPos:=aToPos;
  Text:=aText;
  DirectCode:=aDirectCode;
  FromDirectPos:=aFromDirectPos;
  ToDirectPos:=aToDirectPos;
  IsDirectChange:=aIsDirectChange;
end;

function TSourceChangeCacheEntry.IsDeleteOperation: boolean;
begin
  Result:=(ToPos>FromPos)
   or ((DirectCode<>nil) and (FromDirectPos>0) and (ToDirectPos>FromDirectPos));
end;

function TSourceChangeCacheEntry.IsDeleteOnlyOperation: boolean;
begin
  Result:=IsDeleteOperation and (Text='');
end;

function TSourceChangeCacheEntry.IsAtSamePos(AnEntry: TSourceChangeCacheEntry
  ): boolean;
begin
  Result:=(FromPos=AnEntry.FromPos) and (FromDirectPos=AnEntry.FromDirectPos);
end;

function TSourceChangeCacheEntry.CalcMemSize: PtrUint;
begin
  Result:=PtrUInt(InstanceSize)
    +MemSizeString(Text);
end;


{ TSourceChangeCache }

constructor TSourceChangeCache.Create;
begin
  inherited Create;
  FEntries:=TAVLTree.Create(@CompareSourceChangeCacheEntry);
  MainScanner:=nil;
  FBuffersToModify:=TFPList.Create;
  FBuffersToModifyNeedsUpdate:=false;
  BeautifyCodeOptions:=TBeautifyCodeOptions.Create;
end;

destructor TSourceChangeCache.Destroy;
begin
  Clear;
  BeautifyCodeOptions.Free;
  FBuffersToModify.Free;
  FEntries.FreeAndClear;
  FreeAndNil(FEntries);
  inherited Destroy;
end;

function TSourceChangeCache.FindEntryInRange(
  FromPos, ToPos: integer): TSourceChangeCacheEntry;
var ANode: TAVLTreeNode;
  NextNode: TAVLTreeNode;
begin
  ANode:=FEntries.Root;
  // find nearest node to FromPos
  while ANode<>nil do begin
    Result:=TSourceChangeCacheEntry(ANode.Data);
    if FromPos<=Result.FromPos then
      NextNode:=ANode.Left
    else
      NextNode:=ANode.Right;
    if NextNode=nil then begin
      // ANode is now one behind or at the first candidate
      NextNode:=FEntries.FindPrecessor(ANode);
      if NextNode<>nil then begin
        ANode:=NextNode;
        Result:=TSourceChangeCacheEntry(ANode.Data);
      end;
      while (Result.FromPos<ToPos) do begin
        if (Result.FromPos<Result.ToPos) // entry has a range (is a delete operation)
        and (Result.FromPos<ToPos)
        and (Result.ToPos>FromPos) then begin
          // entry intersects range
          exit;
        end;
        ANode:=FEntries.FindSuccessor(ANode);
        if ANode=nil then begin
          Result:=nil;
          exit;
        end;
        Result:=TSourceChangeCacheEntry(ANode.Data);
      end;
      // not found
      break;
    end;
    ANode:=NextNode;
  end;
  Result:=nil;
end;

function TSourceChangeCache.FindEntryAtPos(
  APos: integer): TSourceChangeCacheEntry;
begin
  Result:=FindEntryInRange(APos,APos);
end;

function TSourceChangeCache.ReplaceEx(FrontGap, AfterGap: TGapTyp;
  FromPos, ToPos: integer;
  DirectCode: TCodeBuffer; FromDirectPos, ToDirectPos: integer;
  const Text: string): boolean;
  
  procedure RaiseDataInvalid;
  begin
    if (MainScanner=nil) then
      RaiseException('TSourceChangeCache.ReplaceEx MainScanner=nil');
    if FromPos>ToPos then
      RaiseException('TSourceChangeCache.ReplaceEx FromPos>ToPos');
    if FromPos<1 then
      RaiseException('TSourceChangeCache.ReplaceEx FromPos<1');
    if (MainScanner<>nil) and (ToPos>MainScanner.CleanedLen+1) then
      RaiseException('TSourceChangeCache.ReplaceEx ToPos>MainScanner.CleanedLen+1');
  end;
  
  procedure RaiseIntersectionFound;
  begin
    RaiseException('TSourceChangeCache.ReplaceEx '
      +'IGNORED, because intersection found');
  end;
  
  procedure RaiseCodeReadOnly(Buffer: TCodeBuffer);
  begin
    RaiseException(ctsfileIsReadOnly+' '+Buffer.Filename);
  end;
  
  procedure RaiseNotInCleanCode;
  begin
    RaiseException('TSourceChangeCache.ReplaceEx not in clean code');
  end;
  
var
  NewEntry: TSourceChangeCacheEntry;
  p: pointer;
  IsDirectChange: boolean;
  IntersectionEntry: TSourceChangeCacheEntry;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TSourceChangeCache.ReplaceEx FrontGap=',dbgs(ord(FrontGap)),
  ' AfterGap=',dbgs(ord(AfterGap)),' CleanPos=',dbgs(FromPos),'-',dbgs(ToPos),
  ' Text="',Text,'"');
  if DirectCode<>nil then
    DebugLn('DirectCode=',DirectCode.Filename,' DirectPos=',dbgs(FromDirectPos),'-',dbgs(ToDirectPos));
  {$ENDIF}
  Result:=false;
  IsDirectChange:=DirectCode<>nil;
  if not IsDirectChange then begin
    if (Text='') and (FromPos=ToPos) then begin
      {$IFDEF CTDEBUG}
      DebugLn('TSourceChangeCache.ReplaceEx SUCCESS NoOperation');
      {$ENDIF}
      Result:=true;
      exit;
    end;
    if (MainScanner=nil)
    or (FromPos>ToPos) or (FromPos<1)
    or (ToPos>MainScanner.CleanedLen+1) then
    begin
      {$IFDEF CTDEBUG}
      DebugLn('TSourceChangeCache.ReplaceEx IGNORED, because data invalid');
      {$ENDIF}
      RaiseDataInvalid;
      exit;
    end;
  end else begin
    // direct code change without MainScanner
    if (Text='') and (FromDirectPos=ToDirectPos) then begin
      {$IFDEF CTDEBUG}
      DebugLn('TSourceChangeCache.ReplaceEx SUCCESS NoOperation');
      {$ENDIF}
    end;
  end;
  IntersectionEntry:=FindEntryInRange(FromPos,ToPos);
  if IntersectionEntry<>nil then begin
    {$IFDEF CTDEBUG}
    DebugLn('TSourceChangeCache.ReplaceEx IGNORED, because intersection found: ',
      dbgs(IntersectionEntry.FromPos),'-',dbgs(IntersectionEntry.ToPos),
      ' IsDelete=',dbgs(IntersectionEntry.IsDeleteOperation));
    {$ENDIF}
    RaiseIntersectionFound;
    exit;
  end;

  if IsDirectChange and (FromDirectPos<ToDirectPos) then begin
    // this is a direct replace/delete operation
    // -> check if the DirectCode is writable
    if DirectCode.ReadOnly then
      RaiseCodeReadOnly(DirectCode);
  end else if FromPos<ToPos then begin
    // this is a replace/delete operation (in cleaned code)
    // -> check the whole range for writable buffers
    if not MainScanner.WholeRangeIsWritable(FromPos,ToPos,true) then exit;
  end;
  if not IsDirectChange then begin
    if not MainScanner.CleanedPosToCursor(FromPos,FromDirectPos,p) then begin
      {$IFDEF CTDEBUG}
      DebugLn('TSourceChangeCache.ReplaceEx IGNORED, because not in clean pos');
      {$ENDIF}
      RaiseNotInCleanCode;
      exit;
    end;
    DirectCode:=TCodeBuffer(p);
    ToDirectPos:=0;
  end;
  // add entry
  NewEntry:=TSourceChangeCacheEntry.Create(FrontGap,AfterGap,FromPos,ToPos,
                      Text,DirectCode,FromDirectPos,ToDirectPos,IsDirectChange);
  FEntries.Add(NewEntry);
  if not IsDirectChange then
    FMainScannerNeeded:=true;
  FBuffersToModifyNeedsUpdate:=true;
  Result:=true;
  {$IFDEF CTDEBUG}
  DebugLn('TSourceChangeCache.ReplaceEx SUCCESS IsDelete=',dbgs(NewEntry.IsDeleteOperation));
  {$ENDIF}
end;

function TSourceChangeCache.Replace(FrontGap, AfterGap: TGapTyp;
  FromPos, ToPos: integer; const Text: string): boolean;
begin
  Result:=ReplaceEx(FrontGap,AfterGap,FromPos,ToPos,nil,0,0,Text);
end;

procedure TSourceChangeCache.Clear;
begin
  FUpdateLock:=0;
  FEntries.FreeAndClear;
  FMainScannerNeeded:=false;
  FBuffersToModify.Clear;
  FBuffersToModifyNeedsUpdate:=true;
end;

procedure TSourceChangeCache.ConsistencyCheck;
var
  CurResult: LongInt;
begin
  CurResult:=FEntries.ConsistencyCheck;
  if CurResult<>0 then
    RaiseCatchableException(IntToStr(CurResult));
  BeautifyCodeOptions.ConsistencyCheck;
end;

procedure TSourceChangeCache.WriteDebugReport;
begin
  DebugLn('[TSourceChangeCache.WriteDebugReport]');
  DebugLn(FEntries.ReportAsString);
  BeautifyCodeOptions.WriteDebugReport;
  ConsistencyCheck;
end;

procedure TSourceChangeCache.CalcMemSize(Stats: TCTMemStats);
var
  Node: TAVLTreeNode;
  m: PtrUInt;
begin
  Stats.Add('TSourceChangeCache',PtrUInt(InstanceSize)
    +PtrUInt(FBuffersToModify.InstanceSize)
    +PtrUInt(FBuffersToModify.Capacity)*SizeOf(Pointer));
  m:=0;
  Node:=FEntries.FindLowest;
  while Node<>nil do begin
    inc(m,TSourceChangeCacheEntry(Node.Data).CalcMemSize);
    Node:=FEntries.FindSuccessor(Node);
  end;
  Stats.Add('TSourceChangeCache.FEntries',m);
  // Note: Src is owned by the TLinkScanner
end;

function TSourceChangeCache.Apply: boolean;
var
  FromPosAdjustment: integer;
  InsertText: string;

  procedure AddAfterGap(EntryNode: TAVLTreeNode);
  var
    ToPos, ToSrcLen: integer;
    ToSrc: string;
    NeededLineEnds, NeededIndent, i, j: integer;
    AfterGap: TGapTyp;
    AnEntry, PrecEntry: TSourceChangeCacheEntry;
    PrecNode: TAVLTreeNode;
  begin
    AnEntry:=TSourceChangeCacheEntry(EntryNode.Data);
    if not AnEntry.IsDirectChange then begin
      ToPos:=AnEntry.ToPos;
      ToSrc:=Src;
    end else begin
      ToPos:=AnEntry.ToDirectPos;
      ToSrc:=AnEntry.DirectCode.Source;
    end;
    AfterGap:=AnEntry.AfterGap;
    if AnEntry.IsDeleteOnlyOperation then begin
      PrecNode:=FEntries.FindPrecessor(EntryNode);
      if PrecNode<>nil then begin
        PrecEntry:=TSourceChangeCacheEntry(PrecNode.Data);
        if PrecEntry.IsAtSamePos(AnEntry) then begin
          AfterGap:=PrecEntry.AfterGap;
        end;
      end;
    end;
    case AfterGap of
      gtSpace:
        begin
          if ((ToPos>length(ToSrc))
              or (not IsSpaceChar[ToSrc[ToPos]])) then
            InsertText:=InsertText+' ';
        end;
      gtNewLine:
        begin
          NeededLineEnds:=CountNeededLineEndsToAddForward(ToSrc,ToPos,1);
          if NeededLineEnds>0 then
            InsertText:=InsertText+BeautifyCodeOptions.LineEnd;
        end;
      gtEmptyLine:
        begin
          NeededLineEnds:=CountNeededLineEndsToAddForward(ToSrc,ToPos,2);
          for i:=1 to NeededLineEnds do
            InsertText:=InsertText+BeautifyCodeOptions.LineEnd;
        end;
    end;
    if AnEntry.AfterGap in [gtNewLine,gtEmptyLine] then begin
      // move the rest of the line behind the insert position to the next line
      // with auto indent
      NeededIndent:=GetLineIndent(ToSrc,ToPos);
      j:=ToPos;
      ToSrcLen:=length(ToSrc);
      while (j<=ToSrcLen) and (IsSpaceChar[ToSrc[j]]) do
        inc(j);
      dec(NeededIndent,j-ToPos);
      if NeededIndent>0 then
        InsertText:=InsertText+GetIndentStr(NeededIndent);
    end;
  end;
  
  procedure AddFrontGap(AnEntry: TSourceChangeCacheEntry);
  var
    NeededLineEnds: integer;
    FromPos: integer;
    FromSrc: string;
    i: integer;
  begin
    if not AnEntry.IsDirectChange then begin
      FromPos:=AnEntry.FromPos;
      FromSrc:=Src;
    end else begin
      FromPos:=AnEntry.FromDirectPos;
      FromSrc:=AnEntry.DirectCode.Source;
    end;
    NeededLineEnds:=0;
    case AnEntry.FrontGap of
      gtSpace:
        begin
          if (FromPos<=1)
          or (not IsSpaceChar[FromSrc[FromPos-1]]) then
            InsertText:=' '+InsertText;
        end;
      gtNewLine:
        begin
          if FromPos>1 then
            NeededLineEnds:=1
          else
            NeededLineEnds:=0;
          NeededLineEnds:=CountNeededLineEndsToAddBackward(FromSrc,FromPos-1,
                                                           NeededLineEnds);
          if NeededLineEnds>0 then
            InsertText:=BeautifyCodeOptions.LineEnd+InsertText;
        end;
      gtEmptyLine:
        begin
          if FromPos>1 then
            NeededLineEnds:=2
          else
            NeededLineEnds:=1;
          NeededLineEnds:=CountNeededLineEndsToAddBackward(FromSrc,FromPos-1,
                                                           NeededLineEnds);
          for i:=1 to NeededLineEnds do
            InsertText:=BeautifyCodeOptions.LineEnd+InsertText;
        end;
    end;
    FromPosAdjustment:=0;
    if (AnEntry.FrontGap in [gtNewLine,gtEmptyLine]) and (NeededLineEnds=0)
    then begin
      // no line end was inserted in front
      // -> adjust the FromPos to replace the space in the existing line
      while (FromPos+FromPosAdjustment>1)
      and (not (FromSrc[FromPos+FromPosAdjustment-1]
        in [#10,#13]))
      do dec(FromPosAdjustment);
    end;
  end;
  
var
  CurNode, PrecNode: TAVLTreeNode;
  CurEntry, PrecEntry, FirstEntry: TSourceChangeCacheEntry;
  BetweenGap: TGapTyp;
  Abort: boolean;
begin
  {$IFDEF CTDEBUG}
  DebugLn('TSourceChangeCache.Apply EntryCount=',dbgs(FEntries.Count));
  {$ENDIF}
  Result:=false;
  if MainScannerNeeded and (MainScanner=nil) then
    RaiseCatchableException('TSourceChangeCache.Apply');
  if FUpdateLock>0 then begin
    Result:=true;
    exit;
  end;
  if FEntries.Count=0 then begin
    Result:=true;
    exit;
  end;
  if Assigned(FOnBeforeApplyChanges) then begin
    Abort:=false;
    FOnBeforeApplyChanges(Abort);
    if Abort then begin
      Clear;
      exit;
    end;
  end;
  try
    if MainScanner<>nil then
      Src:=MainScanner.CleanedSrc
    else
      Src:='';
    SrcLen:=length(Src);
    // apply the changes beginning with the last
    CurNode:=FEntries.FindHighest;
    while CurNode<>nil do begin
      FirstEntry:=TSourceChangeCacheEntry(CurNode.Data);
      {$IFDEF CTDEBUG}
      DebugLn('TSourceChangeCache.Apply Pos=',dbgs(FirstEntry.FromPos),'-',dbgs(FirstEntry.ToPos),
      ' Text="',FirstEntry.Text,'"');
      {$ENDIF}
      InsertText:=FirstEntry.Text;
      // add after gap
      AddAfterGap(CurNode);
      // add text from every node inserted at the same position
      PrecNode:=FEntries.FindPrecessor(CurNode);
      CurEntry:=FirstEntry;
      while (PrecNode<>nil) do begin
        PrecEntry:=TSourceChangeCacheEntry(PrecNode.Data);
        if PrecEntry.IsAtSamePos(CurEntry) then begin
          {$IFDEF CTDEBUG}
          DebugLn('TSourceChangeCache.Apply EntryAtSamePos Pos=',dbgs(PrecEntry.FromPos),'-',dbgs(PrecEntry.ToPos),
          ' InsertText="',InsertText,'"');
          {$ENDIF}
          BetweenGap:=PrecEntry.AfterGap;
          if ord(BetweenGap)<ord(CurEntry.FrontGap) then
            BetweenGap:=CurEntry.FrontGap;
          if not CurEntry.IsDeleteOnlyOperation then begin
            case BetweenGap of
              gtSpace:
                InsertText:=' '+InsertText;
              gtNewLine:
                InsertText:=BeautifyCodeOptions.LineEnd+InsertText;
              gtEmptyLine:
                InsertText:=BeautifyCodeOptions.LineEnd
                              +BeautifyCodeOptions.LineEnd+InsertText;
            end;
          end else begin
            // the behind operation is a delete only operation
            // (Note: With the after gap, it is possible to insert text anyway)
          end;
          InsertText:=PrecEntry.Text+InsertText;
        end else
          break;
        CurNode:=PrecNode;
        CurEntry:=PrecEntry;
        PrecNode:=FEntries.FindPrecessor(CurNode);
      end;
      // add front gap
      AddFrontGap(CurEntry);
      // delete old text in code buffers
      if not FirstEntry.IsDirectChange then
        DeleteCleanText(FirstEntry.FromPos+FromPosAdjustment,FirstEntry.ToPos)
      else
        DeleteDirectText(FirstEntry.DirectCode,
                         FirstEntry.FromDirectPos+FromPosAdjustment,
                         FirstEntry.ToDirectPos);
      // insert new text
      InsertNewText(FirstEntry.DirectCode,
                    FirstEntry.FromDirectPos+FromPosAdjustment,InsertText);
      CurNode:=PrecNode;
    end;
  finally
    if Assigned(FOnAfterApplyChanges) then FOnAfterApplyChanges();
    FEntries.FreeAndClear;
  end;
  Result:=true;
end;

procedure TSourceChangeCache.DeleteCleanText(CleanFromPos,CleanToPos: integer);
begin
  {$IFDEF CTDEBUG}
  DebugLn('[TSourceChangeCache.DeleteCleanText] Pos=',dbgs(CleanFromPos),'-',dbgs(CleanToPos));
  {$ENDIF}
  if CleanFromPos=CleanToPos then exit;
  MainScanner.DeleteRange(CleanFromPos,CleanToPos);
end;

procedure TSourceChangeCache.DeleteDirectText(ACode: TCodeBuffer; DirectFromPos,
  DirectToPos: integer);
begin
  {$IFDEF CTDEBUG}
  DebugLn('[TSourceChangeCache.DeleteDirectText] Code=',ACode.Filename,
  ' Pos=',dbgs(DirectFromPos),'-',dbgs(DirectToPos));
  {$ENDIF}
  if DirectFromPos=DirectToPos then exit;
  ACode.Delete(DirectFromPos,DirectToPos-DirectFromPos);
end;

procedure TSourceChangeCache.InsertNewText(ACode: TCodeBuffer;
  DirectPos: integer; const InsertText: string);
begin
  {$IFDEF CTDEBUG}
  DebugLn('[TSourceChangeCache.InsertNewText] Code=',ACode.Filename,
  ' Pos=',dbgs(DirectPos),' Text="',InsertText,'"');
  {$ENDIF}
  if InsertText='' then exit;
  ACode.Insert(DirectPos,InsertText);
end;

procedure TSourceChangeCache.BeginUpdate;
begin
  inc(FUpdateLock);
end;

function TSourceChangeCache.EndUpdate: boolean;
begin
  Result:=true;
  if FUpdateLock<=0 then exit;
  dec(FUpdateLock);
  if FUpdateLock<=0 then
    Result:=Apply;
end;
    
procedure TSourceChangeCache.SetMainScanner(NewScanner: TLinkScanner);
begin
  if NewScanner=FMainScanner then exit;
  Clear;
  FMainScanner:=NewScanner;
end;

function TSourceChangeCache.GetBuffersToModify(Index: integer): TCodeBuffer;
begin
  UpdateBuffersToModify;
  Result:=TCodeBuffer(FBuffersToModify[Index]);
end;

function TSourceChangeCache.BuffersToModifyCount: integer;
begin
  UpdateBuffersToModify;
  Result:=FBuffersToModify.Count;
end;

function TSourceChangeCache.BufferIsModified(ACode: TCodeBuffer): boolean;
begin
  UpdateBuffersToModify;
  Result:=IndexOfCodeInUniqueList(ACode,FBuffersToModify)>=0;
end;

procedure TSourceChangeCache.UpdateBuffersToModify;
// build a sorted and unique list of all TCodeBuffer(s) which will be modified
// by the 'Apply' operation
var ANode: TAVLTreeNode;
  AnEntry: TSourceChangeCacheEntry;
begin
  if not FBuffersToModifyNeedsUpdate then exit;
  //DebugLn('[TSourceChangeCache.UpdateBuffersToModify]');
  FBuffersToModify.Clear;
  ANode:=FEntries.FindLowest;
  while ANode<>nil do begin
    AnEntry:=TSourceChangeCacheEntry(ANode.Data);
    if AnEntry.IsDirectChange then begin
      if AnEntry.DirectCode=nil then
        RaiseException('TSourceChangeCache.UpdateBuffersToModify AnEntry.DirectCode=nil');
      if FBuffersToModify.IndexOf(AnEntry.DirectCode)<0 then
        FBuffersToModify.Add(AnEntry.DirectCode)
    end else
      MainScanner.FindCodeInRange(AnEntry.FromPos,AnEntry.ToPos,
                                  FBuffersToModify);
    ANode:=FEntries.FindSuccessor(ANode);
  end;
  FBuffersToModifyNeedsUpdate:=false;
end;

procedure TSourceChangeCache.RaiseException(const AMessage: string);
begin
  raise ESourceChangeCacheError.Create(Self,AMessage);
end;

{ TBeautifyCodeOptions }

constructor TBeautifyCodeOptions.Create;
begin
  LineLength:=80;
  LineEnd:=System.LineEnding;
  Indent:=2;
  TabWidth:=8;
  ClassPartInsertPolicy:=cpipLast;
  MixMethodsAndProperties:=false;
  MethodInsertPolicy:=mipClassOrder;
  ForwardProcBodyInsertPolicy:=fpipBehindMethods;
  KeepForwardProcOrder:=true;
  ClassHeaderComments:=true;
  KeyWordPolicy:=wpLowerCase;
  IdentifierPolicy:=wpNone;
  DoNotSplitLineInFront:=DefaultDoNotSplitLineInFront;
  DoNotSplitLineAfter:=DefaultDoNotSplitLineAfter;
  DoInsertSpaceInFront:=DefaultDoInsertSpaceInFront;
  DoInsertSpaceAfter:=DefaultDoInsertSpaceAfter;
  DoNotInsertSpaceInFront:=DefaultDoNotInsertSpaceInFront;
  DoNotInsertSpaceAfter:=DefaultDoNotInsertSpaceAfter;
  PropertyReadIdentPrefix:='Get';
  PropertyWriteIdentPrefix:='Set';
  PropertyStoredIdentPostfix:='IsStored';
  PrivateVariablePrefix:='f';
  UsesInsertPolicy:=DefaultUsesInsertPolicy;
  
  NestedComments:=true;
end;

procedure TBeautifyCodeOptions.AddAtom(var CurCode: string; NewAtom: string);
var
  RestLineLen, LastLineEndInAtom: integer;
  BreakPos: Integer;
  IndentLen: Integer;
begin
  if NewAtom='' then exit;
  //DebugLn('[TBeautifyCodeOptions.AddAtom]  NewAtom=',NewAtom,' s="',s,'"');

  // beautify identifier
  if IsIdentStartChar[NewAtom[1]]
  and (CommentLvl = 0) then begin
    if AllKeyWords.DoItCaseInsensitive(NewAtom) then
      NewAtom:=BeautifyWord(NewAtom,KeyWordPolicy)
    else
      NewAtom:=BeautifyWord(NewAtom,IdentifierPolicy);
  end;
  
  // indent existing line break
  if bcfIndentExistingLineBreaks in CurFlags then begin
    BreakPos:=1;
    while (BreakPos<=length(NewAtom)) do begin
      if NewAtom[BreakPos] in [#10,#13] then begin
        inc(BreakPos);
        if (BreakPos<=length(NewAtom)) and (NewAtom[BreakPos] in [#10,#13])
        and (NewAtom[BreakPos]<>NewAtom[BreakPos-1]) then
          inc(BreakPos);
        IndentLen:=GetLineIndent(CurCode,LastSrcLineStart)+HiddenIndent;
        NewAtom:=copy(NewAtom,1,BreakPos-1)
                +GetIndentStr(IndentLen)
                +copy(NewAtom,BreakPos,length(NewAtom)-BreakPos);
        inc(BreakPos,IndentLen);
        HiddenIndent:=0;
      end else
        inc(BreakPos);
    end;
  end;

  // split long string constants
  if NewAtom[1] in ['''','#'] then
    NewAtom:=SplitStringConstant(NewAtom,LineLength-CurLineLen,LineLength,
                                 Indent+GetLineIndent(CurCode,LastSrcLineStart),
                                 LineEnd);
  
  // find last line end in atom
  LastLineEndInAtom:=length(NewAtom);
  while (LastLineEndInAtom>=1) do begin
    if (not (NewAtom[LastLineEndInAtom] in [#10,#13])) then
      dec(LastLineEndInAtom)
    else
      break;
  end;

  // start new line if necessary
  if (LastLineEndInAtom<1) and (CurLineLen+length(NewAtom)>LineLength)
  and (LastSplitPos>1) then begin
    // new atom does not fit into the line and there is a split position
    // -> split line
    //DebugLn(['[TBeautifyCodeOptions.AddAtom]  NEW LINE CurLineLen=',CurLineLen,' NewAtom=',NewAtom,' "',copy(CurCode,LastSplitPos,5),'" LineLength=',LineLength]);
    RestLineLen:=length(CurCode)-LastSplitPos+1;
    IndentLen:=Indent+GetLineIndent(CurCode,LastSrcLineStart)+HiddenIndent;
    CurCode:=copy(CurCode,1,LastSplitPos-1)+LineEnd
             +GetIndentStr(IndentLen)
             +copy(CurCode,LastSplitPos,RestLineLen)+NewAtom;
    HiddenIndent:=0;
    CurLineLen:=length(CurCode)-LastSplitPos-length(LineEnd)+1;
    LastSplitPos:=-1;
  end else begin
    CurCode:=CurCode+NewAtom;
    if LastLineEndInAtom<1 then begin
      inc(CurLineLen,length(NewAtom));
    end else begin
      // there is a line end in the code
      CurLineLen:=length(NewAtom)-LastLineEndInAtom;
      LastSrcLineStart:=length(CurCode)+1-CurLineLen;
      HiddenIndent:=0;
    end;
  end;
  //debugln(['TBeautifyCodeOptions.AddAtom ',dbgstr(CurCode),' ',CurLineLen]);
end;

procedure TBeautifyCodeOptions.ReadNextAtom;
var c1, c2: char;
begin
  AtomStart:=CurPos;
  if AtomStart<=SrcLen then begin
    c1:=Src[CurPos];
    case c1 of
      'a'..'z','A'..'Z','_': // identifier
        begin
          CurAtomType:=atIdentifier;
          repeat
            inc(CurPos);
          until (CurPos>SrcLen) or (not IsIdentChar[Src[CurPos]]);
          if WordIsKeyWord.DoItCaseInsensitive(Src,AtomStart,CurPos-AtomStart)
          then
            CurAtomType:=atKeyword;
        end;
      #10,#13: // line break
        begin
          EndComment('/',CurPos);
          CurAtomType:=atNewLine;
          inc(CurPos);
          if (CurPos<=SrcLen) and (IsLineEndChar[Src[CurPos]])
          and (Src[CurPos]<>c1) then
            inc(CurPos);
        end;
      #0..#9,#11..#12,#14..#32: // special char
        begin
          CurAtomType:=atSpace;
          repeat
            inc(CurPos);
          until (CurPos>SrcLen) or (not IsSpaceChar[Src[CurPos]]);
        end;
      '0'..'9': // decimal number
        begin
          CurAtomType:=atNumber;
          repeat
            inc(CurPos);
          until (CurPos>SrcLen) or (not IsNumberChar[Src[CurPos]]);
          if (CurPos<SrcLen)
          and (Src[CurPos]='.') and (Src[CurPos+1]<>'.') then
          begin
            // real type number
            inc(CurPos);
            while (CurPos<=SrcLen) and (IsNumberChar[Src[CurPos]])
            do
              inc(CurPos);
            if (CurPos<=SrcLen) and (Src[CurPos] in ['e','E'])
            then begin
              // read exponent
              inc(CurPos);
              if (CurPos<=SrcLen) and (Src[CurPos] in ['-','+'])
              then inc(CurPos);
              while (CurPos<=SrcLen) and (IsNumberChar[Src[CurPos]])
              do
                inc(CurPos);
            end;
          end;
        end;
      '''','#': // string constant
        if CommentLvl=0 then begin
          CurAtomType:=atStringConstant;
          while (CurPos<=SrcLen) do begin
            case (Src[CurPos]) of
            '#':
              begin
                inc(CurPos);
                while (CurPos<=SrcLen)
                and (IsNumberChar[Src[CurPos]]) do
                  inc(CurPos);
              end;
            '''':
              begin
                inc(CurPos);
                while (CurPos<=SrcLen)
                and (Src[CurPos]<>'''') do
                  inc(CurPos);
                inc(CurPos);
              end;
            else
              break;
            end;
          end;
        end else begin
          // normal character
          inc(CurPos);
          CurAtomType:=atSymbol;
        end;
      '%': // binary number
        begin
          CurAtomType:=atNumber;
          repeat
            inc(CurPos);
          until (CurPos>SrcLen) or (not (Src[CurPos] in ['0','1']));
        end;
      '$': // hex number
        begin
          CurAtomType:=atNumber;
          repeat
            inc(CurPos);
          until (CurPos>SrcLen) or (not IsHexNumberChar[Src[CurPos]]);
        end;
      '{': // curly bracket comment or directive
        begin
          StartComment(CurPos);
          inc(CurPos);
          if (CurPos<=SrcLen) and (Src[CurPos]='$') then begin
            inc(CurPos);
            CurAtomType:=atDirectiveStart;
            while (CurPos<=SrcLen) and (IsIdentChar[Src[CurPos]]) do
              inc(CurPos);
            if (CurPos<=SrcLen) and (Src[CurPos] in ['+','-']) then
              inc(CurPos);
          end else begin
            CurAtomType:=atCommentStart;
          end;
        end;
      '}': // curly bracket comment end
        begin
          EndComment('{',CurPos);
          inc(CurPos);
          CurAtomType:=atCommentEnd;
        end;
      '(': // (* comment or directive
        begin
          inc(CurPos);
          if (CurPos<=SrcLen) and (Src[CurPos]='*') then begin
            StartComment(CurPos-1);
            inc(CurPos);
            if (CurPos<=SrcLen) and (Src[CurPos]='$') then begin
              inc(CurPos);
              CurAtomType:=atDirectiveStart;
              while (CurPos<=SrcLen) and (IsIdentChar[Src[CurPos]]) do
                inc(CurPos);
              if (CurPos<=SrcLen) and (Src[CurPos] in ['+','-']) then
                inc(CurPos);
            end else begin
              CurAtomType:=atCommentStart;
            end;
          end else begin
            CurAtomType:=atBracket;
          end;
        end;
      '[', ']', ')':
        begin
          inc(CurPos);
          CurAtomType:=atBracket;
        end;
      '*': // *) comment end
        begin
          inc(CurPos);
          if (CurPos<=SrcLen) and (Src[CurPos]=')') then begin
            EndComment('(',CurPos-1);
            inc(CurPos);
            CurAtomType:=atCommentEnd;
          end else begin
            CurAtomType:=atSymbol;
          end;
        end;
      '/': // line comment or directive
        begin
          inc(CurPos);
          if (CurPos<=SrcLen) and (Src[CurPos]='/') then begin
            StartComment(CurPos-1);
            inc(CurPos);
            if (CurPos<=SrcLen) and (Src[CurPos]='$') then begin
              inc(CurPos);
              CurAtomType:=atDirectiveStart;
              while (CurPos<=SrcLen) and (IsIdentChar[Src[CurPos]]) do
                inc(CurPos);
              if (CurPos<=SrcLen) and (Src[CurPos] in ['+','-']) then
                inc(CurPos);
            end else begin
              CurAtomType:=atCommentStart;
            end;
          end else begin
            CurAtomType:=atSymbol;
          end;
        end;
      else
        begin
          CurAtomType:=atSymbol;
          inc(CurPos);
          if (CurPos<=SrcLen) then begin
            c2:=Src[CurPos];
            // test for double char operators
            // :=, +=, -=, /=, *=, <>, <=, >=, **, ><
            if ((c2='=') and  (IsEqualOperatorStartChar[c1]))
            or ((c1='<') and (c2='>'))
            or ((c1='>') and (c2='<'))
            or ((c1='.') and (c2='.'))
            or ((c1='*') and (c2='*'))
            then
              inc(CurPos);
          end;
          if AtomStart+1=CurPos then
            case c1 of
              '.': CurAtomType:=atPoint;
              ',': CurAtomType:=atComma;
              ':': CurAtomType:=atColon;
              ';': CurAtomType:=atSemicolon;
              '@': CurAtomType:=atAt;
            end;
        end;
    end;
  end else
    CurAtomType:=atNone;
  AtomEnd:=CurPos;
end;

procedure TBeautifyCodeOptions.ReadTilDirectiveEnd;
var
  Lvl: Integer;
begin
  Lvl:=1;
  repeat
    ReadNextAtom;
    if (CurAtomType in [atCommentStart,atDirectiveStart])
    and NestedComments then
      inc(Lvl)
    else if CurAtomType=atCommentEnd then begin
      dec(Lvl);
      if Lvl=0 then break;
    end;
  until CurAtomType=atNone;
end;

procedure TBeautifyCodeOptions.StartComment(p: integer);
begin
  inc(CommentLvl);
  if length(CommentStartPos)<CommentLvl then
    SetLength(CommentStartPos,length(CommentStartPos)*2+10);
  CommentStartPos[CommentLvl-1]:=p;
end;

procedure TBeautifyCodeOptions.EndComment(CommentStart: char; p: integer);
begin
  if (CommentLvl>0)
  and (Src[CommentStartPos[CommentLvl-1]]=CommentStart) then
    dec(CommentLvl);
end;

function TBeautifyCodeOptions.BeautifyProc(const AProcCode: string;
  IndentSize: integer; AddBeginEnd: boolean): string;
begin
  Result:=BeautifyStatement(AProcCode,IndentSize);
  if AddBeginEnd then begin
    AddAtom(Result,LineEnd+GetIndentStr(IndentSize));
    AddAtom(Result,'begin');
    AddAtom(Result,LineEnd+LineEnd+GetIndentStr(IndentSize));
    AddAtom(Result,'end;');
  end;
  {$IFDEF CTDEBUG}
  DebugLn('[TBeautifyCodeOptions.BeautifyProc] Result="',Result,'"');
  {$ENDIF}
end;

function TBeautifyCodeOptions.BeautifyStatement(const AStatement: string;
  IndentSize: integer): string;
begin
  Result:=BeautifyStatement(AStatement,IndentSize,[]);
end;

function TBeautifyCodeOptions.BeautifyStatementLeftAligned(
  const AStatement: string; IndentSize: integer): string;
begin
  Result:=BeautifyStatement(AStatement,IndentSize,[bcfNoIndentOnBreakLine]);
end;

function TBeautifyCodeOptions.BeautifyStatement(const AStatement: string;
  IndentSize: integer; BeautifyFlags: TBeautifyCodeFlags): string;
var CurAtom: string;
  OldIndent: Integer;
  OldAtomStart: LongInt;
begin
  //DebugLn('**********************************************************');
  //DebugLn('[TBeautifyCodeOptions.BeautifyStatement] "',AStatement,'"');
  // set flags
  CurFlags:=BeautifyFlags;
  OldIndent:=Indent;
  try
    if bcfNoIndentOnBreakLine in CurFlags then
      Indent:=0;
    // init
    Src:=AStatement;
    SrcLen:=length(Src);
    if IndentSize>=LineLength-10 then IndentSize:=LineLength-10;
    CurIndent:=IndentSize;
    HiddenIndent:=0;
    if bcfDoNotIndentFirstLine in CurFlags then begin
      Result:='';
      HiddenIndent:=CurIndent;
    end else
      Result:=GetIndentStr(CurIndent);
    CurPos:=1;
    LastSplitPos:=-1;
    LastSrcLineStart:=1;
    CurLineLen:=length(Result);
    LastAtomType:=atNone;
    CommentLvl:=0;
    // read atoms
    while (CurPos<=SrcLen) do begin
      repeat
        ReadNextAtom;
        if CurAtomType=atDirectiveStart then begin
          // don't touch directives: they can contain macros and filenames
          OldAtomStart:=AtomStart;
          ReadTilDirectiveEnd;
          AtomStart:=OldAtomStart;
        end;
        CurAtom:=copy(Src,AtomStart,AtomEnd-AtomStart);
        if CurAtom=' ' then
          AddAtom(Result,' ')
        else
          break;
      until false;
      //DebugLn(['TBeautifyCodeOptions.BeautifyStatement ',CurAtom,' LastAtomType=',AtomTypeNames[LastAtomType],',',LastAtomType in DoNotInsertSpaceAfter,',',LastAtomType in DoInsertSpaceAfter,' CurAtomType=',AtomTypeNames[CurAtomType],',',CurAtomType in DoNotInsertSpaceInFront,',',CurAtomType in DoInsertSpaceInFront]);
      if ((Result='') or (Result[length(Result)]<>' '))
      and (not (CurAtomType in DoNotInsertSpaceInFront))
      and (not (LastAtomType in DoNotInsertSpaceAfter))
      and ((CurAtomType in DoInsertSpaceInFront)
           or (LastAtomType in DoInsertSpaceAfter))
      then begin
        //DebugLn(['TBeautifyCodeOptions.BeautifyStatement ADDING space']);
        AddAtom(Result,' ');
      end;
      if (CurAtomType=atIdentifier) and (LastAtomType=atColon) then begin
        {DebugLn('SPLIT LINE  CurPos='+dbgs(CurPos)+' CurAtom="'+CurAtom+'"'
          +' CurAtomType='+AtomTypeNames[CurAtomType]
          +' LastAtomType=',AtomTypeNames[LastAtomType]
          +' CurNot='+dbgs(CurAtomType in DoNotInsertSpaceInFront)
          +' LastNot='+dbgs(LastAtomType in DoNotInsertSpaceAfter)
          +' Cur='+dbgs(CurAtomType in DoInsertSpaceInFront)
          +' Last='+dbgs(LastAtomType in DoInsertSpaceAfter)
          +' ..."'+copy(Result,length(Result)-10,10)+'"');}
      end;
      
      if (not (CurAtomType in DoNotSplitLineInFront))
      and (not (LastAtomType in DoNotSplitLineAfter)) then
        LastSplitPos:=length(Result)+1;
      {DebugLn('SPLIT LINE  CurPos='+dbgs(CurPos)+' CurAtom="'+CurAtom+'"'
      +' CurAtomType='+AtomTypeNames[CurAtomType]
      +' LastAtomType=',AtomTypeNames[LastAtomType]
      +'  '+dbgs(LastAtomType in DoInsertSpaceAfter)+' LastSplitPos='+dbgs(LastSplitPos)
      +' ..."'+copy(Result,length(Result)-10,10)+'"');}
      AddAtom(Result,CurAtom);
      LastAtomType:=CurAtomType;
    end;
  finally
    Indent:=OldIndent;
    CurFlags:=[];
  end;
  //DebugLn('[TBeautifyCodeOptions.BeautifyStatement] Result="',Result,'"');
  //DebugLn('**********************************************************');
end;

function TBeautifyCodeOptions.AddClassAndNameToProc(const AProcCode, AClassName,
  AMethodName: string): string;
var StartPos, NamePos, ProcLen: integer;
  s: string;
  KeyWordPos: LongInt;
begin
  if CompareSubStrings('CLASS ',AProcCode,1,1,6,false)<>0 then
    StartPos:=1
  else
    StartPos:=6;
  ProcLen:=length(AProcCode);
  // read proc keyword 'procedure', 'function', ...
  while (StartPos<=ProcLen) and (IsSpaceChar[AProcCode[StartPos]]) do
    inc(StartPos);
  KeyWordPos:=StartPos;
  while (StartPos<=ProcLen) and (IsIdentChar[AProcCode[StartPos]]) do
    inc(StartPos);
  if KeyWordPos=StartPos then
    raise Exception.Create('TBeautifyCodeOptions.AddClassAndNameToProc missing keyword');
  while (StartPos<=ProcLen) and (IsSpaceChar[AProcCode[StartPos]]) do
    inc(StartPos);
  NamePos:=StartPos;
  while (StartPos<=ProcLen) and (IsIdentChar[AProcCode[StartPos]]) do
    inc(StartPos);
  if (NamePos=StartPos)
  or (CompareSubStrings('OF',AProcCode,1,NamePos,2,false)=0) then
  begin
    // there is no name yet
    s:=AMethodName;
    if AClassName<>'' then
      s:=AClassName+'.'+s;
    if IsIdentChar[AProcCode[NamePos-1]] then
      s:=' '+s;
    if IsIdentChar[AProcCode[NamePos]] then
      s:=s+' ';
    Result:=copy(AProcCode,1,NamePos-1)+s
           +copy(AProcCode,NamePos,length(AProcCode)-NamePos+1);
  end else begin
    // there is already a name
    if AClassName<>'' then begin
      while (StartPos<=ProcLen) and (IsSpaceChar[AProcCode[StartPos]]) do
        inc(StartPos);
      if (StartPos<=ProcLen) and (AProcCode[StartPos]<>'.') then
        Result:=copy(AProcCode,1,NamePos-1)+AClassName+'.'
               +copy(AProcCode,NamePos,length(AProcCode)-NamePos+1)
      else
        Result:=AProcCode;
    end else
      Result:=AProcCode;
  end;
end;

function TBeautifyCodeOptions.BeautifyWord(const AWord: string;
  WordPolicy: TWordPolicy): string;
begin
  case WordPolicy of
    wpLowerCase: Result:=lowercase(AWord);
    wpUpperCase: Result:=UpperCaseStr(AWord);
    wpLowerCaseFirstLetterUp: Result:=UpperCaseStr(copy(AWord,1,1))
                                     +lowercase(copy(AWord,2,length(AWord)-1));
  else
    Result:=AWord;
  end;
end;

function TBeautifyCodeOptions.BeautifyKeyWord(const AWord: string): string;
begin
  Result:=BeautifyWord(AWord,KeyWordPolicy);
end;

function TBeautifyCodeOptions.BeautifyIdentifier(const AWord: string): string;
begin
  Result:=BeautifyWord(AWord,IdentifierPolicy);
end;

procedure TBeautifyCodeOptions.ConsistencyCheck;
begin
end;

procedure TBeautifyCodeOptions.WriteDebugReport;
begin
  DebugLn('TBeautifyCodeOptions.WriteDebugReport');
  ConsistencyCheck;
end;

{ ESourceChangeCacheError }

constructor ESourceChangeCacheError.Create(ASender: TSourceChangeCache;
  const AMessage: string);
begin
  inherited Create(AMessage);
  Sender:=ASender;
end;

end.

