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

{ $DEFINE CTDEBUG}

uses
  Classes, SysUtils, CodeCache, BasicCodeTools, SourceLog, LinkScanner,
  AVL_Tree, KeywordFuncLists;
  
type
  // TBeautifyCodeOptions
  TClassPartInsertPolicy = (cpipAlphabetically, cpipLast);
  TMethodInsertPolicy = (mipAlphabetically, mipLast, mipClassOrder);
  TForwardProcInsertPolicy = (fpipLast, fpipInFrontOfMethods, fpipBehindMethods);

  TWordPolicy = (wpNone, wpLowerCase, wpUpperCase, wpLowerCaseFirstLetterUp);
  TAtomType = (atNone, atKeyword, atIdentifier, atColon, atSemicolon, atComma,
               atPoint, atAt, atNumber, atStringConstant, atNewLine,
               atSpace, atSymbol);
  TAtomTypes = set of TAtomType;

  TBeautifyCodeOptions = class
  private
    CurLineLen: integer;
    LastSplitPos: integer; // last position where splitting is allowed
    LastSrcLineStart: integer;// last line start, not added by splitting
    CurAtomType, LastAtomType: TAtomType;
    CurPos, AtomStart, AtomEnd, SrcLen, CurIndent: integer;
    Src, UpperSrc: string;
    procedure AddAtom(var s:string; NewAtom: string);
    procedure ReadNextAtom;
  public
    LineLength: integer;
    LineEnd: string; // default: #13#10
    Indent: integer;
    ClassPartInsertPolicy: TClassPartInsertPolicy;
    MethodInsertPolicy: TMethodInsertPolicy;
    ForwardProcInsertPolicy: TForwardProcInsertPolicy;
    KeyWordPolicy: TWordPolicy;
    IdentifierPolicy: TWordPolicy;
    DoNotSplitLineInFront: TAtomTypes;
    DoNotSplitLineAfter: TAtomTypes;
    DoInsertSpaceInFront: TAtomTypes;
    DoInsertSpaceAfter: TAtomTypes;
    PropertyReadIdentPrefix: string;
    PropertyWriteIdentPrefix: string;
    PropertyStoredIdentPostfix: string;
    PrivatVariablePrefix: string;

    function BeautifyProc(const AProcCode: string; IndentSize: integer;
        AddBeginEnd: boolean): string;
    function BeautifyStatement(const AStatement: string; IndentSize: integer
        ): string;
    function AddClassAndNameToProc(const AProcCode, AClassName,
        AMethodName: string): string;
    function BeautifyWord(const AWord: string; WordPolicy: TWordPolicy): string;
    function BeautifyKeyWord(const AWord: string): string;
    function BeautifyIdentifier(const AWord: string): string;
    function ConsistencyCheck: integer;
    procedure WriteDebugReport;
    constructor Create;
  end;

  //----------------------------------------------------------------------------
  // in front of and after a text change can be set a gap.
  // A Gap is for example a space char or a newline. TSourceChangeLog will add
  // the gap if it is not already in the code
  TGapTyp = (gtNone,     // no special gap
             gtSpace,    // at least a single space
             gtNewLine,  // at least a newline
             gtEmptyLine // at least two newlines
             );
  

  TSourceChangeCacheEntry = class
  public
    FrontGap, AfterGap: TGapTyp;
    FromPos, ToPos: integer;
    Text: string;
    FromCode: TCodeBuffer;
    FromDirectPos: integer;
    constructor Create(aFrontGap, anAfterGap: TGapTyp; aFromPos,
        aToPos: integer; const aText: string; aFromCode: TCodeBuffer;
        aFromDirectPos: integer);
  end;
  
  //----------------------------------------------------------------------------
  TOnBeforeApplyChanges = procedure(var Abort: boolean) of object;
  TOnAfterApplyChanges = procedure of object;

  TSourceChangeCache = class
  private
    FMainScanner: TLinkScanner;
    FEntries: TAVLTree;
    FBuffersToModify: TList; // sorted list of TCodeBuffer
    FBuffersToModifyNeedsUpdate: boolean;
    FOnBeforeApplyChanges: TOnBeforeApplyChanges;
    FOnAfterApplyChanges: TOnAfterApplyChanges;
    FUpdateLock: integer;
    Src: string; // current cleaned source
    SrcLen: integer; // same as length(Src)
    procedure DeleteOldText(CleanFromPos,CleanToPos: integer);
    procedure InsertNewText(ACode: TCodeBuffer; DirectPos: integer;
        const InsertText: string);
    procedure SetMainScanner(NewScanner: TLinkScanner);
    function GetBuffersToModify(Index: integer): TCodeBuffer;
    procedure UpdateBuffersToModify;
  public
    BeautifyCodeOptions: TBeautifyCodeOptions;
    procedure BeginUpdate;
    procedure EndUpdate;
    property MainScanner: TLinkScanner read FMainScanner write SetMainScanner;
    function Replace(FrontGap, AfterGap: TGapTyp; FromPos, ToPos: integer;
        const Text: string): boolean;
    function ReplaceEx(FrontGap, AfterGap: TGapTyp; FromPos, ToPos: integer;
        FromCode: TCodeBuffer; FromDirectPos: integer;
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
    function ConsistencyCheck: integer;
    procedure WriteDebugReport;
    constructor Create;
    destructor Destroy; override;
  end;

const
  AtomTypeNames: array[TAtomType] of shortstring = (
      'None', 'Keyword', 'Identifier', 'Colon', 'Semicolon', 'Comma', 'Point',
      'At', 'Number', 'StringConstant', 'NewLine', 'Space', 'Symbol'
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
  ForwardProcInsertPolicyNames: array[TForwardProcInsertPolicy] of shortstring =
    ('Last', 'InFrontOfMethods', 'BehindMethods');
    
    
  DefaultDoNotSplitLineInFront: TAtomTypes =
    [atColon,atComma,atSemicolon,atPoint];
  DefaultDoNotSplitLineAfter: TAtomTypes = [atColon,atAt,atPoint,atKeyWord];
  DefaultDoInsertSpaceInFront: TAtomTypes = [];
  DefaultDoInsertSpaceAfter: TAtomTypes = [atColon,atComma,atSemicolon];

function AtomTypeNameToType(const s: string): TAtomType;
function WordPolicyNameToPolicy(const s: string): TWordPolicy;
function ClassPartPolicyNameToPolicy(const s: string): TClassPartInsertPolicy;
function MethodInsertPolicyNameToPolicy(const s: string): TMethodInsertPolicy;
function ForwardProcInsertPolicyNameToPolicy(
  const s: string): TForwardProcInsertPolicy;

implementation


function AtomTypeNameToType(const s: string): TAtomType;
begin
  for Result:=Low(TAtomType) to High(TAtomType) do
    if AnsiCompareText(AtomTypeNames[Result],s)=0 then exit;
  Result:=atNone;
end;

function WordPolicyNameToPolicy(const s: string): TWordPolicy;
begin
  for Result:=Low(TWordPolicy) to High(TWordPolicy) do
    if AnsiCompareText(WordPolicyNames[Result],s)=0 then exit;
  Result:=wpNone;
end;

function ClassPartPolicyNameToPolicy(const s: string): TClassPartInsertPolicy;
begin
  for Result:=Low(TClassPartInsertPolicy) to High(TClassPartInsertPolicy) do
    if AnsiCompareText(ClassPartInsertPolicyNames[Result],s)=0 then exit;
  Result:=cpipLast;
end;

function MethodInsertPolicyNameToPolicy(
  const s: string): TMethodInsertPolicy;
begin
  for Result:=Low(TMethodInsertPolicy) to High(TMethodInsertPolicy) do
    if AnsiCompareText(MethodInsertPolicyNames[Result],s)=0 then exit;
  Result:=mipLast;
end;

function ForwardProcInsertPolicyNameToPolicy(
  const s: string): TForwardProcInsertPolicy;
begin
  for Result:=Low(TForwardProcInsertPolicy) to High(TForwardProcInsertPolicy) do
    if AnsiCompareText(ForwardProcInsertPolicyNames[Result],s)=0 then exit;
  Result:=fpipBehindMethods;
end;

function CompareSourceChangeCacheEntry(NodeData1, NodeData2: pointer): integer;
var Entry1, Entry2: TSourceChangeCacheEntry;
begin
  Entry1:=TSourceChangeCacheEntry(NodeData1);
  Entry2:=TSourceChangeCacheEntry(NodeData2);
  if Entry1.FromPos>Entry2.FromPos then
    Result:=1
  else if Entry1.FromPos<Entry2.FromPos then
    Result:=-1
  else
    Result:=0;
end;

{ TSourceChangeCacheEntry }

constructor TSourceChangeCacheEntry.Create(aFrontGap, anAfterGap: TGapTyp;
  aFromPos, aToPos: integer; const aText: string; aFromCode: TCodeBuffer;
  aFromDirectPos: integer);
begin
  inherited Create;
  FrontGap:=aFrontGap;
  AfterGap:=anAfterGap;
  FromPos:=aFromPos;
  ToPos:=aToPos;
  Text:=aText;
  FromCode:=aFromCode;
  FromDirectPos:=aFromDirectPos;
end;


{ TSourceChangeCache }

constructor TSourceChangeCache.Create;
begin
  inherited Create;
  FEntries:=TAVLTree.Create(@CompareSourceChangeCacheEntry);
  MainScanner:=nil;
  FBuffersToModify:=TList.Create;
  FBuffersToModifyNeedsUpdate:=false;
  BeautifyCodeOptions:=TBeautifyCodeOptions.Create;
end;

destructor TSourceChangeCache.Destroy;
begin
  Clear;
  BeautifyCodeOptions.Free;
  FBuffersToModify.Free;
  FEntries.FreeAndClear;
  FEntries.Free;
  inherited Destroy;
end;

function TSourceChangeCache.FindEntryInRange(
  FromPos, ToPos: integer): TSourceChangeCacheEntry;
var ANode: TAVLTreeNode;
begin
  ANode:=FEntries.Root;
  while ANode<>nil do begin
    Result:=TSourceChangeCacheEntry(ANode.Data);
    if Result.ToPos<=FromPos then
      ANode:=ANode.Left
    else if Result.FromPos>ToPos then
      ANode:=ANode.Right
    else
      exit;
  end;
  Result:=nil;
end;

function TSourceChangeCache.FindEntryAtPos(
  APos: integer): TSourceChangeCacheEntry;
var ANode: TAVLTreeNode;
begin
  ANode:=FEntries.Root;
  while ANode<>nil do begin
    Result:=TSourceChangeCacheEntry(ANode.Data);
    if Result.ToPos<=APos then
      ANode:=ANode.Left
    else if Result.FromPos>APos then
      ANode:=ANode.Right
    else
      exit;
  end;
  Result:=nil;
end;

function TSourceChangeCache.ReplaceEx(FrontGap, AfterGap: TGapTyp; 
  FromPos, ToPos: integer;
  FromCode: TCodeBuffer; FromDirectPos: integer;
  const Text: string): boolean;
var ANode: TAVLTreeNode;
  NewEntry: TSourceChangeCacheEntry;
  p: pointer;
begin
  {$IFDEF CTDEBUG}
  writeln('TSourceChangeCache.ReplaceEx FrontGap=',ord(FrontGap),
  ' AfterGap=',ord(AfterGap),' FromPos=',FromPos,' ToPos=',ToPos,
  ' Text="',Text,'"');
  if FromCode<>nil then writeln('FromCode=',FromCode.Filename,' FromDirectPos=',FromDirectPos);
  {$ENDIF}
  Result:=false;
  if (MainScanner=nil) or (FromPos>ToPos) or (FromPos<1)
  or (ToPos>MainScanner.CleanedLen+1) then
    exit;
  if FindEntryInRange(FromPos,ToPos)<>nil then exit;
  if ToPos>FromPos then begin
    // this is a delete operation -> check the whole range for writable buffers
    if not MainScanner.WholeRangeIsWritable(FromPos,ToPos) then exit;
  end;
  if FromCode=nil then begin
    if not MainScanner.CleanedPosToCursor(FromPos,FromDirectPos,p) then
      exit;
    FromCode:=TCodeBuffer(p);
  end;
  // add entry
  NewEntry:=TSourceChangeCacheEntry.Create(FrontGap,AfterGap,FromPos,ToPos,
                                           Text,FromCode,FromDirectPos);
  ANode:=FEntries.Add(NewEntry);
  if ToPos=FromPos then
    FEntries.MoveDataLeftMost(ANode)
  else
    // the new entry is a delete operation -> put it rightmost, so that it will
    // be applied first
    FEntries.MoveDataRightMost(ANode);
    
  FBuffersToModifyNeedsUpdate:=true;
  Result:=true;
  {$IFDEF CTDEBUG}
  writeln('TSourceChangeCache.ReplaceEx SUCCESS');
  {$ENDIF}
end;

function TSourceChangeCache.Replace(FrontGap, AfterGap: TGapTyp;
  FromPos, ToPos: integer; const Text: string): boolean;
begin
  Result:=ReplaceEx(FrontGap,AfterGap,FromPos,ToPos,nil,0,Text);
end;

procedure TSourceChangeCache.Clear;
begin
  FUpdateLock:=0;
  FEntries.FreeAndClear;
  FBuffersToModify.Clear;
  FBuffersToModifyNeedsUpdate:=true;
end;

function TSourceChangeCache.ConsistencyCheck: integer;
begin
  Result:=FEntries.ConsistencyCheck;
  if Result<>0 then begin
    dec(Result,100); exit;
  end;
  Result:=BeautifyCodeOptions.ConsistencyCheck;
  if Result<>0 then begin
    dec(Result,200); exit;
  end;
  Result:=0;
end;

procedure TSourceChangeCache.WriteDebugReport;
begin
  writeln('[TSourceChangeCache.WriteDebugReport] Consistency=',
    ConsistencyCheck);
  writeln(FEntries.ReportAsString);
  BeautifyCodeOptions.WriteDebugReport;
end;

function TSourceChangeCache.Apply: boolean;
var CurNode, PrecNode: TAVLTreeNode;
  CurEntry, PrecEntry, FirstEntry: TSourceChangeCacheEntry;
  InsertText: string;
  i, j, NeededLineEnds, NeededIndent, FromPosAdjustment: integer;
  BetweenGap: TGapTyp;
  Abort: boolean;
begin
  {$IFDEF CTDEBUG}
  writeln('TSourceChangeCache.Apply EntryCount=',FEntries.Count);
  {$ENDIF}
  Result:=false;
  if MainScanner=nil then exit;
  if FUpdateLock>0 then begin
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
    Src:=MainScanner.CleanedSrc;
    SrcLen:=length(Src);
    // apply the changes beginning with the last
    CurNode:=FEntries.FindHighest;
    while CurNode<>nil do begin
      FirstEntry:=TSourceChangeCacheEntry(CurNode.Data);
      {$IFDEF CTDEBUG}
      writeln('TSourceChangeCache.Apply Pos=',FirstEntry.FromPos,'-',FirstEntry.ToPos,
      ' Text="',FirstEntry.Text,'"');
      {$ENDIF}
      InsertText:=FirstEntry.Text;
      // add after gap
      case FirstEntry.AfterGap of
        gtSpace:
          begin
            if ((FirstEntry.ToPos>SrcLen)
                or (not IsSpaceChar[Src[FirstEntry.ToPos]])) then
              InsertText:=InsertText+' ';
          end;
        gtNewLine:
          begin
            NeededLineEnds:=CountNeededLineEndsToAddForward(Src,
              FirstEntry.ToPos,1);
            if NeededLineEnds>0 then
              InsertText:=InsertText+BeautifyCodeOptions.LineEnd;
          end;
        gtEmptyLine:
          begin
            NeededLineEnds:=CountNeededLineEndsToAddForward(Src,
              FirstEntry.ToPos,2);
            for i:=1 to NeededLineEnds do
              InsertText:=InsertText+BeautifyCodeOptions.LineEnd;
          end;
      end;
      if FirstEntry.AfterGap in [gtNewLine,gtEmptyLine] then begin
        // move the rest of the line behind the insert position to the next line
        // with auto indent
        NeededIndent:=GetLineIndent(Src,FirstEntry.ToPos);
        j:=FirstEntry.ToPos;
        while (j<=SrcLen) and (IsSpaceChar[Src[j]]) do
          inc(j);
        dec(NeededIndent,j-FirstEntry.ToPos);
        if NeededIndent>0 then
          InsertText:=InsertText+GetIndentStr(NeededIndent);
      end;
      // add text from nodes inserted at the same position
      PrecNode:=FEntries.FindPrecessor(CurNode);
      CurEntry:=FirstEntry;
      while (PrecNode<>nil) do begin
        PrecEntry:=TSourceChangeCacheEntry(PrecNode.Data);
        if PrecEntry.FromPos=CurEntry.FromPos then begin
          BetweenGap:=PrecEntry.AfterGap;
          if ord(BetweenGap)<ord(CurEntry.FrontGap) then
            BetweenGap:=CurEntry.FrontGap;
          case BetweenGap of
            gtSpace:
              InsertText:=' '+InsertText;
            gtNewLine:
              InsertText:=BeautifyCodeOptions.LineEnd+InsertText;
            gtEmptyLine:
              InsertText:=BeautifyCodeOptions.LineEnd
                            +BeautifyCodeOptions.LineEnd+InsertText;
          end;
          InsertText:=PrecEntry.Text+InsertText;
        end else
          break;
        CurNode:=PrecNode;
        CurEntry:=PrecEntry;
        PrecNode:=FEntries.FindPrecessor(CurNode);
      end;
      // add front gap
      NeededLineEnds:=0;
      case CurEntry.FrontGap of
        gtSpace:
          begin
            if (CurEntry.FromPos=1)
            or (not IsSpaceChar[Src[CurEntry.FromPos-1]]) then
              InsertText:=' '+InsertText;
          end;
        gtNewLine:
          begin
            NeededLineEnds:=CountNeededLineEndsToAddBackward(Src,
                                      CurEntry.FromPos-1,1);
            if NeededLineEnds>0 then
              InsertText:=BeautifyCodeOptions.LineEnd+InsertText;
          end;
        gtEmptyLine:
          begin
            NeededLineEnds:=CountNeededLineEndsToAddBackward(Src,
                                CurEntry.FromPos-1,2);
            for i:=1 to NeededLineEnds do
              InsertText:=BeautifyCodeOptions.LineEnd+InsertText;
          end;
      end;
      FromPosAdjustment:=0;
      if (CurEntry.FrontGap in [gtNewLine,gtEmptyLine]) and (NeededLineEnds=0)
      then begin
        // no line end was inserted in front
        // -> adjust the FromPos to replace the space in the existing line
        while (FirstEntry.FromPos+FromPosAdjustment>1)
        and (not (Src[FirstEntry.FromPos+FromPosAdjustment-1]
          in [#10,#13]))
        do dec(FromPosAdjustment);
      end;
      // delete old text in code buffers
      DeleteOldText(FirstEntry.FromPos+FromPosAdjustment,FirstEntry.ToPos);
      // insert new text
      InsertNewText(FirstEntry.FromCode,
                    FirstEntry.FromDirectPos+FromPosAdjustment,InsertText);
      CurNode:=PrecNode;
    end;
  finally
    if Assigned(FOnAfterApplyChanges) then FOnAfterApplyChanges();
    FEntries.FreeAndClear;
  end;
  Result:=true;
end;

procedure TSourceChangeCache.DeleteOldText(CleanFromPos,CleanToPos: integer);
begin
  {$IFDEF CTDEBUG}
  writeln('[TSourceChangeCache.DeleteOldText] Pos=',CleanFromPos,'-',CleanToPos);
  {$ENDIF}
  MainScanner.DeleteRange(CleanFromPos,CleanToPos);
end;

procedure TSourceChangeCache.InsertNewText(ACode: TCodeBuffer;
  DirectPos: integer; const InsertText: string);
begin
  {$IFDEF CTDEBUG}
  writeln('[TSourceChangeCache.InsertNewText] Code=',ACode.Filename,
  ' Pos=',DirectPos,' Text="',InsertText,'"');
  {$ENDIF}
  ACode.Insert(DirectPos,InsertText);
end;

procedure TSourceChangeCache.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TSourceChangeCache.EndUpdate;
begin
  if FUpdateLock<=0 then exit;
  dec(FUpdateLock);
  if FUpdateLock<=0 then
    Apply;
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
  Result:=FBuffersToModify.IndexOf(ACode)>=0;
end;

procedure TSourceChangeCache.UpdateBuffersToModify;
// build a sorted and unique list of all TCodeBuffer(s) which will be modified
// by the 'Apply' operation
var ANode: TAVLTreeNode;
  AnEntry: TSourceChangeCacheEntry;
begin
  if not FBuffersToModifyNeedsUpdate then exit;
  //writeln('[TSourceChangeCache.UpdateBuffersToModify]');
  FBuffersToModify.Clear;
  ANode:=FEntries.FindLowest;
  while ANode<>nil do begin
    AnEntry:=TSourceChangeCacheEntry(ANode.Data);
    MainScanner.FindCodeInRange(AnEntry.FromPos,AnEntry.ToPos,FBuffersToModify);
    ANode:=FEntries.FindSuccessor(ANode);
  end;
  FBuffersToModifyNeedsUpdate:=false;
end;

{ TBeautifyCodeOptions }

constructor TBeautifyCodeOptions.Create;
begin
  LineLength:=80;
  LineEnd:={$IFDEF win32}#13+{$ENDIF}#10;
  Indent:=2;
  ClassPartInsertPolicy:=cpipLast;
  MethodInsertPolicy:=mipClassOrder;
  ForwardProcInsertPolicy:=fpipBehindMethods;
  KeyWordPolicy:=wpLowerCase;
  IdentifierPolicy:=wpNone;
  DoNotSplitLineInFront:=DefaultDoNotSplitLineInFront;
  DoNotSplitLineAfter:=DefaultDoNotSplitLineAfter;
  DoInsertSpaceInFront:=DefaultDoInsertSpaceInFront;
  DoInsertSpaceAfter:=DefaultDoInsertSpaceAfter;
  PropertyReadIdentPrefix:='Get';
  PropertyWriteIdentPrefix:='Set';
  PropertyStoredIdentPostfix:='IsStored';
  PrivatVariablePrefix:='f';
end;

procedure TBeautifyCodeOptions.AddAtom(var s:string; NewAtom: string);
var
  RestLineLen, LastLineEndInAtom: integer;
begin
  if NewAtom='' then exit;
  //writeln('[TBeautifyCodeOptions.AddAtom]  NewAtom=',NewAtom,' s="',s,'"');
  if IsIdentStartChar[NewAtom[1]] then begin
    if WordIsKeyWord.DoItCaseInsensitive(NewAtom) then
      NewAtom:=BeautifyWord(NewAtom,KeyWordPolicy)
    else
      NewAtom:=BeautifyWord(NewAtom,IdentifierPolicy);
  end;
  LastLineEndInAtom:=length(NewAtom);
  while (LastLineEndInAtom>=1) and (not (NewAtom[LastLineEndInAtom] in [#10,#13]))
  do dec(LastLineEndInAtom);
  if (LastLineEndInAtom<1) and (CurLineLen+length(NewAtom)>LineLength)
  and (LastSplitPos>1) then begin
    //writeln('[TBeautifyCodeOptions.AddAtom]  NEW LINE CurLineLen=',CurLineLen,' NewAtom=',NewAtom,' "',copy(s,LastSplitPos,5));
    RestLineLen:=length(s)-LastSplitPos+1;
    s:=copy(s,1,LastSplitPos-1)+LineEnd
       +GetIndentStr(CurIndent+Indent+GetLineIndent(s,LastSrcLineStart))
       +copy(s,LastSplitPos,RestLineLen)+NewAtom;
    CurLineLen:=length(s)-LastSplitPos-length(LineEnd)+1;
    LastSplitPos:=-1;
  end else begin
    s:=s+NewAtom;
    if LastLineEndInAtom<1 then begin
      inc(CurLineLen,length(NewAtom));
    end else begin
      // there is a line end in the code
      CurLineLen:=length(NewAtom)-LastLineEndInAtom;
      LastSrcLineStart:=length(s)+1-CurLineLen;
    end;
  end;
end;

procedure TBeautifyCodeOptions.ReadNextAtom;
var c1, c2: char;
begin
  AtomStart:=CurPos;
  if AtomStart<=SrcLen then begin
    c1:=UpperSrc[CurPos];
    case c1 of
      'A'..'Z','_':
        begin
          CurAtomType:=atIdentifier;
          repeat
            inc(CurPos);
          until (CurPos>SrcLen) or (not IsIdentChar[Src[CurPos]]);
          if WordIsKeyWord.DoItUpperCase(UpperSrc,AtomStart,CurPos-AtomStart)
          then
            CurAtomType:=atKeyword;
        end;
      #10,#13:
        begin
          CurAtomType:=atNewLine;
          inc(CurPos);
          if (CurPos<=SrcLen) and (IsLineEndChar[Src[CurPos]])
          and (Src[CurPos]<>c1) then
            inc(CurPos);
        end;
      #0..#9,#11..#12,#14..#32:
        begin
          CurAtomType:=atSpace;
          repeat
            inc(CurPos);
          until (CurPos>SrcLen) or (not IsSpaceChar[Src[CurPos]]);
        end;
      '0'..'9':
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
            if (CurPos<=SrcLen) and (UpperSrc[CurPos]='E')
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
      '''','#':
        begin
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
        end;
      '%':
        begin
          CurAtomType:=atNumber;
          repeat
            inc(CurPos);
          until (CurPos>SrcLen) or (not (Src[CurPos] in ['0','1']));
        end;
      '$':
        begin
          CurAtomType:=atNumber;
          repeat
            inc(CurPos);
          until (CurPos>SrcLen) or (not IsHexNumberChar[Src[CurPos]]);
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
            or ((c1='/') and (c2='/'))
            or ((c1='(') and (c2='*'))
            or ((c1='*') and (c2=')'))
            then
              inc(CurPos);
          end;
          if AtomStart+1=CurPos then begin
            if c1='.' then CurAtomType:=atPoint
            else if c1=',' then CurAtomType:=atComma
            else if c1=':' then CurAtomType:=atColon
            else if c1=';' then CurAtomType:=atSemicolon
            else if c1='@' then CurAtomType:=atAt;
          end;
        end;
    end;
  end else
    CurAtomType:=atNone;
  AtomEnd:=CurPos;
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
  writeln('[TBeautifyCodeOptions.BeautifyProc] Result="',Result,'"');
  {$ENDIF}
end;

function TBeautifyCodeOptions.BeautifyStatement(const AStatement: string;
  IndentSize: integer): string;
var CurAtom: string;
begin
  //writeln('**********************************************************');
  //writeln('[TBeautifyCodeOptions.BeautifyStatement] "',AStatement,'"');
  Src:=AStatement;
  UpperSrc:=UpperCaseStr(Src);
  SrcLen:=length(Src);
  if IndentSize>=LineLength-10 then IndentSize:=LineLength-10;
  CurIndent:=IndentSize;
  Result:=GetIndentStr(CurIndent);
  CurPos:=1;
  LastSplitPos:=-1;
  LastSrcLineStart:=1;
  CurLineLen:=length(Result);
  LastAtomType:=atNone;
  while (CurPos<=SrcLen) do begin
    repeat
      ReadNextAtom;
      CurAtom:=copy(Src,AtomStart,AtomEnd-AtomStart);
      if CurAtom=' ' then
        AddAtom(Result,' ')
      else
        break;
    until false;
    if ((Result='') or (Result[length(Result)]<>' '))
    and ((CurAtomType in DoInsertSpaceInFront)
    or (LastAtomType in DoInsertSpaceAfter)) then
      AddAtom(Result,' ');
    if (not (CurAtomType in DoNotSplitLineInFront))
    and (not (LastAtomType in DoNotSplitLineAfter)) then
      LastSplitPos:=length(Result)+1;
    {writeln('SPLIT LINE  CurPos=',CurPos,' CurAtom="',CurAtom,
    '" CurAtomType=',AtomTypeNames[CurAtomType],' LastAtomType=',AtomTypeNames[LastAtomType],
    '  ',LastAtomType in DoInsertSpaceAfter,' LastSplitPos=',LastSplitPos,
    ' ..."',copy(Result,length(Result)-10,10),'"');}
    AddAtom(Result,CurAtom);
    LastAtomType:=CurAtomType;
  end;
  //writeln('[TBeautifyCodeOptions.BeautifyStatement] Result="',Result,'"');
  //writeln('**********************************************************');
end;

function TBeautifyCodeOptions.AddClassAndNameToProc(const AProcCode, AClassName,
  AMethodName: string): string;
var StartPos, NamePos, ProcLen: integer;
  s: string;
begin
  if CompareSubStrings('CLASS ',AProcCode,1,1,6,false)<>0 then
    StartPos:=1
  else
    StartPos:=6;
  ProcLen:=length(AProcCode);
  // read proc keyword 'procedure', 'function', ...
  while (StartPos<=ProcLen) and (IsSpaceChar[AProcCode[StartPos]]) do
    inc(StartPos);
  while (StartPos<=ProcLen) and (IsIdentChar[AProcCode[StartPos]]) do
    inc(StartPos);
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
           +copy(AProcCode,NamePos,length(AProcCode)-NamePos+1)
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

function TBeautifyCodeOptions.ConsistencyCheck: integer;
begin
  Result:=0;
end;

procedure TBeautifyCodeOptions.WriteDebugReport;
begin
  writeln('TBeautifyCodeOptions.WriteDebugReport Consistency=',
    ConsistencyCheck);
    
end;


end.

