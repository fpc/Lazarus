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
    - save and load TBeautifyCodeOptions from/to XML
}
unit SourceChanger;

{$ifdef fpc}{$mode objfpc}{$endif}{$H+}

interface

uses
  Classes, SysUtils, SourceLog, LinkScanner, AVL_Tree, CodeCache,
  KeywordFuncLists, BasicCodeTools;
  
type
  // TBeautifyCodeOptions
  TClassPartInsertPolicy = (cpipAlphabetically, cpipLast);
  TProcedureInsertPolicy = (pipAlphabetically, pipLast, pipClassOrder);
  TWordPolicy = (wpNone, wpLowerCase, wpUpperCase, wpLowerCaseFirstLetterUp);
  TAtomType = (atNone, atKeyword, atIdentifier, atColon, atSemicolon, atComma,
               atPoint, atAt, atNumber, atStringConstant, atNewLine,
               atSpace, atSymbol);
  TAtomTypes = set of TAtomType;

  TBeautifyCodeOptions = class
  private
    CurLineLen: integer;
    LastSplitPos: integer;
    CurAtomType, LastAtomType: TAtomType;
    CurPos, AtomStart, AtomEnd, SrcLen: integer;
    Src, UpperSrc, IndentStr: string;
    procedure AddAtom(var s:string; NewAtom: string);
    procedure ReadNextAtom;
  public
    LineLength: integer;
    LineEnd: string; // default: #13#10
    Indent: integer;
    ClassPartInsertPolicy: TClassPartInsertPolicy;
    ProcedureInsertPolicy: TProcedureInsertPolicy;
    KeyWordPolicy : TWordPolicy;
    IdentifierPolicy: TWordPolicy;
    DoNotSplitLineBefore: TAtomTypes;
    DoNotSplitLineAfter: TAtomTypes;
    DoInsertSpaceBefore: TAtomTypes;
    DoInsertSpaceAfter: TAtomTypes;
    PropertyReadIdentPrefix: string;
    PropertyWriteIdentPrefix: string;
    PropertyStoredFunction: string;
    PrivatVariablePrefix: string;

    function BeautifyProc(const AProcCode: string; IndentSize: integer;
        AddBeginEnd: boolean): string;
    function BeautifyStatement(const AStatement: string; IndentSize: integer
        ): string;
    function AddClassNameToProc(const AProcCode, AClassName: string): string;
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

  TSourceChangeCache = class
  private
    FMainScanner: TLinkScanner;
    FEntries: TAVLTree;
    FBuffersToModify: TList; // sorted list of TCodeBuffer
    FBuffersToModifyNeedsUpdate: boolean;
    FOnBeforeApplyChanges: TOnBeforeApplyChanges;
    Src: string;
    procedure DeleteOldText(CleanFromPos,CleanToPos: integer);
    procedure InsertNewText(ACode: TCodeBuffer; DirectPos: integer;
        const InsertText: string);
    function CountNeededLineEndsToAddForward(
        CleanPos, MinLineEnds: integer): integer;
    function CountNeededLineEndsToAddBackward(
        CleanPos, MinLineEnds: integer): integer;
    procedure SetMainScanner(NewScanner: TLinkScanner);
    function GetBuffersToModify(Index: integer): TCodeBuffer;
    procedure UpdateBuffersToModify;
  public
    BeautifyCodeOptions: TBeautifyCodeOptions;
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
    property OnBeforeApplyChanges: TOnBeforeApplyChanges
        read FOnBeforeApplyChanges write FOnBeforeApplyChanges;
    procedure Clear;
    function ConsistencyCheck: integer;
    procedure WriteDebugReport;
    constructor Create;
    destructor Destroy; override;
  end;

const
  AtomTypeNames: array[TAtomType] of shortstring = (
      'None', 'Keyword', 'Identifier', 'atColon', 'Semicolon', 'Comma', 'Point',
      'At', 'Number', 'StringConstant', 'NewLine', 'Space', 'Symbol'
    );


implementation


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
writeln('TSourceChangeCache.ReplaceEx FrontGap=',ord(FrontGap),
' AfterGap=',ord(AfterGap),' FromPos=',FromPos,' ToPos=',ToPos,
' Text="',Text,'"');
if FromCode<>nil then writeln('FromCode=',FromCode.Filename,' FromDirectPos=',FromDirectPos);
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
writeln('TSourceChangeCache.ReplaceEx SUCCESS');
end;

function TSourceChangeCache.Replace(FrontGap, AfterGap: TGapTyp;
  FromPos, ToPos: integer; const Text: string): boolean;
begin
  Result:=ReplaceEx(FrontGap,AfterGap,FromPos,ToPos,nil,0,Text);
end;

procedure TSourceChangeCache.Clear;
begin
  FEntries.FreeAndClear;
  FBuffersToModify.Clear;
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
  i, j, NeededLineEnds, NeededIndent: integer;
  BetweenGap: TGapTyp;
  Abort: boolean;
begin
writeln('TSourceChangeCache.Apply EntryCount=',FEntries.Count);
  Result:=false;
  if MainScanner=nil then exit;
  if Assigned(FOnBeforeApplyChanges) then begin
    Abort:=false;
    FOnBeforeApplyChanges(Abort);
    if Abort then begin
      Clear;
      exit;
    end;
  end;
  Src:=MainScanner.CleanedSrc;
  // apply the changes beginning with the last
  CurNode:=FEntries.FindHighest;
  while CurNode<>nil do begin
    FirstEntry:=TSourceChangeCacheEntry(CurNode.Data);
writeln('TSourceChangeCache.Apply Pos=',FirstEntry.FromPos,'-',FirstEntry.ToPos,
' Text="',FirstEntry.Text,'"');
    InsertText:=FirstEntry.Text;
    // add after gap
    case FirstEntry.AfterGap of
      gtSpace:
        begin
          if ((FirstEntry.ToPos>MainScanner.CleanedLen)
              or (not IsSpaceChar[MainScanner.Src[FirstEntry.ToPos]])) then
            InsertText:=InsertText+' ';
        end;
      gtNewLine:
        begin
          NeededLineEnds:=CountNeededLineEndsToAddForward(FirstEntry.ToPos,1);
          if NeededLineEnds>0 then
            InsertText:=InsertText+BeautifyCodeOptions.LineEnd;
        end;
      gtEmptyLine:
        begin
          NeededLineEnds:=CountNeededLineEndsToAddForward(FirstEntry.ToPos,2);
          for i:=1 to NeededLineEnds do
            InsertText:=InsertText+BeautifyCodeOptions.LineEnd;
        end;
    end;
    if FirstEntry.AfterGap in [gtNewLine,gtEmptyLine] then begin
      NeededIndent:=GetLineIndent(MainScanner.Src,FirstEntry.ToPos);
      j:=FirstEntry.ToPos;
      while (j<=MainScanner.SrcLen) and (IsSpaceChar[MainScanner.Src[j]]) do
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
            InsertText:=BeautifyCodeOptions.LineEnd+BeautifyCodeOptions.LineEnd
                           +InsertText;
        end;
        InsertText:=PrecEntry.Text+InsertText;
      end else
        break;
      CurNode:=PrecNode;
      CurEntry:=PrecEntry;
      PrecNode:=FEntries.FindPrecessor(CurNode);
    end;
    // add front gap
    case CurEntry.FrontGap of
      gtSpace:
        begin
          if (CurEntry.FromPos=1)
          or (not IsSpaceChar[MainScanner.Src[CurEntry.FromPos-1]]) then
            InsertText:=' '+InsertText;
        end;
      gtNewLine:
        begin
          NeededLineEnds:=CountNeededLineEndsToAddBackward(CurEntry.FromPos-1,1);
          if NeededLineEnds>0 then
            InsertText:=BeautifyCodeOptions.LineEnd+InsertText;
        end;
      gtEmptyLine:
        begin
          NeededLineEnds:=CountNeededLineEndsToAddBackward(CurEntry.FromPos-1,2);
          for i:=1 to NeededLineEnds do
            InsertText:=BeautifyCodeOptions.LineEnd+InsertText;
        end;
    end;
    // delete old text in code buffers
    DeleteOldText(FirstEntry.FromPos,FirstEntry.ToPos);
    // insert new text
    InsertNewText(FirstEntry.FromCode,FirstEntry.FromDirectPos,InsertText);
    CurNode:=PrecNode;
  end;
  FEntries.FreeAndClear;
  Result:=true;
end;

function TSourceChangeCache.CountNeededLineEndsToAddForward(
  CleanPos, MinLineEnds: integer): integer;
var c:char;
begin
  Result:=MinLineEnds;
  if CleanPos<1 then exit;
  while (CleanPos<=length(Src)) do begin
    c:=Src[CleanPos];
    if IsLineEndChar[c] then begin
      dec(Result);
      inc(CleanPos);
      if (CleanPos<=length(Src))
      and (IsLineEndChar[Src[CleanPos]])
      and (Src[CleanPos]<>c) then
        inc(CleanPos);
    end else if IsSpaceChar[c] then
      inc(CleanPos)
    else
      break;
  end;
end;

function TSourceChangeCache.CountNeededLineEndsToAddBackward(
  CleanPos, MinLineEnds: integer): integer;
var c:char;
begin
  Result:=MinLineEnds;
  if (CleanPos>length(Src)) then exit;
  while (CleanPos>=1) do begin
    c:=Src[CleanPos];
    if IsLineEndChar[c] then begin
      dec(Result);
      dec(CleanPos);
      if (CleanPos>=1)
      and (IsLineEndChar[Src[CleanPos]])
      and (Src[CleanPos]<>c) then
        dec(CleanPos);
    end else if IsSpaceChar[c] then
      dec(CleanPos)
    else
      break;
  end;
end;

procedure TSourceChangeCache.DeleteOldText(CleanFromPos,CleanToPos: integer);
begin
writeln('[TSourceChangeCache.DeleteOldText] Pos=',CleanFromPos,'-',CleanToPos);
  MainScanner.DeleteRange(CleanFromPos,CleanToPos);
end;

procedure TSourceChangeCache.InsertNewText(ACode: TCodeBuffer;
  DirectPos: integer; const InsertText: string);
begin
writeln('[TSourceChangeCache.InsertNewText] Code=',ACode.Filename,
' Pos=',DirectPos,' Text="',InsertText,'"');
  ACode.Insert(DirectPos,InsertText);
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

procedure TSourceChangeCache.UpdateBuffersToModify;
// build a sorted and unique list of all TCodeBuffer(s) which will be modified
// by the 'Apply' operation
var ANode: TAVLTreeNode;
  AnEntry: TSourceChangeCacheEntry;
begin
  if not FBuffersToModifyNeedsUpdate then exit;
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
  LineEnd:=#13#10;
  Indent:=2;
  ClassPartInsertPolicy:=cpipLast;
  ProcedureInsertPolicy:=pipClassOrder;
  KeyWordPolicy:=wpLowerCase;
  IdentifierPolicy:=wpNone;
  DoNotSplitLineBefore:=[atColon,atComma,atSemicolon,atPoint];
  DoNotSplitLineAfter:=[atColon,atAt,atPoint];
  DoInsertSpaceBefore:=[];
  DoInsertSpaceAfter:=[atColon,atComma,atSemicolon];
  PropertyReadIdentPrefix:='Get';
  PropertyWriteIdentPrefix:='Set';
  PropertyStoredFunction:='IsStored';
  PrivatVariablePrefix:='f';
end;

procedure TBeautifyCodeOptions.AddAtom(var s:string; NewAtom: string);
var RestLineLen: integer;
begin
  if NewAtom='' then exit;
//writeln('    AddAtom="',NewAtom,'"');
  if IsIdentStartChar[NewAtom[1]] then begin
    if WordIsKeyWord.DoIt(NewAtom) then
      NewAtom:=BeautifyWord(NewAtom,KeyWordPolicy)
    else
      NewAtom:=BeautifyWord(NewAtom,IdentifierPolicy);
  end;
  if (CurLineLen+length(NewAtom)>LineLength) and (LastSplitPos>0) then begin
//writeln('    NEW LINE  "',copy(s,LastSplitPos,5));
    RestLineLen:=length(s)-LastSplitPos+1;
    s:=copy(s,1,LastSplitPos-1)+LineEnd+IndentStr
       +copy(s,LastSplitPos,RestLineLen)+NewAtom;
    CurLineLen:=length(s)-LastSplitPos-length(LineEnd)+1;
    LastSplitPos:=-1;
  end else begin
    s:=s+NewAtom;
    inc(CurLineLen,length(NewAtom));
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
              inc(CurPos)
            else begin
              if c1='.' then CurAtomType:=atPoint
              else if c1=',' then CurAtomType:=atComma
              else if c1=':' then CurAtomType:=atColon
              else if c1=';' then CurAtomType:=atSemicolon
              else if c1='@' then CurAtomType:=atAt;
            end;
          end;
        end;
    end;
  end;
  AtomEnd:=CurPos;
end;

function TBeautifyCodeOptions.BeautifyProc(const AProcCode: string;
  IndentSize: integer; AddBeginEnd: boolean): string;
begin
  Result:=BeautifyStatement(AProcCode,IndentSize);
  if AddBeginEnd then begin
    SetLength(IndentStr,IndentSize);
    if IndentSize>0 then
      FillChar(IndentStr[1],length(IndentStr),' ');
    AddAtom(Result,LineEnd+IndentStr);
    AddAtom(Result,'begin');
    AddAtom(Result,LineEnd+LineEnd+IndentStr);
    AddAtom(Result,'end;');
  end;
writeln('[TBeautifyCodeOptions.BeautifyProc] Result="',Result,'"');
end;

function TBeautifyCodeOptions.BeautifyStatement(const AStatement: string;
  IndentSize: integer): string;
var CurAtom: string;
begin
  Src:=AStatement;
  UpperSrc:=UpperCaseStr(Src);
  SrcLen:=length(Src);
  if IndentSize>=LineLength-10 then IndentSize:=LineLength-10;
  SetLength(Result,IndentSize);
  if IndentSize>0 then
    FillChar(Result[1],length(Result),' ');
  SetLength(IndentStr,IndentSize+Indent);
  if length(IndentStr)>0 then
    FillChar(IndentStr[1],length(IndentStr),' ');
  CurPos:=1;
  LastSplitPos:=-1;
  CurLineLen:=length(Result);
  LastAtomType:=atNone;
  while (CurPos<=SrcLen) do begin
    ReadNextAtom;
    CurAtom:=copy(Src,AtomStart,AtomEnd-AtomStart);
    if ((CurAtomType in DoInsertSpaceBefore) and (not (LastAtomType=atSpace)))
    or ((CurAtomType<>atSpace) and (LastAtomType in DoInsertSpaceAfter)) then
      AddAtom(Result,' ');
    if (not (CurAtomType in DoNotSplitLineBefore))
    and (not (LastAtomType in DoNotSplitLineAfter)) then
      LastSplitPos:=length(Result)+1;
//writeln('  CurPos=',CurPos,' CurAtom="',CurAtom,
//'" CurAtomType=',AtomTypeNames[CurAtomType],' LastAtomType=',AtomTypeNames[LastAtomType],
//'  ',LastAtomType in DoInsertSpaceAfter);
    AddAtom(Result,CurAtom);
    LastAtomType:=CurAtomType;
  end;
writeln('[TBeautifyCodeOptions.BeautifyStatement] Result="',Result,'"');
end;

function TBeautifyCodeOptions.AddClassNameToProc(
  const AProcCode, AClassName: string): string;
var StartPos, NamePos, ProcLen: integer;
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
  if (StartPos<=ProcLen) and (AProcCode[StartPos]<>'.') then
    Result:=copy(AProcCode,1,NamePos-1)+AClassName+'.'
           +copy(AProcCode,NamePos,length(AProcCode)-NamePos+1)
  else
    Result:=AProcCode;
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

