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
    TCustomCodeTool is the ancestor class for code tools which parses code
    beginning with the Main Source code. It can parse atoms, the smallest code
    elements in source code, create new code tree nodes and provides several
    useful functions for parsing and changing code.

}
unit CustomCodeTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

{ $DEFINE ShowIgnoreError}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeToolsStrConsts, CodeTree, CodeAtom, SourceLog,
  KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, AVL_Tree, TypInfo,
  SourceChanger;

const
  CodeToolPhaseNone  = 0;
  CodeToolPhaseScan  = 1;
  CodeToolPhaseParse = 2;
  CodeToolPhaseTool  = 3; // or higher

type
  TCustomCodeTool = class(TObject)
  private
    FLastScannerChangeStep: integer;
    FScanner: TLinkScanner;
    FOnGetGlobalWriteLockInfo: TOnGetWriteLockInfo;
    FOnSetGlobalWriteLock: TOnSetWriteLock;
  protected
    FIgnoreErrorAfter: TCodePosition;
    KeyWordFuncList: TKeyWordFunctionList;
    FForceUpdateNeeded: boolean;
    function DefaultKeyWordFunc: boolean;
    procedure BuildDefaultKeyWordFunctions; virtual;
    procedure SetScanner(NewScanner: TLinkScanner); virtual;
    procedure DoDeleteNodes; virtual;
    procedure RaiseIdentExpectedButAtomFound;
    procedure RaiseBracketOpenExpectedButAtomFound;
    procedure RaiseBracketCloseExpectedButAtomFound;
    procedure SetIgnoreErrorAfter(const AValue: TCodePosition); virtual;
  protected
    LastErrorMessage: string;
    LastErrorCurPos: TAtomPosition;
    LastErrorPhase: integer;
    LastErrorValid: boolean;
    LastErrorBehindIgnorePosition: boolean;
    LastErrorCheckedForIgnored: boolean;
    CurrentPhase: integer;
    procedure RaiseException(const AMessage: string); virtual;
    procedure RaiseExceptionFmt(const AMessage: string;
      const args : array of const);
    procedure SaveRaiseException(const AMessage: string); virtual;
    procedure SaveRaiseExceptionFmt(const AMessage: string;
      const args : array of const);
    procedure ClearLastError;
    procedure RaiseLastError;
  public
    Tree: TCodeTree;

    // current Values, Position, Node ...
    CurPos: TAtomPosition;
    Src: string;
    UpperSrc: string;
    SrcLen: integer;
    CurNode: TCodeTreeNode;
    LastAtoms: TAtomRing;
    NextPos: TAtomPosition;
    
    CheckFilesOnDisk: boolean;
    IndentSize: integer;
    VisibleEditorLines: integer;
    JumpCentered: boolean;
    CursorBeyondEOL: boolean;
    
    ErrorPosition: TCodeXYPosition;
    
    property Scanner: TLinkScanner read FScanner write SetScanner;
    function MainFilename: string;
    
    function FindDeepestNodeAtPos(P: integer;
      ExceptionOnNotFound: boolean): TCodeTreeNode;
    function FindDeepestNodeAtPos(StartNode: TCodeTreeNode; P: integer;
      ExceptionOnNotFound: boolean): TCodeTreeNode;
    function CaretToCleanPos(Caret: TCodeXYPosition;
        var CleanPos: integer): integer;  // 0=valid CleanPos
              //-1=CursorPos was skipped, CleanPos between two links
              // 1=CursorPos beyond scanned code
              //-2=X,Y beyond source
    function CleanPosToCaret(CleanPos: integer;
        var Caret:TCodeXYPosition): boolean; // true=ok, false=invalid CleanPos
    function CleanPosToCaretAndTopLine(CleanPos: integer;
        var Caret:TCodeXYPosition; var NewTopLine: integer): boolean; // true=ok, false=invalid CleanPos
    procedure GetLineInfo(ACleanPos: integer;
        var ALineStart, ALineEnd, AFirstAtomStart, ALastAtomEnd: integer);
    function FindLineEndOrCodeAfterPosition(StartPos: integer): integer;
    function FindLineEndOrCodeInFrontOfPosition(StartPos: integer): integer;
    function FindLineEndOrCodeInFrontOfPosition(StartPos: integer;
        StopAtDirectives: boolean): integer;
    function FindFirstLineEndAfterInCode(StartPos: integer): integer;

    function UpdateNeeded(OnlyInterfaceNeeded: boolean): boolean;
    procedure BeginParsing(DeleteNodes, OnlyInterfaceNeeded: boolean); virtual;
    procedure BeginParsingAndGetCleanPos(DeleteNodes,
        OnlyInterfaceNeeded: boolean; CursorPos: TCodeXYPosition;
        var CleanCursorPos: integer);

    function StringIsKeyWord(const Word: string): boolean;
    
    procedure MoveCursorToNodeStart(ANode: TCodeTreeNode);
    procedure MoveCursorToCleanPos(ACleanPos: integer);
    procedure MoveCursorToCleanPos(ACleanPos: PChar);
    function IsPCharInSrc(ACleanPos: PChar): boolean;
    function ReadTilBracketClose(ExceptionOnNotFound: boolean): boolean;
    function ReadBackTilBracketOpen(ExceptionOnNotFound: boolean): boolean;
    function DoAtom: boolean; virtual;
    procedure ReadNextAtom;
    procedure UndoReadNextAtom;
    procedure ReadPriorAtom;
    function AtomIs(const AnAtom: shortstring): boolean;
    function UpAtomIs(const AnAtom: shortstring): boolean;
    function ReadNextAtomIs(const AnAtom: shortstring): boolean;
    function ReadNextUpAtomIs(const AnAtom: shortstring): boolean;
    function ReadNextAtomIsChar(const c: char): boolean;
    function AtomIsChar(const c: char): boolean;
    function AtomIsKeyWord: boolean;
    function AtomIsNumber: boolean;
    function AtomIsRealNumber: boolean;
    function AtomIsStringConstant: boolean;
    function AtomIsCharConstant: boolean;
    function AtomIsIdentifier(ExceptionOnNotFound: boolean): boolean;
    function LastAtomIs(BackIndex: integer;
        const AnAtom: shortstring): boolean; // 0=current, 1=prior current, ...
    function LastUpAtomIs(BackIndex: integer;
        const AnAtom: shortstring): boolean; // 0=current, 1=prior current, ...
    function GetAtom: string;
    function GetUpAtom: string;
    function GetAtom(Atom: TAtomPosition): string;
    function GetUpAtom(Atom: TAtomPosition): string;
    function CompareNodeIdentChars(ANode: TCodeTreeNode;
        const AnUpperIdent: string): integer;
    function CompareSrcIdentifiers(
      CleanStartPos1, CleanStartPos2: integer): boolean;
    function CompareSrcIdentifier(CleanStartPos: integer;
      const Identifier: string): boolean;
    function CompareSrcIdentifiers(Identifier1, Identifier2: PChar): boolean;
    function CompareSrcIdentifiers(CleanStartPos: integer;
      AnIdentifier: PChar): boolean;
    function ExtractIdentifier(CleanStartPos: integer): string;

    procedure CreateChildNode;
    procedure EndChildNode;
    
    procedure ActivateGlobalWriteLock; virtual;
    procedure DeactivateGlobalWriteLock; virtual;
    property OnGetGlobalWriteLockInfo: TOnGetWriteLockInfo
      read FOnGetGlobalWriteLockInfo write FOnGetGlobalWriteLockInfo;
    property OnSetGlobalWriteLock: TOnSetWriteLock
      read FOnSetGlobalWriteLock write FOnSetGlobalWriteLock;
      
    property IgnoreErrorAfter: TCodePosition
      read FIgnoreErrorAfter write SetIgnoreErrorAfter;
    procedure ClearIgnoreErrorAfter;
    function IgnoreErrAfterPositionIsInFrontOfLastErrMessage: boolean;
    function IgnoreErrorAfterValid: boolean;
    function IgnoreErrorAfterCleanedPos: integer;
    function LastErrorsInFrontOfCleanedPos(ACleanedPos: integer): boolean;
    procedure RaiseLastErrorIfInFrontOfCleanedPos(ACleanedPos: integer);

    procedure Clear; virtual;
    function NodeDescToStr(Desc: integer): string;
    function NodeSubDescToStr(Desc, SubDesc: integer): string;
    function ConsistencyCheck: integer; virtual; // 0 = ok
    procedure WriteDebugTreeReport;
    constructor Create;
    destructor Destroy; override;
  end;

  ECodeToolError = class(Exception)
    Sender: TCustomCodeTool;
    constructor Create(ASender: TCustomCodeTool; const AMessage: string);
  end;


implementation


{ TCustomCodeTool }

constructor TCustomCodeTool.Create;
begin
  inherited Create;
  Tree:=TCodeTree.Create;
  KeyWordFuncList:=TKeyWordFunctionList.Create;
  BuildDefaultKeyWordFunctions;
  LastAtoms:=TAtomRing.Create;
  IndentSize:=2;
  VisibleEditorLines:=20;
  CursorBeyondEOL:=true;
  FForceUpdateNeeded:=false;
  Clear;
end;

destructor TCustomCodeTool.Destroy;
begin
  Clear;
  LastAtoms.Free;
  Tree.Free;
  KeyWordFuncList.Free;
  inherited Destroy;
end;

procedure TCustomCodeTool.Clear;
begin
  if Tree<>nil then DoDeleteNodes;
  CurPos:=StartAtomPosition;
  LastAtoms.Clear;
  NextPos.StartPos:=-1;
  ClearLastError;
end;

procedure TCustomCodeTool.RaiseException(const AMessage: string);
var CaretXY: TCodeXYPosition;
  CursorPos: integer;
  Node: TCodeTreeNode;
begin
  ErrorPosition.Code:=nil;
  CursorPos:=CurPos.StartPos;
  // close all open nodes, so that FindDeepestNodeAtPos works in the code
  // already parsed
  Node:=CurNode;
  while (Node<>nil) do begin
    if (Node.StartPos>=Node.EndPos) then
      Node.EndPos:=CursorPos;
    Node:=Node.Parent;
  end;
  // convert cursor pos to caret pos, which is more human readable
  if (CursorPos>SrcLen) and (SrcLen>0) then CursorPos:=SrcLen;
  if (CleanPosToCaret(CursorPos,CaretXY))
  and (CaretXY.Code<>nil) then begin
    ErrorPosition:=CaretXY;
  end else if (Scanner<>nil) and (Scanner.MainCode<>nil) then begin
    ErrorPosition.Code:=TCodeBuffer(Scanner.MainCode);
    ErrorPosition.Y:=-1;
  end;
  // raise the exception
  CurrentPhase:=CodeToolPhaseNone;
  raise ECodeToolError.Create(Self,AMessage);
end;

procedure TCustomCodeTool.RaiseExceptionFmt(const AMessage: string;
  const args: array of const);
begin
  RaiseException(Format(AMessage,args));
end;

procedure TCustomCodeTool.SaveRaiseException(const AMessage: string);
begin
  LastErrorMessage:=AMessage;
  LastErrorCurPos:=CurPos;
  LastErrorPhase:=CurrentPhase;
  LastErrorValid:=true;
  RaiseException(AMessage);
end;

procedure TCustomCodeTool.SaveRaiseExceptionFmt(const AMessage: string;
  const args: array of const);
begin
  SaveRaiseException(Format(AMessage,args));
end;

procedure TCustomCodeTool.ClearLastError;
begin
  LastErrorPhase:=CodeToolPhaseNone;
  LastErrorValid:=false;
  LastErrorCheckedForIgnored:=false;
end;

procedure TCustomCodeTool.RaiseLastError;
begin
  CurPos:=LastErrorCurPos;
  CurNode:=nil;
  CurrentPhase:=LastErrorPhase;
writeln('TCustomCodeTool.RaiseLastError "',LastErrorMessage,'"');
  SaveRaiseException(LastErrorMessage);
end;

procedure TCustomCodeTool.SetScanner(NewScanner: TLinkScanner);
begin
  if NewScanner=FScanner then exit;
  LastErrorCheckedForIgnored:=false;
  Clear;
  FScanner:=NewScanner; begin
  if Scanner<>nil then
    FLastScannerChangeStep:=Scanner.ChangeStep;
    Scanner.SetIgnoreErrorAfter(IgnoreErrorAfter.P,IgnoreErrorAfter.Code);
  end;
  FForceUpdateNeeded:=true;
end;

function TCustomCodeTool.NodeDescToStr(Desc: integer): string;
begin
  Result:=NodeDescriptionAsString(Desc);
end;

function TCustomCodeTool.NodeSubDescToStr(Desc, SubDesc: integer): string;
begin
  if SubDesc<>0 then
    Result:=Format(ctsUnknownSubDescriptor,[IntToStr(SubDesc)])
  else
    Result:='';
  case Desc of
  ctnProcedure:
    begin
      if (SubDesc and ctnsForwardDeclaration)>0 then Result:=ctsForward;
    end;
  ctnProcedureHead, ctnBeginBlock:
    begin
      if (SubDesc and ctnsNeedJITParsing)>0 then Result:=ctsUnparsed;
    end;
  ctnClass:
    begin
      Result:='';
      if (SubDesc and ctnsForwardDeclaration)>0 then Result:=ctsForward;
      if (SubDesc and ctnsNeedJITParsing)>0 then Result:=Result+ctsUnparsed;
    end;
  end;
end;

function TCustomCodeTool.AtomIs(const AnAtom: shortstring): boolean;
var AnAtomLen,i : integer;
begin
  Result:=false;
  if (CurPos.StartPos<=SrcLen) and (CurPos.EndPos<=SrcLen+1)
  and (CurPos.StartPos>=1) then begin
    AnAtomLen:=length(AnAtom);
    if AnAtomLen=CurPos.EndPos-CurPos.StartPos then begin
      for i:=1 to AnAtomLen do
        if AnAtom[i]<>Src[CurPos.StartPos-1+i] then exit;
      Result:=true;
    end;
  end;
end;

function TCustomCodeTool.UpAtomIs(const AnAtom: shortstring): boolean;
var AnAtomLen,i : integer;
begin
  Result:=false;
  if (CurPos.StartPos<SrcLen) and (CurPos.EndPos<=SrcLen+1)
  and (CurPos.StartPos>=1) then begin
    AnAtomLen:=length(AnAtom);
    if AnAtomLen=CurPos.EndPos-CurPos.StartPos then begin
      for i:=1 to AnAtomLen do
        if AnAtom[i]<>UpperSrc[CurPos.StartPos-1+i] then exit;
      Result:=true;
    end;
  end;
end;

function TCustomCodeTool.ReadNextAtomIs(const AnAtom: shortstring): boolean;
begin
  ReadNextAtom;
  Result:=AtomIs(AnAtom);
end;

function TCustomCodeTool.ReadNextAtomIsChar(const c: char): boolean;
begin
  ReadNextAtom;
  Result:=AtomIsChar(c);
end;

function TCustomCodeTool.ReadNextUpAtomIs(const AnAtom: shortstring): boolean;
begin
  ReadNextAtom;
  Result:=UpAtomIs(AnAtom);
end;

function TCustomCodeTool.CompareNodeIdentChars(ANode: TCodeTreeNode;
  const AnUpperIdent: string): integer;
var AnIdentLen, i, NodeSrcLen, MinLen, p: integer;
begin
  if (ANode.StartPos<=SrcLen) and (ANode.EndPos<=SrcLen+1)
  and (ANode.StartPos>=1) then begin
    AnIdentLen:=length(AnUpperIdent);
    NodeSrcLen:=ANode.EndPos-ANode.StartPos;
    if AnIdentLen<NodeSrcLen then
      MinLen:=AnIdentLen
    else
      MinLen:=NodeSrcLen;
    i:=1;
    p:=ANode.StartPos-1+i;
    while (i<=MinLen) and (IsIdentChar[Src[p]]) do begin
      if AnUpperIdent[i]<>UpperSrc[p] then begin
        // identifiers different in one letter
        if UpperSrc[p]>AnUpperIdent[i] then
          Result:=-1
        else
          Result:=1;
        exit;
      end;
      inc(i);
      inc(p);
    end;
    if (i>MinLen) and (i>AnIdentLen) then begin
      // node is longer than AnUpperIdent
      if (i>NodeSrcLen) or (not IsIdentChar[Src[p]]) then
        // node identifier is equal to AnUpperIdent
        Result:=0
      else
        // node Identifier is longer than AnUpperIdent
        Result:=-1;
    end else
      // node identifier is shorter than AnUpperIdent
      Result:=1
  end else
    Result:=1;
end;

function TCustomCodeTool.CompareSrcIdentifiers(
  CleanStartPos1, CleanStartPos2: integer): boolean;
begin
  Result:=(CleanStartPos1>=1) and (CleanStartPos1<=SrcLen)
          and (CleanStartPos2>=1) and (CleanStartPos2<=SrcLen);
  if not Result then exit;
  while (CleanStartPos1<=SrcLen) and (IsIdentChar[Src[CleanStartPos1]]) do begin
    if (UpperSrc[CleanStartPos1]<>UpperSrc[CleanStartPos2]) then begin
      Result:=false;
      exit;
    end;
    inc(CleanStartPos1);
    inc(CleanStartPos2);
  end;
  Result:=(CleanStartPos2>SrcLen) or (not IsIdentChar[Src[CleanStartPos2]]);
end;

function TCustomCodeTool.AtomIsChar(const c: char): boolean;
begin
  Result:=(CurPos.StartPos<=SrcLen)
      and (CurPos.EndPos-CurPos.StartPos=1)
      and (Src[CurPos.StartPos]=c);
end;

function TCustomCodeTool.AtomIsKeyWord: boolean;
begin
  Result:=(CurPos.StartPos<=SrcLen)
      and (IsIdentStartChar[UpperSrc[CurPos.StartPos]])
      and (WordIsKeyWord.DoItUpperCase(UpperSrc,CurPos.StartPos,
             CurPos.EndPos-CurPos.StartPos));
end;

function TCustomCodeTool.AtomIsIdentifier(ExceptionOnNotFound: boolean):boolean;

  procedure RaiseIdentExpectedButEOFFound;
  begin
    SaveRaiseException(ctsIdentExpectedButEOFFound);
  end;

begin
  if CurPos.StartPos<=SrcLen then begin
    if IsIdentStartChar[UpperSrc[CurPos.StartPos]] then begin
      if not WordIsKeyWord.DoItUpperCase(UpperSrc,CurPos.StartPos,
             CurPos.EndPos-CurPos.StartPos) then
        Result:=true
      else begin
        if ExceptionOnNotFound then
          RaiseIdentExpectedButAtomFound
        else
          Result:=false;
      end;
    end else begin
      if ExceptionOnNotFound then
        RaiseIdentExpectedButAtomFound
      else
        Result:=false;
    end;
  end else begin
    if ExceptionOnNotFound then
      RaiseIdentExpectedButEOFFound
    else
      Result:=false;
  end;
end;

function TCustomCodeTool.AtomIsNumber: boolean;
begin
  Result:=(CurPos.StartPos<=SrcLen)
      and (Src[CurPos.StartPos] in ['0'..'9','%','$']);
end;

function TCustomCodeTool.AtomIsRealNumber: boolean;
var i: integer;
begin
  Result:=false;
  i:=CurPos.StartPos;
  if (i<=SrcLen) and (IsNumberChar[Src[i]]) then begin
    while (i<=SrcLen) and (IsNumberChar[Src[i]]) do
      inc(i);
    if (i<=SrcLen) and (Src[i]='.') then
      Result:=true;
  end;
end;

function TCustomCodeTool.AtomIsStringConstant: boolean;
begin
  Result:=(CurPos.StartPos<=SrcLen)
      and (Src[CurPos.StartPos] in ['''','#']);
end;

function TCustomCodeTool.AtomIsCharConstant: boolean;
var i: integer;
begin
  Result:=false;
  if (CurPos.StartPos<=SrcLen) then begin
    case Src[CurPos.StartPos] of
    
    '#':
      begin
        i:=CurPos.StartPos+1;
        if (i<=SrcLen) then begin
          if IsNumberChar[Src[i]] then begin
            // decimal
            while (i<=SrcLen) and (IsNumberChar[Src[i]]) do
              inc(i);
          end else if Src[i]='$' then begin
            // hexadecimal
            while (i<=SrcLen) and (IsHexNumberChar[Src[i]]) do
              inc(i);
          end;
          if (i<=SrcLen)
          and (not (Src[i] in ['''','#'])) then
            Result:=true;
        end;
      end;

    '''':
      begin
        if (CurPos.StartPos+2<=SrcLen) and (Src[CurPos.StartPos+1]<>'''')
        and (Src[CurPos.StartPos+2]='''') then begin
          // a single char
          if (CurPos.StartPos+2<SrcLen)
          and (not (Src[CurPos.StartPos+3] in ['''','#'])) then
            Result:=true;
        end;
      end;
      
    end;
  end;
end;

function TCustomCodeTool.LastAtomIs(BackIndex: integer;
  const AnAtom: shortstring): boolean;
var ap: TAtomPosition;
  AnAtomLen: integer;
  i: integer;
begin
  Result:=false;
  if (BackIndex>=0) and (BackIndex<LastAtoms.Count) then begin
    ap:=LastAtoms.GetValueAt(BackIndex);
    Result:=false;
    if (ap.StartPos<SrcLen) and (ap.EndPos<=SrcLen+1)
    and (ap.StartPos>=1) then begin
      AnAtomLen:=length(AnAtom);
      if AnAtomLen=ap.EndPos-ap.StartPos then begin
        for i:=1 to AnAtomLen do
          if AnAtom[i]<>Src[ap.StartPos-1+i] then exit;
        Result:=true;
      end;
    end;
  end;
end;

function TCustomCodeTool.LastUpAtomIs(BackIndex: integer;
  const AnAtom: shortstring): boolean;
var ap: TAtomPosition;
  AnAtomLen: integer;
  i: integer;
begin
  Result:=false;
  if (BackIndex>=0) and (BackIndex<LastAtoms.Count) then begin
    ap:=LastAtoms.GetValueAt(BackIndex);
    Result:=false;
    if (ap.StartPos<SrcLen) and (ap.EndPos<=SrcLen+1)
    and (ap.StartPos>=1) then begin
      AnAtomLen:=length(AnAtom);
      if AnAtomLen=ap.EndPos-ap.StartPos then begin
        for i:=1 to AnAtomLen do
          if AnAtom[i]<>UpperSrc[ap.StartPos-1+i] then exit;
        Result:=true;
      end;
    end;
  end;
end;

function TCustomCodeTool.GetAtom: string;
begin
  Result:=copy(Src,CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
end;

function TCustomCodeTool.GetUpAtom: string;
begin
  Result:=copy(UpperSrc,CurPos.StartPos,CurPos.EndPos-CurPos.StartPos);
end;

function TCustomCodeTool.GetAtom(Atom: TAtomPosition): string;
begin
  Result:=copy(Src,Atom.StartPos,Atom.EndPos-Atom.StartPos);
end;

function TCustomCodeTool.GetUpAtom(Atom: TAtomPosition): string;
begin
  Result:=copy(UpperSrc,Atom.StartPos,Atom.EndPos-Atom.StartPos);
end;

procedure TCustomCodeTool.ReadNextAtom;
var c1, c2: char;
  CommentLvl: integer;
begin
  {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
  {$R-}
  if (CurPos.StartPos<CurPos.EndPos) then
    LastAtoms.Add(CurPos);
  if NextPos.StartPos>=1 then begin
    CurPos:=NextPos;
    NextPos.StartPos:=-1;
    exit;
  end;
  CurPos.StartPos:=CurPos.EndPos;
  CurPos.Flag:=cafNone;
  if CurPos.StartPos>SrcLen then
    exit;
  // Skip all spaces and comments
  CommentLvl:=0;
  while true do begin
    case Src[CurPos.StartPos] of
    #0:
      if CurPos.StartPos>SrcLen then
        break
      else
        inc(CurPos.StartPos);
    #1..#32:
      inc(CurPos.StartPos);
    '{': // pascal comment
      begin
        CommentLvl:=1;
        inc(CurPos.StartPos);
        while (CurPos.StartPos<=SrcLen) and (CommentLvl>0) do begin
          case Src[CurPos.StartPos] of
          '{': if Scanner.NestedComments then inc(CommentLvl);
          '}': dec(CommentLvl);
          end;
          inc(CurPos.StartPos);
        end;
      end;
    '/':  // Delphi comment
      if (Src[CurPos.StartPos+1]<>'/') then begin
        break;
      end else begin
        inc(CurPos.StartPos,2);
        while (CurPos.StartPos<=SrcLen)
        and (not (Src[CurPos.StartPos] in [#10,#13])) do
          inc(CurPos.StartPos);
        inc(CurPos.StartPos);
        if (CurPos.StartPos<=SrcLen) and (Src[CurPos.StartPos] in [#10,#13])
        and (Src[CurPos.StartPos-1]<>Src[CurPos.StartPos]) then
          inc(CurPos.StartPos);
      end;
    '(': // old turbo pascal comment
      if (Src[CurPos.StartPos+1]<>'*') then begin
        break;
      end else begin
        inc(CurPos.StartPos,3);
        while (CurPos.StartPos<=SrcLen)
        and ((Src[CurPos.StartPos-1]<>'*') or (Src[CurPos.StartPos]<>')')) do
          inc(CurPos.StartPos);
        inc(CurPos.StartPos);
      end;
    else
      break;
    end;
  end;
  CurPos.EndPos:=CurPos.StartPos;
  if CurPos.StartPos>SrcLen then
    exit;
  // read atom
  c1:=UpperSrc[CurPos.EndPos];
  case c1 of
  '_','A'..'Z':
    begin
      inc(CurPos.EndPos);
      while (IsIdentChar[UpperSrc[CurPos.EndPos]]) do
        inc(CurPos.EndPos);
      CurPos.Flag:=cafWord;
      case c1 of
      'B':
        if (CurPos.EndPos-CurPos.StartPos=5)
        and UpAtomIs('BEGIN')
        then
          CurPos.Flag:=cafBegin;
      'E':
        if (CurPos.EndPos-CurPos.StartPos=3)
        and (UpperSrc[CurPos.StartPos+1]='N')
        and (UpperSrc[CurPos.StartPos+2]='D')
        then
          CurPos.Flag:=cafEnd;
      'R':
        if (CurPos.EndPos-CurPos.StartPos=7)
        and UpAtomIs('RECORD')
        then
          CurPos.Flag:=cafRecord;
      end;
    end;
  '''','#':
    begin
      while (CurPos.EndPos<=SrcLen) do begin
        case (Src[CurPos.EndPos]) of
        '#':
          begin
            inc(CurPos.EndPos);
            if (CurPos.EndPos<=SrcLen) then begin
              if (IsNumberChar[Src[CurPos.EndPos]]) then begin
                // decimal
                repeat
                  inc(CurPos.EndPos);
                until (CurPos.EndPos>SrcLen)
                      or (not IsNumberChar[Src[CurPos.EndPos]]);
              end else if Src[CurPos.EndPos]='$' then begin
                // hexadecimal
                repeat
                  inc(CurPos.EndPos);
                until (CurPos.EndPos>SrcLen)
                      or (not IsHexNumberChar[Src[CurPos.EndPos]]);
              end;
            end;
          end;
        '''':
          begin
            inc(CurPos.EndPos);
            while (CurPos.EndPos<=SrcLen) do begin
              case Src[CurPos.EndPos] of

              '''':
                begin
                  inc(CurPos.EndPos);
                  break;
                end;

              #10,#13:
                break;

              else
                inc(CurPos.EndPos);
              end;
            end;
          end;
        else
          break;
        end;
      end;
    end;
  '0'..'9':
    begin
      inc(CurPos.EndPos);
      while (CurPos.EndPos<=SrcLen) and (IsNumberChar[Src[CurPos.EndPos]]) do
        inc(CurPos.EndPos);
      if (CurPos.EndPos<SrcLen)
      and (Src[CurPos.EndPos]='.') and (Src[CurPos.EndPos+1]<>'.') then begin
        // real type number
        inc(CurPos.EndPos);
        while (CurPos.EndPos<=SrcLen) and (IsNumberChar[Src[CurPos.EndPos]])
        do
          inc(CurPos.EndPos);
        if (CurPos.EndPos<=SrcLen) and (UpperSrc[CurPos.EndPos]='E') then
        begin
          // read exponent
          inc(CurPos.EndPos);
          if (CurPos.EndPos<=SrcLen) and (Src[CurPos.EndPos] in ['-','+'])
          then inc(CurPos.EndPos);
          while (CurPos.EndPos<=SrcLen) and (IsNumberChar[Src[CurPos.EndPos]])
          do
            inc(CurPos.EndPos);
        end;
      end;
    end;
  '%':
    begin
      inc(CurPos.EndPos);
      while (CurPos.EndPos<=SrcLen) and (Src[CurPos.EndPos] in ['0'..'1']) do
        inc(CurPos.EndPos);
    end;
  '$':
    begin
      inc(CurPos.EndPos);
      while (CurPos.EndPos<=SrcLen)
      and (IsHexNumberChar[UpperSrc[CurPos.EndPos]]) do
        inc(CurPos.EndPos);
    end;
  ';':
    begin
      inc(CurPos.EndPos);
      CurPos.Flag:=cafSemicolon;
    end;
  ',':
    begin
      inc(CurPos.EndPos);
      CurPos.Flag:=cafComma;
    end;
  '=':
    begin
      inc(CurPos.EndPos);
      CurPos.Flag:=cafEqual;
    end;
  '(':
    begin
      inc(CurPos.EndPos);
      CurPos.Flag:=cafRoundBracketOpen;
    end;
  ')':
    begin
      inc(CurPos.EndPos);
      CurPos.Flag:=cafRoundBracketClose;
    end;
  '[':
    begin
      inc(CurPos.EndPos);
      CurPos.Flag:=cafEdgedBracketOpen;
    end;
  ']':
    begin
      inc(CurPos.EndPos);
      CurPos.Flag:=cafEdgedBracketClose;
    end;
  ':':
    begin
      inc(CurPos.EndPos);
      if (Src[CurPos.EndPos]<>'=') then begin
        CurPos.Flag:=cafColon;
      end else begin
        // :=
        inc(CurPos.EndPos);
      end;
    end;
  '.':
    begin
      inc(CurPos.EndPos);
      if (Src[CurPos.EndPos]<>'.') then begin
        CurPos.Flag:=cafPoint;
      end else begin
        // ..
        inc(CurPos.EndPos);
      end;
    end;
  else
    inc(CurPos.EndPos);
    c2:=Src[CurPos.EndPos];
    // test for double char operators :=, +=, -=, /=, *=, <>, <=, >=, **, ><
    if ((c2='=') and  (IsEqualOperatorStartChar[c1]))
    or ((c1='<') and (c2='>')) // not equal
    or ((c1='>') and (c2='<'))
    or ((c1='.') and (c2='.')) // subrange
    or ((c1='*') and (c2='*'))
    then inc(CurPos.EndPos);
    if ((c1='@') and (c2='@')) then begin
      repeat
        inc(CurPos.EndPos);
      until (CurPos.EndPos>SrcLen) or (not IsIdentChar[Src[CurPos.EndPos]]);
    end;
  end;
  {$IFDEF RangeChecking}{$R+}{$ENDIF}
end;

procedure TCustomCodeTool.ReadPriorAtom;
var
 CommentLvl, PrePos, OldPrePos: integer;
 IsStringConstant: boolean;

  procedure ReadStringConstantBackward;
  var PrePos: integer;
  begin
    while (CurPos.StartPos>1) do begin
      case UpperSrc[CurPos.StartPos-1] of
      '''':
        begin
          dec(CurPos.StartPos);
          repeat
            dec(CurPos.StartPos);
          until (CurPos.StartPos<1) or (Src[CurPos.StartPos]='''');
        end;
      '0'..'9','A'..'Z':
        begin
          // test if char constant
          PrePos:=CurPos.StartPos-1;
          while (PrePos>1) and (IsHexNumberChar[Src[PrePos]]) do
            dec(PrePos);
          if (PrePos<1) then break;
          if (Src[PrePos]='$') then begin
            dec(PrePos);
            if (PrePos<1) then break;
          end;
          if (Src[PrePos]='#') then
            CurPos.StartPos:=PrePos
          else
            break;
        end;
      else
        break;
      end;
    end;
  end;
  
  procedure ReadBackTilCodeLineEnd;
  begin
    dec(CurPos.StartPos);
    if (CurPos.StartPos>=1) and (Src[CurPos.StartPos] in [#10,#13])
    and (Src[CurPos.StartPos+1]<>Src[CurPos.StartPos]) then
      dec(CurPos.StartPos);

    // read backwards till line start
    PrePos:=CurPos.StartPos;
    while (PrePos>=1) and (not (Src[PrePos] in [#10,#13])) do
      dec(PrePos);
    // read line forward to find out,
    // if line ends in comment or string constant
    IsStringConstant:=false;
    repeat
      inc(PrePos);
      case Src[PrePos] of

      '/':
        if Src[PrePos+1]='/' then begin
          // this was a delphi comment -> skip comment
          CurPos.StartPos:=PrePos-1;
          break;
        end;

      '{':
        begin
          // skip pascal comment
          CommentLvl:=1;
          inc(PrePos);
          while (PrePos<=CurPos.StartPos) do begin
            case Src[PrePos] of
            '{': if Scanner.NestedComments then inc(CommentLvl);
            '}':
              begin
                dec(CommentLvl);
                if CommentLvl=0 then break;
              end;
            end;
            inc(PrePos);
          end;
        end;

      '(':
        begin
          if Src[PrePos+1]='*' then begin
            // skip turbo pascal comment
            inc(PrePos,2);
            while (PrePos<CurPos.StartPos)
            and ((Src[PrePos]<>'*') or (Src[PrePos+1]<>')')) do
              inc(PrePos);
            inc(PrePos);
          end;
        end;

      '''':
        begin
          // a string constant -> skip it
          OldPrePos:=PrePos;
          repeat
            inc(PrePos);
            case Src[PrePos] of

            '''':
              break;

            #10,#13:
              begin
                // string constant right border is the line end
                // -> last atom of line found
                IsStringConstant:=true;
                break;
              end;

            end;
          until false;
          if IsStringConstant then break;
        end;

      #10,#13:
        // no comment and no string constant found
        break;

      end;
    until PrePos>=CurPos.StartPos;
  end;

type
  TNumberType = (ntDecimal, ntHexadecimal, ntBinary, ntIdentifier,
    ntCharConstant, ntFloat, ntFloatWithExponent);
  TNumberTypes = set of TNumberType;

const
  AllNumberTypes: TNumberTypes = [ntDecimal, ntHexadecimal, ntBinary,
    ntIdentifier, ntCharConstant, ntFloat, ntFloatWithExponent];

var c1, c2: char;
  ForbiddenNumberTypes: TNumberTypes;
begin
  if LastAtoms.Count>0 then begin
    UndoReadNextAtom;
    exit;
  end;
  NextPos:=CurPos;
  // Skip all spaces and comments
  CommentLvl:=0;
  dec(CurPos.StartPos);
  CurPos.Flag:=cafNone;
  IsStringConstant:=false;
  OldPrePos:=0;
  while CurPos.StartPos>=1 do begin
    if IsCommentEndChar[Src[CurPos.StartPos]] then begin
      case Src[CurPos.StartPos] of
      
      '}': // pascal comment
        begin
          CommentLvl:=1;
          dec(CurPos.StartPos);
          while (CurPos.StartPos>=1) and (CommentLvl>0) do begin
            case Src[CurPos.StartPos] of
            '}': if Scanner.NestedComments then inc(CommentLvl);
            '{': dec(CommentLvl);
            end;
            dec(CurPos.StartPos);
          end;
        end;
        
      #10,#13: // possible Delphi comment
        ReadBackTilCodeLineEnd;

      ')': // old turbo pascal comment
        if (CurPos.StartPos>1) and (Src[CurPos.StartPos-1]='*') then begin
          dec(CurPos.StartPos,3);
          while (CurPos.StartPos>=1)
          and ((Src[CurPos.StartPos]<>'(') or (Src[CurPos.StartPos+1]<>'*')) do
            dec(CurPos.StartPos);
          dec(CurPos.StartPos);
        end else
          break;
          
      end;
    end else if IsSpaceChar[Src[CurPos.StartPos]] then begin
      repeat
        dec(CurPos.StartPos);
      until (CurPos.StartPos<1) or (Src[CurPos.StartPos] in [#10,#13])
      or (not (IsSpaceChar[Src[CurPos.StartPos]]));
    end else begin
      break;
    end;
  end;
  // CurPos.StartPos now points to the last char of the prior atom
  CurPos.EndPos:=CurPos.StartPos+1;
  if CurPos.StartPos<1 then
    exit;
  // read atom
  if IsStringConstant then begin
    CurPos.StartPos:=OldPrePos;
    if (CurPos.StartPos>1) and (Src[CurPos.StartPos-1]='''') then begin
      ReadStringConstantBackward;
    end;
    exit;
  end;
  c2:=UpperSrc[CurPos.StartPos];
  case c2 of
    '_','A'..'Z':
      begin
        // identifier or keyword or hexnumber
        while (CurPos.StartPos>1) do begin
          if (IsIdentChar[UpperSrc[CurPos.StartPos-1]]) then
            dec(CurPos.StartPos)
          else begin
            case Src[CurPos.StartPos-1] of
            '@':
              // assembler label
              if (CurPos.StartPos>2)
              and (Src[CurPos.StartPos-2]='@') then
                dec(CurPos.StartPos,2);
            '$':
              // hex number
              dec(CurPos.StartPos);
            else
              WordToAtomFlag.DoItUpperCase(UpperSrc,CurPos.StartPos,
                                           CurPos.EndPos-CurPos.StartPos);
              CurPos.Flag:=WordToAtomFlag.Flag;
              if CurPos.Flag=cafNone then
                CurPos.Flag:=cafWord;
            end;
            break;
          end;
        end;
      end;
    '''':
      begin
        inc(CurPos.StartPos);
        ReadStringConstantBackward;
      end;
    '0'..'9':
      begin
        // could be a decimal number, an identifier, a hex number,
        // a binary number, a char constant, a float, a float with exponent
        ForbiddenNumberTypes:=[];
        while true do begin
          case UpperSrc[CurPos.StartPos] of
          '0'..'1':
            ;
          '2'..'9':
            ForbiddenNumberTypes:=ForbiddenNumberTypes+[ntBinary];
          'A'..'D','F':
            ForbiddenNumberTypes:=ForbiddenNumberTypes
               +[ntBinary,ntDecimal,ntCharConstant,ntFloat,ntFloatWithExponent];
          'E':
            ForbiddenNumberTypes:=ForbiddenNumberTypes
               +[ntBinary,ntDecimal,ntCharConstant,ntFloat];
          'G'..'Z','_':
            ForbiddenNumberTypes:=AllNumberTypes-[ntIdentifier];
          '.':
            begin
              // could be the point of a float
              if (ntFloat in ForbiddenNumberTypes)
              or (CurPos.StartPos<=1) or (Src[CurPos.StartPos-1]='.') then begin
                inc(CurPos.StartPos);
                break;
              end;
              dec(CurPos.StartPos);
              // this was the part of a float after the point
              //  -> read decimal in front
              ForbiddenNumberTypes:=AllNumberTypes-[ntDecimal];
            end;
          '+','-':
            begin
              // could be part of an exponent
              if (ntFloatWithExponent in ForbiddenNumberTypes)
              or (CurPos.StartPos<=1) or (UpperSrc[CurPos.StartPos-1]<>'E') then
              begin
                inc(CurPos.StartPos);
                break;
              end;
              dec(CurPos.StartPos);
              // this was the exponent of a float -> read the float
              ForbiddenNumberTypes:=AllNumberTypes-[ntFloat];
            end;
          '#': // char constant found
            begin
              if (ntCharConstant in ForbiddenNumberTypes) then
                inc(CurPos.StartPos);
              ReadStringConstantBackward;
              break;
            end;
          '$':
            begin
              // hexadecimal number found
              if (ntHexadecimal in ForbiddenNumberTypes) then
                inc(CurPos.StartPos);
              break;
            end;
          '%':
            begin
              // binary number found
              if (ntBinary in ForbiddenNumberTypes) then
                inc(CurPos.StartPos);
              break;
            end;
          '@':
            begin
              if (CurPos.StartPos=1) or (Src[CurPos.StartPos-1]<>'@')
              or (([ntIdentifier,ntDecimal]*ForbiddenNumberTypes)=[]) then
                // atom start found
                inc(CurPos.StartPos)
              else
                // label found
                dec(CurPos.StartPos);
              break;
            end;
          else
            begin
              inc(CurPos.StartPos);
              break;
            end;
          end;
          if ForbiddenNumberTypes=AllNumberTypes then begin
            inc(CurPos.StartPos);
            break;
          end;
          if CurPos.StartPos<=1 then break;
          dec(CurPos.StartPos);
        end;
        if IsIdentStartChar[Src[CurPos.StartPos]] then begin
          // it is an identifier
          WordToAtomFlag.DoItUpperCase(UpperSrc,CurPos.StartPos,
                                       CurPos.EndPos-CurPos.StartPos);
          CurPos.Flag:=WordToAtomFlag.Flag;
          if CurPos.Flag=cafNone then
            CurPos.Flag:=cafWord;
        end;
      end;
      
    ';': CurPos.Flag:=cafSemicolon;
    ':': CurPos.Flag:=cafColon;
    ',': CurPos.Flag:=cafComma;
    '(': CurPos.Flag:=cafRoundBracketOpen;
    ')': CurPos.Flag:=cafRoundBracketClose;
    '[': CurPos.Flag:=cafEdgedBracketOpen;
    ']': CurPos.Flag:=cafEdgedBracketClose;

    else
      begin
        case c2 of
        '=': CurPos.Flag:=cafEqual;
        '.': CurPos.Flag:=cafPoint;
        end;
        if CurPos.StartPos>1 then begin
          c1:=Src[CurPos.StartPos-1];
          // test for double char operators :=, +=, -=, /=, *=, <>, <=, >=, **, ><
          if ((c2='=') and  (IsEqualOperatorStartChar[c1]))
          or ((c1='<') and (c2='>'))
          or ((c1='>') and (c2='<'))
          or ((c1='.') and (c2='.'))
          or ((c1='*') and (c2='*'))
          or ((c1='@') and (c2='@'))
          then begin
            dec(CurPos.StartPos);
            CurPos.Flag:=cafNone;
          end;
        end;
      end;
  end;
end;

procedure TCustomCodeTool.UndoReadNextAtom;

  procedure RaiseUndoImpossible;
  begin
    RaiseException('TCustomCodeTool.UndoReadNextAtom impossible');
  end;

begin
  if LastAtoms.Count>0 then begin
    NextPos:=CurPos;
    CurPos:=LastAtoms.GetValueAt(0);
    LastAtoms.UndoLastAdd;
  end else
    RaiseUndoImpossible;
end;

function TCustomCodeTool.ReadTilBracketClose(
  ExceptionOnNotFound: boolean): boolean;
// reads code brackets (not comment brackets)
var CloseBracket, AntiCloseBracket: TCommonAtomFlag;
  Start: TAtomPosition;
  
  procedure RaiseBracketNotFound;
  begin
    if CloseBracket=cafRoundBracketOpen then
      SaveRaiseExceptionFmt(ctsBracketNotFound,['('])
    else
      SaveRaiseExceptionFmt(ctsBracketNotFound,['[']);
  end;
  
begin
  Result:=false;
  if (Curpos.Flag=cafRoundBracketOpen) then begin
    CloseBracket:=cafRoundBracketClose;
    AntiCloseBracket:=cafEdgedBracketClose;
  end else if (Curpos.Flag=cafEdgedBracketOpen) then begin
    CloseBracket:=cafEdgedBracketClose;
    AntiCloseBracket:=cafRoundBracketClose;
  end else begin
    if ExceptionOnNotFound then
      RaiseBracketOpenExpectedButAtomFound;
    exit;
  end;
  Start:=CurPos;
  repeat
    ReadNextAtom;
    if (CurPos.Flag=CloseBracket) then break;
    if (CurPos.StartPos>SrcLen)
    or (CurPos.Flag in [cafEnd,cafRecord,AntiCloseBracket])
    then begin
      CurPos:=Start;
      if ExceptionOnNotFound then begin
        RaiseBracketNotFound;
      end;
      exit;
    end;
    if (CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen]) then begin
      if not ReadTilBracketClose(ExceptionOnNotFound) then exit;
    end;
  until false;
  Result:=true;
end;

function TCustomCodeTool.ReadBackTilBracketOpen(
  ExceptionOnNotFound: boolean): boolean;
// reads code brackets (not comment brackets)
var OpenBracket, AntiOpenBracket: TCommonAtomFlag;
  Start: TAtomPosition;
  
  procedure RaiseBracketNotFound;
  begin
    if OpenBracket=cafRoundBracketOpen then
      SaveRaiseExceptionFmt(ctsBracketNotFound,['('])
    else
      SaveRaiseExceptionFmt(ctsBracketNotFound,['[']);
  end;
  
begin
  Result:=false;
  if (CurPos.Flag=cafRoundBracketClose) then begin
    OpenBracket:=cafRoundBracketOpen;
    AntiOpenBracket:=cafEdgedBracketOpen;
  end else if (CurPos.Flag=cafEdgedBracketClose) then begin
    OpenBracket:=cafEdgedBracketOpen;
    AntiOpenBracket:=cafRoundBracketOpen;
  end else begin
    if ExceptionOnNotFound then
      RaiseBracketCloseExpectedButAtomFound;
    exit;
  end;
  Start:=CurPos;
  repeat
    ReadPriorAtom;
    if (CurPos.Flag=OpenBracket) then break;
    if (CurPos.StartPos<1)
    or (CurPos.Flag in [AntiOpenBracket,cafEND,cafBegin]) then begin
      CurPos:=Start;
      if ExceptionOnNotFound then
        RaiseBracketNotFound;
      exit;
    end;
    if CurPos.Flag in [cafRoundBracketClose,cafEdgedBracketClose] then begin
      if not ReadBackTilBracketOpen(ExceptionOnNotFound) then exit;
    end;
  until false;
  Result:=true;
end;

procedure TCustomCodeTool.BeginParsing(DeleteNodes,
  OnlyInterfaceNeeded: boolean);
begin
  // scan
  CurrentPhase:=CodeToolPhaseScan;
  try
    Scanner.Scan(OnlyInterfaceNeeded,CheckFilesOnDisk);
    // update scanned code
    if FLastScannerChangeStep<>Scanner.ChangeStep then begin
      // code has changed
      ClearLastError;
      FLastScannerChangeStep:=Scanner.ChangeStep;
      Src:=Scanner.CleanedSrc;
      UpperSrc:=UpperCaseStr(Src);
      SrcLen:=length(Src);
      FForceUpdateNeeded:=true;
      if DeleteNodes then DoDeleteNodes;
    end else begin
      if LastErrorPhase=CodeToolPhaseScan then
        RaiseLastError;
    end;
    // init parsing values
    CurPos:=StartAtomPosition;
    LastAtoms.Clear;
    NextPos.StartPos:=-1;
    CurNode:=nil;
  finally
    CurrentPhase:=CodeToolPhaseNone;
  end;
end;

procedure TCustomCodeTool.BeginParsingAndGetCleanPos(DeleteNodes,
  OnlyInterfaceNeeded: boolean; CursorPos: TCodeXYPosition;
  var CleanCursorPos: integer);
var Dummy: integer;
begin
  if UpdateNeeded(OnlyInterfaceNeeded) then
    BeginParsing(DeleteNodes,OnlyInterfaceNeeded);
  // find the CursorPos in cleaned source
  Dummy:=CaretToCleanPos(CursorPos, CleanCursorPos);
  if (Dummy<>0) and (Dummy<>-1) then
    RaiseException(ctsCursorPosOutsideOfCode);
end;

function TCustomCodeTool.IgnoreErrAfterPositionIsInFrontOfLastErrMessage: boolean;
var
  IgnoreErrorAfterCleanPos: integer;
begin
  //writeln('TCustomCodeTool.IgnoreErrAfterPositionIsInFrontOfLastErrMessage ',
  //  ' LastErrorCheckedForIgnored=',LastErrorCheckedForIgnored,
  //  ' LastErrorBehindIgnorePosition=',LastErrorBehindIgnorePosition);
  if LastErrorCheckedForIgnored then begin
    Result:=LastErrorBehindIgnorePosition;
  end else begin
    if (Scanner<>nil) then begin
      IgnoreErrorAfterCleanPos:=Scanner.IgnoreErrorAfterCleanedPos;
      //writeln('  IgnoreErrorAfterCleanPos=',IgnoreErrorAfterCleanPos,
      //  ' LastErrorCurPos.EndPos=',LastErrorCurPos.EndPos,
      //  ' LastErrorPhase>CodeToolPhaseParse=',LastErrorPhase>CodeToolPhaseParse);
      if IgnoreErrorAfterCleanPos>0 then begin
        // ignore position in scanned code
        // -> check if last error behind ignore position
        if (not LastErrorValid)
        or (IgnoreErrorAfterCleanPos<=LastErrorCurPos.EndPos) then
          Result:=true
        else
          Result:=false;
      end else
        Result:=false;
    end else
      Result:=false;
    LastErrorBehindIgnorePosition:=Result;
    LastErrorCheckedForIgnored:=true;
  end;
  {$IFDEF ShowIgnoreErrorAfter}
  writeln('TCustomCodeTool.IgnoreErrAfterPositionIsInFrontOfLastErrMessage ',Result);
  {$ENDIF}
end;

function TCustomCodeTool.IgnoreErrorAfterValid: boolean;
begin
  Result:=(Scanner<>nil) and (Scanner.IgnoreErrorAfterValid);
  {$IFDEF ShowIgnoreErrorAfter}
  writeln('TCustomCodeTool.IgnoreErrorAfterValid ',Result);
  {$ENDIF}
end;

function TCustomCodeTool.IgnoreErrorAfterCleanedPos: integer;
begin
  if Scanner<>nil then
    Result:=Scanner.IgnoreErrorAfterCleanedPos
  else
    Result:=-1;
  {$IFDEF ShowIgnoreErrorAfter}
  writeln('TCustomCodeTool.IgnoreErrorAfterCleanedPos ',Result);
  {$ENDIF}
end;

function TCustomCodeTool.LastErrorsInFrontOfCleanedPos(ACleanedPos: integer
  ): boolean;
begin
  if (Scanner<>nil) and Scanner.LastErrorsInFrontOfCleanedPos(ACleanedPos)
  then
    Result:=true
  else if (LastErrorValid)
  and (LastErrorCurPos.EndPos<=ACleanedPos) then
    Result:=true
  else
    Result:=false;
  {$IFDEF ShowIgnoreErrorAfter}
  writeln('TCustomCodeTool.LastErrorsInFrontOfCleanedPos ACleanedPos=',ACleanedPos,
    Result);
  {$ENDIF}
end;

procedure TCustomCodeTool.RaiseLastErrorIfInFrontOfCleanedPos(
  ACleanedPos: integer);
begin
  {$IFDEF ShowIgnoreErrorAfter}
  writeln('TCustomCodeTool.RaiseLastErrorIfInFrontOfCleanedPos A ACleanedPos=',ACleanedPos,
    ' ');
  {$ENDIF}
  if Scanner<>nil then Scanner.RaiseLastErrorIfInFrontOfCleanedPos(ACleanedPos);
  //writeln('TCustomCodeTool.RaiseLastErrorIfInFrontOfCleanedPos B ',LastErrorPhase<CodeToolPhaseTool,' ',LastErrorCurPos.EndPos);
  if LastErrorValid
  and (LastErrorCurPos.EndPos<=ACleanedPos) then
    RaiseLastError;
  //writeln('TCustomCodeTool.RaiseLastErrorIfInFrontOfCleanedPos END ');
end;

function TCustomCodeTool.StringIsKeyWord(const Word: string): boolean;
begin
  Result:=(Word<>'') and IsIdentStartChar[Word[1]]
                           and WordIsKeyWord.DoItUpperCase(Word,1,length(Word));
end;

procedure TCustomCodeTool.MoveCursorToNodeStart(ANode: TCodeTreeNode);
begin
  MoveCursorToCleanPos(ANode.StartPos);
  CurNode:=ANode;
end;

procedure TCustomCodeTool.MoveCursorToCleanPos(ACleanPos: integer);
begin
  CurPos.StartPos:=ACleanPos;
  CurPos.EndPos:=ACleanPos;
  CurPos.Flag:=cafNone;
  LastAtoms.Clear;
  NextPos.StartPos:=-1;
  CurNode:=nil;
end;

procedure TCustomCodeTool.MoveCursorToCleanPos(ACleanPos: PChar);

  procedure RaiseSrcEmpty;
  begin
    RaiseException('[TCustomCodeTool.MoveCursorToCleanPos - PChar] Src empty');
  end;
  
  procedure RaiseNotInSrc;
  begin
    RaiseException('[TCustomCodeTool.MoveCursorToCleanPos - PChar] '
      +'CleanPos not in Src');
  end;

var NewPos: integer;
begin
  if Src='' then
    RaiseSrcEmpty;
  NewPos:=Integer(ACleanPos)-Integer(@Src[1])+1;
  if (NewPos<1) or (NewPos>SrcLen) then
    RaiseNotInSrc;
  MoveCursorToCleanPos(NewPos);
end;

function TCustomCodeTool.IsPCharInSrc(ACleanPos: PChar): boolean;
var NewPos: integer;
begin
  Result:=false;
  if Src='' then exit;
  NewPos:=Integer(ACleanPos)-Integer(@Src[1])+1;
  if (NewPos<1) or (NewPos>SrcLen) then exit;
  Result:=true;
end;

procedure TCustomCodeTool.CreateChildNode;
var NewNode: TCodeTreeNode;
begin
  NewNode:=NodeMemManager.NewNode;
  Tree.AddNodeAsLastChild(CurNode,NewNode);
  CurNode:=NewNode;
  CurNode.StartPos:=CurPos.StartPos;
end;

procedure TCustomCodeTool.EndChildNode;
begin
  CurNode:=CurNode.Parent;
end;

procedure TCustomCodeTool.BuildDefaultKeyWordFunctions;
begin
  KeyWordFuncList.Clear;
  KeyWordFuncList.DefaultKeyWordFunction:=
    {$ifdef FPC}@{$endif}DefaultKeyWordFunc;
end;

function TCustomCodeTool.DoAtom: boolean;
begin
  if (CurPos.StartPos>SrcLen) or (CurPos.EndPos<=CurPos.StartPos) then
    Result:=false
  else if IsIdentStartChar[Src[CurPos.StartPos]] then
    Result:=KeyWordFuncList.DoItUppercase(UpperSrc,CurPos.StartPos,
            CurPos.EndPos-CurPos.StartPos)
  else
    Result:=true;
end;

procedure TCustomCodeTool.SetIgnoreErrorAfter(const AValue: TCodePosition);
begin
  if (IgnoreErrorAfter.Code=AValue.Code)
  and (IgnoreErrorAfter.P=AValue.P) then exit;
  FIgnoreErrorAfter:=AValue;
  LastErrorCheckedForIgnored:=false;
  {$IFDEF ShowIgnoreErrorAfter}
  write('TCustomCodeTool.SetIgnoreErrorAfter ');
  if FIgnoreErrorAfter.Code<>nil then
    write(FIgnoreErrorAfter.Code.Filename)
  else
    write('nil');
  write(' ',FIgnoreErrorAfter.P);
  writeln('');
  {$ENDIF}
  if Scanner<>nil then
    Scanner.SetIgnoreErrorAfter(IgnoreErrorAfter.P,IgnoreErrorAfter.Code);
end;

function TCustomCodeTool.DefaultKeyWordFunc: boolean;
begin
  Result:=true;
end;

function TCustomCodeTool.ConsistencyCheck: integer;
// 0 = ok
begin
  Result:=Tree.ConsistencyCheck;
  if Result<>0 then begin
    dec(Result,100);  exit;
  end;
  Result:=0;
end;

procedure TCustomCodeTool.WriteDebugTreeReport;

  procedure WriteSrcSubString(A,Len: integer);
  var i: integer;
  begin
    write('"');
    for i:=A to A+Len-1 do begin
      if (i>0) and (i<SrcLen) and (ord(Src[i])>31) then
        write(Src[i]);
    end;
    write('"');
  end;

  procedure WriteSubTree(RootNode: TCodeTreeNode; Indent: string);
  begin
    while RootNode<>nil do begin
      write(Indent);
      with RootNode do begin
        write(NodeDescToStr(Desc),'(',NodeSubDescToStr(Desc,SubDesc),')  ');
        write(' Start=',StartPos,' ');
        WriteSrcSubString(StartPos,5);
        write(' End=',EndPos,' ');
        WriteSrcSubString(EndPos-5,5);
        {$ifdef fpc}
        write(' Self=',HexStr(Cardinal(RootNode),8));
        write(' P=',HexStr(Cardinal(Parent),8));
        write(' NB=',HexStr(Cardinal(NextBrother),8));
        //write(' PB=',HexStr(Cardinal(PriorBrother),8));
        //write(' FC=',HexStr(Cardinal(FirstChild),8));
        //write(' LC=',HexStr(Cardinal(LastChild),8));
        {$endif}
      end;
      writeln('');
      WriteSubTree(RootNode.FirstChild,Indent+'  ');
      RootNode:=RootNode.NextBrother;
    end;
  end;

begin
  writeln('[TCustomCodeTool.WriteDebugTreeReport] Consistency=',
     ConsistencyCheck);
  WriteSubTree(Tree.Root,'  ');
end;

function TCustomCodeTool.FindDeepestNodeAtPos(P: integer;
  ExceptionOnNotFound: boolean): TCodeTreeNode;
begin
  Result:=FindDeepestNodeAtPos(Tree.Root,P,ExceptionOnNotFound);
end;

function TCustomCodeTool.FindDeepestNodeAtPos(StartNode: TCodeTreeNode;
  P: integer; ExceptionOnNotFound: boolean): TCodeTreeNode;
  
  procedure RaiseNoNodeFoundAtCursor;
  begin
    SaveRaiseException(ctsNoNodeFoundAtCursor);
  end;
  
begin
  if StartNode<>nil then begin
//writeln('SearchInNode ',NodeDescriptionAsString(ANode.Desc),
//',',ANode.StartPos,',',ANode.EndPos,', p=',p,
//' "',copy(Src,ANode.StartPos,4),'" - "',copy(Src,ANode.EndPos-5,4),'"');
    if (StartNode.StartPos<=P)
    and ((StartNode.EndPos>P) or (StartNode.EndPos<1)) then begin
      // StartNode contains P
      // -> search for a child that contains P
      Result:=FindDeepestNodeAtPos(StartNode.FirstChild,P,false);
      if Result=nil then begin
        // no child found -> search in nextbrothers that contains P
        while (StartNode.NextBrother<>nil)
        and (StartNode.NextBrother.StartPos<=P) do
          StartNode:=StartNode.NextBrother;
        Result:=StartNode;
      end;
    end else
      // search in next node
      Result:=FindDeepestNodeAtPos(StartNode.NextBrother,P,false);
  end else
    Result:=nil;
  if (Result=nil) and ExceptionOnNotFound then begin
    MoveCursorToCleanPos(P);
    RaiseNoNodeFoundAtCursor;
  end;
end;

function TCustomCodeTool.CaretToCleanPos(Caret: TCodeXYPosition;
  var CleanPos: integer): integer;
begin
//writeln('TCustomCodeTool.CaretToCleanPos A ',Caret.Code.Filename,' ',Caret.Code.SourceLength);
  Caret.Code.LineColToPosition(Caret.Y,Caret.X,CleanPos);
//writeln('TCustomCodeTool.CaretToCleanPos B ',CleanPos,',',Caret.Y,',',Caret.X);
  if (CleanPos>=1) then
    Result:=Scanner.CursorToCleanPos(CleanPos,Caret.Code,CleanPos)
  else
    Result:=-2; // x,y beyond source
//writeln('TCustomCodeTool.CaretToCleanPos C CleanPos=',CleanPos,' Result=',Result);
end;

function TCustomCodeTool.CleanPosToCaret(CleanPos: integer;
  var Caret:TCodeXYPosition): boolean; // true=ok, false=invalid CleanPos
var p: integer;
  Code: Pointer;
begin
  Result:=Scanner.CleanedPosToCursor(CleanPos,p,Code);
  if Result then begin
    Caret.Code:=TCodeBuffer(Code);
    TCodeBuffer(Code).AbsoluteToLineCol(p,Caret.Y,Caret.X);
    Result:=(Caret.Y>=0);
  end;
end;

function TCustomCodeTool.CleanPosToCaretAndTopLine(CleanPos: integer;
  var Caret:TCodeXYPosition; var NewTopLine: integer): boolean;
// true=ok, false=invalid CleanPos
begin
  Result:=CleanPosToCaret(CleanPos,Caret);
  if Result then begin
    if JumpCentered then begin
      NewTopLine:=Caret.Y-(VisibleEditorLines shr 1);
      if NewTopLine<1 then NewTopLine:=1;
    end else
      NewTopLine:=Caret.Y;
  end;
end;

procedure TCustomCodeTool.GetLineInfo(ACleanPos: integer;
  var ALineStart, ALineEnd, AFirstAtomStart, ALastAtomEnd: integer);
begin
  if ACleanPos>=1 then begin
    if ACleanPos<=SrcLen then begin
      // search line start
      ALineStart:=ACleanPos-1;
      while (ALineStart>=1) and (not (Src[ALineStart] in [#10,#13])) do
        dec(ALineStart);
      inc(ALineStart);
      // search line end
      ALineEnd:=ACleanPos;
      while (ALineEnd>=1) and (not (Src[ALineEnd] in [#10,#13])) do
        inc(ALineEnd);
      // search first atom in line
      MoveCursorToCleanPos(ALineStart);
      ReadNextAtom;
      AFirstAtomStart:=CurPos.StartPos;
      // search last atom in line
      repeat
        ALastAtomEnd:=CurPos.EndPos;
        ReadNextAtom;
      until CurPos.EndPos>ALineEnd;
    end else begin
      ALineStart:=Srclen+1;
      ALineEnd:=Srclen+1;
      AFirstAtomStart:=Srclen+1;
      ALastAtomEnd:=Srclen+1;
    end;
  end else begin
    ALineStart:=1;
    ALineEnd:=1;
    AFirstAtomStart:=1;
    ALastAtomEnd:=1;
  end;
end;

function TCustomCodeTool.FindLineEndOrCodeAfterPosition(StartPos: integer
  ): integer;
{ Searches a nice position in the cleaned source after StartPos.
  It will skip any space or comments (not directives) till next
  line end or compiler directive or code or include file end.
}
var
  LinkIndex, LinkEnd: integer;
begin
  LinkIndex:=Scanner.LinkIndexAtCleanPos(StartPos);
  LinkEnd:=Scanner.LinkCleanedEndPos(LinkIndex);
  if LinkEnd>StartPos then
    Result:=BasicCodeTools.FindLineEndOrCodeAfterPosition(Src,
                        StartPos,LinkEnd-1,Scanner.NestedComments)
  else
    Result:=StartPos;
end;

function TCustomCodeTool.FindLineEndOrCodeInFrontOfPosition(StartPos: integer
  ): integer;
begin
  Result:=FindLineEndOrCodeInFrontOfPosition(StartPos,true);
end;

function TCustomCodeTool.FindLineEndOrCodeInFrontOfPosition(StartPos: integer;
  StopAtDirectives: boolean): integer;
{ Searches a nice position in the cleaned source in front of StartPos.
  It will skip any space or comments (not directives) till next
  line end or compiler directive or code or include file end.
}
var
  LinkIndex, LinkStart: integer;
begin
  LinkIndex:=Scanner.LinkIndexAtCleanPos(StartPos);
  LinkStart:=Scanner.Links[LinkIndex].CleanedPos;
  Result:=BasicCodeTools.FindLineEndOrCodeInFrontOfPosition(Src,
                    StartPos,LinkStart,Scanner.NestedComments,StopAtDirectives);
end;

function TCustomCodeTool.FindFirstLineEndAfterInCode(StartPos: integer
  ): integer;
{ Searches a line end or code break in the cleaned source after StartPos.
  It will skip any line ends in comments.
}
var
  LinkIndex, LinkEnd: integer;
begin
  LinkIndex:=Scanner.LinkIndexAtCleanPos(StartPos);
  LinkEnd:=Scanner.LinkCleanedEndPos(LinkIndex);
  if LinkEnd>StartPos then
    Result:=BasicCodeTools.FindFirstLineEndAfterInCode(Src,
                        StartPos,LinkEnd-1,Scanner.NestedComments)
  else
    Result:=StartPos;
end;

procedure TCustomCodeTool.ClearIgnoreErrorAfter;
begin
  IgnoreErrorAfter:=CodePosition(0,nil);
end;

function TCustomCodeTool.UpdateNeeded(OnlyInterfaceNeeded: boolean): boolean;
begin
  {$IFDEF CTDEBUG}
  writeln('TCustomCodeTool.UpdateNeeded A ',Scanner<>nil,' FForceUpdateNeeded=',FForceUpdateNeeded);
  {$ENDIF}
  if FForceUpdateNeeded then begin
    Result:=true;
    exit;
  end;
  Result:=(FLastScannerChangeStep<>Scanner.ChangeStep)
           or (Scanner.UpdateNeeded(OnlyInterfaceNeeded, CheckFilesOnDisk));
  FForceUpdateNeeded:=Result;
  {$IFDEF CTDEBUG}
  writeln('TCustomCodeTool.UpdateNeeded END  Result=',Result);
  {$ENDIF}
end;

function TCustomCodeTool.CompareSrcIdentifier(CleanStartPos: integer;
  const Identifier: string): boolean;
var IdentPos, Len: integer;
begin
  Result:=false;
  Len:=length(Identifier);
  if (CleanStartPos<1) or (CleanStartPos>SrcLen-Len+1) or (Identifier='') then
    exit;
  IdentPos:=1;
  while (IdentPos<=Len) and (IsIdentChar[Src[CleanStartPos]]) do begin
    if UpChars[Identifier[IdentPos]]<>UpperSrc[CleanStartPos] then
      exit;
    inc(IdentPos);
    inc(CleanStartPos);
  end;
  Result:=(IdentPos>Len)
    and ((CleanStartPos>Srclen) or (not IsIdentChar[Src[CleanStartPos]]));
end;

function TCustomCodeTool.CompareSrcIdentifiers(Identifier1, Identifier2: PChar
  ): boolean;
begin
  Result:=false;
  if (Identifier1=nil) or (Identifier2=nil) then exit;
  while IsIdentChar[Identifier1[0]] do begin
    if (UpChars[Identifier1[0]]=UpChars[Identifier2[0]]) then begin
      inc(Identifier1);
      inc(Identifier2);
    end else
      exit;
  end;
  Result:=(not IsIdentChar[Identifier2[0]]);
end;

function TCustomCodeTool.CompareSrcIdentifiers(CleanStartPos: integer;
  AnIdentifier: PChar): boolean;
begin
  Result:=false;
  if (AnIdentifier=nil) or (CleanStartPos<1) or (CleanStartPos>SrcLen) then
    exit;
  while IsIdentChar[AnIdentifier[0]] do begin
    if (UpChars[AnIdentifier[0]]=UpperSrc[CleanStartPos]) then begin
      inc(AnIdentifier);
      inc(CleanStartPos);
      if CleanStartPos>SrcLen then break;
    end else
      exit;
  end;
  Result:=(CleanStartPos>SrcLen) or (not IsIdentChar[Src[CleanStartPos]]);
end;

function TCustomCodeTool.ExtractIdentifier(CleanStartPos: integer): string;
var len: integer;
begin
  if (CleanStartPos>=1) then begin
    len:=0;
    while (CleanStartPos<=SrcLen)
    and (IsIdentChar[Src[CleanStartPos+len]]) do
      inc(len);
    SetLength(Result,len);
    if len>0 then
      Move(Src[CleanStartPos],Result[1],len);
  end else
    Result:='';
end;

procedure TCustomCodeTool.DoDeleteNodes;
begin
  Tree.Clear;
end;

procedure TCustomCodeTool.RaiseIdentExpectedButAtomFound;
begin
  SaveRaiseExceptionFmt(ctsIdentExpectedButKeyWordFound,[GetAtom])
end;

procedure TCustomCodeTool.RaiseBracketOpenExpectedButAtomFound;
begin
  SaveRaiseExceptionFmt(ctsBracketOpenExpectedButAtomFound,[GetAtom]);
end;

procedure TCustomCodeTool.RaiseBracketCloseExpectedButAtomFound;
begin
  SaveRaiseExceptionFmt(ctsBracketCloseExpectedButAtomFound,[GetAtom]);
end;

procedure TCustomCodeTool.ActivateGlobalWriteLock;
begin
  if Assigned(OnSetGlobalWriteLock) then OnSetGlobalWriteLock(true);
end;

procedure TCustomCodeTool.DeactivateGlobalWriteLock;
begin
  if Assigned(OnSetGlobalWriteLock) then OnSetGlobalWriteLock(false);
end;

function TCustomCodeTool.MainFilename: string;
begin
  if (Scanner<>nil) and (Scanner.MainCode<>nil) then
    Result:=TCodeBuffer(Scanner.MainCode).Filename
  else
    Result:=ctsUnknownMainFilename;
end;

{ ECodeToolError }

constructor ECodeToolError.Create(ASender: TCustomCodeTool;
  const AMessage: string);
begin
  inherited Create(AMessage);
  Sender:=ASender;
end;

end.
