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

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeToolsStrConsts, CodeTree, CodeAtom, SourceLog,
  KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, AVL_Tree, TypInfo,
  SourceChanger;

type
  TCustomCodeTool = class(TObject)
  private
    FLastScannerChangeStep: integer;
    FScanner: TLinkScanner;
    FOnGetGlobalWriteLockInfo: TOnGetWriteLockInfo;
    FOnSetGlobalWriteLock: TOnSetWriteLock;
  protected
    KeyWordFuncList: TKeyWordFunctionList;
    FForceUpdateNeeded: boolean;
    function DefaultKeyWordFunc: boolean;
    procedure BuildDefaultKeyWordFunctions; virtual;
    procedure SetScanner(NewScanner: TLinkScanner); virtual;
    procedure RaiseException(const AMessage: string); virtual;
    procedure RaiseExceptionFmt(const AMessage: string;
      const args : array of const); virtual;
    procedure DoDeleteNodes; virtual;
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

    function UpdateNeeded(OnlyInterfaceNeeded: boolean): boolean;
    procedure BeginParsing(DeleteNodes, OnlyInterfaceNeeded: boolean); virtual;
    
    procedure MoveCursorToNodeStart(ANode: TCodeTreeNode);
    procedure MoveCursorToCleanPos(ACleanPos: integer);
    procedure MoveCursorToCleanPos(ACleanPos: PChar);
    function IsPCharInSrc(ACleanPos: PChar): boolean;
    function ReadTilSection(SectionType: TCodeTreeNodeDesc): boolean;
    function ReadTilBracketClose(ExceptionOnNotFound: boolean): boolean;
    function ReadBackTilBracketClose(ExceptionOnNotFound: boolean): boolean;
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
    function AtomIsWord: boolean;
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
  CurPos.StartPos:=1;
  CurPos.EndPos:=-1;
  LastAtoms.Clear;
  NextPos.StartPos:=-1;
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
  raise ECodeToolError.Create(Self,AMessage);
end;

procedure TCustomCodeTool.RaiseExceptionFmt(const AMessage: string;
  const args: array of const);
begin
  RaiseException(Format(AMessage,args));
end;

procedure TCustomCodeTool.SetScanner(NewScanner: TLinkScanner);
begin
  if NewScanner=FScanner then exit;
  Clear;
  FScanner:=NewScanner;
  if FScanner<>nil then
    FLastScannerChangeStep:=Scanner.ChangeStep;
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

function TCustomCodeTool.AtomIsWord: boolean;
begin
  Result:=(CurPos.StartPos<=SrcLen)
      and (IsIdentStartChar[UpperSrc[CurPos.StartPos]])
end;

function TCustomCodeTool.AtomIsKeyWord: boolean;
begin
  Result:=(CurPos.StartPos<=SrcLen)
      and (IsIdentStartChar[UpperSrc[CurPos.StartPos]])
      and (WordIsKeyWord.DoItUpperCase(UpperSrc,CurPos.StartPos,
             CurPos.EndPos-CurPos.StartPos));
end;

function TCustomCodeTool.AtomIsIdentifier(ExceptionOnNotFound: boolean):boolean;
begin
  if CurPos.StartPos<=SrcLen then begin
    if IsIdentStartChar[UpperSrc[CurPos.StartPos]] then begin
      if not WordIsKeyWord.DoItUpperCase(UpperSrc,CurPos.StartPos,
             CurPos.EndPos-CurPos.StartPos) then
        Result:=true
      else begin
        if ExceptionOnNotFound then
          RaiseExceptionFmt(ctsIdentExpectedButKeyWordFound,[GetAtom])
        else
          Result:=false;
      end;
    end else begin
      if ExceptionOnNotFound then
        RaiseExceptionFmt(ctsIdentExpectedButAtomFound,[GetAtom])
      else
        Result:=false;
    end;
  end else begin
    if ExceptionOnNotFound then
      RaiseException(ctsIdentExpectedButEOFFound)
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
        while (i<=SrcLen) and (IsNumberChar[Src[i]]) do
          inc(i);
        if (i<=SrcLen)
        and (not (Src[i] in ['''','#'])) then
          Result:=true;
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

procedure TCustomCodeTool.ReadNextAtom;
var c1, c2: char;
  CommentLvl: integer;
begin
  if (CurPos.StartPos<CurPos.EndPos) and (CurPos.StartPos>=1) then
    LastAtoms.Add(CurPos);
  if NextPos.StartPos>=1 then begin
    CurPos:=NextPos;
    NextPos.StartPos:=-1;
    exit;
  end;
  CurPos.StartPos:=CurPos.EndPos;
  // Skip all spaces and comments
  CommentLvl:=0;
  //if CurPos.StartPos<1 then CurPos.StartPos:=SrcLen+1;
  while CurPos.StartPos<=SrcLen do begin
    if IsCommentStartChar[Src[CurPos.StartPos]] then begin
      case Src[CurPos.StartPos] of
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
        if (CurPos.StartPos<SrcLen) and (Src[CurPos.StartPos+1]='/') then begin
          inc(CurPos.StartPos,2);
          while (CurPos.StartPos<=SrcLen)
          and (not (Src[CurPos.StartPos] in [#10,#13])) do
            inc(CurPos.StartPos);
          inc(CurPos.StartPos);
          if (CurPos.StartPos<=SrcLen) and (Src[CurPos.StartPos] in [#10,#13])
          and (Src[CurPos.StartPos-1]<>Src[CurPos.StartPos]) then
            inc(CurPos.StartPos);
        end else
          break;
      '(': // old turbo pascal comment
        if (CurPos.StartPos<SrcLen) and (Src[CurPos.StartPos+1]='*') then begin
          inc(CurPos.StartPos,3);
          while (CurPos.StartPos<=SrcLen)
          and ((Src[CurPos.StartPos-1]<>'*') or (Src[CurPos.StartPos]<>')')) do
            inc(CurPos.StartPos);
          inc(CurPos.StartPos);  
        end else
          break;
      end;
    end else if IsSpaceChar[Src[CurPos.StartPos]] then begin
      repeat
        inc(CurPos.StartPos);
      until (CurPos.StartPos>SrcLen)
      or (not (IsSpaceChar[Src[CurPos.StartPos]]));
    end else begin
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
        while (CurPos.EndPos<=SrcLen)
        and (IsIdentChar[UpperSrc[CurPos.EndPos]]) do
          inc(CurPos.EndPos);
      end;
    '''','#':
      begin
        while (CurPos.EndPos<=SrcLen) do begin
          case (Src[CurPos.EndPos]) of
          '#':
            begin
              inc(CurPos.EndPos);
              while (CurPos.EndPos<=SrcLen)
              and (IsNumberChar[Src[CurPos.EndPos]]) do
                inc(CurPos.EndPos);
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
    else
      inc(CurPos.EndPos);
      if CurPos.EndPos<=SrcLen then begin
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
  end;
end;

procedure TCustomCodeTool.ReadPriorAtom;

  procedure ReadStringConstantBackward;
  var PrePos: integer;
  begin
    while (CurPos.StartPos>1) do begin
      case Src[CurPos.StartPos-1] of
      '''':
        begin
          dec(CurPos.StartPos);
          repeat
            dec(CurPos.StartPos);
          until (CurPos.StartPos<1) or (Src[CurPos.StartPos]='''');
        end;
      '0'..'9':
        begin
          // test if char constant
          PrePos:=CurPos.StartPos-1;
          while (PrePos>1) and (IsNumberChar[Src[PrePos]]) do
            dec(PrePos);
          if (PrePos<1) then break;
          if Src[PrePos]='#' then
            CurPos.StartPos:=PrePos
          else
            break;
        end;
      else
        break;
      end;
    end;
  end;

type
  TNumberType = (ntDecimal, ntHexadecimal, ntBinary, ntIdentifier,
    ntCharConstant, ntFloat, ntFloatWithExponent);
  TNumberTypes = set of TNumberType;

const
  AllNumberTypes: TNumberTypes = [ntDecimal, ntHexadecimal, ntBinary,
    ntIdentifier, ntCharConstant, ntFloat, ntFloatWithExponent];

var c1, c2: char;
  CommentLvl, PrePos, OldPrePos: integer;
  IsStringConstant: boolean;
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
                while (PrePos<=CurPos.StartPos) and (CommentLvl>0) do begin
                  case Src[PrePos] of
                  '{': if Scanner.NestedComments then inc(CommentLvl);
                  '}': dec(CommentLvl);
                  end;
                  inc(PrePos);
                end;
              end;
              
            '(':
              begin
                inc(PrePos);
                if Src[PrePos]='*' then begin
                  // skip turbo pascal comment
                  inc(PrePos);
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
        end; // end of possible Delphi comment
        
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
        // definitely an identifier or a keyword
        while (CurPos.StartPos>1)
        and (IsIdentChar[UpperSrc[CurPos.StartPos-1]]) do
          dec(CurPos.StartPos);
        if (CurPos.StartPos>2)
        and (Src[CurPos.StartPos-1]='@') and (Src[CurPos.StartPos-2]='@') then
          dec(CurPos.StartPos,2);
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
          if CurPos.StartPos<=1 then exit;
          dec(CurPos.StartPos);
        end;
      end;
    else
      if CurPos.StartPos>1 then begin
        c1:=Src[CurPos.StartPos-1];
        // test for double char operators :=, +=, -=, /=, *=, <>, <=, >=, **, ><
        if ((c2='=') and  (IsEqualOperatorStartChar[c1]))
        or ((c1='<') and (c2='>'))
        or ((c1='>') and (c2='<'))
        or ((c1='.') and (c2='.'))
        or ((c1='*') and (c2='*'))
        or ((c1='@') and (c2='@'))
        then dec(CurPos.StartPos);
      end;
  end;
end;

procedure TCustomCodeTool.UndoReadNextAtom;
begin
  if LastAtoms.Count>0 then begin
    NextPos:=CurPos;
    CurPos:=LastAtoms.GetValueAt(0);
    LastAtoms.UndoLastAdd;
  end else
    RaiseException('TCustomCodeTool.UndoReadNextAtom impossible');
end;

function TCustomCodeTool.ReadTilSection(
  SectionType: TCodeTreeNodeDesc): boolean;
var SectionID: TCodeTreeNodeDesc;
begin
  Result:=false;
  if not (SectionType in AllCodeSections) then exit;
  Result:=false;
  repeat
    ReadNextAtom;
    if (CurPos.StartPos>SrcLen) then break;
    if IsKeyWordSection.DoItUppercase(UpperSrc,CurPos.StartPos,
      CurPos.EndPos-CurPos.StartPos)
    and (not LastAtomIs(1,'=')) then begin
      if UpAtomIs('UNIT') then
        SectionID:=ctnUnit
      else if UpAtomIs('PROGRAM') then
        SectionID:=ctnProgram
      else if UpAtomIs('PACKAGE') then
        SectionID:=ctnPackage
      else if UpAtomIs('LIBRARY') then
        SectionID:=ctnLibrary
      else if UpAtomIs('INTERFACE') then
        SectionID:=ctnInterface
      else if UpAtomIs('IMPLEMENTATION') then
        SectionID:=ctnImplementation
      else if UpAtomIs('INITIALIZATION') then
        SectionID:=ctnInitialization
      else if UpAtomIs('FINALIZATION') then
        SectionID:=ctnFinalization
      else
        SectionID:=ctnNone;
      if (SectionType=SectionID)
      or ((SectionType=ctnInterface)
      and (SectionID in [ctnProgram,ctnPackage,ctnLibrary])) then begin
        Result:=true;  exit;
      end;
      if SectionID>SectionType then
        exit;
    end;
  until false;
end;

function TCustomCodeTool.ReadTilBracketClose(
  ExceptionOnNotFound: boolean): boolean;
// reads code brackets (not comment brackets)
var CloseBracket, AntiCloseBracket: char;
  Start: TAtomPosition;
begin
  Result:=false;
  if AtomIsChar('(') then begin
    CloseBracket:=')';
    AntiCloseBracket:=']';
  end else if AtomIsChar('[') then begin
    CloseBracket:=']';
    AntiCloseBracket:=')';
  end else begin
    if ExceptionOnNotFound then
      RaiseExceptionFmt(ctsBracketOpenExpectedButAtomFound,[GetAtom]);
    exit;
  end;
  Start:=CurPos;
  repeat
    ReadNextAtom;
    if (AtomIsChar(CloseBracket)) then break;
    if (CurPos.StartPos>SrcLen) or AtomIsChar(AntiCloseBracket)
    or UpAtomIs('END') then begin
      CurPos:=Start;
      if ExceptionOnNotFound then
        RaiseExceptionFmt(ctsBracketNotFound,[CloseBracket]);
      exit;
    end;
    if (AtomIsChar('(')) or (AtomIsChar('[')) then begin
      if not ReadTilBracketClose(ExceptionOnNotFound) then exit;
    end;
  until false;
  Result:=true;
end;

function TCustomCodeTool.ReadBackTilBracketClose(
  ExceptionOnNotFound: boolean): boolean;
// reads code brackets (not comment brackets)
var CloseBracket, AntiCloseBracket: char;
  Start: TAtomPosition;
begin
  Result:=false;
  if AtomIsChar(')') then begin
    CloseBracket:='(';
    AntiCloseBracket:='[';
  end else if AtomIsChar(']') then begin
    CloseBracket:='[';
    AntiCloseBracket:='(';
  end else begin
    if ExceptionOnNotFound then
      RaiseExceptionFmt(ctsBracketCloseExpectedButAtomFound,[GetAtom]);
    exit;
  end;
  Start:=CurPos;
  repeat
    ReadPriorAtom;
    if (AtomIsChar(CloseBracket)) then break;
    if (CurPos.StartPos<1) or AtomIsChar(AntiCloseBracket)
    or UpAtomIs('END') or UpAtomIs('BEGIN') then begin
      CurPos:=Start;
      if ExceptionOnNotFound then
        RaiseExceptionFmt(ctsBracketNotFound,[CloseBracket]);
      exit;
    end;
    if (AtomIsChar(')')) or (AtomIsChar(']')) then begin
      if not ReadBackTilBracketClose(ExceptionOnNotFound) then exit;
    end;
  until false;
  Result:=true;
end;

procedure TCustomCodeTool.BeginParsing(DeleteNodes,
  OnlyInterfaceNeeded: boolean);
begin
  Scanner.Scan(OnlyInterfaceNeeded,CheckFilesOnDisk);
  if FLastScannerChangeStep<>Scanner.ChangeStep then begin
    FLastScannerChangeStep:=Scanner.ChangeStep;
    Src:=Scanner.CleanedSrc;
    UpperSrc:=UpperCaseStr(Src);
    SrcLen:=length(Src);
    FForceUpdateNeeded:=true;
  end;
  CurPos.StartPos:=1;
  CurPos.EndPos:=1;
  LastAtoms.Clear;
  NextPos.StartPos:=-1;
  CurNode:=nil;
  if DeleteNodes then DoDeleteNodes;
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
  LastAtoms.Clear;
  NextPos.StartPos:=-1;
  CurNode:=nil;
end;

procedure TCustomCodeTool.MoveCursorToCleanPos(ACleanPos: PChar);
var NewPos: integer;
begin
  if Src='' then
    RaiseException('[TCustomCodeTool.MoveCursorToCleanPos - PChar] Src empty');
  NewPos:=Integer(ACleanPos)-Integer(@Src[1])+1;
  if (NewPos<1) or (NewPos>SrcLen) then
    RaiseException('[TCustomCodeTool.MoveCursorToCleanPos - PChar] '
      +'CleanPos not in Src');
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
begin
  if StartNode<>nil then begin
//writeln('SearchInNode ',NodeDescriptionAsString(ANode.Desc),
//',',ANode.StartPos,',',ANode.EndPos,', p=',p,
//' "',copy(Src,ANode.StartPos,4),'" - "',copy(Src,ANode.EndPos-5,4),'"');
    if (StartNode.StartPos<=P)
    and ((StartNode.EndPos>P) or (StartNode.EndPos<1)) then begin
      // first search in childs
      Result:=FindDeepestNodeAtPos(StartNode.FirstChild,P,false);
      if Result=nil then
        // no child found -> take this node
        Result:=StartNode;
    end else
      // search in next node
      Result:=FindDeepestNodeAtPos(StartNode.NextBrother,P,false);
  end else
    Result:=nil;
  if (Result=nil) and ExceptionOnNotFound then begin
    MoveCursorToCleanPos(P);
    RaiseException(ctsNoNodeFoundAtCursor);
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
      CurPos.StartPos:=ALineStart;
      CurPos.EndPos:=ALineStart;
      NextPos.StartPos:=-1;
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
