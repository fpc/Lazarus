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
  Classes, SysUtils, CodeTree, CodeAtom, SourceLog, KeywordFuncLists,
  BasicCodeTools, LinkScanner, CodeCache, AVL_Tree, TypInfo, SourceChanger;

type
  TCustomCodeTool = class(TObject)
  private
    //FIgnoreMissingIncludeFiles: boolean;
    FLastScannerChangeStep: integer;
    FScanner: TLinkScanner;
  protected
    KeyWordFuncList: TKeyWordFunctionList;
    FForceUpdateNeeded: boolean;
    function DefaultKeyWordFunc: boolean;
    procedure BuildDefaultKeyWordFunctions; virtual;
    procedure SetScanner(NewScanner: TLinkScanner); virtual;
    procedure RaiseException(const AMessage: string); virtual;
  public
    Tree: TCodeTree;

    // current Values, Position, Node ...
    CurPos: TAtomPosition;
    Src: string;
    UpperSrc: string;
    SrcLen: integer;
    CurNode: TCodeTreeNode;
    LastAtoms: TAtomRing;
    
    CheckFilesOnDisk: boolean;
    IndentSize: integer;
    VisibleEditorLines: integer;
    JumpCentered: boolean;
    CursorBeyondEOL: boolean;
    
    ErrorPosition: TCodeXYPosition;
    
    property Scanner: TLinkScanner read FScanner write SetScanner;
    
    function FindDeepestNodeAtPos(P: integer): TCodeTreeNode;
    function CaretToCleanPos(Caret: TCodeXYPosition;
        var CleanPos: integer): integer;  // 0=valid CleanPos
              //-1=CursorPos was skipped, CleanPos between two links
              // 1=CursorPos beyond scanned code
              //-2=X,Y beyond source
    function CleanPosToCaret(CleanPos: integer;
        var Caret:TCodeXYPosition): boolean; // true=ok, false=invalid CleanPos
    procedure GetLineInfo(ACleanPos: integer;
        var ALineStart, ALineEnd, AFirstAtomStart, ALastAtomEnd: integer);

    function UpdateNeeded(OnlyInterfaceNeeded: boolean): boolean;
    procedure BeginParsing(DeleteNodes, OnlyInterfaceNeeded: boolean); virtual;
    procedure MoveCursorToNodeStart(ANode: TCodeTreeNode); virtual;
    procedure MoveCursorToCleanPos(ACleanPos: integer); virtual;
    function ReadTilSection(SectionType: TCodeTreeNodeDesc): boolean;
    function ReadTilBracketClose(ExceptionOnNotFound: boolean): boolean;
    function DoAtom: boolean; virtual;
    procedure ReadNextAtom; virtual;
    procedure UndoReadNextAtom; virtual;
    function AtomIs(const AnAtom: shortstring): boolean;
    function UpAtomIs(const AnAtom: shortstring): boolean;
    function ReadNextAtomIs(const AnAtom: shortstring): boolean;
    function ReadNextUpAtomIs(const AnAtom: shortstring): boolean;
    function ReadNextAtomIsChar(const c: char): boolean;
    function AtomIsChar(const c: char): boolean;
    function AtomIsWord: boolean;
    function AtomIsKeyWord: boolean;
    function AtomIsNumber: boolean;
    function AtomIsStringConstant: boolean;
    function AtomIsIdentifier(ExceptionOnNotFound: boolean): boolean;
    function LastAtomIs(BackIndex: integer;
        const AnAtom: shortstring): boolean; // 0=current, 1=prior current, ...
    function LastUpAtomIs(BackIndex: integer;
        const AnAtom: shortstring): boolean; // 0=current, 1=prior current, ...
    function GetAtom: string;
    function GetUpAtom: string;
    function CompareNodeSrc(ANode: TCodeTreeNode;
        const ASource: string): integer;
    function CompareNodeUpSrc(ANode: TCodeTreeNode;
        const ASource: string): integer;

    procedure CreateChildNode; virtual;
    procedure EndChildNode; virtual;
    
    procedure Clear; virtual;
    function NodeDescToStr(Desc: integer): string;
    function NodeSubDescToStr(Desc, SubDesc: integer): string;
    function ConsistencyCheck: integer; // 0 = ok
    procedure WriteDebugTreeReport;
    constructor Create;
    destructor Destroy; override;
  end;

  ECodeToolError = class(Exception);


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
  Tree.Clear;
  CurPos.StartPos:=1;
  CurPos.EndPos:=-1;
  LastAtoms.Clear;
end;

procedure TCustomCodeTool.RaiseException(const AMessage: string);
var CaretXY: TCodeXYPosition;
begin
  ErrorPosition.Code:=nil;
  if (CleanPosToCaret(CurPos.StartPos,CaretXY))
  and (CaretXY.Code<>nil) then begin
    ErrorPosition:=CaretXY;
  end else if (Scanner<>nil) and (Scanner.MainCode<>nil) then begin
    ErrorPosition.Code:=TCodeBuffer(Scanner.MainCode);
    ErrorPosition.Y:=-1;
  end;
  raise ECodeToolError.Create(AMessage);
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
  case Desc of
  // CodeTreeNodeDescriptors
  ctnNone            : Result:='None';

  ctnClass           : Result:='Class';
  ctnClassPublished  : Result:='Published';
  ctnClassPrivate    : Result:='Private';
  ctnClassProtected  : Result:='Protected';
  ctnClassPublic     : Result:='Public';

  ctnProcedure       : Result:='Method';
  ctnProcedureHead   : Result:='Method Head';
  ctnParameterList   : Result:='Param List';

  ctnBeginBlock      : Result:='Begin';
  ctnAsmBlock        : Result:='Asm';
  ctnWithBlock        : Result:='With';

  ctnProgram         : Result:='Program';
  ctnPackage         : Result:='Package';
  ctnLibrary         : Result:='Library';
  ctnUnit            : Result:='Unit';
  ctnInterface       : Result:='Interface';
  ctnImplementation  : Result:='Implementation';
  ctnInitialization  : Result:='Initialization';
  ctnFinalization    : Result:='Finalization';

  ctnTypeSection     : Result:='Type Section';
  ctnVarSection      : Result:='Var Section';
  ctnConstSection    : Result:='Const Section';
  ctnResStrSection   : Result:='Resource String Section';
  ctnUsesSection     : Result:='Uses Section';

  ctnTypeDefinition  : Result:='Type Definition';
  ctnVarDefinition   : Result:='Variable Definition';
  ctnConstDefinition : Result:='Const Definition';
  
  ctnProperty        : Result:='Property';
  
  ctnIdentifier      : Result:='Identifier';
  ctnArrayType       : Result:='Array Type';
  ctnRecordType      : Result:='Record Type';
  ctnRecordCase      : Result:='Record Case';
  ctnRecordVariant   : Result:='Record Variant';
  ctnProcedureType   : Result:='Procedure Type';
  ctnSetType         : Result:='Set Type';
  ctnRangeType       : Result:='Subrange Type';
  ctnEnumType        : Result:='Enumeration Type';
  ctnLabelType       : Result:='Label Type';
  ctnTypeType        : Result:='''Type'' Type';
  ctnFileType        : Result:='File Type';
  ctnPointerType     : Result:='Pointer ''^'' Type';
  ctnClassOfType     : Result:='Class Of Type';

  else
    Result:='(unknown descriptor '+IntToStr(Desc)+')';
  end;
end;

function TCustomCodeTool.NodeSubDescToStr(Desc, SubDesc: integer): string;
begin
  if SubDesc<>0 then
    Result:='(unknown subdescriptor '+IntToStr(SubDesc)+')'
  else
    Result:='';
  case Desc of
  ctnProcedure:
    case SubDesc of
    // CodeTreeNodeSubDescriptors
    ctnsForwardDeclaration : Result:='Forward';
    end;
  ctnClass:
    case SubDesc of
    // CodeTreeNodeSubDescriptors
    ctnsForwardDeclaration : Result:='Forward';
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

function TCustomCodeTool.CompareNodeSrc(ANode: TCodeTreeNode;
  const ASource: string): integer;
var ASrcLen, i, NodeSrcLen : integer;
begin
  if (ANode.StartPos<=SrcLen) and (ANode.EndPos<=SrcLen+1)
  and (ANode.StartPos>=1) then begin
    ASrcLen:=length(ASource);
    NodeSrcLen:=ANode.EndPos-ANode.StartPos;
    if ASrcLen=NodeSrcLen then begin
      for i:=1 to ASrcLen do
        if ASource[i]<>Src[ANode.StartPos-1+i] then begin
          if ASource[i]>Src[ANode.StartPos-1+i] then
            Result:=1
          else
            Result:=-1;
          exit;
        end;
      Result:=0;
    end else if ASrcLen<NodeSrcLen then
      Result:=1
    else
      Result:=-1;
  end else
    Result:=-1;
end;

function TCustomCodeTool.CompareNodeUpSrc(ANode: TCodeTreeNode;
  const ASource: string): integer;
var ASrcLen, i, NodeSrcLen : integer;
begin
  if (ANode.StartPos<=SrcLen) and (ANode.EndPos<=SrcLen+1)
  and (ANode.StartPos>=1) then begin
    ASrcLen:=length(ASource);
    NodeSrcLen:=ANode.EndPos-ANode.StartPos;
    if ASrcLen<=NodeSrcLen then begin
      i:=1;
      while (i<=ASrcLen) and (IsIdentChar[Src[ANode.StartPos-1+i]]) do begin
        if ASource[i]<>UpperSrc[ANode.StartPos-1+i] then begin
          if ASource[i]>UpperSrc[ANode.StartPos-1+i] then
            Result:=1
          else
            Result:=-1;
          exit;
        end;
        inc(i);
      end;
      Result:=0;
    end else
      Result:=-1;
  end else
    Result:=-1;
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
          RaiseException(
            'syntax error: identifier expected, but keyword '+GetAtom+' found')
        else
          Result:=false;
      end;
    end else begin
      if ExceptionOnNotFound then
        RaiseException(
          'syntax error: identifier expected, but '+GetAtom+' found')
      else
        Result:=false;
    end;
  end else begin
    if ExceptionOnNotFound then
      RaiseException('unexpected end of file (identifier expected)')
    else
      Result:=false;
  end;
end;

function TCustomCodeTool.AtomIsNumber: boolean;
begin
  Result:=(CurPos.StartPos<=SrcLen)
      and (Src[CurPos.StartPos] in ['0'..'9','%','$']);
end;

function TCustomCodeTool.AtomIsStringConstant: boolean;
begin
  Result:=(CurPos.StartPos<=SrcLen)
      and (Src[CurPos.StartPos] in ['''','#']);
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
  // Skip all spaces and comments
  CommentLvl:=0;
  if (CurPos.StartPos<CurPos.EndPos) and (CurPos.StartPos>=1) then
    LastAtoms.Add(CurPos);
  CurPos.StartPos:=CurPos.EndPos;
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
              while (CurPos.EndPos<=SrcLen)
              and (Src[CurPos.EndPos]<>'''') do
                inc(CurPos.EndPos);
              inc(CurPos.EndPos);
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
        or ((c1='<') and (c2='>'))
        or ((c1='>') and (c2='<'))
        or ((c1='.') and (c2='.'))
        or ((c1='*') and (c2='*'))
        or ((c1='@') and (c2='@'))
        then inc(CurPos.EndPos);
      end;
  end;
end;

procedure TCustomCodeTool.UndoReadNextAtom;
begin
  if LastAtoms.Count>0 then begin
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
      RaiseException(
        'syntax error: bracket open expected, but '+GetAtom+' found');
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
        RaiseException(
          'syntax error: bracket '+CloseBracket+' not found');
      exit;
    end;
    if (AtomIsChar('(')) or (AtomIsChar('[')) then begin
      if not ReadTilBracketClose(ExceptionOnNotFound) then exit;
    end;
  until false;
  Result:=true;
end;

procedure TCustomCodeTool.BeginParsing(DeleteNodes,
  OnlyInterfaceNeeded: boolean);
begin
  Scanner.Scan(OnlyInterfaceNeeded,CheckFilesOnDisk);
  Src:=Scanner.CleanedSrc;
  FLastScannerChangeStep:=Scanner.ChangeStep;
  UpperSrc:=UpperCaseStr(Src);
  SrcLen:=length(Src);
  CurPos.StartPos:=1;
  CurPos.EndPos:=1;
  LastAtoms.Clear;
  CurNode:=nil;
  if DeleteNodes then Tree.Clear;
end;

procedure TCustomCodeTool.MoveCursorToNodeStart(ANode: TCodeTreeNode);
begin
  CurPos.StartPos:=ANode.StartPos;
  CurPos.EndPos:=ANode.StartPos;
  LastAtoms.Clear;
  CurNode:=ANode;
end;

procedure TCustomCodeTool.MoveCursorToCleanPos(ACleanPos: integer);
begin
  CurPos.StartPos:=ACleanPos;
  CurPos.EndPos:=ACleanPos;
  LastAtoms.Clear;
  CurNode:=nil;
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

function TCustomCodeTool.FindDeepestNodeAtPos(P: integer): TCodeTreeNode;

  function SearchInNode(ANode: TCodeTreeNode): TCodeTreeNode;
  begin
    if ANode<>nil then begin
//writeln('SearchInNode ',NodeDescriptionAsString(ANode.Desc),
//',',ANode.StartPos,',',ANode.EndPos,', p=',p,
//' "',copy(Src,ANode.StartPos,20),'"');
      if (ANode.StartPos<=P) and ((ANode.EndPos>P) or (ANode.EndPos<1)) then
      begin
        // first search in childs
        Result:=SearchInNode(ANode.FirstChild);
        if Result=nil then
          // no child found -> take this node
          Result:=ANode;
      end else
        // search in next node
        Result:=SearchInNode(ANode.NextBrother);
    end else
      Result:=nil;
  end;

// TCustomCodeTool.FindDeepestNodeAtPos
begin
  Result:=SearchInNode(Tree.Root);
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

procedure TCustomCodeTool.GetLineInfo(ACleanPos: integer;
  var ALineStart, ALineEnd, AFirstAtomStart, ALastAtomEnd: integer);
begin
  if ACleanPos>=1 then begin
    if ACleanPos<=SrcLen then begin
      // search line start
      ALineStart:=ACleanPos;
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
writeln('TCustomCodeTool.UpdateNeeded A ',Scanner<>nil);
{$ENDIF}
  if FForceUpdateNeeded then begin
    Result:=true;
    exit;
  end;
  Result:=(FLastScannerChangeStep<>Scanner.ChangeStep)
           or (Scanner.UpdateNeeded(OnlyInterfaceNeeded, CheckFilesOnDisk));
  FForceUpdateNeeded:=Result;
{$IFDEF CTDEBUG}
writeln('TCustomCodeTool.UpdateNeeded END');
{$ENDIF}
end;



end.
