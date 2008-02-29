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
    A simple C parser.
}
unit CCodeParserTool;

{$mode objfpc}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, FileProcs, CodeToolsStructs, BasicCodeTools,
  KeywordFuncLists, LinkScanner, CodeAtom, CodeCache, AVL_Tree,
  CodeTree, NonPascalCodeTools;

type
  TCCodeNodeDesc = word;

const
  // descriptors
  ccnBase      = 1000;
  ccnNone      =  0+ccnBase;
  
  ccnRoot      =  1+ccnBase;
  ccnDirective =  2+ccnBase;// e.g. "#define a" ,can be multiple lines, without line end
  ccnExtern    =  3+ccnBase;// e.g. extern "C" {}
  ccnEnums     =  4+ccnBase;// e.g. enum {};
  ccnEnum      =  5+ccnBase;// e.g. name = value;
  ccnConstant  =  6+ccnBase;// e.g. 1
  ccnTypedef   =  7+ccnBase;// e.g. typedef int TInt;
  ccnStruct    =  8+ccnBase;// e.g. struct{};
  ccnVariable  =  9+ccnBase;// e.g. int i

type
  TCCodeParserTool = class;

  { ECCodeParserException }

  ECCodeParserException = class(Exception)
  public
    Sender: TCCodeParserTool;
    constructor Create(ASender: TCCodeParserTool; const AMessage: string);
  end;

  { TCCodeParserTool }

  TCCodeParserTool = class
  private
    FChangeStep: integer;
    FDefaultTokenList: TKeyWordFunctionList;

    function OtherToken: boolean;
    function DirectiveToken: boolean;
    function EnumToken: boolean;
    function ExternToken: boolean;
    function TypedefToken: boolean;
    procedure InitKeyWordList;

    procedure InitParser;
    procedure CreateChildNode(Desc: TCCodeNodeDesc);
    procedure EndChildNode;
    procedure CloseNodes;
    
    procedure ReadEnum;
    procedure ReadStruct;
    procedure ReadConstant;
    procedure ReadVariable;
    
    procedure RaiseException(const AMessage: string);
    procedure RaiseExpectedButAtomFound(const AToken: string);
  public
    Code: TCodeBuffer;
    Src: string;
    SrcLen: integer;
    Tree: TCodeTree;
    CurNode: TCodeTreeNode;
    SrcPos: Integer;
    AtomStart: integer;
    ParseChangeStep: integer;
    
    LastSrcPos: integer;
    LastAtomStart: integer;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure Parse;
    procedure Parse(aCode: TCodeBuffer);
    function UpdateNeeded: boolean;
    
    procedure MoveCursorToPos(p: integer);
    procedure ReadNextAtom;
    procedure UndoReadNextAtom;
    function ReadTilBracketClose(ExceptionOnNotFound: boolean): boolean;
    function AtomIs(const s: shortstring): boolean;
    function AtomIsChar(const c: char): boolean;
    function UpAtomIs(const s: shortstring): boolean;
    function AtomIsIdentifier: boolean;
    function AtomIsStringConstant: boolean;
    function GetAtom: string;

    procedure Replace(FromPos, ToPos: integer; const NewSrc: string);

    procedure IncreaseChangeStep;
    procedure WriteDebugReport;
    
    property ChangeStep: integer read FChangeStep;
  end;
  
function CCNodeDescAsString(Desc: TCCodeNodeDesc): string;


implementation


function CCNodeDescAsString(Desc: TCCodeNodeDesc): string;
begin
  case Desc of
  ccnNone     : Result:='None';
  ccnRoot     : Result:='Root';
  ccnDirective: Result:='Directive';
  else          Result:='?';
  end;
end;

{ ECCodeParserException }

constructor ECCodeParserException.Create(ASender: TCCodeParserTool;
  const AMessage: string);
begin
  inherited Create(AMessage);
  Sender:=ASender;
end;

{ TCCodeParserTool }

function TCCodeParserTool.OtherToken: boolean;
begin
  Result:=false;
  RaiseException('unexpected token '+GetAtom);
end;

function TCCodeParserTool.DirectiveToken: boolean;
begin
  Result:=true;
  CreateChildNode(ccnDirective);
  // read til end of line
  ReadTilCLineEnd(Src,SrcPos);
  AtomStart:=SrcPos;
  EndChildNode;
end;

function TCCodeParserTool.EnumToken: boolean;
begin
  Result:=true;
  ReadEnum;
  // read semicolon
  ReadNextAtom;
  if not AtomIsChar(';') then
    RaiseExpectedButAtomFound(';');
end;

function TCCodeParserTool.ExternToken: boolean;
begin
  Result:=true;
  CreateChildNode(ccnExtern);
  ReadNextAtom;
  if not AtomIsStringConstant then
    RaiseExpectedButAtomFound('string constant');
  ReadNextAtom;
  if not AtomIsChar('{') then
    RaiseExpectedButAtomFound('{');
end;

procedure TCCodeParserTool.ReadEnum;
(* For example:
  enum {
    TEST_ENUM1 = 1, /* Enum starts at 1 */
    TEST_ENUM2,
    TEST_ENUM3
  };

*)
begin
  CreateChildNode(ccnEnums);
  ReadNextAtom;
  if not AtomIsChar('{') then
    RaiseExpectedButAtomFound('{');
  // read enums. Examples
  // name,
  // name = constant,
  ReadNextAtom;
  repeat
    if AtomIsIdentifier then begin
      // read enum
      CreateChildNode(ccnEnum);
      ReadNextAtom;
      if AtomIsChar('=') then begin
        // read value
        ReadNextAtom;
        ReadConstant;
      end;
      EndChildNode;
    end;
    if AtomIsChar(',') then begin
      // next enum
      ReadNextAtom;
      if not AtomIsIdentifier then
        RaiseExpectedButAtomFound('identifier');
    end else if AtomIsChar('}') then begin
      break;
    end else
      RaiseExpectedButAtomFound('}');
  until false;
  EndChildNode;
end;

procedure TCCodeParserTool.ReadStruct;
(* For example:
  typedef struct {
    uint8_t b[6]; // implicit type
  } __attribute__((packed)) bdaddr_t;

*)
begin
  CreateChildNode(ccnStruct);
  ReadNextAtom;
  // read {
  if not AtomIsChar('{') then
    RaiseExpectedButAtomFound('{');
  repeat
    ReadNextAtom;
    // read variables
    if AtomIsIdentifier then begin
      ReadVariable;
      ReadNextAtom;
      if AtomIsChar('}') then
        break
      else if AtomIsChar(';') then begin
        // next identifier
      end else
        RaiseExpectedButAtomFound('}');
    end else if AtomIsChar('}') then
      break
    else
      RaiseExpectedButAtomFound('identifier');
  until false;
  // read attributes
  ReadNextAtom;
  if AtomIs('__attribute__') then begin
    ReadNextAtom;
    if not AtomIsChar('(') then
      RaiseExpectedButAtomFound('(');
    ReadTilBracketClose(true);
  end else begin
    UndoReadNextAtom;
  end;
  // close node
  EndChildNode;
end;

function TCCodeParserTool.TypedefToken: boolean;
begin
  Result:=true;
  CreateChildNode(ccnTypedef);
  // read type
  ReadNextAtom;
  if AtomIs('enum') then
    ReadEnum
  else if AtomIs('struct') then
    ReadStruct
  else if AtomIsIdentifier then begin

  end else
    RaiseExpectedButAtomFound('identifier');
  // read typedef name
  ReadNextAtom;
  if not AtomIsIdentifier then
    RaiseExpectedButAtomFound('identifier');
  // read semicolon
  ReadNextAtom;
  if not AtomIsChar(';') then
    RaiseExpectedButAtomFound(';');
  EndChildNode;
end;

procedure TCCodeParserTool.InitKeyWordList;
begin
  if FDefaultTokenList=nil then begin
    FDefaultTokenList:=TKeyWordFunctionList.Create;
    with FDefaultTokenList do begin
      Add('#',{$ifdef FPC}@{$endif}DirectiveToken);
      Add('extern',{$ifdef FPC}@{$endif}ExternToken);
      Add('enum',{$ifdef FPC}@{$endif}EnumToken);
      Add('typedef',{$ifdef FPC}@{$endif}TypedefToken);
      DefaultKeyWordFunction:={$ifdef FPC}@{$endif}OtherToken;
    end;
  end;
end;

procedure TCCodeParserTool.InitParser;
begin
  ParseChangeStep:=Code.ChangeStep;
  IncreaseChangeStep;
  InitKeyWordList;
  Src:=Code.Source;
  SrcLen:=length(Src);
  if Tree=nil then
    Tree:=TCodeTree.Create
  else
    Tree.Clear;
  SrcPos:=1;
  AtomStart:=1;
  CurNode:=nil;
  CreateChildNode(ccnRoot);
end;

procedure TCCodeParserTool.CreateChildNode(Desc: TCCodeNodeDesc);
var
  NewNode: TCodeTreeNode;
begin
  NewNode:=NodeMemManager.NewNode;
  Tree.AddNodeAsLastChild(CurNode,NewNode);
  NewNode.Desc:=Desc;
  CurNode:=NewNode;
  CurNode.StartPos:=AtomStart;
  DebugLn([GetIndentStr(CurNode.GetLevel*2),'TCCodeParserTool.CreateChildNode ']);
end;

procedure TCCodeParserTool.EndChildNode;
begin
  DebugLn([GetIndentStr(CurNode.GetLevel*2),'TCCodeParserTool.EndChildNode ']);
  if CurNode.EndPos<=0 then
    CurNode.EndPos:=AtomStart;
  CurNode:=CurNode.Parent;
end;

procedure TCCodeParserTool.CloseNodes;
var
  Node: TCodeTreeNode;
begin
  Node:=CurNode;
  while Node<>nil do begin
    Node.EndPos:=AtomStart;
    Node:=Node.Parent;
  end;
end;

procedure TCCodeParserTool.ReadConstant;
var
  EndPos: LongInt;
begin
  if AtomIsChar(',') or AtomIsChar(';') then
    RaiseExpectedButAtomFound('identifier');
  CreateChildNode(ccnConstant);
  repeat
    if AtomIsChar('(') or AtomIsChar('[') then
      ReadTilBracketClose(true);
    EndPos:=SrcPos;
    ReadNextAtom;
    if AtomIsChar(',') or AtomIsChar(';')
    or AtomIsChar(')') or AtomIsChar(']') or AtomIsChar('}')
    then
      break;
  until false;
  CurNode.EndPos:=EndPos;
  EndChildNode;
end;

procedure TCCodeParserTool.ReadVariable;
(* Examples:
  int i
  uint8_t b[6]
*)
begin
  CreateChildNode(ccnVariable);
  // read name
  ReadNextAtom;
  if not AtomIsIdentifier then
    RaiseExpectedButAtomFound('identifier');
  ReadNextAtom;
  if AtomIsChar('[') then begin
    ReadTilBracketClose(true);
  end else begin
    UndoReadNextAtom;
  end;
  EndChildNode;
end;

procedure TCCodeParserTool.RaiseException(const AMessage: string);
begin
  CloseNodes;
  raise ECCodeParserException.Create(Self,AMessage);
end;

procedure TCCodeParserTool.RaiseExpectedButAtomFound(const AToken: string);
begin
  RaiseException(AToken+' expected, but '+GetAtom+' found');
end;

constructor TCCodeParserTool.Create;
begin
  Tree:=TCodeTree.Create;
end;

destructor TCCodeParserTool.Destroy;
begin
  FreeAndNil(Tree);
  inherited Destroy;
end;

procedure TCCodeParserTool.Clear;
begin
  Tree.Clear;
end;

procedure TCCodeParserTool.Parse;
begin
  Parse(Code);
end;

procedure TCCodeParserTool.Parse(aCode: TCodeBuffer);
begin
  if (Code=aCode) and (not UpdateNeeded) then
    exit;
  Code:=aCode;
  InitParser;
  repeat
    ReadNextAtom;
    if SrcPos<=SrcLen then begin
      FDefaultTokenList.DoItCaseSensitive(Src,AtomStart,SrcPos-AtomStart);
    end else begin
      break;
    end;
  until false;
  CloseNodes;
end;

function TCCodeParserTool.UpdateNeeded: boolean;
begin
  Result:=true;
  if (Code=nil) or (Tree=nil) or (Tree.Root=nil) then exit;
  if Code.ChangeStep<>ParseChangeStep then exit;
  Result:=false;
end;

procedure TCCodeParserTool.MoveCursorToPos(p: integer);
begin
  SrcPos:=p;
  AtomStart:=p;
  LastAtomStart:=0;
  LastSrcPos:=0;
end;

procedure TCCodeParserTool.ReadNextAtom;
begin
  DebugLn(['TCCodeParserTool.ReadNextAtom START ',AtomStart,'-',SrcPos,' ',Src[SrcPos]]);
  LastSrcPos:=SrcPos;
  LastAtomStart:=AtomStart;
  repeat
    ReadRawNextCAtom(Src,SrcPos,AtomStart);
  until (SrcPos>SrcLen) or (not (Src[AtomStart] in [#10,#13]));
  DebugLn(['TCCodeParserTool.ReadNextAtom END ',AtomStart,'-',SrcPos,' "',copy(Src,AtomStart,SrcPos-AtomStart),'"']);
end;

procedure TCCodeParserTool.UndoReadNextAtom;
begin
  if LastSrcPos>0 then begin
    SrcPos:=LastSrcPos;
    AtomStart:=LastAtomStart;
    LastSrcPos:=0;
    LastAtomStart:=0;
  end else begin
    SrcPos:=AtomStart;
  end;
end;

function TCCodeParserTool.ReadTilBracketClose(
  ExceptionOnNotFound: boolean): boolean;
// AtomStart must be on bracket open
// after reading AtomStart is on closing bracket
var
  CloseBracket: Char;
  StartPos: LongInt;
begin
  case Src[AtomStart] of
  '{': CloseBracket:='}';
  '[': CloseBracket:=']';
  '(': CloseBracket:=')';
  '<': CloseBracket:='>';
  else
    if ExceptionOnNotFound then
      RaiseExpectedButAtomFound('(');
    exit(false);
  end;
  StartPos:=AtomStart;
  {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
  {$R-}
  repeat
    ReadRawNextCAtom(Src,SrcPos,AtomStart);
    if AtomStart>SrcLen then begin
      AtomStart:=StartPos;
      SrcPos:=AtomStart+1;
      if ExceptionOnNotFound then
        RaiseException('closing bracket not found');
      exit;
    end;
    case Src[AtomStart] of
    '{','(','[':
      // skip nested bracketss
      begin
        if not ReadTilBracketClose(ExceptionOnNotFound) then
          exit;
      end;
    else
      if Src[AtomStart]=CloseBracket then exit(true);
    end;
  until false;
  {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}
end;

function TCCodeParserTool.AtomIs(const s: shortstring): boolean;
var
  len: Integer;
  i: Integer;
begin
  len:=length(s);
  if (len<>SrcPos-AtomStart) then exit(false);
  if SrcPos>SrcLen then exit(false);
  for i:=1 to len do
    if Src[AtomStart+i-1]<>s[i] then exit(false);
  Result:=true;
end;

function TCCodeParserTool.AtomIsChar(const c: char): boolean;
begin
  if SrcPos-AtomStart<>1 then exit(false);
  if SrcPos>SrcLen then exit(false);
  if Src[AtomStart]<>c then exit(false);
  Result:=true;
end;

function TCCodeParserTool.UpAtomIs(const s: shortstring): boolean;
var
  len: Integer;
  i: Integer;
begin
  len:=length(s);
  if (len<>SrcPos-AtomStart) then exit(false);
  if SrcPos>SrcLen then exit(false);
  for i:=1 to len do
    if UpChars[Src[AtomStart+i-1]]<>s[i] then exit(false);
  Result:=true;
end;

function TCCodeParserTool.AtomIsIdentifier: boolean;
var
  p: Integer;
begin
  if (AtomStart>=SrcPos) then exit(false);
  if (SrcPos>SrcLen) or (SrcPos-AtomStart>255) then exit(false);
  if not IsIdentStartChar[Src[AtomStart]] then exit(false);
  p:=AtomStart+1;
  while (p<SrcPos) do begin
    if not IsIdentChar[Src[p]] then exit(false);
    inc(p);
  end;
  Result:=true;
end;

function TCCodeParserTool.AtomIsStringConstant: boolean;
begin
  Result:=(AtomStart<SrcLen) and (Src[AtomStart]='"');
end;

function TCCodeParserTool.GetAtom: string;
begin
  Result:=copy(Src,AtomStart,SrcPos-AtomStart);
end;

procedure TCCodeParserTool.Replace(FromPos, ToPos: integer; const NewSrc: string
  );
var
  Node: TCodeTreeNode;
  DiffPos: Integer;
begin
  DebugLn(['TCCodeParserTool.Replace ',FromPos,'-',ToPos,' Old="',copy(Src,FromPos,ToPos-FromPos),'" New="',NewSrc,'"']);
  IncreaseChangeStep;
  Code.Replace(FromPos,ToPos-FromPos,NewSrc);
  Src:=Code.Source;
  SrcLen:=length(Src);
  // update positions
  DiffPos:=length(NewSrc)-(ToPos-FromPos);
  if DiffPos<>0 then begin
    Node:=Tree.Root;
    while Node<>nil do begin
      AdjustPositionAfterInsert(Node.StartPos,true,FromPos,ToPos,DiffPos);
      AdjustPositionAfterInsert(Node.EndPos,false,FromPos,ToPos,DiffPos);
      Node:=Node.Next;
    end;
  end;
end;

procedure TCCodeParserTool.IncreaseChangeStep;
begin
  if FChangeStep<>$7fffffff then
    inc(FChangeStep)
  else
    FChangeStep:=-$7fffffff;
end;

procedure TCCodeParserTool.WriteDebugReport;
var
  Node: TCodeTreeNode;
begin
  DebugLn(['TCCodeParserTool.WriteDebugReport ']);
  if Tree<>nil then begin
    Node:=Tree.Root;
    while Node<>nil do begin
      DebugLn([GetIndentStr(Node.GetLevel*2)+CCNodeDescAsString(Node.Desc)]);
      Node:=Node.Next;
    end;
  end;
end;

end.

