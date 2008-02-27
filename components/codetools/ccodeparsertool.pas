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
    procedure InitKeyWordList;

    procedure InitParser;
    procedure CreateChildNode(Desc: TCCodeNodeDesc);
    procedure EndChildNode;
    procedure CloseNodes;
    
    procedure RaiseException(const AMessage: string);
  public
    Code: TCodeBuffer;
    Src: string;
    SrcLen: integer;
    Tree: TCodeTree;
    CurNode: TCodeTreeNode;
    SrcPos: Integer;
    AtomStart: integer;
    ParseChangeStep: integer;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure Parse;
    procedure Parse(aCode: TCodeBuffer);
    function UpdateNeeded: boolean;
    
    procedure MoveCursorToPos(p: integer);
    procedure ReadNextAtom;
    function AtomIs(const s: shortstring): boolean;
    function UpAtomIs(const s: shortstring): boolean;
    function AtomIsIdentifier: boolean;
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

procedure TCCodeParserTool.InitKeyWordList;
begin
  if FDefaultTokenList=nil then begin
    FDefaultTokenList:=TKeyWordFunctionList.Create;
    with FDefaultTokenList do begin
      Add('#',{$ifdef FPC}@{$endif}DirectiveToken);
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

procedure TCCodeParserTool.RaiseException(const AMessage: string);
begin
  CloseNodes;
  raise ECCodeParserException.Create(Self,AMessage);
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
      FDefaultTokenList.DoIt(Src,AtomStart,SrcPos-AtomStart);
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
end;

procedure TCCodeParserTool.ReadNextAtom;
begin
  DebugLn(['TCCodeParserTool.ReadNextAtom START ',AtomStart,'-',SrcPos,' ',Src[SrcPos]]);
  repeat
    ReadRawNextCAtom(Src,SrcPos,AtomStart);
  until (SrcPos>SrcLen) or (not (Src[AtomStart] in [#10,#13]));
  DebugLn(['TCCodeParserTool.ReadNextAtom END ',AtomStart,'-',SrcPos,' "',copy(Src,AtomStart,SrcPos-AtomStart),'"']);
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

