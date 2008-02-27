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
  ccnBase     = 1000;
  ccnNone     =  0+ccnBase;

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
  //DebugLn(['TCCodeParserTool.ReadNextAtom START ',AtomStart,'-',SrcPos,' ',Src[SrcPos]]);
  ReadRawNextCAtom(Src,SrcPos,AtomStart);
  //DebugLn(['TCCodeParserTool.ReadNextAtom END ',AtomStart,'-',SrcPos,' ',copy(Src,AtomStart,SrcPos-AtomStart)]);
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

