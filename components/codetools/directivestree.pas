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
    Functions to parse and edit compiler directives.
}
unit DirectivesTree; 

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, FileProcs, CodeToolsStructs, BasicCodeTools,
  ExprEval, KeywordFuncLists, LinkScanner, CodeCache, AVL_Tree,
  CodeToolMemManager, CodeTree;

type
  TCompilerDirectiveNodeDesc = word;
  
const
  // descriptors
  cdnBase     = 1000;
  cdnNone     =  0+cdnBase;
  
  cdnRoot     =  1+cdnBase;

  cdnDefine   = 11+cdnBase;

  cdnIf       = 21+cdnBase;
  cdnElseIf   = 22+cdnBase;
  cdnElse     = 23+cdnBase;
  cdnEnd      = 24+cdnBase;
  
  // sub descriptors
  cdnsBase        = 10000;
  cdnsNone        =  0+cdnsBase;
  
  cdnsIfdef       =  1+cdnsBase;
  cdnsIfC         =  2+cdnsBase;
  cdnsIfndef      =  3+cdnsBase;
  cdnsIf          =  4+cdnsBase;
  cdnsIfOpt       =  5+cdnsBase;
  cdnsEndif       = 11+cdnsBase;
  cdnsEndC        = 12+cdnsBase;
  cdnsIfEnd       = 13+cdnsBase;
  cdnsElse        = 21+cdnsBase;
  cdnsElseC       = 22+cdnsBase;
  cdnsElseIf      = 23+cdnsBase;
  cdnsElIfC       = 24+cdnsBase;
  cdnsDefine      = 31+cdnsBase;
  cdnsUndef       = 32+cdnsBase;
  cdnsSetC        = 33+cdnsBase;
  cdnsInclude     = 41+cdnsBase;
  cdnsIncludePath = 42+cdnsBase;
  cdnsShortSwitch = 51+cdnsBase;
  cdnsLongSwitch  = 52+cdnsBase;
  cdnsMode        = 53+cdnsBase;
  cdnsThreading   = 54+cdnsBase;

type
  CDirectiveParserException = class(Exception)
  end;

  { TCompilerDirectivesTree }

  TCompilerDirectivesTree = class
  private
    FDefaultDirectiveFuncList: TKeyWordFunctionList;
    function IfdefDirective: boolean;
    function IfCDirective: boolean;
    function IfndefDirective: boolean;
    function IfDirective: boolean;
    function IfOptDirective: boolean;
    function EndifDirective: boolean;
    function EndCDirective: boolean;
    function IfEndDirective: boolean;
    function ElseDirective: boolean;
    function ElseCDirective: boolean;
    function ElseIfDirective: boolean;
    function ElIfCDirective: boolean;
    function DefineDirective: boolean;
    function UndefDirective: boolean;
    function SetCDirective: boolean;
    function IncludeDirective: boolean;
    function IncludePathDirective: boolean;
    function ShortSwitchDirective: boolean;
    function ReadNextSwitchDirective: boolean;
    function LongSwitchDirective: boolean;
    function ModeDirective: boolean;
    function ThreadingDirective: boolean;
    procedure InitKeyWordList;

    procedure InitParser;
    procedure CreateChildNode(Desc: TCompilerDirectiveNodeDesc;
                              SubDesc: TCompilerDirectiveNodeDesc = cdnNone);
    procedure EndChildNode;
    procedure EndIFNode(const ErrorMsg: string);
  public
    Code: TCodeBuffer;
    Src: string;
    SrcLen: integer;
    NestedComments: boolean;
    Tree: TCodeTree;
    CurNode: TCodeTreeNode;
    SrcPos: Integer;
    AtomStart: integer;
    constructor Create;
    destructor Destroy; override;
    function Parse(aCode: TCodeBuffer; aNestedComments: boolean): boolean;
    function SimplifyExpressions: boolean;// true if changed
    function DisableUnusedDefines(Delete: boolean): boolean;
    function GetExpression(Node: TCodeTreeNode;
                           out ExprStart, ExprEnd: integer): boolean;
    procedure MoveCursorToPos(p: integer);
    procedure ReadNextAtom;
    function AtomIs(const s: shortstring): boolean;
    function UpAtomIs(const s: shortstring): boolean;
    function AtomIsIdentifier: boolean;
    function GetAtom: string;
    procedure Replace(FromPos, ToPos: integer; const NewSrc: string);
  end;


implementation

{ TCompilerDirectivesTree }

function TCompilerDirectivesTree.IfdefDirective: boolean;
// example: {$IFDEF macroname}
begin
  Result:=true;
  CreateChildNode(cdnIf,cdnsIfdef);
end;

function TCompilerDirectivesTree.IfCDirective: boolean;
// example: {$IFC expression}
begin
  Result:=true;
  CreateChildNode(cdnIf,cdnsIfC);
end;

function TCompilerDirectivesTree.IfndefDirective: boolean;
// example: {$IFNDEF macroname}
begin
  Result:=true;
  CreateChildNode(cdnIf,cdnsIfndef);
end;

function TCompilerDirectivesTree.IfDirective: boolean;
// example: {$IF expression}
begin
  Result:=true;
  CreateChildNode(cdnIf,cdnsIf);
end;

function TCompilerDirectivesTree.IfOptDirective: boolean;
// {$ifopt o+} or {$ifopt o-}
begin
  Result:=true;
  CreateChildNode(cdnIf,cdnsIfOpt);
end;

function TCompilerDirectivesTree.EndifDirective: boolean;
// example: {$ENDIF comment}
begin
  Result:=true;
  EndIFNode('EndIf without IfDef');
  CreateChildNode(cdnEnd,cdnsEndif);
  EndChildNode;
end;

function TCompilerDirectivesTree.EndCDirective: boolean;
// example: {$ENDC comment}
begin
  Result:=true;
  EndIFNode('EndC without IfC');
  CreateChildNode(cdnEnd,cdnsEndC);
  EndChildNode;
end;

function TCompilerDirectivesTree.IfEndDirective: boolean;
// {$IfEnd comment}
begin
  Result:=true;
  EndIFNode('IfEnd without IfDef');
  CreateChildNode(cdnEnd,cdnsIfEnd);
  EndChildNode;
end;

function TCompilerDirectivesTree.ElseDirective: boolean;
// {$Else comment}
begin
  Result:=true;
  EndIFNode('Else without IfDef');
  CreateChildNode(cdnElse,cdnsElse);
end;

function TCompilerDirectivesTree.ElseCDirective: boolean;
// {$elsec comment}
begin
  Result:=true;
  EndIFNode('ElseC without IfC');
  CreateChildNode(cdnElse,cdnsElseC);
end;

function TCompilerDirectivesTree.ElseIfDirective: boolean;
// {$elseif expression}
begin
  Result:=true;
  EndIFNode('ElseIf without IfDef');
  CreateChildNode(cdnElseIf,cdnsElseIf);
end;

function TCompilerDirectivesTree.ElIfCDirective: boolean;
// {$elifc expression}
begin
  Result:=true;
  EndIFNode('ElIfC without IfC');
  CreateChildNode(cdnElseIf,cdnsElIfC);
end;

function TCompilerDirectivesTree.DefineDirective: boolean;
// {$define name} or {$define name:=value}
begin
  Result:=true;
  CreateChildNode(cdnDefine,cdnsDefine);
  EndChildNode;
end;

function TCompilerDirectivesTree.UndefDirective: boolean;
// {$undefine macroname}
begin
  Result:=true;
  CreateChildNode(cdnDefine,cdnsUndef);
  EndChildNode;
end;

function TCompilerDirectivesTree.SetCDirective: boolean;
// {$setc macroname} or {$setc macroname:=value}
begin
  Result:=true;
  CreateChildNode(cdnDefine,cdnsSetC);
  EndChildNode;
end;

function TCompilerDirectivesTree.IncludeDirective: boolean;
begin
  Result:=true;
end;

function TCompilerDirectivesTree.IncludePathDirective: boolean;
// {$includepath path_addition}
begin
  Result:=true;
end;

function TCompilerDirectivesTree.ShortSwitchDirective: boolean;
// example: {$H+} or {$H+, R- comment}
begin
  Result:=true;
  CreateChildNode(cdnDefine,cdnsShortSwitch);
  EndChildNode;
end;

function TCompilerDirectivesTree.ReadNextSwitchDirective: boolean;
begin
  Result:=true;
end;

function TCompilerDirectivesTree.LongSwitchDirective: boolean;
// example: {$ASSERTIONS ON comment}
begin
  Result:=true;
  CreateChildNode(cdnDefine,cdnsLongSwitch);
  EndChildNode;
end;

function TCompilerDirectivesTree.ModeDirective: boolean;
// example: {$MODE ObjFPC comment}
begin
  Result:=true;
  CreateChildNode(cdnDefine,cdnsMode);
  EndChildNode;
end;

function TCompilerDirectivesTree.ThreadingDirective: boolean;
// example: {$threading on}
begin
  Result:=true;
  CreateChildNode(cdnDefine,cdnsThreading);
  EndChildNode;
end;

procedure TCompilerDirectivesTree.InitKeyWordList;
var
  c: Char;
begin
  if FDefaultDirectiveFuncList=nil then begin
    FDefaultDirectiveFuncList:=TKeyWordFunctionList.Create;
    with FDefaultDirectiveFuncList do begin
      for c:='A' to 'Z' do begin
        if CompilerSwitchesNames[c]<>'' then begin
          Add(c,{$ifdef FPC}@{$endif}ShortSwitchDirective);
          Add(CompilerSwitchesNames[c],{$ifdef FPC}@{$endif}LongSwitchDirective);
        end;
      end;
      Add('IFDEF',{$ifdef FPC}@{$endif}IfdefDirective);
      Add('IFC',{$ifdef FPC}@{$endif}IfCDirective);
      Add('IFNDEF',{$ifdef FPC}@{$endif}IfndefDirective);
      Add('IF',{$ifdef FPC}@{$endif}IfDirective);
      Add('IFOPT',{$ifdef FPC}@{$endif}IfOptDirective);
      Add('ENDIF',{$ifdef FPC}@{$endif}EndIfDirective);
      Add('ENDC',{$ifdef FPC}@{$endif}EndCDirective);
      Add('ELSE',{$ifdef FPC}@{$endif}ElseDirective);
      Add('ELSEC',{$ifdef FPC}@{$endif}ElseCDirective);
      Add('ELSEIF',{$ifdef FPC}@{$endif}ElseIfDirective);
      Add('ELIFC',{$ifdef FPC}@{$endif}ElIfCDirective);
      Add('IFEND',{$ifdef FPC}@{$endif}IfEndDirective);
      Add('DEFINE',{$ifdef FPC}@{$endif}DefineDirective);
      Add('UNDEF',{$ifdef FPC}@{$endif}UndefDirective);
      Add('SETC',{$ifdef FPC}@{$endif}SetCDirective);
      Add('INCLUDE',{$ifdef FPC}@{$endif}IncludeDirective);
      Add('INCLUDEPATH',{$ifdef FPC}@{$endif}IncludePathDirective);
      Add('MODE',{$ifdef FPC}@{$endif}ModeDirective);
      Add('THREADING',{$ifdef FPC}@{$endif}ThreadingDirective);
    end;
  end;
end;

procedure TCompilerDirectivesTree.InitParser;
begin
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
  CreateChildNode(cdnRoot);
  CurNode.Desc:=cdnRoot;
end;

procedure TCompilerDirectivesTree.CreateChildNode(
  Desc: TCompilerDirectiveNodeDesc;
  SubDesc: TCompilerDirectiveNodeDesc);
var NewNode: TCodeTreeNode;
begin
  NewNode:=NodeMemManager.NewNode;
  Tree.AddNodeAsLastChild(CurNode,NewNode);
  NewNode.Desc:=Desc;
  NewNode.SubDesc:=SubDesc;
  CurNode:=NewNode;
  CurNode.StartPos:=AtomStart;
  //DebugLn([GetIndentStr(CurNode.GetLevel*2),'TCompilerDirectivesTree.CreateChildNode ']);
end;

procedure TCompilerDirectivesTree.EndChildNode;
begin
  //DebugLn([GetIndentStr(CurNode.GetLevel*2),'TCompilerDirectivesTree.EndChildNode ']);
  CurNode:=CurNode.Parent;
end;

procedure TCompilerDirectivesTree.EndIFNode(const ErrorMsg: string);

  procedure RaiseMissingStartNode;
  begin
    raise CDirectiveParserException.Create(ErrorMsg);
  end;

begin
  if (CurNode.Desc<>cdnIf) and (CurNode.Desc<>cdnElseIf) then
    RaiseMissingStartNode;
  EndChildNode;
end;

constructor TCompilerDirectivesTree.Create;
begin
  Tree:=TCodeTree.Create;
end;

destructor TCompilerDirectivesTree.Destroy;
begin
  Tree.Free;
  FDefaultDirectiveFuncList.Free;
  inherited Destroy;
end;

function TCompilerDirectivesTree.Parse(aCode: TCodeBuffer;
  aNestedComments: boolean): boolean;
  
  procedure RaiseDanglingIFDEF;
  begin
    raise CDirectiveParserException.Create('missing EndIf');
  end;
  
var
  DirectiveName: PChar;
begin
  {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
  {$R-}
  Result:=false;
  Code:=aCode;
  NestedComments:=aNestedComments;
  InitParser;
  repeat
    ReadRawNextPascalAtom(Src,SrcPos,AtomStart,NestedComments);
    //DebugLn(['TCompilerDirectivesTree.Parse ',copy(Src,AtomStart,SrcPos-AtomStart)]);
    if SrcPos<=SrcLen then begin
      if (Src[AtomStart]='{') and (Src[AtomStart+1]='$') then begin
        // compiler directive
        DirectiveName:=@Src[AtomStart+2];
        //DebugLn(['ParseCompilerDirectives ',GetIdentifier(DirectiveName)]);
        FDefaultDirectiveFuncList.DoIt(DirectiveName);
      end;
    end else begin
      break;
    end;
  until false;
  if CurNode<>Tree.Root then
    RaiseDanglingIFDEF;
  
  Result:=true;
  {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}
end;

function TCompilerDirectivesTree.SimplifyExpressions: boolean;

  function DirectiveIsIfDefinedMacro(Node: TCodeTreeNode): boolean;
  // check if {$IF defined(MacroName)}
  //       or {$IF !defined(MacroName)}
  //       or {$IF not defined(MacroName)}
  var
    ExprStart: integer;
    ExprEnd: integer;
    MacroNameStart: LongInt;
    Negated: Boolean;
    NewDirective: String;
  begin
    Result:=false;
    if (Node.Desc<>cdnIf) or (Node.SubDesc<>cdnsIf) then exit;
    if not GetExpression(Node,ExprStart,ExprEnd) then exit;
    Negated:=false;
    MoveCursorToPos(ExprStart);
    ReadNextAtom;
    if UpAtomIs('NOT') or AtomIs('!') then begin
      Negated:=true;
      ReadNextAtom;
    end;
    if not UpAtomIs('DEFINED') then exit;
    ReadNextAtom;
    if not AtomIs('(') then exit;
    ReadNextAtom;
    if not AtomIsIdentifier then exit;
    MacroNameStart:=AtomStart;
    ReadNextAtom;
    if not AtomIs(')') then exit;
    ReadNextAtom;
    if SrcPos<=ExprEnd then exit;
    
    if Negated then
      NewDirective:='IFNDEF'
    else
      NewDirective:='IFDEF';
    NewDirective:='{$'+NewDirective+' '+GetIdentifier(@Src[MacroNameStart])+'}';

    Replace(Node.StartPos,FindCommentEnd(Src,Node.StartPos,NestedComments),NewDirective);
    if Negated then
      Node.SubDesc:=cdnsIfNdef
    else
      Node.SubDesc:=cdnsIfdef;

    Result:=true;
  end;

var
  Node: TCodeTreeNode;
begin
  Result:=false;
  Node:=Tree.Root;
  while Node<>nil do begin
    //DebugLn(['TCompilerDirectivesTree.SimplifyExpressions ',copy(Src,Node.StartPos,20)]);
    if (Node.Desc=cdnIf) or (Node.Desc=cdnElseIf) then begin
      //DebugLn(['TCompilerDirectivesTree.SimplifyExpressions Expr="',copy(Src,ExprStart,ExprEnd-ExprStart),'"']);
      if DirectiveIsIfDefinedMacro(Node) then begin
        Result:=true;
      end;
    end;
    Node:=Node.Next;
  end;
end;

function TCompilerDirectivesTree.DisableUnusedDefines(Delete: boolean
  ): boolean;
var
  Node: TCodeTreeNode;
  ExprStart: integer;
  ExprEnd: integer;
begin
  Result:=false;
  Node:=Tree.Root;
  while Node<>nil do begin
    if (Node.Desc=cdnIf) or (Node.Desc=cdnElseIf) then begin
      if GetExpression(Node,ExprStart,ExprEnd) then begin
        MoveCursorToPos(ExprStart);
        ReadNextAtom;
        
      end;
      // mark all used variables
    end;
    Node:=Node.Next;
  end;
end;

function TCompilerDirectivesTree.GetExpression(Node: TCodeTreeNode;
  out ExprStart, ExprEnd: integer): boolean;
var
  p: LongInt;
begin
  Result:=false;
  ExprStart:=-1;
  ExprEnd:=-1;
  p:=Node.StartPos+2;
  if p>SrcLen then exit;
  inc(p,GetIdentLen(@Src[p]));
  if (p>SrcLen) or (not IsSpaceChar[Src[p]]) then exit;
  inc(p);
  ExprStart:=p;
  while (p<=SrcLen) and (Src[p]<>'}') do inc(p);
  ExprEnd:=p;
  Result:=true;
end;

procedure TCompilerDirectivesTree.MoveCursorToPos(p: integer);
begin
  SrcPos:=p;
  AtomStart:=p;
end;

procedure TCompilerDirectivesTree.ReadNextAtom;
begin
  //DebugLn(['TCompilerDirectivesTree.ReadNextAtom START ',AtomStart,'-',SrcPos,' ',Src[SrcPos]]);
  ReadRawNextPascalAtom(Src,SrcPos,AtomStart,NestedComments);
  //DebugLn(['TCompilerDirectivesTree.ReadNextAtom END ',AtomStart,'-',SrcPos,' ',copy(Src,AtomStart,SrcPos-AtomStart)]);
end;

function TCompilerDirectivesTree.AtomIs(const s: shortstring): boolean;
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

function TCompilerDirectivesTree.UpAtomIs(const s: shortstring): boolean;
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

function TCompilerDirectivesTree.AtomIsIdentifier: boolean;
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

function TCompilerDirectivesTree.GetAtom: string;
begin
  Result:=copy(Src,AtomStart,SrcPos-AtomStart);
end;

procedure TCompilerDirectivesTree.Replace(FromPos, ToPos: integer;
  const NewSrc: string);
var
  Node: TCodeTreeNode;
  DiffPos: Integer;
begin
  DebugLn(['TCompilerDirectivesTree.Replace Old="',copy(Src,FromPos,ToPos-FromPos),'" New="',NewSrc,'"']);
  Code.Replace(FromPos,ToPos-FromPos,NewSrc);
  Src:=Code.Source;
  SrcLen:=length(Src);
  // update positions
  DiffPos:=length(NewSrc)-(ToPos-FromPos);
  if DiffPos<>0 then begin
    Node:=Tree.Root;
    while Node<>nil do begin
      if Node.StartPos>FromPos then inc(Node.StartPos,DiffPos);
      if Node.EndPos>FromPos then inc(Node.EndPos,DiffPos);
      Node:=Node.Next;
    end;
  end;
end;

end.

