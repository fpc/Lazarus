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
  KeywordFuncLists, LinkScanner, CodeCache, AVL_Tree,
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
    FDisableUnusedDefines: boolean;
    FRemoveDisabledDirectives: boolean;
    FSimplifyExpressions: boolean;
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

    procedure CheckAndImproveExpr_Brackets(Node: TCodeTreeNode;
                                           var Changed: boolean);
    procedure CheckAndImproveExpr_IfDefinedMacro(Node: TCodeTreeNode;
                                                 var Changed: boolean);
    procedure DisableAllUnusedDefines(var Changed: boolean);
    procedure DisableDefineNode(Node: TCodeTreeNode; var Changed: boolean);
    procedure DisableIfNode(Node: TCodeTreeNode; var Changed: boolean);
    procedure DisableNode(Node: TCodeTreeNode; var Changed: boolean);
    procedure RemoveNode(Node: TCodeTreeNode);
    procedure RemoveEmptyNodes(var Changed: boolean);
  public
    Code: TCodeBuffer;
    Src: string;
    SrcLen: integer;
    NestedComments: boolean;
    Tree: TCodeTree;
    CurNode: TCodeTreeNode;
    SrcPos: Integer;
    AtomStart: integer;
    Macros: TAVLTree;// tree of TCompilerMacroStats

    constructor Create;
    destructor Destroy; override;
    
    function Parse(aCode: TCodeBuffer; aNestedComments: boolean): boolean;
    procedure ReduceCompilerDirectives(var Changed: boolean);
    procedure GatherH2PasFunctions(out ListOfH2PasFunctions: TFPList;
                                   FindDefNodes: boolean);
    procedure FixMissingH2PasDirectives(var Changed: boolean);
    
    function GetDirectiveName(Node: TCodeTreeNode): string;
    function GetDirective(Node: TCodeTreeNode): string;
    function GetIfExpression(Node: TCodeTreeNode;
                             out ExprStart, ExprEnd: integer): boolean;
    function GetDefineNameAndValue(DefineNode: TCodeTreeNode;
          out NameStart: integer; out HasValue: boolean; out ValueStart: integer
          ): boolean;
    function FindNodeAtPos(p: integer): TCodeTreeNode;
    procedure MoveCursorToPos(p: integer);
    procedure ReadNextAtom;
    function ReadTilBracketClose(CloseBracket: char): boolean;
    function AtomIs(const s: shortstring): boolean;
    function UpAtomIs(const s: shortstring): boolean;
    function AtomIsIdentifier: boolean;
    function GetAtom: string;
    procedure Replace(FromPos, ToPos: integer; const NewSrc: string);
    procedure WriteDebugReport;
  public
    property SimplifyExpressions: boolean read FSimplifyExpressions
                                          write FSimplifyExpressions;
    property DisableUnusedDefines: boolean read FDisableUnusedDefines
                                           write FDisableUnusedDefines;
    property RemoveDisabledDirectives: boolean read FRemoveDisabledDirectives
                                               write FRemoveDisabledDirectives;
  end;

  TCompilerMacroStatus = (
    cmsUnknown,   // never seen
    cmsDefined,   // set to a specific value e.g. by $Define or by $IfDef
    cmsUndefined, // undefined e.g. by $Undef
    cmsComplex    // value depends on complex expressions. e.g. {$if A or B}.
    );

  TCompilerMacroStats = class
  public
    Name: string;
    Value: string;
    Status: TCompilerMacroStatus;
    LastDefineNode: TCodeTreeNode;// define or undef node
    LastReadNode: TCodeTreeNode;// if node
  end;
  
  { TH2PasFunction }

  TH2PasFunction = class
  public
    Name: string;
    HeaderStart: integer;
    HeaderEnd: integer;
    BeginStart: integer;
    BeginEnd: integer;
    IsForward: boolean;
    IsExternal: boolean;
    InInterface: boolean;
    DefNode: TH2PasFunction;// the corresponding node
    function NeedsBody: boolean;
    procedure AdjustPositionsAfterInsert(StartPos, DiffPos: integer);
  end;
  
function CompareCompilerMacroStats(Data1, Data2: Pointer): integer;
function ComparePCharWithCompilerMacroStats(Name, MacroStats: Pointer): integer;
function CompareH2PasFuncByNameAndPos(Data1, Data2: Pointer): integer;
function ComparePCharWithH2PasFuncName(Name, H2PasFunc: Pointer): integer;

function CDNodeDescAsString(Desc: TCompilerDirectiveNodeDesc): string;

procedure AdjustPositionAfterInsert(var p: integer; StartPos, DiffPos: integer);

implementation

function CompareCompilerMacroStats(Data1, Data2: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(Pointer(TCompilerMacroStats(Data1).Name),
                                Pointer(TCompilerMacroStats(Data2).Name));
end;

function ComparePCharWithCompilerMacroStats(Name, MacroStats: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(Name,
                                Pointer(TCompilerMacroStats(MacroStats).Name));
end;

function CompareH2PasFuncByNameAndPos(Data1, Data2: Pointer): integer;
var
  F1: TH2PasFunction;
  F2: TH2PasFunction;
begin
  F1:=TH2PasFunction(Data1);
  F2:=TH2PasFunction(Data2);
  Result:=CompareIdentifierPtrs(Pointer(F1.Name),Pointer(F2.Name));
  if Result<>0 then exit;
  if F1.HeaderStart>F2.HeaderStart then
    exit(1)
  else if F1.HeaderStart<F2.HeaderStart then
    exit(-1)
  else
    exit(0);
end;

function ComparePCharWithH2PasFuncName(Name, H2PasFunc: Pointer): integer;
begin
  Result:=CompareIdentifierPtrs(Name,Pointer(TH2PasFunction(H2PasFunc).Name));
end;

function CDNodeDescAsString(Desc: TCompilerDirectiveNodeDesc): string;
begin
  case Desc of
  cdnNone     : Result:='None';

  cdnRoot     : Result:='Root';

  cdnDefine   : Result:='Define';

  cdnIf       : Result:='If';
  cdnElseIf   : Result:='ElseIf';
  cdnElse     : Result:='Else';
  cdnEnd      : Result:='End';
  else          Result:='?';
  end;
end;

procedure AdjustPositionAfterInsert(var p: integer; StartPos, DiffPos: integer);
begin
  if p>=StartPos then inc(p,DiffPos);
end;


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
  CurNode.EndPos:=AtomStart;
  CurNode:=CurNode.Parent;
end;

procedure TCompilerDirectivesTree.EndIFNode(const ErrorMsg: string);

  procedure RaiseMissingStartNode;
  begin
    WriteDebugReport;
    raise CDirectiveParserException.Create(ErrorMsg);
  end;

begin
  if (CurNode.Desc<>cdnIf) and (CurNode.Desc<>cdnElse)
  and (CurNode.Desc<>cdnElseIf) then
    RaiseMissingStartNode;
  EndChildNode;
end;

procedure TCompilerDirectivesTree.CheckAndImproveExpr_Brackets(
  Node: TCodeTreeNode; var Changed: boolean);
var
  ExprStart: integer;
  ExprEnd: integer;
  NameStart: LongInt;
  FromPos: LongInt;
  ToPos: LongInt;
begin
  if not SimplifyExpressions then exit;
  if (Node.SubDesc<>cdnsIf) and (Node.SubDesc<>cdnElseIf) then exit;
  if not GetIfExpression(Node,ExprStart,ExprEnd) then exit;

  // improve (MacroName) to MacroName
  MoveCursorToPos(ExprStart);
  repeat
    ReadNextAtom;
    if UpAtomIs('DEFINED') then begin
      // the function defined(): skip keyword and bracket
      ReadNextAtom;
      ReadNextAtom;
    end;
    if AtomIs('(') then begin
      FromPos:=AtomStart;
      ReadNextAtom;
      if AtomIsIdentifier then begin
        NameStart:=AtomStart;
        ReadNextAtom;
        if AtomIs(')') then begin
          ToPos:=SrcPos;
          Replace(FromPos,ToPos,GetIdentifier(@Src[NameStart]));
          MoveCursorToPos(FromPos);
        end;
      end;
    end;
  until SrcPos>=ExprEnd;
end;

procedure TCompilerDirectivesTree.CheckAndImproveExpr_IfDefinedMacro(
  Node: TCodeTreeNode; var Changed: boolean);
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
  if not SimplifyExpressions then exit;
  if (Node.SubDesc<>cdnsIf) then exit;
  if not GetIfExpression(Node,ExprStart,ExprEnd) then exit;
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

  Changed:=true;
end;

procedure TCompilerDirectivesTree.DisableAllUnusedDefines(var Changed: boolean);
var
  AVLNode: TAVLTreeNode;
  MacroNode: TCompilerMacroStats;
  NextAVLNode: TAVLTreeNode;
begin
  if Macros=nil then exit;
  if not DisableUnusedDefines then exit;
  AVLNode:=Macros.FindLowest;
  while AVLNode<>nil do begin
    NextAVLNode:=Macros.FindSuccessor(AVLNode);
    MacroNode:=TCompilerMacroStats(AVLNode.Data);
    if (MacroNode.LastDefineNode<>nil)
    and (MacroNode.LastReadNode=nil) then begin
      // this Define/Undef is not used
      DisableDefineNode(MacroNode.LastDefineNode,Changed);
    end;
    AVLNode:=NextAVLNode;
  end;
end;

procedure TCompilerDirectivesTree.DisableDefineNode(Node: TCodeTreeNode;
  var Changed: boolean);
var
  FromPos: LongInt;
  ToPos: LongInt;
  NewSrc: String;
begin
  if not DisableUnusedDefines then exit;
  DebugLn(['TCompilerDirectivesTree.DisableDefineNode ',GetDirective(Node)]);
  if RemoveDisabledDirectives then begin
    // remove directive (including space+empty lines in front and spaces behind)
    FromPos:=Node.StartPos;
    while (FromPos>1) and (IsSpaceChar[Src[FromPos-1]]) do dec(FromPos);
    ToPos:=FindCommentEnd(Src,Node.StartPos,NestedComments);
    ToPos:=FindLineEndOrCodeAfterPosition(Src,ToPos,SrcLen+1,NestedComments);
    NewSrc:='';
    if (FromPos=1) and (ToPos<SrcLen) and (Src[ToPos] in [#10,#13]) then begin
      inc(ToPos);
      if (ToPos<=SrcLen) and (Src[ToPos] in [#10,#13])
      and (Src[ToPos]<>Src[ToPos-1]) then
        inc(ToPos);
    end;
    Replace(FromPos,ToPos,NewSrc);
  end else begin
    // disable directive -> {$off Define MacroName}
    Replace(Node.StartPos+1,Node.StartPos+1,'off ');
  end;
  Changed:=true;
  RemoveNode(Node);
end;

procedure TCompilerDirectivesTree.DisableIfNode(Node: TCodeTreeNode;
  var Changed: boolean);
  
  procedure RaiseImpossible;
  begin
    raise CDirectiveParserException.Create('TCompilerDirectivesTree.DisableIfNode');
  end;
  
  function GetExpr(ExprNode: TCodeTreeNode): string;
  var
    ExprStart: integer;
    ExprEnd: integer;
  begin
    if not GetIfExpression(ExprNode,ExprStart,ExprEnd) then
      RaiseImpossible;
    Result:=copy(Src,ExprStart,ExprEnd-ExprStart);
  end;
  
var
  FromPos: LongInt;
  ToPos: LongInt;
  RemoveNextBrother: Boolean;
  Expr: String;
  ElseNode: TCodeTreeNode;
  ElseName: String;
  Expr2: String;
  NewSrc: String;
  PrevNode: TCodeTreeNode;
  NewDesc: TCompilerDirectiveNodeDesc;
  NewSubDesc: TCompilerDirectiveNodeDesc;
begin
  if (Node.FirstChild<>nil) or (Node.NextBrother=nil) then
    RaiseImpossible;
  RemoveNextBrother:=Node.NextBrother.Desc=cdnEnd;

  Changed:=true;
  
  // fix all following elseif and else nodes
  Expr:=GetExpr(Node);
  ElseNode:=Node.NextBrother;
  while ElseNode<>nil do begin
    if (ElseNode.Desc=cdnElse) or (ElseNode.Desc=cdnElseIf) then begin
      PrevNode:=ElseNode.PriorBrother;
      if (PrevNode.SubDesc=cdnsElIfC) or (PrevNode.SubDesc=cdnsElseC) then begin
        if PrevNode.Desc=cdnIf then begin
          NewDesc:=cdnIf;
          NewSubDesc:=cdnsIfC;
          ElseName:='IfC';
        end else begin
          NewDesc:=cdnElseIf;
          NewSubDesc:=cdnsElIfC;
          ElseName:='ElIfC';
        end;
      end else begin
        if PrevNode.Desc=cdnIf then begin
          NewDesc:=cdnIf;
          NewSubDesc:=cdnsIf;
          ElseName:='If';
        end else begin
          NewDesc:=cdnElseIf;
          NewSubDesc:=cdnsElseIf;
          ElseName:='ElseIf';
        end;
      end;
      // convert {$Else} to {$ElseIf not (Expr)}
      // convert {$ElseIf Expr2} to {$ElseIf (Expr2) and not (Expr)}
      if ElseNode.Desc=cdnElse then
        NewSrc:='{$'+ElseName+' not ('+Expr+')}'
      else begin
        Expr2:=GetExpr(ElseNode);
        NewSrc:='{$'+ElseName+' ('+Expr2+') and not ('+Expr+')}';
      end;
      Replace(ElseNode.StartPos,
              FindCommentEnd(Src,ElseNode.StartPos,NestedComments),NewSrc);
      ElseNode.Desc:=NewDesc;
      ElseNode.SubDesc:=NewSubDesc;
    end else begin
      break;
    end;
    ElseNode:=ElseNode.NextBrother;
  end;
  
  FromPos:=Node.StartPos;
  if RemoveNextBrother then begin
    ToPos:=FindCommentEnd(Src,Node.NextBrother.StartPos,NestedComments);
    ToPos:=FindLineEndOrCodeAfterPosition(Src,ToPos,SrcLen+1,NestedComments);
  end else
    ToPos:=Node.NextBrother.StartPos;
  if RemoveDisabledDirectives then begin
    // remove node source completely
    Replace(FromPos,ToPos,'');
  end else begin
    // disable directive -> {$off IfDef MacroName}
    Replace(Node.StartPos+1,Node.StartPos+1,'off ');
  end;
  
  if RemoveNextBrother then
    RemoveNode(Node.NextBrother);
  RemoveNode(Node);
end;

procedure TCompilerDirectivesTree.DisableNode(Node: TCodeTreeNode;
  var Changed: boolean);

  procedure RaiseRemoveImpossible;
  begin
    raise CDirectiveParserException.Create('TCompilerDirectivesTree.RemoveNodeWithCode not implemented yet');
  end;

begin
  case Node.SubDesc of
  cdnsDefine, cdnsUndef, cdnsSetC, cdnsShortSwitch, cdnsLongSwitch, cdnsMode:
    DisableDefineNode(Node,Changed);
  cdnsIfdef:
    DisableIfNode(Node,Changed);
  else
    RaiseRemoveImpossible;
  end;
end;

procedure TCompilerDirectivesTree.RemoveNode(Node: TCodeTreeNode);
var
  AVLNode: TAVLTreeNode;
  MacroNode: TCompilerMacroStats;
begin
  // clear references
  AVLNode:=Macros.FindLowest;
  while AVLNode<>nil do begin
    MacroNode:=TCompilerMacroStats(AVLNode.Data);
    if MacroNode.LastDefineNode=Node then
      MacroNode.LastDefineNode:=nil;
    if MacroNode.LastReadNode=Node then
      MacroNode.LastReadNode:=nil;
    AVLNode:=Macros.FindSuccessor(AVLNode);
  end;

  // free node
  Tree.DeleteNode(Node);
end;

procedure TCompilerDirectivesTree.RemoveEmptyNodes(var Changed: boolean);
var
  Node: TCodeTreeNode;
  NextNode: TCodeTreeNode;
  
  procedure CheckNode;
  begin
    case Node.Desc of
    cdnIf,cdnElse,cdnElseIf: ;
    else exit;
    end;
    
    if (Node.NextBrother=nil) or (Node.FirstChild<>nil) then exit;
    case Node.NextBrother.Desc of
    cdnEnd,cdnElse,cdnElseIf:
      begin
        MoveCursorToPos(Node.StartPos);
        // skip directive
        ReadNextAtom;
        // read the following atom (token or directive)
        ReadNextAtom;
        if AtomStart=Node.NextBrother.StartPos then begin
          // node is empty
          NextNode:=Node.NextBrother;
          if NextNode.Desc=cdnEnd then
            NextNode:=NextNode.Next;
          DisableIfNode(Node,Changed);
        end;
      end;
    end;
  end;
  
begin
  DebugLn(['TCompilerDirectivesTree.RemoveEmptyNodes ']);
  Node:=Tree.Root;
  while Node<>nil do begin
    NextNode:=Node.Next;
    CheckNode;
    Node:=NextNode;
  end;
end;

constructor TCompilerDirectivesTree.Create;
begin
  Tree:=TCodeTree.Create;
  SimplifyExpressions:=true;
  DisableUnusedDefines:=true;
  RemoveDisabledDirectives:=true;
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
    WriteDebugReport;
    raise CDirectiveParserException.Create('missing EndIf');
  end;
  
var
  DirectiveName: PChar;
  Node: TCodeTreeNode;
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
  // close nodes
  Node:=CurNode;
  while Node<>nil do begin
    Node.EndPos:=AtomStart;
    Node:=Node.Parent;
  end;
  if CurNode<>Tree.Root then
    RaiseDanglingIFDEF;
  
  Result:=true;
  {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}
end;

procedure TCompilerDirectivesTree.ReduceCompilerDirectives(var Changed: boolean);

  function GetMacroNode(p: PChar): TCompilerMacroStats;
  var
    AVLNode: TAVLTreeNode;
  begin
    AVLNode:=Macros.FindKey(p,@ComparePCharWithCompilerMacroStats);
    if AVLNode<>nil then
      Result:=TCompilerMacroStats(AVLNode.Data)
    else
      Result:=nil;
  end;
  
  procedure CheckMacroInExpression(Node: TCodeTreeNode; NameStart: integer;
    Complex: boolean; var Changed: boolean);
  var
    MacroNode: TCompilerMacroStats;
  begin
    MacroNode:=GetMacroNode(@Src[NameStart]);
    if MacroNode=nil then begin
      MacroNode:=TCompilerMacroStats.Create;
      MacroNode.Name:=GetIdentifier(@Src[NameStart]);
      Macros.Add(MacroNode);
    end;
    MacroNode.LastReadNode:=Node;
    
    if not Complex then begin

    end;
  end;
  
  procedure CheckDefine(Node: TCodeTreeNode; var Changed: boolean);
  var
    MacroNode: TCompilerMacroStats;
    NameStart: integer;
    HasValue: boolean;
    ValueStart: integer;
  begin
    if (Node.SubDesc<>cdnsDefine) and (Node.SubDesc<>cdnsUndef)
    and (Node.SubDesc<>cdnsSetC) then exit;
    if not GetDefineNameAndValue(Node,NameStart,HasValue,ValueStart) then exit;
    MacroNode:=GetMacroNode(@Src[NameStart]);
    if MacroNode=nil then begin
      MacroNode:=TCompilerMacroStats.Create;
      MacroNode.Name:=GetIdentifier(@Src[NameStart]);
      Macros.Add(MacroNode);
    end;
    if (MacroNode.LastReadNode=nil) and (MacroNode.LastDefineNode<>nil)
    and (MacroNode.LastDefineNode.Parent=Node.Parent) then begin
      // last define was never used -> disable it
      DisableDefineNode(MacroNode.LastDefineNode,Changed);
    end;
    
    MacroNode.LastReadNode:=nil;
    MacroNode.LastDefineNode:=Node;
  end;
  
var
  Node: TCodeTreeNode;
  ExprStart: integer;
  ExprEnd: integer;
  Complex: Boolean;
  AtomCount: Integer;
  NextNode: TCodeTreeNode;
begin
  WriteDebugReport;
  Macros:=TAVLTree.Create(@CompareCompilerMacroStats);
  try
    Node:=Tree.Root;
    while Node<>nil do begin
      NextNode:=Node.Next;
    
      case Node.Desc of
      cdnIf,cdnElseIf:
        if GetIfExpression(Node,ExprStart,ExprEnd) then begin
          // improve expression
          CheckAndImproveExpr_Brackets(Node,Changed);
          CheckAndImproveExpr_IfDefinedMacro(Node,Changed);
        
          DebugLn(['TCompilerDirectivesTree.DisableUnusedDefines Expr=',copy(Src,ExprStart,ExprEnd-ExprStart)]);
          // check if it is a complex expression or just one macro
          AtomCount:=0;
          if (Node.SubDesc=cdnsIf) or (Node.SubDesc=cdnsIfC)
          or (Node.SubDesc=cdnsElseIf) then begin
            MoveCursorToPos(ExprStart);
            repeat
              ReadNextAtom;
              inc(AtomCount);
            until AtomStart>=ExprEnd;
          end;
          Complex:=AtomCount>1;
          DebugLn(['TCompilerDirectivesTree.DisableUnusedDefines Complex=',Complex]);
          
          // mark all macros as read
          MoveCursorToPos(ExprStart);
          repeat
            ReadNextAtom;
            if AtomIsIdentifier then begin
              CheckMacroInExpression(Node,AtomStart,Complex,Changed);
            end;
          until AtomStart>=ExprEnd;
        end;
        
      cdnDefine:
        CheckDefine(Node,Changed);
      end;
      
      Node:=NextNode;
    end;
    
    DisableAllUnusedDefines(Changed);
    
    RemoveEmptyNodes(Changed);
  finally
    Macros.FreeAndClear;
    FreeAndNil(Macros);
  end;
end;

procedure TCompilerDirectivesTree.GatherH2PasFunctions(out
  ListOfH2PasFunctions: TFPList; FindDefNodes: boolean);
var
  InInterface: boolean;

  procedure ReadFunction;
  var
    HeaderStart: LongInt;
    HeaderEnd: LongInt;
    FuncName: String;
    IsForward: Boolean;
    BlockLevel: Integer;
    FuncEnd: LongInt;
    CurH2PasFunc: TH2PasFunction;
    BeginStart: Integer;
    BeginEnd: Integer;
    IsExternal: Boolean;
  begin
    HeaderStart:=AtomStart;
    // read name
    ReadNextAtom;
    if not AtomIsIdentifier then exit;
    FuncName:=GetAtom;
    // read parameter list
    ReadNextAtom;
    if AtomIs('(') then begin
      if not ReadTilBracketClose(')') then exit;
      ReadNextAtom;
    end;
    // read colon
    if not AtomIs(':') then exit;
    // read result type
    ReadNextAtom;
    if not AtomIsIdentifier then exit;
    // read semicolon
    ReadNextAtom;
    if not AtomIs(';') then exit;
    HeaderEnd:=SrcPos;
    // read function modifiers
    IsForward:=false;
    IsExternal:=false;
    repeat
      ReadNextAtom;
      if (AtomStart<=SrcLen)
      and IsKeyWordProcedureSpecifier.DoIt(@Src[AtomStart]) then begin
        if UpAtomIs('EXTERNAL') then
          IsExternal:=true;
        if UpAtomIs('FORWARD') then
          IsForward:=true;
        repeat
          ReadNextAtom;
        until (AtomStart>SrcLen) or AtomIs(';');
        HeaderEnd:=SrcPos;
      end else
        break;
    until false;

    // read begin..end block
    BeginStart:=-1;
    BeginEnd:=-1;
    if (not IsForward) and (not InInterface) and (not IsExternal)
    and UpAtomIs('BEGIN') then begin
      BeginStart:=AtomStart;
      BlockLevel:=1;
      repeat
        ReadNextAtom;
        if (AtomStart>SrcLen) then break;
        if UpAtomIs('END') then begin
          dec(BlockLevel);
          if BlockLevel=0 then begin
            BeginEnd:=SrcPos;
            break;
          end;
        end else if UpAtomIs('BEGIN') or UpAtomIs('ASM') then
          inc(BlockLevel);
      until false;
    end else begin
      // undo forward read to make sure that current atom is the last of the function
      MoveCursorToPos(HeaderEnd);
    end;
    FuncEnd:=SrcPos;

    // found a function
    DebugLn(['ReadFunction ',copy(Src,HeaderStart,FuncEnd-HeaderStart)]);
    CurH2PasFunc:=TH2PasFunction.Create;
    CurH2PasFunc.Name:=FuncName;
    CurH2PasFunc.HeaderStart:=HeaderStart;
    CurH2PasFunc.HeaderEnd:=HeaderEnd;
    CurH2PasFunc.BeginStart:=BeginStart;
    CurH2PasFunc.BeginEnd:=BeginEnd;
    CurH2PasFunc.IsForward:=IsForward;
    CurH2PasFunc.InInterface:=InInterface;
    CurH2PasFunc.IsExternal:=IsExternal;
    if ListOfH2PasFunctions=nil then ListOfH2PasFunctions:=TFPList.Create;
    ListOfH2PasFunctions.Add(CurH2PasFunc);
  end;
  
  procedure DoFindDefNodes;
  var
    i: Integer;
    CurH2PasFunc: TH2PasFunction;
    TreeOfForwardFuncs: TAVLTree;
    TreeOfBodyFuncs: TAVLTree;
    AVLNode: TAVLTreeNode;
    BodyAVLNode: TAVLTreeNode;
    BodyFunc: TH2PasFunction;
  begin
    if ListOfH2PasFunctions=nil then exit;
    
    // create a tree of the function definitions
    // and a tree of the function bodies
    TreeOfForwardFuncs:=TAVLTree.Create(@CompareH2PasFuncByNameAndPos);
    TreeOfBodyFuncs:=TAVLTree.Create(@CompareH2PasFuncByNameAndPos);
    for i:=0 to ListOfH2PasFunctions.Count-1 do begin
      CurH2PasFunc:=TH2PasFunction(ListOfH2PasFunctions[i]);
      if CurH2PasFunc.NeedsBody then
        TreeOfForwardFuncs.Add(CurH2PasFunc)
      else if (CurH2PasFunc.BeginStart>0) then
        TreeOfBodyFuncs.Add(CurH2PasFunc);
    end;
    
    // search for every definition the corresponding body
    AVLNode:=TreeOfForwardFuncs.FindLowest;
    while AVLNode<>nil do begin
      CurH2PasFunc:=TH2PasFunction(AVLNode.Data);
      if CurH2PasFunc.DefNode=nil then begin
        BodyAVLNode:=TreeOfBodyFuncs.FindLeftMostKey(Pointer(CurH2PasFunc.Name),
                                                @ComparePCharWithH2PasFuncName);
        if BodyAVLNode<>nil then begin
          // there is at least one body with this name
          repeat
            BodyFunc:=TH2PasFunction(BodyAVLNode.Data);
            if BodyFunc.DefNode=nil then begin
              // this body node with the same name has not yet a definition node
              // => found the corresponding node
              BodyFunc.DefNode:=CurH2PasFunc;
              CurH2PasFunc.DefNode:=BodyFunc;
              break;
            end else begin
              // this body node has already a definition node
              // search next body node with same name
              BodyAVLNode:=TreeOfBodyFuncs.FindSuccessor(BodyAVLNode);
              if (BodyAVLNode=nil)
              or (ComparePCharWithH2PasFuncName(
                                Pointer(CurH2PasFunc.Name),BodyAVLNode.Data)<>0)
              then
                break;
            end;
          until false;
        end;
      end;
      AVLNode:=TreeOfBodyFuncs.FindSuccessor(AVLNode);
    end;
    
    // clean up
    TreeOfForwardFuncs.Free;
    TreeOfBodyFuncs.Free;
  end;

begin
  ListOfH2PasFunctions:=nil;

  InInterface:=false;
  MoveCursorToPos(1);
  repeat
    ReadNextAtom;
    if SrcPos>SrcLen then break;
    if UpAtomIs('FUNCTION') then begin
      ReadFunction;
    end else if UpAtomIs('INTERFACE') then begin
      InInterface:=true;
    end else if UpAtomIs('IMPLEMENTATION') then begin
      InInterface:=false;
    end;
  until false;
  
  if FindDefNodes then
    DoFindDefNodes;
end;

procedure TCompilerDirectivesTree.FixMissingH2PasDirectives(var Changed: boolean
  );
{ Adds the directives around the function bodies, that h2pas forgets to add.

}
type
  TBodyBlock = record
    Definition: TCodeTreeNode;
    FirstBodyFunc: TH2PasFunction;
    LastBodyFunc: TH2PasFunction;
  end;

var
  CurBodyBlock: TBodyBlock;
  MacroNames: TStrings; // the Objects are the TCodeTreeNode
  ListOfH2PasFunctions: TFPList;

  function IsSameDirective(OldNode: TCodeTreeNode; Position: integer;
    out NewNode: TCodeTreeNode): boolean;
  begin
    NewNode:=FindNodeAtPos(Position);
    Result:=(NewNode<>nil) and (NewNode=OldNode);
  end;
  
  function HasCodeBetween(FromPos, ToPos: integer): boolean;
  begin
    if FromPos<1 then FromPos:=1;
    if FromPos>ToPos then exit;
    MoveCursorToPos(FromPos);
    ReadNextAtom;
    Result:=AtomStart<ToPos;
  end;
  
  function GetMacroNameForNode(Node: TCodeTreeNode; out IsNew: boolean): string;
  var
    i: Integer;
  begin
    if MacroNames=nil then
      MacroNames:=TStringList.Create;
    for i:=0 to MacroNames.Count-1 do
      if MacroNames.Objects[i]=Node then begin
        Result:=MacroNames[i];
        IsNew:=false;
        exit;
      end;
    IsNew:=true;
    Result:='H2PAS_FUNCTIONS_'+IntToStr(MacroNames.Count+1);
    MacroNames.AddObject(Result,Node);
  end;
  
  procedure LokalReplace(FromPos, ToPos: integer; const NewSrc: string);
  var
    DiffPos: Integer;
    i: Integer;
    Func: TH2PasFunction;
  begin
    Replace(FromPos,ToPos,NewSrc);
    // update positions
    DiffPos:=length(NewSrc)-(ToPos-FromPos);
    if DiffPos<>0 then begin
      for i:=0 to ListOfH2PasFunctions.Count do begin
        Func:=TH2PasFunction(ListOfH2PasFunctions[i]);
        Func.AdjustPositionsAfterInsert(ToPos,DiffPos);
      end;
    end;
  end;
  
  procedure StartBodyBlock(BodyFunc: TH2PasFunction; DefNode: TCodeTreeNode);
  begin
    CurBodyBlock.Definition:=DefNode;
    CurBodyBlock.FirstBodyFunc:=BodyFunc;
    CurBodyBlock.LastBodyFunc:=BodyFunc;
  end;
  
  procedure EndBodyBlock;
  var
    MacroName: String;
    InsertPos: LongInt;
    IsNewMacro: boolean;
  begin
    if CurBodyBlock.Definition=nil then exit;
    if CurBodyBlock.Definition<>Tree.Root then begin
      // create unique macro name
      MacroName:=GetMacroNameForNode(CurBodyBlock.Definition,IsNewMacro);
      if IsNewMacro then begin
        // insert $DEFINE
        InsertPos:=FindCommentEnd(Src,CurBodyBlock.Definition.StartPos,NestedComments);
        LokalReplace(InsertPos,InsertPos,'{$DEFINE '+MacroName+'}');
      end;
      // insert $IFDEF
      InsertPos:=FindLineEndOrCodeInFrontOfPosition(Src,
                  CurBodyBlock.FirstBodyFunc.HeaderStart,1,NestedComments,true);
      LokalReplace(InsertPos,InsertPos,'{$IFDEF '+MacroName+'}');
      // insert $ENDIF
      InsertPos:=FindLineEndOrCodeAfterPosition(Src,
                      CurBodyBlock.LastBodyFunc.BeginEnd,1,NestedComments,true);
      LokalReplace(InsertPos,InsertPos,LineEnding+'{$ENDIF '+MacroName+'}');
    end;
    FillChar(CurBodyBlock,SizeOf(TBodyBlock),0);
  end;
  
var
  i: Integer;
  BodyFunc: TH2PasFunction;
  LastDefNode: TCodeTreeNode;
begin
  ListOfH2PasFunctions:=nil;
  MacroNames:=nil;
  try
    GatherH2PasFunctions(ListOfH2PasFunctions,true);
    if ListOfH2PasFunctions=nil then exit;
    FillChar(CurBodyBlock,SizeOf(TBodyBlock),0);
    LastDefNode:=nil;
    for i:=0 to ListOfH2PasFunctions.Count-1 do begin
      BodyFunc:=TH2PasFunction(ListOfH2PasFunctions[i]);
      DebugLn(['TCompilerDirectivesTree.FixMissingH2PasDirectives DefNode=',(BodyFunc.DefNode<>nil),' Body="',copy(Src,BodyFunc.HeaderStart,BodyFunc.HeaderEnd-BodyFunc.HeaderStart),'"']);
      if (BodyFunc.BeginStart<1) or (BodyFunc.DefNode=nil) then
        continue;
      DebugLn(['TCompilerDirectivesTree.FixMissingH2PasDirectives Body="',copy(Src,BodyFunc.HeaderStart,BodyFunc.BeginEnd-BodyFunc.HeaderStart),'"']);
      // this function is a body and has a definition
      
      if (CurBodyBlock.LastBodyFunc<>nil)
      and HasCodeBetween(CurBodyBlock.LastBodyFunc.BeginEnd,BodyFunc.HeaderStart)
      then begin
        // there is code between last function body and current function body
        // end last block
        EndBodyBlock;
      end;
      
      if not IsSameDirective(LastDefNode,
        BodyFunc.DefNode.HeaderStart,LastDefNode)
      then begin
        // another directive block => end last block
        EndBodyBlock;
      end;
      
      if (CurBodyBlock.Definition=nil) then begin
        // a new block
        StartBodyBlock(BodyFunc, LastDefNode);
      end else begin
        // continue current block
        CurBodyBlock.LastBodyFunc:=BodyFunc;
      end;
    end;
    // end last block
    EndBodyBlock;
    
  finally
    if ListOfH2PasFunctions<>nil then;
      for i:=0 to ListOfH2PasFunctions.Count-1 do
        TObject(ListOfH2PasFunctions[i]).Free;
    ListOfH2PasFunctions.Free;
    MacroNames.Free;
  end;

end;

function TCompilerDirectivesTree.GetDirectiveName(Node: TCodeTreeNode): string;
begin
  Result:=GetIdentifier(@Src[Node.StartPos+2]);
end;

function TCompilerDirectivesTree.GetDirective(Node: TCodeTreeNode): string;
begin
  Result:=copy(Src,Node.StartPos,
               FindCommentEnd(Src,Node.StartPos,NestedComments)-Node.StartPos);
end;

function TCompilerDirectivesTree.GetIfExpression(Node: TCodeTreeNode;
  out ExprStart, ExprEnd: integer): boolean;
var
  p: LongInt;
begin
  Result:=false;
  ExprStart:=-1;
  ExprEnd:=-1;
  p:=Node.StartPos+2;
  if p>SrcLen then exit;
  while (p<=SrcLen) and IsIdentChar[Src[p]] do inc(p);
  if (p>SrcLen) or (not IsSpaceChar[Src[p]]) then exit;
  inc(p);
  ExprStart:=p;
  while (p<=SrcLen) and (Src[p]<>'}') do inc(p);
  ExprEnd:=p;
  Result:=true;
end;

function TCompilerDirectivesTree.GetDefineNameAndValue(
  DefineNode: TCodeTreeNode; out NameStart: integer; out HasValue: boolean; out
  ValueStart: integer): boolean;
var
  p: LongInt;
begin
  Result:=false;
  NameStart:=-1;
  HasValue:=false;
  ValueStart:=-1;
  p:=DefineNode.StartPos+2;
  if p>SrcLen then exit;
  // skip keyword
  while (p<=SrcLen) and (IsIdentChar[Src[p]]) do inc(p);
  while (p<=SrcLen) and (IsSpaceChar[Src[p]]) do inc(p);
  // check name
  if p>SrcLen then exit;
  NameStart:=p;
  if not IsIdentStartChar[Src[p]] then exit;
  Result:=true;
  
  // skip name
  while (p<=SrcLen) and (IsIdentChar[Src[p]]) do inc(p);
  while (p<=SrcLen) and (IsSpaceChar[Src[p]]) do inc(p);
  if p>SrcLen then exit;
  if (Src[p]=':') and (p<SrcLen) and (Src[p+1]='=') then begin
    // has value
    HasValue:=true;
    inc(p,2);
    while (p<=SrcLen) and (IsSpaceChar[Src[p]]) do inc(p);
    ValueStart:=p;
  end;
end;

function TCompilerDirectivesTree.FindNodeAtPos(p: integer): TCodeTreeNode;
begin
  Result:=Tree.Root;
  while Result<>nil do begin
    if Result.StartPos>p then
      exit(nil);
    if (Result.EndPos>p)
    or  ((Result.EndPos=p) and (Result.NextBrother<>nil)
          and (Result.NextBrother.StartPos>p))
    then begin
      // p is in range of Result => check childs
      if (Result.FirstChild=nil)
      or (Result.FirstChild.StartPos>p) then
        exit;
      Result:=Result.FirstChild;
    end else begin
      // p is behind => next
      Result:=Result.NextSkipChilds;
    end;
  end;
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

function TCompilerDirectivesTree.ReadTilBracketClose(CloseBracket: char
  ): boolean;
begin
  Result:=false;
  repeat
    ReadNextAtom;
    if AtomStart>SrcLen then exit;
    if SrcPos-AtomStart=1 then begin
      if Src[AtomStart]=CloseBracket then
        exit(true)
      else if Src[AtomStart]='(' then
        ReadTilBracketClose(')')
      else if Src[AtomStart]='[' then
        ReadTilBracketClose(']');
    end;
  until false;
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

procedure TCompilerDirectivesTree.WriteDebugReport;
var
  Node: TCodeTreeNode;
begin
  DebugLn(['TCompilerDirectivesTree.WriteDebugReport ']);
  if Tree<>nil then begin
    Node:=Tree.Root;
    while Node<>nil do begin
      DebugLn([GetIndentStr(Node.GetLevel*2)+CDNodeDescAsString(Node.Desc),' ',GetDirective(Node)]);
      Node:=Node.Next;
    end;
  end;
end;

{ TH2PasFunction }

function TH2PasFunction.NeedsBody: boolean;
begin
  Result:=(IsForward or InInterface) and (not IsExternal) and (BeginStart<0);
end;

procedure TH2PasFunction.AdjustPositionsAfterInsert(StartPos, DiffPos: integer);
begin
  AdjustPositionAfterInsert(HeaderStart,StartPos,DiffPos);
  AdjustPositionAfterInsert(HeaderEnd,StartPos,DiffPos);
  AdjustPositionAfterInsert(BeginStart,StartPos,DiffPos);
  AdjustPositionAfterInsert(BeginEnd,StartPos,DiffPos);
end;

end.

