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
    TCodeCompletionCodeTool enhances TEventsCodeTool.
    

  ToDo:
    -ProcExists: search procs in ancestors too
    -VarExists: search vars in ancestors too
    -pipClassOrder
    -proc body -> add proc definition
}
unit CodeCompletionTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeTree, CodeAtom, PascalParserTool, EventCodeTool,
  SourceLog, KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, AVL_Tree,
  TypInfo, SourceChanger;

type
  NewClassPart = (ncpProcs, ncpVars);

  TCodeCompletionCodeTool = class(TEventsCodeTool)
  private
    ClassNode, StartNode: TCodeTreeNode;
    FirstInsert: TCodeTreeNodeExtension;
    JumpToProcName: string;
    ASourceChangeCache: TSourceChangeCache;
    NewPrivatSectionIndent, NewPrivatSectionInsertPos: integer;
    FSetPropertyVariablename: string;
    function ProcExists(const NameAndParams: string): boolean;
    function VarExists(const UpperName: string): boolean;
    procedure AddInsert(PosNode: TCodeTreeNode;
        const CleanDef, Def, IdentifierName: string);
    function NodeExtIsVariable(ANodeExt: TCodeTreeNodeExtension): boolean;
    function CompleteProperty(PropNode: TCodeTreeNode): boolean;
    procedure InsertNewClassParts(PartType: NewClassPart);
    function InsertAllNewClassParts: boolean;
    function CreateMissingProcBodies: boolean;
  public
    function CompleteCode(CursorPos: TCodeXYPosition;
        var NewPos: TCodeXYPosition; var NewTopLine: integer;
        SourceChangeCache: TSourceChangeCache): boolean;
    constructor Create;
    property SetPropertyVariablename: string
      read FSetPropertyVariablename write FSetPropertyVariablename;
  end;


implementation


{ TCodeCompletionCodeTool }

function TCodeCompletionCodeTool.ProcExists(
  const NameAndParams: string): boolean;
// NameAndParams should be uppercase and contains the proc name and the
// parameter list without names and default values
// and should not contain any comments, result types
var ANodeExt: TCodeTreeNodeExtension;
begin
  Result:=false;
  // search in new nodes, which will be inserted
  ANodeExt:=FirstInsert;
  while ANodeExt<>nil do begin
    if CompareTextIgnoringSpace(ANodeExt.Txt,NameAndParams,true)=0 then begin
      Result:=true;
      exit;
    end;
    ANodeExt:=ANodeExt.Next;
  end;
  if not Result then begin
    // ToDo: check ancestor procs too
    // search in current class
    Result:=(FindProcNode(StartNode,NameAndParams,[phpInUpperCase])<>nil);
  end;
end;

function TCodeCompletionCodeTool.VarExists(const UpperName: string): boolean;
var ANodeExt: TCodeTreeNodeExtension;
begin
  Result:=false;
  // search in new nodes, which will be inserted
  ANodeExt:=FirstInsert;
  while ANodeExt<>nil do begin
    if CompareTextIgnoringSpace(ANodeExt.Txt,UpperName,true)=0 then begin
      Result:=true;
      exit;
    end;
    ANodeExt:=ANodeExt.Next;
  end;
  if not Result then begin
    // ToDo: check ancestor vars too
    // search in current class
    Result:=(FindVarNode(StartNode,UpperName)<>nil);
  end;
end;

procedure TCodeCompletionCodeTool.AddInsert(PosNode: TCodeTreeNode;
  const CleanDef, Def, IdentifierName: string);
var NewInsert, InsertPos, LastInsertPos: TCodeTreeNodeExtension;
begin
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.AddInsert] ',CleanDef,',',Def,',',Identifiername);
{$ENDIF}
  NewInsert:=NodeExtMemManager.NewNode;
  with NewInsert do begin
    Node:=PosNode;
    Txt:=CleanDef;
    ExtTxt1:=Def;
    ExtTxt2:=IdentifierName;
  end;
  if FirstInsert=nil then begin
    FirstInsert:=NewInsert;
    exit;
  end;
  if ASourceChangeCache.BeautifyCodeOptions.ClassPartInsertPolicy=cpipLast then
  begin
    // add as last to inserts
    InsertPos:=FirstInsert;
    while (InsertPos.Next<>nil) do
      InsertPos:=InsertPos.Next;
    InsertPos.Next:=NewInsert;
  end else begin
    // insert alphabetically
    InsertPos:=FirstInsert;
    LastInsertPos:=nil;
//writeln('GGG "',InsertPos.Txt,'" "',CleanDef,'" ',CompareTextIgnoringSpace(InsertPos.Txt,CleanDef,false));
    while (InsertPos<>nil)
    and (CompareTextIgnoringSpace(InsertPos.Txt,CleanDef,false)>=0) do begin
      LastInsertPos:=InsertPos;
      InsertPos:=InsertPos.Next;
    end;
    if LastInsertPos<>nil then begin
      // insert after LastInsertPos
      NewInsert.Next:=LastInsertPos.Next;
      LastInsertPos.Next:=NewInsert;
    end else begin
      // insert as first
      NewInsert.Next:=InsertPos;
      FirstInsert:=NewInsert;
    end;
{InsertPos:=FirstInsert;
while InsertPos<>nil do begin
  writeln(' HHH ',InsertPos.Txt);
  InsertPos:=InsertPos.Next;
end;}
  end;
end;

function TCodeCompletionCodeTool.NodeExtIsVariable(
  ANodeExt: TCodeTreeNodeExtension): boolean;
// a variable has the form 'Name:Type;'
var APos, TxtLen: integer;
begin
  APos:=1;
  TxtLen:=length(ANodeExt.ExtTxt1);
  while (APos<=TxtLen) and (IsIdentChar[ANodeExt.ExtTxt1[APos]]) do
    inc(APos);
  while (APos<=TxtLen) and (IsSpaceChar[ANodeExt.ExtTxt1[APos]]) do
    inc(APos);
  Result:=(APos<=TxtLen) and (ANodeExt.ExtTxt1[APos]=':');
end;

function TCodeCompletionCodeTool.CompleteProperty(
  PropNode: TCodeTreeNode): boolean;
{
 examples:
   property Visible;
   property Count: integer;
   property Color: TColor read FColor write SetColor;
   property Items[Index1, Index2: integer]: integer read GetItems; default;
   property X: integer index 1 read GetCoords write SetCoords stored IsStored;
   property Col8: ICol8 read FCol8 write FCol8 implements ICol8;

   property specifiers without parameters:
     ;nodefault, ;default

   property specifiers with parameters:
     index <constant>, read <id>, write <id>, implements <id>,
     stored <id>, default <constant>
}
type
  TPropPart = (ppName,ppParamList, ppType, ppIndexWord, ppIndex, ppReadWord,
               ppRead, ppWriteWord, ppWrite, ppStoredWord, ppStored,
               ppImplementsWord, ppImplements, ppDefaultWord, ppDefault,
               ppNoDefaultWord);
var Parts: array[TPropPart] of TAtomPosition;
  APart: TPropPart;
  
  function ReadSimpleSpec(SpecWord, SpecParam: TPropPart): boolean;
  begin
    if Parts[SpecWord].StartPos>=1 then
      RaiseException('property specifier already defined: '+GetAtom);
    Parts[SpecWord]:=CurPos;
    ReadNextAtom;
    Result:=AtomIsWord;
    if not Result then
      RaiseException('expected identifier but '+GetAtom+' found');
    if WordIsPropertySpecifier.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos) then exit;
    Parts[SpecParam]:=CurPos;
    ReadNextAtom;
  end;

var AccessParam, AccessParamPrefix, CleanAccessFunc, AccessFunc,
  CleanParamList, ParamList, PropType: string;
  InsertPos: integer;
begin
  Result:=false;
  for APart:=Low(TPropPart) to High(TPropPart) do
    Parts[APart].StartPos:=-1;
  MoveCursorToNodeStart(PropNode);
  ReadNextAtom; // read 'property'
  ReadNextAtom; // read name
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] Checking Property ',GetAtom);
{$ENDIF}
  Parts[ppName]:=CurPos;
  ReadNextAtom;
  if AtomIsChar('[') then begin
    // read parameter list '[ ... ]'
    Parts[ppParamList].StartPos:=CurPos.StartPos;
    InitExtraction;
    if not ReadParamList(true,true,[phpInUpperCase,phpWithoutBrackets])
    then begin
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] error parsing param list');
{$ENDIF}
      RaiseException('error in paramlist');
    end;
    CleanParamList:=GetExtraction;
    Parts[ppParamList].EndPos:=CurPos.EndPos;
  end else
    CleanParamList:='';
  if not AtomIsChar(':') then begin
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] no type : found -> ignore property');
{$ENDIF}
    // no type -> ignore this property
    Result:=true;
    exit;
  end;
  ReadNextAtom; // read type
  if (CurPos.StartPos>PropNode.EndPos)
  or UpAtomIs('END') or AtomIsChar(';') or (not AtomIsIdentifier(false))
  or AtomIsKeyWord then begin
    // no type name found -> ignore this property
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] error: no type name found');
{$ENDIF}
    Result:=true;
    exit;
  end;
  Parts[ppType]:=CurPos;
  // read specifiers
  ReadNextAtom;
  if UpAtomIs('INDEX') then begin
    if Parts[ppIndexWord].StartPos>=1 then 
      RaiseException('index specifier redefined');
    Parts[ppIndexWord]:=CurPos;
    ReadNextAtom;
    if WordIsPropertySpecifier.DoItUpperCase(UpperSrc,CurPos.StartPos,
      CurPos.EndPos-CurPos.StartPos) then 
      RaiseException('index parameter expected, but '+GetAtom+' found');
    Parts[ppIndex].StartPos:=CurPos.StartPos;
    if not ReadConstant(true,false,[]) then exit;
    Parts[ppIndex].EndPos:=LastAtoms.GetValueAt(0).EndPos;
  end;
  if UpAtomIs('READ') and not ReadSimpleSpec(ppReadWord,ppRead) then exit;
  if UpAtomIs('WRITE') and not ReadSimpleSpec(ppWriteWord,ppWrite) then
    exit;
  while (CurPos.StartPos<PropNode.EndPos) and (not AtomIsChar(';'))
  and (not UpAtomIs('END')) do begin
    if UpAtomIs('STORED') then begin
      if not ReadSimpleSpec(ppStoredWord,ppStored) then
        exit;
    end else if UpAtomIs('DEFAULT') then begin
      if Parts[ppDefaultWord].StartPos>=1 then 
        RaiseException('default specifier redefined');
      Parts[ppDefaultWord]:=CurPos;
      ReadNextAtom;
      if WordIsPropertySpecifier.DoItUpperCase(UpperSrc,CurPos.StartPos,
        CurPos.EndPos-CurPos.StartPos) then 
        RaiseException('default parameter expected, but '+GetAtom+' found');
      Parts[ppDefault].StartPos:=CurPos.StartPos;
      if not ReadConstant(true,false,[]) then exit;
      Parts[ppDefault].EndPos:=LastAtoms.GetValueAt(0).EndPos;
    end else if UpAtomIs('IMPLEMENTS') then begin
      if not ReadSimpleSpec(ppImplementsWord,ppImplements) then exit;
    end else if UpAtomIs('NODEFAULT') then begin
      if Parts[ppNoDefaultWord].StartPos>=1 then 
        RaiseException('nodefault specifier defined twice');
      Parts[ppNoDefaultWord]:=CurPos;
      ReadNextAtom;
    end else
      RaiseException('; expected, but '+GetAtom+' found');
  end;
  if (CurPos.StartPos>PropNode.EndPos) then
    RaiseException('Reparsing error (Complete Property)');
  PropType:=copy(Src,Parts[ppType].StartPos,
               Parts[ppType].EndPos-Parts[ppType].StartPos);
  // check read specifier
  if (Parts[ppReadWord].StartPos>0) or (Parts[ppWriteWord].StartPos<1) then
  begin
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] read specifier needed');
{$ENDIF}
    AccessParamPrefix:=
      ASourceChangeCache.BeautifyCodeOptions.PropertyReadIdentPrefix;
    if Parts[ppRead].StartPos>0 then
      AccessParam:=copy(Src,Parts[ppRead].StartPos,
          Parts[ppRead].EndPos-Parts[ppRead].StartPos)
    else
      AccessParam:='';
    if (Parts[ppParamList].StartPos>0) or (Parts[ppIndexWord].StartPos>0)
    or (AnsiCompareText(AccessParamPrefix,
            LeftStr(AccessParam,length(AccessParamPrefix)))=0) then
    begin
      // the read identifier is a function
      if Parts[ppRead].StartPos<1 then
        AccessParam:=AccessParamPrefix+copy(Src,Parts[ppName].StartPos,
            Parts[ppName].EndPos-Parts[ppName].StartPos);
      if (Parts[ppParamList].StartPos>0) then begin
        if (Parts[ppIndexWord].StartPos<1) then begin
          // param list, no index
          CleanAccessFunc:=UpperCaseStr(AccessParam)+'('+CleanParamList+');';
        end else begin
          // index + param list
          CleanAccessFunc:=UpperCaseStr(AccessParam)+'(:INTEGER;'
                          +CleanParamList+');';
        end;
      end else begin
        if (Parts[ppIndexWord].StartPos<1) then begin
          // no param list, no index
          CleanAccessFunc:=UpperCaseStr(AccessParam)+';';
        end else begin
          // index, no param list
          CleanAccessFunc:=UpperCaseStr(AccessParam)+'(:INTEGER);';
        end;
      end;
      // check if function exists
      if not ProcExists(CleanAccessFunc) then begin
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] CleanAccessFunc ',CleanAccessFunc,' does not exist');
{$ENDIF}
        // add insert demand for function
        // build function code
        if (Parts[ppParamList].StartPos>0) then begin
          MoveCursorToCleanPos(Parts[ppParamList].StartPos);
          ReadNextAtom;
          InitExtraction;
          if not ReadParamList(true,true,[phpWithParameterNames,
                               phpWithoutBrackets,phpWithVarModifiers,
                               phpWithComments])
          then begin
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] Error reading param list');
{$ENDIF}
            RaiseException('error in parameter list');
          end;
          ParamList:=GetExtraction;
          if (Parts[ppIndexWord].StartPos<1) then begin
            // param list, no index
            AccessFunc:='function '+AccessParam
                        +'('+ParamList+'):'+PropType+';';
          end else begin
            // index + param list
            AccessFunc:='function '+AccessParam
                        +'(Index:integer;'+ParamList+'):'+PropType+';';
          end;
        end else begin
          if (Parts[ppIndexWord].StartPos<1) then begin
            // no param list, no index
            AccessFunc:='function '+AccessParam+':'+PropType+';';
          end else begin
            // index, no param list
            AccessFunc:='function '+AccessParam
                        +'(Index:integer):'+PropType+';';
          end;
        end;
        // add new Insert Node
        AddInsert(PropNode,CleanAccessFunc,AccessFunc,AccessParam);
      end;
    end else begin
      if Parts[ppRead].StartPos<1 then
        AccessParam:=ASourceChangeCache.BeautifyCodeOptions.PrivatVariablePrefix
             +copy(Src,Parts[ppName].StartPos,
               Parts[ppName].EndPos-Parts[ppName].StartPos);
      // the read identifier is a variable
      if not VarExists(UpperCaseStr(AccessParam)) then begin
        // variable does not exist yet -> add insert demand for variable
        AddInsert(PropNode,UpperCaseStr(AccessParam),
                  AccessParam+':'+PropType+';',AccessParam);
      end;
    end;
    if Parts[ppRead].StartPos<0 then begin
      // insert read specifier
      if Parts[ppReadWord].StartPos>0 then begin
        // 'read' keyword exists -> insert read identifier behind
        InsertPos:=Parts[ppReadWord].EndPos;
        ASourceChangeCache.Replace(gtSpace,gtNone,InsertPos,InsertPos,
           AccessParam);
      end else begin
        // 'read' keyword does not exist -> insert behind index and type
        if Parts[ppIndexWord].StartPos>0 then
          InsertPos:=Parts[ppIndexWord].EndPos
        else if Parts[ppIndex].StartPos>0 then
          InsertPos:=Parts[ppIndex].EndPos
        else
          InsertPos:=Parts[ppType].EndPos;
        ASourceChangeCache.Replace(gtSpace,gtNone,InsertPos,InsertPos,
           ASourceChangeCache.BeautifyCodeOptions.BeautifyKeyWord('read')
           +' '+AccessParam);
      end;
    end;
  end;
  // check write specifier
  if (Parts[ppWriteWord].StartPos>0) or (Parts[ppReadWord].StartPos<1) then
  begin
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] write specifier needed');
{$ENDIF}
    AccessParamPrefix:=
      ASourceChangeCache.BeautifyCodeOptions.PropertyWriteIdentPrefix;
    if Parts[ppWrite].StartPos>0 then
      AccessParam:=copy(Src,Parts[ppWrite].StartPos,
            Parts[ppWrite].EndPos-Parts[ppWrite].StartPos)
    else
      AccessParam:=AccessParamPrefix+copy(Src,Parts[ppName].StartPos,
            Parts[ppName].EndPos-Parts[ppName].StartPos);
    if (Parts[ppParamList].StartPos>0) or (Parts[ppIndexWord].StartPos>0)
    or (AnsiCompareText(AccessParamPrefix,
            LeftStr(AccessParam,length(AccessParamPrefix)))=0) then
    begin
      // the write identifier is a procedure
      if (Parts[ppParamList].StartPos>0) then begin
        if (Parts[ppIndexWord].StartPos<1) then begin
          // param list, no index
          CleanAccessFunc:=UpperCaseStr(AccessParam)+'('+CleanParamList+';'
                             +' :'+UpperCaseStr(PropType)+');';
        end else begin
          // index + param list
          CleanAccessFunc:=UpperCaseStr(AccessParam)+'(:INTEGER;'
                    +CleanParamList+'; :'+UpperCaseStr(PropType)+');';
        end;
      end else begin
        if (Parts[ppIndexWord].StartPos<1) then begin
          // no param list, no index
          CleanAccessFunc:=UpperCaseStr(AccessParam)
                              +'( :'+UpperCaseStr(PropType)+');';
        end else begin
          // index, no param list
          CleanAccessFunc:=UpperCaseStr(AccessParam)+'(:INTEGER;'
                              +' :'+UpperCaseStr(PropType)+');';
        end;
      end;
      // check if procedure exists
      if not ProcExists(CleanAccessFunc) then begin
        // add insert demand for function
        // build function code
        if (Parts[ppParamList].StartPos>0) then begin
          MoveCursorToCleanPos(Parts[ppParamList].StartPos);
          ReadNextAtom;
          InitExtraction;
          if not ReadParamList(true,true,[phpWithParameterNames,
                               phpWithoutBrackets,phpWithVarModifiers,
                               phpWithComments])
          then
            RaiseException('error in param list');
          ParamList:=GetExtraction;
          if (Parts[ppIndexWord].StartPos<1) then begin
            // param list, no index
            AccessFunc:='procedure '+AccessParam
                        +'('+ParamList+';const '+SetPropertyVariablename+': '
                        +PropType+');';
          end else begin
            // index + param list
            AccessFunc:='procedure '+AccessParam
                        +'(Index:integer;'+ParamList+';'
                        +'const '+SetPropertyVariablename+': '+PropType+');';
          end;
        end else begin
          if (Parts[ppIndexWord].StartPos<1) then begin
            // no param list, no index
            AccessFunc:='procedure '+AccessParam
                        +'(const '+SetPropertyVariablename+': '+PropType+');';
          end else begin
            // index, no param list
            AccessFunc:='procedure '+AccessParam
                        +'(Index:integer; const '+SetPropertyVariablename+': '
                        +PropType+');';
          end;
        end;
        // add new Insert Node
        AddInsert(PropNode,CleanAccessFunc,AccessFunc,AccessParam);
      end;
    end else begin
      // the write identifier is a variable
      if not VarExists(UpperCaseStr(AccessParam)) then begin
        // variable does not exist yet -> add insert demand for variable
        AddInsert(PropNode,UpperCaseStr(AccessParam),
                  AccessParam+':'+PropType+';',AccessParam);
      end;
    end;
    if Parts[ppWrite].StartPos<0 then begin
      // insert write specifier
      if Parts[ppWriteWord].StartPos>0 then begin
        // 'write' keyword exists -> insert write identifier behind
        InsertPos:=Parts[ppWriteWord].EndPos;
        ASourceChangeCache.Replace(gtSpace,gtNone,InsertPos,InsertPos,
           AccessParam);
      end else begin
        // 'write' keyword does not exist
        //  -> insert behind type, index and write specifier
        if Parts[ppRead].StartPos>0 then
          InsertPos:=Parts[ppRead].EndPos
        else if Parts[ppReadWord].StartPos>0 then
          InsertPos:=Parts[ppReadWord].EndPos
        else if Parts[ppIndexWord].StartPos>0 then
          InsertPos:=Parts[ppIndexWord].EndPos
        else if Parts[ppIndex].StartPos>0 then
          InsertPos:=Parts[ppIndex].EndPos
        else
          InsertPos:=Parts[ppType].EndPos;
        ASourceChangeCache.Replace(gtSpace,gtNone,InsertPos,InsertPos,
           ASourceChangeCache.BeautifyCodeOptions.BeautifyKeyWord('write')
           +' '+AccessParam);
      end;
    end;
  end;
  // check stored specifier
  if (Parts[ppStoredWord].StartPos>0) then begin
{$IFDEF CTDEBUG}
writeln('[TCodeCompletionCodeTool.CompleteProperty] stored specifier needed');
{$ENDIF}
    if Parts[ppStored].StartPos>0 then
      AccessParam:=copy(Src,Parts[ppStored].StartPos,
            Parts[ppStored].EndPos-Parts[ppStored].StartPos)
    else
      AccessParam:=copy(Src,Parts[ppName].StartPos,
        Parts[ppName].EndPos-Parts[ppName].StartPos)
        +ASourceChangeCache.BeautifyCodeOptions.PropertyStoredIdentPostfix;
    CleanAccessFunc:=UpperCaseStr(AccessParam);
    // check if procedure exists
    if (not ProcExists(CleanAccessFunc)) and (not VarExists(CleanAccessFunc))
    then begin
      // add insert demand for function
      // build function code
      AccessFunc:='function '+AccessParam+':boolean;';
      // add new Insert Node
      AddInsert(PropNode,CleanAccessFunc,AccessFunc,AccessParam);
    end;
    if Parts[ppStored].StartPos<0 then begin
      // insert stored specifier
      InsertPos:=Parts[ppStoredWord].EndPos;
      ASourceChangeCache.Replace(gtSpace,gtNone,InsertPos,InsertPos,
           AccessParam);
    end;
  end;
  Result:=true;
end;

procedure TCodeCompletionCodeTool.InsertNewClassParts(PartType: NewClassPart);
var ANodeExt: TCodeTreeNodeExtension;
  PrivatNode, ANode, InsertNode: TCodeTreeNode;
  Indent, InsertPos: integer;
  CurCode: string;
  IsVariable: boolean;
begin
  ANodeExt:=FirstInsert;
  while ANodeExt<>nil do begin
    IsVariable:=NodeExtIsVariable(ANodeExt);
    if ((PartType=ncpVars)=IsVariable) then begin
      // search a privat section in front of the node
      PrivatNode:=ANodeExt.Node.Parent.PriorBrother;
      while (PrivatNode<>nil) and (PrivatNode.Desc<>ctnClassPrivate) do
        PrivatNode:=PrivatNode.PriorBrother;
      if PrivatNode=nil then begin
        // there is no privat section node in front of the property
        if NewPrivatSectionInsertPos<1 then begin
          // -> insert one at the end of the first published node
          // Note: the first node is a fake published section, so the first
          //       real section is the second
          ANode:=ClassNode.FirstChild.NextBrother;
          if ANode=nil then ANode:=ClassNode;
          NewPrivatSectionIndent:=GetLineIndent(Src,ANode.StartPos);
          ANode:=ClassNode.FirstChild;
          if (ANode.FirstChild=nil) and (ANode.NextBrother<>nil)
          and (ANode.NextBrother.Desc=ctnClassPublished) then
            ANode:=ANode.NextBrother;
          NewPrivatSectionInsertPos:=ANode.EndPos;
          ASourceChangeCache.Replace(gtNewLine,gtNewLine,
            NewPrivatSectionInsertPos,NewPrivatSectionInsertPos,
            ASourceChangeCache.BeautifyCodeOptions.BeautifyKeyWord(
              'private'));
        end;
        Indent:=NewPrivatSectionIndent
                    +ASourceChangeCache.BeautifyCodeOptions.Indent;
        InsertPos:=NewPrivatSectionInsertPos;
      end else begin
        // there is a privat section in front of the property
        InsertNode:=nil; // the new part will be inserted after this node
                         //   nil means insert as first
        ANode:=PrivatNode.FirstChild;
        if PartType=ncpProcs then begin
          // insert procs after variables
          while (ANode<>nil) and (ANode.Desc=ctnVarDefinition) do begin
            InsertNode:=ANode;
            ANode:=ANode.NextBrother;
          end;
        end;
        case ASourceChangeCache.BeautifyCodeOptions.ClassPartInsertPolicy of
          cpipAlphabetically:
            begin
              while ANode<>nil do begin
                if (PartType=ncpVars) then begin
                  if (ANode.Desc<>ctnVarDefinition)
                  or (CompareNodeIdentChars(ANode,ANodeExt.Txt)<0) then
                    break;
                end else begin
                  case ANode.Desc of
                    ctnProcedure:
                      begin
                        CurCode:=ExtractProcName(ANode,false);
                        if AnsiCompareStr(CurCode,ANodeExt.ExtTxt2)>0 then
                          break;
                      end;
                    ctnProperty:
                      begin
                        CurCode:=ExtractPropName(ANode,false);
                        if AnsiCompareStr(CurCode,ANodeExt.ExtTxt2)>0 then
                          break;
                      end;
                  end;
                end;
                InsertNode:=ANode;
                ANode:=ANode.NextBrother;
              end;
            end;
        else
          // cpipLast
          begin
            while ANode<>nil do begin
              if (PartType=ncpVars) and (ANode.Desc<>ctnVarDefinition) then
                break;
              InsertNode:=ANode;
              ANode:=ANode.NextBrother;
            end;
          end
        end;
        if InsertNode<>nil then begin
          // insert after InsertNode
          Indent:=GetLineIndent(Src,InsertNode.StartPos);
          InsertPos:=FindFirstLineEndAfterInCode(Src,InsertNode.EndPos,
                       Scanner.NestedComments);
        end else begin
          // insert as first variable/proc
          Indent:=GetLineIndent(Src,PrivatNode.StartPos)
                    +ASourceChangeCache.BeautifyCodeOptions.Indent;
          InsertPos:=FindFirstLineEndAfterInCode(Src,PrivatNode.StartPos,
                       Scanner.NestedComments);
        end;
      end;
      CurCode:=ANodeExt.ExtTxt1;
      CurCode:=ASourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
                          CurCode,Indent);
      ASourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,
         CurCode);
      if (not IsVariable)
      and (ASourceChangeCache.BeautifyCodeOptions.MethodInsertPolicy
        =mipClassOrder) then
      begin
        // this was a new method defnition and the body should be added in
        // Class Order
        // -> save information about the inserted position
        ANodeExt.Position:=InsertPos;
      end;
    end;
    ANodeExt:=ANodeExt.Next;
  end;
end;
  
function TCodeCompletionCodeTool.InsertAllNewClassParts: boolean;
begin
  if FirstInsert=nil then begin
    Result:=true;
    exit;
  end;
  NewPrivatSectionInsertPos:=-1;
  InsertNewClassParts(ncpVars);
  InsertNewClassParts(ncpProcs);
  Result:=true;
end;

function TCodeCompletionCodeTool.CreateMissingProcBodies: boolean;
var
  Indent, InsertPos: integer;
  TheClassName: string;
   
  procedure InsertProcBody(ANodeExt: TCodeTreeNodeExtension);
  var ProcCode: string;
  begin
    ProcCode:=ANodeExt.ExtTxt1;
    ProcCode:=ASourceChangeCache.BeautifyCodeOptions.AddClassAndNameToProc(
                 ProcCode,TheClassName,'');
{$IFDEF CTDEBUG}
writeln('>>> InsertProcBody ',TheClassName,' "',ProcCode,'"');
{$ENDIF}
    ProcCode:=ASourceChangeCache.BeautifyCodeOptions.BeautifyProc(
                 ProcCode,Indent,true);
    ASourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,
      ProcCode);
    if JumpToProcName='' then begin
      // remember a proc body to set the cursor at
      JumpToProcName:=UpperCaseStr(TheClassName)+'.'+ANodeExt.Txt;
    end;
  end;

var
  ProcBodyNodes, ClassProcs: TAVLTree;
  ANodeExt, ANodeExt2, NewNodeExt: TCodeTreeNodeExtension;
  ExistingNode, MissingNode, AnAVLNode, NextAVLNode,
  NearestAVLNode: TAVLTreeNode;
  cmp, MissingNodePosition: integer;
  FirstExistingProcBody, LastExistingProcBody, ImplementationNode,
  ANode, ANode2, TypeSectionNode: TCodeTreeNode;
  ClassStartComment, ProcCode, s: string;
  Caret1, Caret2: TCodeXYPosition;
  MethodInsertPolicy: TMethodInsertPolicy;
  NearestNodeValid: boolean;
begin
{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CreateMissingProcBodies Gather existing method bodies ... ');
{$ENDIF}
  Result:=false;
  MethodInsertPolicy:=ASourceChangeCache.BeautifyCodeOptions.MethodInsertPolicy;
  // gather existing class proc bodies
  TypeSectionNode:=ClassNode.Parent;
  if (TypeSectionNode<>nil) and (TypeSectionNode.Parent<>nil)
  and (TypeSectionNode.Parent.Desc=ctnTypeSection) then
    TypeSectionNode:=TypeSectionNode.Parent;
  ClassProcs:=nil;
  ProcBodyNodes:=GatherProcNodes(TypeSectionNode,
     [phpInUpperCase,phpIgnoreForwards,phpOnlyWithClassname],
     ExtractClassName(ClassNode,true));
  try
    ExistingNode:=ProcBodyNodes.FindLowest;
    if ExistingNode<>nil then 
      LastExistingProcBody:=TCodeTreeNodeExtension(ExistingNode.Data).Node
    else
      LastExistingProcBody:=nil;
    FirstExistingProcBody:=LastExistingProcBody;
    while ExistingNode<>nil do begin
      ANode:=TCodeTreeNodeExtension(ExistingNode.Data).Node;
      if ANode.StartPos<FirstExistingProcBody.StartPos then
        FirstExistingProcBody:=ANode;
      if ANode.StartPos>LastExistingProcBody.StartPos then
        LastExistingProcBody:=ANode;
      ExistingNode:=ProcBodyNodes.FindSuccessor(ExistingNode);
    end;

{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CreateMissingProcBodies Gather existing method declarations ... ');
{$ENDIF}
    TheClassName:=ExtractClassName(ClassNode,false);

    // gather existing class proc definitions
    ClassProcs:=GatherProcNodes(StartNode,[phpInUpperCase,phpAddClassName],
       ExtractClassName(ClassNode,true));
       
    // check for double defined methods in ClassProcs
    AnAVLNode:=ClassProcs.FindLowest;
    while AnAVLNode<>nil do begin
      NextAVLNode:=ClassProcs.FindSuccessor(AnAVLNode);
      if NextAVLNode<>nil then begin
        ANodeExt:=TCodeTreeNodeExtension(AnAVLNode.Data);
        ANodeExt2:=TCodeTreeNodeExtension(NextAVLNode.Data);
        if CompareTextIgnoringSpace(ANodeExt.Txt,ANodeExt2.Txt,false)=0 then
        begin
          // proc redefined -> error
          if ANodeExt.Node.StartPos>ANodeExt2.Node.StartPos then begin
            ANode:=ANodeExt.Node;
            ANode2:=ANodeExt2.Node;
          end else begin
            ANode:=ANodeExt2.Node;
            ANode2:=ANodeExt.Node;
          end;
          CleanPosToCaret(ANode.FirstChild.StartPos,Caret1);
          CleanPosToCaret(ANode2.FirstChild.StartPos,Caret2);
          s:=IntToStr(Caret2.Y)+','+IntToStr(Caret2.X);
          if Caret1.Code<>Caret2.Code then
            s:=s+' in '+Caret2.Code.Filename;
          MoveCursorToNodeStart(ANode.FirstChild);
          RaiseException('procedure redefined (first at '+s+')');
        end;
      end;
      AnAVLNode:=NextAVLNode;
    end;

    CurNode:=FirstExistingProcBody;
    
    {AnAVLNode:=ClassProcs.FindLowest;
    while AnAVLNode<>nil do begin
      writeln(' AAA ',TCodeTreeNodeExtension(AnAVLNode.Data).Txt);
      AnAVLNode:=ClassProcs.FindSuccessor(AnAVLNode);
    end;}
    
    // add new property access methods to ClassProcs
    ANodeExt:=FirstInsert;
    while ANodeExt<>nil do begin
      if not NodeExtIsVariable(ANodeExt) then begin
        if FindNodeInTree(ClassProcs,ANodeExt.Txt)=nil then begin
          NewNodeExt:=TCodeTreeNodeExtension.Create;
          with NewNodeExt do begin
            Txt:=UpperCaseStr(TheClassName)+'.'
                  +ANodeExt.Txt;       // Name+ParamTypeList
            ExtTxt1:=ASourceChangeCache.BeautifyCodeOptions.AddClassAndNameToProc(
               ANodeExt.ExtTxt1,TheClassName,''); // complete proc head code
            Position:=ANodeExt.Position;
          end;
          ClassProcs.Add(NewNodeExt);
        end;
      end;
      ANodeExt:=ANodeExt.Next;
    end;
    
    {AnAVLNode:=ClassProcs.FindLowest;
    while AnAVLNode<>nil do begin
      writeln(' BBB ',TCodeTreeNodeExtension(AnAVLNode.Data).Txt);
      AnAVLNode:=ClassProcs.FindSuccessor(AnAVLNode);
    end;}
    
    if MethodInsertPolicy=mipClassOrder then begin
      // insert in ClassOrder -> get a definition position for every method
      AnAVLNode:=ClassProcs.FindLowest;
      while AnAVLNode<>nil do begin
        ANodeExt:=TCodeTreeNodeExtension(AnAVLNode.Data);
        if ANodeExt.Position<1 then
          // position not set => this proc was already there => there is a node
          ANodeExt.Position:=ANodeExt.Node.StartPos;
        // find corresponding proc body
        NextAVLNode:=ProcBodyNodes.Find(ANodeExt);
        if NextAVLNode<>nil then begin
          // NextAVLNode.Data is the TCodeTreeNodeExtension for the method body
          // (note 1)
          ANodeExt.Data:=NextAVLNode.Data;
        end;
        AnAVLNode:=ClassProcs.FindSuccessor(AnAVLNode);
      end;
      // sort the method definitions with the definition position
      ClassProcs.OnCompare:=@CompareCodeTreeNodeExtWithPos;
    end;

    {AnAVLNode:=ClassProcs.FindLowest;
    while AnAVLNode<>nil do begin
      writeln(' CCC ',TCodeTreeNodeExtension(AnAVLNode.Data).Txt);
      AnAVLNode:=ClassProcs.FindSuccessor(AnAVLNode);
    end;}

    // search for missing proc bodies
    if (ProcBodyNodes.Count=0) then begin
      // there were no old proc bodies of the class -> start class
      if NodeHasParentOfType(ClassNode,ctnInterface) then begin
        // class is in interface section
        // -> insert at the end of the implementation section
        ImplementationNode:=FindImplementationNode;
        if ImplementationNode=nil then 
          RaiseException('implementation node not found');
        Indent:=GetLineIndent(Src,ImplementationNode.StartPos);
        if (ImplementationNode.LastChild=nil)
        or (ImplementationNode.LastChild.Desc<>ctnBeginBlock) then
          InsertPos:=ImplementationNode.EndPos
        else begin
          InsertPos:=FindLineEndOrCodeInFrontOfPosition(Src,
             ImplementationNode.LastChild.StartPos,Scanner.NestedComments);
        end;
      end else begin
        // class is not in interface section
        // -> insert at the end of the type section
        ANode:=ClassNode.Parent; // type definition
        if ANode=nil then 
          RaiseException('class node without parent node');
        if ANode.Parent.Desc=ctnTypeSection then
          ANode:=ANode.Parent; // type section
        if ANode=nil then
          RaiseException('type section of class section not found');
        Indent:=GetLineIndent(Src,ANode.StartPos);
        InsertPos:=ANode.EndPos;
      end;
      // insert class comment
      if ClassProcs.Count>0 then begin
        ClassStartComment:=GetIndentStr(Indent)
                            +'{ '+ExtractClassName(ClassNode,false)+' }';
        ASourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,
           ClassStartComment);
      end;
      // insert all missing proc bodies
      MissingNode:=ClassProcs.FindHighest;
      while (MissingNode<>nil) do begin
        ANodeExt:=TCodeTreeNodeExtension(MissingNode.Data);
        ProcCode:=ANodeExt.ExtTxt1;
        if (ProcCode='') then begin
          ANode:=TCodeTreeNodeExtension(MissingNode.Data).Node;
          if (ANode<>nil) and (ANode.Desc=ctnProcedure) then begin
            ProcCode:=ExtractProcHead(ANode,[phpWithStart,phpAddClassname,
                 phpWithParameterNames,phpWithResultType,phpWithVarModifiers]);
          end;
        end;
        if ProcCode<>'' then begin
          ProcCode:=ASourceChangeCache.BeautifyCodeOptions.BeautifyProc(
                     ProcCode,Indent,true);
          ASourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,
            InsertPos,ProcCode);
          if JumpToProcName='' then begin
            // remember a proc body to set the cursor at
            JumpToProcName:=ANodeExt.Txt;
          end;
        end;
        MissingNode:=ProcBodyNodes.FindPrecessor(MissingNode);
      end;
    end else begin
      // there were old class procs already
      // -> search a good Insert Position behind or in front of
      //    another proc body of this class
      
      // set default insert position
      Indent:=GetLineIndent(Src,LastExistingProcBody.StartPos);
      InsertPos:=FindLineEndOrCodeAfterPosition(Src,
                        LastExistingProcBody.EndPos,Scanner.NestedComments);
      // check for all defined class methods (MissingNode), if there is a body
      MissingNode:=ClassProcs.FindHighest;
      NearestNodeValid:=false;
      while (MissingNode<>nil) do begin
//writeln('NEXT STEP MissingNode=',ANodeExt.Txt,' ',ANodeExt.ExtTxt1);
        ExistingNode:=ProcBodyNodes.Find(MissingNode.Data);
        if ExistingNode=nil then begin
          ANodeExt:=TCodeTreeNodeExtension(MissingNode.Data);
          // MissingNode does not have a body -> insert proc body
          case MethodInsertPolicy of
          mipAlphabetically:
            begin
              // search alphabetically nearest proc body
              ExistingNode:=ProcBodyNodes.FindNearest(MissingNode.Data);
              cmp:=CompareCodeTreeNodeExt(ExistingNode.Data,MissingNode.Data);
//writeln('  ALPHA Nearest=',TCodeTreeNodeExtension(ExistingNode.Data).Txt,' ',TCodeTreeNodeExtension(ExistingNode.Data).ExtTxt1,' cmp=',cmp);
              if (cmp<0) then begin
                AnAVLNode:=ProcBodyNodes.FindSuccessor(ExistingNode);
                if AnAVLNode<>nil then begin
                  ExistingNode:=AnAVLNode;
                  cmp:=1;
                end;
              end;
              ANodeExt2:=TCodeTreeNodeExtension(ExistingNode.Data);
              ANode:=ANodeExt2.Node;
//writeln('  ALPHA Nearest2=',ANodeExt2.Txt,' ',ANodeExt2.ExtTxt1,' cmp=',cmp);
              Indent:=GetLineIndent(Src,ANode.StartPos);
              if cmp>0 then begin
//writeln('  ALPHA Insert behind');
                // insert behind ExistingNode
                InsertPos:=FindLineEndOrCodeAfterPosition(Src,
                            ANode.EndPos,Scanner.NestedComments);
              end else begin
//writeln('  ALPHA Insert in front');
                // insert in front of ExistingNode
                InsertPos:=FindLineEndOrCodeInFrontOfPosition(Src,
                              ANode.StartPos,Scanner.NestedComments);
              end;
            end;

          mipClassOrder:
            begin
              // search definition-position nearest proc node
              MissingNodePosition:=ANodeExt.Position;
//writeln('  CLASSORDER NearestNodeValid=',NearestNodeValid,' MissingNodePosition=',MissingNodePosition);
              if not NearestNodeValid then begin
                // search NearestAVLNode method with body in front of MissingNode
                // and NextAVLNode method with body behind MissingNode
                NearestAVLNode:=nil;
                NextAVLNode:=ClassProcs.FindHighest;
                NearestNodeValid:=true;
              end;
              while (NextAVLNode<>nil) do begin
                ANodeExt2:=TCodeTreeNodeExtension(NextAVLNode.Data);
//writeln('  CLASSORDER LOOP NextAVLNode=',ANodeExt2.Txt,' P=',ANodeExt2.Position,' Data=',ANodeExt2.Data<>nil);
                if ANodeExt2.Data<>nil then begin
                  // method has body
                  if ANodeExt2.Position>MissingNodePosition then
                    break;
                  NearestAVLNode:=NextAVLNode;
                end;
                NextAVLNode:=ClassProcs.FindPrecessor(NextAVLNode);
              end;
//writeln('  CLASSORDER NearestAVLNode=',NearestAVLNode<>nil,' NextAVLNode=',NextAVLNode<>nil);
              if NearestAVLNode<>nil then begin
                // there is a NearestAVLNode in front -> insert behind body
                ANodeExt2:=TCodeTreeNodeExtension(NearestAVLNode.Data);
                // see above (note 1) for ANodeExt2.Data
                ANode:=TCodeTreeNodeExtension(ANodeExt2.Data).Node;
//writeln('  CLASSORDER Insert behind ',ANodeExt2.Txt);
                Indent:=GetLineIndent(Src,ANode.StartPos);
                InsertPos:=FindLineEndOrCodeAfterPosition(Src,
                            ANode.EndPos,Scanner.NestedComments);
              end else if NextAVLNode<>nil then begin
                // there is a NextAVLNode behind -> insert in front of body
                ANodeExt2:=TCodeTreeNodeExtension(NextAVLNode.Data);
                // see above (note 1) for ANodeExt2.Data
                ANode:=TCodeTreeNodeExtension(ANodeExt2.Data).Node;
//writeln('  CLASSORDER Insert in front of ',ANodeExt2.Txt,' ',ANode<>nil);
                Indent:=GetLineIndent(Src,ANode.StartPos);
                InsertPos:=FindLineEndOrCodeInFrontOfPosition(Src,
                            ANode.StartPos,Scanner.NestedComments);
              end;
            end;
          end;
          ProcCode:=ANodeExt.ExtTxt1;
          if (ProcCode='') then begin
            ANode:=ANodeExt.Node;
            if (ANode<>nil) and (ANode.Desc=ctnProcedure) then begin
              ProcCode:=ExtractProcHead(ANode,[phpWithStart,phpAddClassname,
               phpWithParameterNames,phpWithResultType,phpWithVarModifiers]);
            end;
          end;
          if (ProcCode<>'') then begin
            ProcCode:=
              ASourceChangeCache.BeautifyCodeOptions.AddClassAndNameToProc(
                ProcCode,TheClassName,'');
            ProcCode:=ASourceChangeCache.BeautifyCodeOptions.BeautifyProc(
                        ProcCode,Indent,true);
            ASourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,
                  InsertPos,InsertPos,ProcCode);
            if JumpToProcName='' then begin
              // remember a proc body to set the cursor at
              JumpToProcName:=ANodeExt.Txt;
            end;
          end;
        end;
        MissingNode:=ProcBodyNodes.FindPrecessor(MissingNode);
      end;
    end;
    Result:=true;
  finally
    if ClassProcs<>nil then begin
      ClassProcs.FreeAndClear;
      ClassProcs.Free;
    end;
    ProcBodyNodes.FreeAndClear;
    ProcBodyNodes.Free;
  end;
end;

function TCodeCompletionCodeTool.CompleteCode(CursorPos: TCodeXYPosition;
  var NewPos: TCodeXYPosition; var NewTopLine: integer;
  SourceChangeCache: TSourceChangeCache): boolean;
var CleanCursorPos, Dummy, Indent, insertPos: integer;
  CursorNode, ProcNode, ImplementationNode, SectionNode,
  ANode: TCodeTreeNode;
  ProcCode: string;
  ANodeExt: TCodeTreeNodeExtension;
begin
  Result:=false;
  if (SourceChangeCache=nil) then 
    RaiseException('need a SourceChangeCache');
  // in a class or in a forward proc?
  BuildTreeAndGetCleanPos(false,CursorPos, CleanCursorPos);
  // find CodeTreeNode at cursor
  CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
  ASourceChangeCache:=SourceChangeCache;
  SourceChangeCache.MainScanner:=Scanner;
{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode A CleanCursorPos=',CleanCursorPos,' NodeDesc=',NodeDescriptionAsString(CursorNode.Desc));
{$ENDIF}
  ImplementationNode:=FindImplementationNode;
  if ImplementationNode=nil then ImplementationNode:=Tree.Root;
  FirstInsert:=nil;

  // first test if in a class
  ClassNode:=CursorNode;
  while (ClassNode<>nil) and (ClassNode.Desc<>ctnClass) do
    ClassNode:=ClassNode.Parent;
  if ClassNode<>nil then begin
{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode In-a-class ',NodeDescriptionAsString(ClassNode.Desc));
{$ENDIF}
    // cursor is in class/object definition
    if (CursorNode.SubDesc and ctnsForwardDeclaration)>0 then exit;
    // parse class and build CodeTreeNodes for all properties/methods
{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode C ',CleanCursorPos,', |',copy(Src,CleanCursorPos,8));
{$ENDIF}
    BuildSubTreeForClass(ClassNode);
    StartNode:=ClassNode.FirstChild;
    while (StartNode<>nil) and (StartNode.FirstChild=nil) do
      StartNode:=StartNode.NextBrother;
    if StartNode=nil then 
      RaiseException('error parsing class');
    StartNode:=StartNode.FirstChild;
    JumpToProcName:='';
    try
      // go through all properties and procs
      //  insert read + write prop specifiers
      //  demand Variables + Procs + Proc Bodies
{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode Complete Properties ... ');
{$ENDIF}
      SectionNode:=ClassNode.FirstChild;
      while SectionNode<>nil do begin
        ANode:=SectionNode.FirstChild;
        while ANode<>nil do begin
          if ANode.Desc=ctnProperty then begin
            // check if property is complete
            if not CompleteProperty(ANode) then 
              RaiseException('unable to complete property');
          end;
          ANode:=ANode.NextBrother;
        end;
        SectionNode:=SectionNode.NextBrother;
      end;

{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode Insert new variables and methods ... ');
{$ENDIF}
      // insert all new variables and procs definitions
      if not InsertAllNewClassParts then 
        RaiseException('error during inserting new class parts');

{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode Insert new method bodies ... ');
{$ENDIF}
      // insert all missing proc bodies
      if not CreateMissingProcBodies then 
        RaiseException('error during creation of new proc bodies');

{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode Apply ... ');
{$ENDIF}
      // apply the changes and jump to first new proc body
      if not SourceChangeCache.Apply then 
        RaiseException('unable to apply changes');

      if JumpToProcName<>'' then begin
{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode Jump to new proc body ... ');
{$ENDIF}
        // there was a new proc body
        // -> find it and jump to

        // reparse code
        BuildTree(false);
        if not EndOfSourceFound then 
          RaiseException('End of source not found');
        // find the CursorPos in cleaned source
        Dummy:=CaretToCleanPos(CursorPos, CleanCursorPos);
        if (Dummy<>0) and (Dummy<>-1) then 
          RaiseException('cursor pos outside of code');
        // find CodeTreeNode at cursor
        CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);

        ClassNode:=CursorNode;
        while (ClassNode<>nil) and (ClassNode.Desc<>ctnClass) do
          ClassNode:=ClassNode.Parent;
        if ClassNode=nil then 
          RaiseException('oops, I loosed your class');
        ANode:=ClassNode.Parent;
        if ANode=nil then 
          RaiseException('class without parent node');
        if (ANode.Parent<>nil) and (ANode.Parent.Desc=ctnTypeSection) then
          ANode:=ANode.Parent;
        ProcNode:=FindProcNode(ANode,JumpToProcName,
                   [phpInUpperCase,phpIgnoreForwards]);
        if ProcNode=nil then 
          RaiseException('new proc body not found');
        Result:=FindJumpPointInProcNode(ProcNode,NewPos,NewTopLine);
        exit;
      end else begin
{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode Adjust Cursor ... ');
{$ENDIF}
        // there was no new proc body
        // -> adjust cursor
        NewPos:=CursorPos;
        NewPos.Code.AdjustCursor(NewPos.Y,NewPos.X);
        NewTopLine:=NewPos.Y-(VisibleEditorLines div 2);
        if NewTopLine<1 then NewTopLine:=1;
        Result:=true;
        exit;
      end;

    finally
      // dispose all new variables/procs definitions
      while FirstInsert<>nil do begin
        ANodeExt:=FirstInsert;
        FirstInsert:=FirstInsert.Next;
        NodeExtMemManager.DisposeNode(ANodeExt);
      end;
    end;
    
  end else begin
{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode not in-a-class ... ');
{$ENDIF}
    // then test if forward proc
    ProcNode:=CursorNode;
    if ProcNode.Desc=ctnProcedureHead then ProcNode:=ProcNode.Parent;
    if (ProcNode.Desc=ctnProcedure)
    and ((ProcNode.SubDesc and ctnsForwardDeclaration)>0) then begin
      // Node is forward Proc
{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode in a forward procedure ... ');
{$ENDIF}
        
      // check if proc already exists
      ProcCode:=ExtractProcHead(ProcNode,[phpInUpperCase]);
      if FindProcNode(FindNextNodeOnSameLvl(ProcNode),ProcCode,
             [phpInUpperCase])<>nil
      then exit;
        
{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode Body not found -> create it ... ');
{$ENDIF}
      // -> create proc body at end of implementation

      Indent:=GetLineIndent(Src,ImplementationNode.StartPos);
      if (ImplementationNode.LastChild=nil)
      or (ImplementationNode.LastChild.Desc<>ctnBeginBlock) then
        // insert at end of code
        InsertPos:=FindLineEndOrCodeInFrontOfPosition(Src,
           ImplementationNode.EndPos,Scanner.NestedComments)
      else begin
        // insert in front of main program begin..end.
        InsertPos:=FindLineEndOrCodeInFrontOfPosition(Src,
           ImplementationNode.LastChild.StartPos,Scanner.NestedComments);
      end;

      // build nice proc
      ProcCode:=ExtractProcHead(ProcNode,[phpWithStart,phpWithVarModifiers,
                  phpWithParameterNames,phpWithResultType,phpWithComments]);
      if ProcCode='' then 
        RaiseException('unable to reparse proc node');
      ProcCode:=SourceChangeCache.BeautifyCodeOptions.BeautifyProc(ProcCode,
                         Indent,true);
      if not SourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,
        InsertPos,InsertPos,ProcCode) then 
          RaiseException('unable to insert new proc body');
      if not SourceChangeCache.Apply then 
        RaiseException('unable to apply changes');
        
      // reparse code and find jump point into new proc
      Result:=FindJumpPoint(CursorPos,NewPos,NewTopLine);
      exit;
    end else begin
{$IFDEF CTDEBUG}
writeln('TCodeCompletionCodeTool.CompleteCode  nothing to complete ... ');
{$ENDIF}
    end;
  end;
end;

constructor TCodeCompletionCodeTool.Create;
begin
  inherited Create;
  FSetPropertyVariablename:='AValue';
end;


end.

