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
    TFindDeclarationTool enhances the TPascalParserTool with the ability
    to find the source position or code tree node of a declaration.


  ToDo:
    - many things, search for 'ToDo'

    - Difficulties:
       1. SOLVED. Searching recursively
            - ParentNodes
            - Ancestor Classes/Objects/Interfaces
            - with statements
            - operators: '.', '()', 'A()', '^', 'inherited'
       2. SOLVED. Searching enums must be searched in sub nodes
            -> all classes node trees must be built
       3. SOLVED. Searching in used units (interface USES and implementation USES)
       4. SOLVED. Searching forward for pointer types e.g. ^Tralala
       5. Mass Search: searching a compatible proc will result
          in searching every parameter type of every reachable proc
            (implementation section + interface section
    	    + used interface sections + class and ancestor methods)
          How can this be achieved in good time?
            -> Caching
    - Caching:
       Where:
         For each section node (Interface, Implementation, ...)
         For each BeginBlock
       Entries: (What, Declaration Pos)
         What: Identifier -> Ansistring (to reduce memory usage,
           maintain a list of all identifier ansistrings)
         Pos: Code+SrcPos
           1. Source: TCodeTreeNode
           2. PPU, PPW, DCU, ...
}
unit FindDeclarationTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

{$DEFINE CTDEBUG}
{ $DEFINE ShowTriedFiles}
{ $DEFINE ShowTriedContexts}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeTree, CodeAtom, CustomCodeTool, SourceLog,
  KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, AVL_Tree, TypInfo,
  PascalParserTool, FileProcs, DefineTemplates;

type
  TFindDeclarationTool = class;

  // searchpath delimiter is semicolon
  TOnGetSearchPath = function(Sender: TObject): string of object;
  TOnGetCodeToolForBuffer = function(Sender: TObject;
    Code: TCodeBuffer): TFindDeclarationTool of object;
  
  
  TFindDeclarationFlag = (
    fdfSearchInParentNodes, // if identifier not found in current context,
                            //    proceed in prior nodes on same lvl and parents
    fdfSearchInAncestors,   // if context is a class, search also in
                            //    ancestors/interfaces
    fdfIgnoreCurContextNode,// skip context and proceed in prior/parent context
    fdfExceptionOnNotFound, // raise exception if identifier not found
    fdfIgnoreUsedUnits,     // stay in current source
    fdfSearchForward,       // instead of searching in prior nodes, search in
                            //    next nodes (successors)
    fdfIgnoreClassVisibility,//find inaccessible private+protected fields
    fdfClassPublished,fdfClassPublic,fdfClassProtected,fdfClassPrivate);
  TFindDeclarationFlags = set of TFindDeclarationFlag;

  TFindDeclarationInput = record
    Flags: TFindDeclarationFlags;
    Identifier: PChar;
    ContextNode: TCodeTreeNode;
  end;
  
  TFindDeclarationParams = class;
  
  TFindContext = record
    Node: TCodeTreeNode;
    Tool: TFindDeclarationTool;
  end;

  TFindDeclarationParams = class(TObject)
  public
    Flags: TFindDeclarationFlags;
    Identifier: PChar;
    ContextNode: TCodeTreeNode;
    NewNode: TCodeTreeNode;
    NewCleanPos: integer;
    NewCodeTool: TFindDeclarationTool;
    NewPos: TCodeXYPosition;
    NewTopLine: integer;
    constructor Create;
    procedure Clear;
    procedure Save(var Input: TFindDeclarationInput);
    procedure Load(var Input: TFindDeclarationInput);
    procedure SetResult(AFindContext: TFindContext);
    procedure SetResult(ANewCodeTool: TFindDeclarationTool;
      ANewNode: TCodeTreeNode);
    procedure SetResult(ANewCodeTool: TFindDeclarationTool;
      ANewNode: TCodeTreeNode;  ANewCleanPos: integer);
    procedure ConvertResultCleanPosToCaretPos;
    procedure ClearResult;
    procedure ClearInput;
  end;

  TFindDeclarationTool = class(TPascalParserTool)
  private
    FOnGetUnitSourceSearchPath: TOnGetSearchPath;
    FOnGetCodeToolForBuffer: TOnGetCodeToolForBuffer;
    {$IFDEF CTDEBUG}
    DebugPrefix: string;
    procedure IncPrefix;
    procedure DecPrefix;
    {$ENDIF}
    function FindDeclarationInUsesSection(UsesNode: TCodeTreeNode;
      CleanPos: integer;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function IsIncludeDirectiveAtPos(CleanPos, CleanCodePosInFront: integer;
      var IncludeCode: TCodeBuffer): boolean;
    function FindEnumInContext(Params: TFindDeclarationParams): boolean;
    // sub methods for FindIdentifierInContext
    function FindIdentifierInProcContext(ProcContextNode: TCodeTreeNode;
      Params: TFindDeclarationParams): boolean;
    function FindIdentifierInClassOfMethod(ProcContextNode: TCodeTreeNode;
      Params: TFindDeclarationParams): boolean;
    function FindIdentifierInWithVarContext(WithVarNode: TCodeTreeNode;
      Params: TFindDeclarationParams): boolean;
    function FindIdentifierInAncestors(ClassNode: TCodeTreeNode;
      Params: TFindDeclarationParams): boolean;
    function FindIdentifierInUsesSection(UsesNode: TCodeTreeNode;
      Params: TFindDeclarationParams): boolean;
    function FindIdentifierInHiddenUsedUnits(
      Params: TFindDeclarationParams): boolean;
    function FindIdentifierInUsedUnit(const AnUnitName: string;
      Params: TFindDeclarationParams): boolean;
  protected
    function FindDeclarationOfIdentifier(
      Params: TFindDeclarationParams): boolean;
    function FindContextNodeAtCursor(
      Params: TFindDeclarationParams): TFindContext;
    function FindIdentifierInContext(Params: TFindDeclarationParams): boolean;
    function FindBaseTypeOfNode(Params: TFindDeclarationParams;
      Node: TCodeTreeNode): TFindContext;
    function FindClassOfMethod(ProcNode: TCodeTreeNode;
      Params: TFindDeclarationParams; FindClassContext: boolean): boolean;
    function FindAncestorOfClass(ClassNode: TCodeTreeNode;
      Params: TFindDeclarationParams; FindClassContext: boolean): boolean;
    function FindForwardIdentifier(Params: TFindDeclarationParams;
      var IsForward: boolean): boolean;
    function FindExpressionResultType(Params: TFindDeclarationParams;
      StartPos, EndPos: integer): TFindContext;
    function FindCodeToolForUsedUnit(UnitNameAtom,
      UnitInFileAtom: TAtomPosition;
      ExceptionOnNotFound: boolean): TFindDeclarationTool;
    function FindIdentifierInInterface(AskingTool: TFindDeclarationTool;
      Params: TFindDeclarationParams): boolean;
    function CompareNodeIdentifier(Node: TCodeTreeNode;
      Params: TFindDeclarationParams): boolean;
    function GetInterfaceNode: TCodeTreeNode;
  public
    function FindDeclaration(CursorPos: TCodeXYPosition;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindUnitSource(const AnUnitName,
      AnUnitInFilename: string): TCodeBuffer;
    property OnGetUnitSourceSearchPath: TOnGetSearchPath
      read FOnGetUnitSourceSearchPath write FOnGetUnitSourceSearchPath;
    property OnGetCodeToolForBuffer: TOnGetCodeToolForBuffer
      read FOnGetCodeToolForBuffer write FOnGetCodeToolForBuffer;
  end;


implementation


const
  fdfAllClassVisibilities = [fdfClassPublished,fdfClassPublic,fdfClassProtected,
                             fdfClassPrivate];
  fdfGlobals = [fdfExceptionOnNotFound, fdfIgnoreUsedUnits];


{ TFindContext }

function CreateFindContext(NewTool: TFindDeclarationTool;
  NewNode: TCodeTreeNode): TFindContext;
begin
  Result.Node:=NewNode;
  Result.Tool:=NewTool;
end;

function CreateFindContext(Params: TFindDeclarationParams): TFindContext;
begin
  Result.Node:=Params.NewNode;
  Result.Tool:=TFindDeclarationTool(Params.NewCodeTool);
end;


{ TFindDeclarationTool }

function TFindDeclarationTool.FindDeclaration(CursorPos: TCodeXYPosition;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
var CleanCursorPos: integer;
  CursorNode, ClassNode: TCodeTreeNode;
  Params: TFindDeclarationParams;
begin
  Result:=false;
  // build code tree
{$IFDEF CTDEBUG}
writeln(DebugPrefix,'TFindDeclarationTool.FindDeclaration A CursorPos=',CursorPos.X,',',CursorPos.Y);
{$ENDIF}
  BuildTreeAndGetCleanPos(false,CursorPos,CleanCursorPos);
{$IFDEF CTDEBUG}
writeln(DebugPrefix,'TFindDeclarationTool.FindDeclaration C CleanCursorPos=',CleanCursorPos);
{$ENDIF}
  // find CodeTreeNode at cursor
  CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
  if IsIncludeDirectiveAtPos(CleanCursorPos,CursorNode.StartPos,NewPos.Code)
  then begin
    NewPos.X:=1;
    NewPos.Y:=1;
    NewTopLine:=1;
    Result:=true;
    exit;
  end;
{$IFDEF CTDEBUG}
writeln('TFindDeclarationTool.FindDeclaration D CursorNode=',NodeDescriptionAsString(CursorNode.Desc));
{$ENDIF}
  if CursorNode.Desc=ctnUsesSection then begin
    // find used unit
    Result:=FindDeclarationInUsesSection(CursorNode,CleanCursorPos,
                                         NewPos,NewTopLine);
  end else begin
    // first test if in a class
    ClassNode:=CursorNode;
    while (ClassNode<>nil) and (ClassNode.Desc<>ctnClass) do
      ClassNode:=ClassNode.Parent;
    if ClassNode<>nil then begin
      // cursor is in class/object definition
      if (ClassNode.SubDesc and ctnsForwardDeclaration)=0 then begin
        // parse class and build CodeTreeNodes for all properties/methods
        BuildSubTreeForClass(ClassNode);
        CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
      end;
    end;
    if CursorNode.Desc=ctnBeginBlock then begin
      BuildSubTreeForBeginBlock(CursorNode);
      CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
    end;
    MoveCursorToCleanPos(CleanCursorPos);
    while (CurPos.StartPos>1) and (IsIdentChar[Src[CurPos.StartPos-1]]) do
      dec(CurPos.StartPos);
    if (CurPos.StartPos>=1) and (IsIdentStartChar[Src[CurPos.StartPos]]) then
    begin
      CurPos.EndPos:=CurPos.StartPos;
      while (CurPos.EndPos<=SrcLen) and IsIdentChar[Src[CurPos.EndPos]] do
        inc(CurPos.EndPos);
      // find declaration of identifier
      Params:=TFindDeclarationParams.Create;
      try
        Params.ContextNode:=CursorNode;
        Params.Identifier:=@Src[CurPos.StartPos];
        Params.Flags:=[fdfSearchInAncestors,fdfSearchInParentNodes,
                       fdfExceptionOnNotFound];
        Result:=FindDeclarationOfIdentifier(Params);
        if Result then begin
          Params.ConvertResultCleanPosToCaretPos;
          NewPos:=Params.NewPos;
          NewTopLine:=Params.NewTopLine;
        end;
      finally
        Params.Free;
      end;
    end else begin
      // find declaration of not identifier
      
    end;
  end;
end;

function TFindDeclarationTool.FindDeclarationInUsesSection(
  UsesNode: TCodeTreeNode; CleanPos: integer;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
var UnitName, UnitInFilename: string;
  UnitNamePos, UnitInFilePos: TAtomPosition;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TFindDeclarationTool.FindDeclarationInUsesSection A');
{$ENDIF}
  // reparse uses section
  MoveCursorToNodeStart(UsesNode);
  ReadNextAtom;
  if not UpAtomIs('USES') then
    RaiseException('expected uses, but '+GetAtom+' found');
  repeat
    ReadNextAtom;  // read name
    if CurPos.StartPos>CleanPos then break;
    if AtomIsChar(';') then break;
    AtomIsIdentifier(true);
    UnitNamePos:=CurPos;
    ReadNextAtom;
    if UpAtomIs('IN') then begin
      ReadNextAtom;
      if not AtomIsStringConstant then
        RaiseException(
          'string constant expected, but '+GetAtom+' found');
      UnitInFilePos:=CurPos;
      ReadNextAtom;
    end else
      UnitInFilePos.StartPos:=-1;
    if CleanPos<UnitNamePos.EndPos then begin
      // cursor is on a unitname -> try to locate it
      UnitName:=copy(Src,UnitNamePos.StartPos,
                     UnitNamePos.EndPos-UnitNamePos.StartPos);
      if UnitInFilePos.StartPos>=1 then
        UnitInFilename:=copy(Src,UnitInFilePos.StartPos,
                     UnitInFilePos.EndPos-UnitInFilePos.StartPos)
      else
        UnitInFilename:='';
      NewPos.Code:=FindUnitSource(UnitName,UnitInFilename);
      if NewPos.Code=nil then
        RaiseException('unit not found: '+UnitName);
      NewPos.X:=1;
      NewPos.Y:=1;
      NewTopLine:=1;
      Result:=true;
      exit;
    end;
    if AtomIsChar(';') then break;
    if not AtomIsChar(',') then
      RaiseException('; expected, but '+GetAtom+' found')
  until (CurPos.StartPos>SrcLen);
{$IFDEF CTDEBUG}
writeln('TFindDeclarationTool.FindDeclarationInUsesSection END cursor not on unitname');
{$ENDIF}
end;

function TFindDeclarationTool.FindUnitSource(const AnUnitName,
  AnUnitInFilename: string): TCodeBuffer;

  function LoadFile(const ExpandedFilename: string;
    var NewCode: TCodeBuffer): boolean;
  begin
{$IFDEF ShowTriedFiles}
writeln('TFindDeclarationTool.FindUnitSource.LoadFile ',ExpandedFilename);
{$ENDIF}
    NewCode:=TCodeBuffer(Scanner.OnLoadSource(Self,ExpandedFilename));
    Result:=NewCode<>nil;
  end;
  
  function SearchUnitFileInDir(const ADir, AnUnitName: string): TCodeBuffer;
  var APath: string;
  begin
    APath:=ADir;
    if (APath<>'') and (APath[length(APath)]<>PathDelim) then
      APath:=APath+PathDelim;
    {$IFNDEF win32}
    if LoadFile(ADir+lowercase(AnUnitName)+'.pp',Result) then exit;
    if LoadFile(ADir+lowercase(AnUnitName)+'.pas',Result) then exit;
    {$ENDIF}
    if LoadFile(ADir+AnUnitName+'.pp',Result) then exit;
    if LoadFile(ADir+AnUnitName+'.pas',Result) then exit;
    Result:=nil;
  end;

  function SearchUnitFileInPath(const APath, TheUnitName: string): TCodeBuffer;
  var PathStart, PathEnd: integer;
    ADir: string;
  begin
    PathStart:=1;
    while PathStart<=length(APath) do begin
      PathEnd:=PathStart;
      while (PathEnd<=length(APath)) and (APath[PathEnd]<>';') do inc(PathEnd);
      if PathEnd>PathStart then begin
        ADir:=copy(APath,PathStart,PathEnd-PathStart);
        if (ADir<>'') and (ADir[length(ADir)]<>PathDelim) then
          ADir:=ADir+PathDelim;
        if not FilenameIsAbsolute(ADir) then
          ADir:=ExtractFilePath(TCodeBuffer(Scanner.MainCode).Filename)+ADir;
        Result:=SearchUnitFileInDir(ADir,TheUnitName);
        if Result<>nil then exit;
      end;
      PathStart:=PathEnd+1;
    end;
    Result:=nil;
  end;

  function SearchFileInPath(const APath, RelativeFilename: string): TCodeBuffer;
  var PathStart, PathEnd: integer;
    ADir: string;
  begin
    PathStart:=1;
    while PathStart<=length(APath) do begin
      PathEnd:=PathStart;
      while (PathEnd<=length(APath)) and (APath[PathEnd]<>';') do inc(PathEnd);
      if PathEnd>PathStart then begin
        ADir:=copy(APath,PathStart,PathEnd-PathStart);
        if (ADir<>'') and (ADir[length(ADir)]<>PathDelim) then
          ADir:=ADir+PathDelim;
        if not FilenameIsAbsolute(ADir) then
          ADir:=ExtractFilePath(TCodeBuffer(Scanner.MainCode).Filename)+ADir;
        if LoadFile(ADir+RelativeFilename,Result) then exit;
      end;
      PathStart:=PathEnd+1;
    end;
    Result:=nil;
  end;
  
  function SearchUnitInUnitLinks(const TheUnitName: string): TCodeBuffer;
  var UnitLinks, CurFilename: string;
    UnitLinkStart, UnitLinkEnd: integer;
  begin
    Result:=nil;
    UnitLinks:=Scanner.Values[ExternalMacroStart+'UnitLinks'];
{$IFDEF ShowTriedFiles}
//writeln('TFindDeclarationTool.FindUnitSource.SearchUnitInUnitLinks');
{$ENDIF}
    UnitLinkStart:=1;
    while UnitLinkStart<=length(UnitLinks) do begin
      while (UnitLinkStart<=length(UnitLinks))
      and (UnitLinks[UnitLinkStart] in [#10,#13]) do
        inc(UnitLinkStart);
      UnitLinkEnd:=UnitLinkStart;
      while (UnitLinkEnd<=length(UnitLinks)) and (UnitLinks[UnitLinkEnd]<>' ')
      do
        inc(UnitLinkEnd);
      if UnitLinkEnd>UnitLinkStart then begin
{$IFDEF ShowTriedFiles}
//writeln('  unit "',copy(UnitLinks,UnitLinkStart,UnitLinkEnd-UnitLinkStart),'"');
{$ENDIF}
        if AnsiCompareText(TheUnitName,
                     copy(UnitLinks,UnitLinkStart,UnitLinkEnd-UnitLinkStart))=0
        then begin
          // unit found -> parse filename
          UnitLinkStart:=UnitLinkEnd+1;
          UnitLinkEnd:=UnitLinkStart;
          while (UnitLinkEnd<=length(UnitLinks))
          and (not (UnitLinks[UnitLinkEnd] in [#10,#13])) do
            inc(UnitLinkEnd);
          if UnitLinkEnd>UnitLinkStart then begin
            CurFilename:=copy(UnitLinks,UnitLinkStart,UnitLinkEnd-UnitLinkStart);
            LoadFile(CurFilename,Result);
            exit;
          end;
        end else begin
          UnitLinkStart:=UnitLinkEnd+1;
          while (UnitLinkStart<=length(UnitLinks))
          and (not (UnitLinks[UnitLinkStart] in [#10,#13])) do
            inc(UnitLinkStart);
        end;
      end else
        break;
    end;
  end;


var CurDir, UnitSrcSearchPath: string;
  MainCodeIsVirtual: boolean;
begin
{$IFDEF CTDEBUG}
writeln('TFindDeclarationTool.FindUnitSource A AnUnitName=',AnUnitName,' AnUnitInFilename=',AnUnitInFilename);
{$ENDIF}
  Result:=nil;
  if (AnUnitName='') or (Scanner=nil) or (Scanner.MainCode=nil)
  or (not (TObject(Scanner.MainCode) is TCodeBuffer))
  or (Scanner.OnLoadSource=nil) then
    exit;
  if Assigned(OnGetUnitSourceSearchPath) then
    UnitSrcSearchPath:=OnGetUnitSourceSearchPath(Self)
  else
    UnitSrcSearchPath:=Scanner.Values[ExternalMacroStart+'SrcPath'];
{$IFDEF CTDEBUG}
writeln('TFindDeclarationTool.FindUnitSource UnitSrcSearchPath=',UnitSrcSearchPath);
{$ENDIF}
//writeln('>>>>>',Scanner.Values.AsString,'<<<<<');
  if AnUnitInFilename<>'' then begin
    // unitname in 'filename'
    if FilenameIsAbsolute(AnUnitInFilename) then begin
      Result:=TCodeBuffer(Scanner.OnLoadSource(Self,AnUnitInFilename));
    end else begin
      // search AnUnitInFilename in searchpath
      Result:=SearchFileInPath(UnitSrcSearchPath,AnUnitInFilename);
    end;
  end else begin
    // normal unit name -> search as the compiler would search
    // first search in current directory (= where the maincode is)
    MainCodeIsVirtual:=TCodeBuffer(Scanner.MainCode).IsVirtual;
    if not MainCodeIsVirtual then begin
      CurDir:=ExtractFilePath(TCodeBuffer(Scanner.MainCode).Filename);
    end else begin
      CurDir:='';
    end;
{$IFDEF CTDEBUG}
writeln('TFindDeclarationTool.FindUnitSource Search in current dir=',CurDir);
{$ENDIF}
    Result:=SearchUnitFileInDir(CurDir,AnUnitName);
    if Result=nil then begin
      // search in search path
{$IFDEF CTDEBUG}
writeln('TFindDeclarationTool.FindUnitSource Search in search path=',UnitSrcSearchPath);
{$ENDIF}
      Result:=SearchUnitFileInPath(UnitSrcSearchPath,AnUnitName);
      if Result=nil then begin
        // search in FPC source directory
        Result:=SearchUnitInUnitLinks(AnUnitName);
      end;
    end;
  end;
end;

function TFindDeclarationTool.IsIncludeDirectiveAtPos(CleanPos,
  CleanCodePosInFront: integer; var IncludeCode: TCodeBuffer): boolean;
var LinkIndex, CommentStart, CommentEnd: integer;
  SrcLink: TSourceLink;
begin
  Result:=false;
  if (Scanner=nil) then exit;
  LinkIndex:=Scanner.LinkIndexAtCleanPos(CleanPos);
  if (LinkIndex<0) or (LinkIndex>=Scanner.LinkCount-1) then exit;
  SrcLink:=Scanner.Links[LinkIndex+1];
  if (SrcLink.Code=nil) or (SrcLink.Code=Scanner.Links[LinkIndex].Code) then
    exit;
  if CleanPosIsInComment(CleanPos,CleanCodePosInFront,CommentStart,CommentEnd)
  and (CommentEnd=SrcLink.CleanedPos) then begin
    IncludeCode:=TCodeBuffer(SrcLink.Code);
    Result:=true;
    exit;
  end;
end;

function TFindDeclarationTool.FindDeclarationOfIdentifier(
  Params: TFindDeclarationParams): boolean;
{ searches an identifier in clean code, parses code in front and after the
  identifier

  Params:
    Identifier in clean source
    ContextNode  // = DeepestNode at Cursor
    
  Result:
    true, if NewPos+NewTopLine valid

  For example:
    A^.B().C[].Identifier
}
var OldContextNode: TCodeTreeNode;
  NewContext: TFindContext;
begin
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindDeclarationOfIdentifier] Identifier=',
  GetIdentifier(Params.Identifier),
  ' ContextNode=',NodeDescriptionAsString(Params.ContextNode.Desc));
{$ENDIF}
  Result:=false;
  MoveCursorToCleanPos(Params.Identifier);
  OldContextNode:=Params.ContextNode;
  NewContext:=FindContextNodeAtCursor(Params);
  Params.Flags:=[fdfSearchInAncestors]
                +fdfAllClassVisibilities+(fdfGlobals*Params.Flags);
  if NewContext.Node=OldContextNode then begin
    Params.Flags:=Params.Flags+[fdfSearchInParentNodes,fdfIgnoreCurContextNode];
  end;
  if NewContext.Tool<>Self then begin
    // search in used unit
    Exclude(Params.Flags,fdfClassPrivate);
    if NewContext.Node.Desc=ctnClass then begin
      // ToDo: if context node is not the class of the method the
      //       search started, remove fdfClassProtected from Flags
      
    end;
  end;
  if (OldContextNode.Desc=ctnTypeDefinition)
  and (OldContextNode.FirstChild<>nil)
  and (OldContextNode.FirstChild.Desc=ctnClass)
  and ((OldContextNode.FirstChild.SubDesc and ctnsForwardDeclaration)>0)
  then
    Include(Params.Flags,fdfSearchForward);

  Params.ContextNode:=NewContext.Node;

  Result:=NewContext.Tool.FindIdentifierInContext(Params);
end;

function TFindDeclarationTool.FindIdentifierInContext(
  Params: TFindDeclarationParams): boolean;
{ searches an identifier in context node
  It does not care about code in front of the identifier like 'a.Identifer'.
  
  Params:
    Identifier
    ContextNode  // = DeepestNode at Cursor

  Result:
    true, if NewPos+NewTopLine valid
}
var LastContextNode, StartContextNode, ContextNode: TCodeTreeNode;
  IsForward: boolean;
begin
  ContextNode:=Params.ContextNode;
  StartContextNode:=ContextNode;
  Result:=false;

  if (fdfSearchForward in Params.Flags) then begin
  
    // ToDo: check for circles
    
  end;
  
  if ContextNode<>nil then begin
    repeat
{$IFDEF ShowTriedContexts}
writeln('[TFindDeclarationTool.FindIdentifierInContext] A Ident=',
GetIdentifier(Params.Identifier),
' Context=',ContextNode.DescAsString,' "',copy(Src,ContextNode.StartPos,8),'"',
' P=',fdfSearchInParentNodes in Params.Flags,
' A=',fdfSearchInAncestors in Params.Flags,
' IUU=',fdfIgnoreUsedUnits in Params.Flags
);
if (ContextNode.Desc=ctnClass) then
  writeln('  ContextNode.LastChild=',ContextNode.LastChild<>nil);
{$ENDIF}
      LastContextNode:=ContextNode;
      if not (fdfIgnoreCurContextNode in Params.Flags) then begin
        case ContextNode.Desc of

        ctnTypeSection, ctnVarSection, ctnConstSection, ctnResStrSection,
        ctnInterface, ctnImplementation,
        ctnClassPublic, ctnClassPrivate, ctnClassProtected, ctnClassPublished,
        ctnClass,
        ctnRecordType, ctnRecordCase, ctnRecordVariant,
        ctnParameterList:
          begin
            if ContextNode.Desc=ctnClass then begin
              // just-in-time parsing for class node
              BuildSubTreeForClass(ContextNode);
            end;
            if (ContextNode.LastChild<>nil) then begin
              if not (fdfSearchForward in Params.Flags) then
                ContextNode:=ContextNode.LastChild
              else
                ContextNode:=ContextNode.FirstChild;
            end;
          end;
          
        ctnTypeDefinition, ctnVarDefinition, ctnConstDefinition:
          begin
            if CompareSrcIdentifiers(ContextNode.StartPos,Params.Identifier)
            then begin
{$IFDEF ShowTriedContexts}
writeln('  Definition Identifier found=',GetIdentifier(Params.Identifier));
{$ENDIF}
              // identifier found
              Result:=true;
              Params.SetResult(Self,ContextNode);
              exit;
            end;
            // search for enums
            Params.ContextNode:=ContextNode;
            Result:=FindEnumInContext(Params);
            if Result then exit;
          end;

        ctnProcedure:
          begin
            Result:=FindIdentifierInProcContext(ContextNode,Params);
            if Result then exit;
          end;

        ctnProcedureHead:
          begin
            BuildSubTreeForProcHead(ContextNode);
            if ContextNode.FirstChild<>nil then
              ContextNode:=ContextNode.FirstChild;
          end;

        ctnProgram, ctnPackage, ctnLibrary, ctnUnit:
          begin
            MoveCursorToNodeStart(ContextNode);
            ReadNextAtom; // read keyword
            ReadNextAtom; // read name
            if CompareSrcIdentifiers(CurPos.StartPos,Params.Identifier) then
            begin
              // identifier found
{$IFDEF ShowTriedContexts}
writeln('  Source Name Identifier found=',GetIdentifier(Params.Identifier));
{$ENDIF}
              Result:=true;
              Params.SetResult(Self,ContextNode,CurPos.StartPos);
              exit;
            end;
            Result:=FindIdentifierInHiddenUsedUnits(Params);
            if Result then exit;
          end;

        ctnProperty:
          begin
            if (Params.Identifier[0]<>'[') then begin
              MoveCursorToNodeStart(ContextNode);
              ReadNextAtom; // read keyword 'property'
              ReadNextAtom; // read name
              if CompareSrcIdentifiers(CurPos.StartPos,Params.Identifier) then
              begin
                // identifier found
                
                // ToDo: identifiers after 'read', 'write' are procs with
                //       special parameter lists
                
{$IFDEF ShowTriedContexts}
writeln('  Property Identifier found=',GetIdentifier(Params.Identifier));
{$ENDIF}
                Result:=true;
                Params.SetResult(Self,ContextNode,CurPos.StartPos);
                exit;
              end;
            end else begin
              // the default property is searched
              Result:=PropertyIsDefault(ContextNode);
              if Result then exit;
            end;
          end;

        ctnUsesSection:
          begin
            Result:=FindIdentifierInUsesSection(ContextNode,Params);
            if Result then exit;
          end;

        ctnWithVariable:
          begin
            Result:=FindIdentifierInWithVarContext(ContextNode,Params);
            if Result then exit;
          end;

        ctnPointerType:
          begin
            // pointer types can be forward definitions
            Params.ContextNode:=ContextNode.Parent;
            Result:=FindForwardIdentifier(Params,IsForward);
            exit;
          end;

        end;
      end else begin
        Exclude(Params.Flags,fdfIgnoreCurContextNode);
{$IFDEF ShowTriedContexts}
writeln('[TFindDeclarationTool.FindIdentifierInContext] IgnoreCurContext');
{$ENDIF}
      end;
      if LastContextNode=ContextNode then begin
        // same context -> search in prior context
        if (not ContextNode.HasAsParent(StartContextNode)) then begin
          // searching in a prior node, will leave the start context
          if (not (fdfSearchInParentNodes in Params.Flags)) then begin
            // searching in any parent context is not permitted
            if not ((fdfSearchInAncestors in Params.Flags)
            and (ContextNode.Desc=ctnClass)) then begin
              // even searching in ancestors contexts is not permitted
              // -> there is no prior context accessible any more
              // -> identifier not found
{$IFDEF ShowTriedContexts}
writeln('[TFindDeclarationTool.FindIdentifierInContext] no prior node accessible   ContextNode=',ContextNode.DescAsString);
{$ENDIF}
              exit;
            end;
          end;
        end;

        repeat
          // search for prior node
{$IFDEF ShowTriedContexts}
//writeln('[TFindDeclarationTool.FindIdentifierInContext] Searching prior node of ',ContextNode.DescAsString);
{$ENDIF}
          if (ContextNode.Desc=ctnClass)
          and (fdfSearchInAncestors in Params.Flags) then
          begin
            Result:=FindIdentifierInAncestors(ContextNode,Params);
            if Result then exit;
          end;
          
          if ((not (fdfSearchForward in Params.Flags))
              and (ContextNode.PriorBrother<>nil))
          or ((fdfSearchForward in Params.Flags)
              and (ContextNode.NextBrother<>nil)
              and (ContextNode.NextBrother.Desc<>ctnImplementation)) then
          begin
            if not (fdfSearchForward in Params.Flags) then
              ContextNode:=ContextNode.PriorBrother
            else
              ContextNode:=ContextNode.NextBrother;
{$IFDEF ShowTriedContexts}
writeln('[TFindDeclarationTool.FindIdentifierInContext] Searching in PriorBrother  ContextNode=',ContextNode.DescAsString);
{$ENDIF}
            // it is not always allowed to search in every node on the same lvl:

            // -> test if class visibility valid
            case ContextNode.Desc of
            ctnClassPublished: if (fdfClassPublished in Params.Flags) then break;
            ctnClassPublic:    if (fdfClassPublic    in Params.Flags) then break;
            ctnClassProtected: if (fdfClassProtected in Params.Flags) then break;
            ctnClassPrivate:   if (fdfClassPrivate   in Params.Flags) then break;
            else
              break;
            end;
          end else if ContextNode.Parent<>nil then begin
            ContextNode:=ContextNode.Parent;
{$IFDEF ShowTriedContexts}
writeln('[TFindDeclarationTool.FindIdentifierInContext] Searching in Parent  ContextNode=',ContextNode.DescAsString);
{$ENDIF}
            case ContextNode.Desc of
            
            ctnTypeSection, ctnVarSection, ctnConstSection, ctnResStrSection,
            ctnInterface, ctnImplementation,
            ctnClassPublished,ctnClassPublic,ctnClassProtected, ctnClassPrivate,
            ctnRecordCase, ctnRecordVariant,
            ctnProcedureHead, ctnParameterList:
              // these codetreenodes build a parent-child-relationship, but
              // for pascal it is only a range, hence after searching in the
              // childs of the last node, it must be searched next in the childs
              // of the prior node
              ;

            ctnClass, ctnRecordType:
              // do not search again in this node, go on ...
              ;
              
            ctnProcedure:
              begin
                Result:=FindIdentifierInClassOfMethod(ContextNode,Params);
                if Result then exit;
              end;
              
            else
              break;
            end;
          end else begin
            ContextNode:=nil;
            break;
          end;
        until false;
      end;
    until ContextNode=nil;
  end else begin
    // DeepestNode=nil -> ignore
  end;
  if fdfExceptionOnNotFound in Params.Flags then begin
    if IsPCharInSrc(Params.Identifier) then
      MoveCursorToCleanPos(Params.Identifier);
    RaiseException('Identifier not found '+GetIdentifier(Params.Identifier));
  end;
end;

function TFindDeclarationTool.FindEnumInContext(
  Params: TFindDeclarationParams): boolean;
{ search all subnodes for ctnEnumType

  Params:
    Identifier
    ContextNode  // = DeepestNode at Cursor

  Result:
    true, if NewPos+NewTopLine valid
 }
var OldContextNode: TCodeTreeNode;
begin
  Result:=false;
  if Params.ContextNode=nil then exit;
  OldContextNode:=Params.ContextNode;
  try
    if Params.ContextNode.Desc=ctnClass then
      BuildSubTreeForClass(Params.ContextNode);
    Params.ContextNode:=Params.ContextNode.FirstChild;
    while Params.ContextNode<>nil do begin
      if (Params.ContextNode.Desc in [ctnEnumIdentifier])
      and CompareSrcIdentifiers(Params.ContextNode.StartPos,Params.Identifier)
      then begin
        // identifier found
        Result:=true;
        Params.SetResult(Self,Params.ContextNode);
        exit;
      end;
      Result:=FindEnumInContext(Params);
      if Result then exit;
      Params.ContextNode:=Params.ContextNode.NextBrother;
    end;
  finally
    Params.ContextNode:=OldContextNode;
  end;
end;

function TFindDeclarationTool.FindContextNodeAtCursor(
  Params: TFindDeclarationParams): TFindContext;
{ searches for the context node for a specific cursor pos
  Params.Context should contain the deepest node at cursor
  if there is no special context, then result is equal to Params.Context
  

  Examples:
  
  1. A.B     - CleanPos points to B: if A is a class, the context node will be
               the class node (ctnRecordType).
  2. A().B   - same as above
          
  3. inherited A - CleanPos points to A: if in a method, the context node will
                   be the class node (ctnClass) of the current method.
          
  4. A[].    - CleanPos points to '.': if A is an array, the context node will
               be the array type node (ctnArrayType).
  
  5. A[].B   - CleanPos points to B: if A is an array of record, the context
               node will be the record type node (ctnRecordType).

  6. A^.     - CleanPos points to '.': if A is a pointer of record, the context
               node will be the record type node (ctnRecordType).
               
  7. (A).    - CleanPos points to '.': if A is a class, the context node will be
               the class node (ctnClass).
               
  8. (A as B) - CleanPos points to ')': if B is a classtype, the context node
                will be the class node (ctnClass)

}
type
  TAtomType = (atNone, atSpace, atIdentifier, atPoint, atAS, atINHERITED, atUp,
               atRoundBracketOpen, atRoundBracketClose,
               atEdgedBracketOpen, atEdgedBracketClose,
               atRead, atWrite);
const
  AtomTypeNames: array[TAtomType] of string =
    ('<None>','Space','Ident','Point','AS','INHERITED','Up^',
     'Bracket(','Bracket)','Bracket[','Bracket]','READ','WRITE');

  function GetCurrentAtomType: TAtomType;
  begin
    if (CurPos.StartPos=CurPos.EndPos) then
      Result:=atSpace
    else if UpAtomIs('READ') then
      Result:=atRead
    else if UpAtomIs('WRITE') then
      Result:=atWrite
    else if AtomIsIdentifier(false) then
      Result:=atIdentifier
    else if (CurPos.StartPos>=1) and (CurPos.StartPos<=SrcLen)
    and (CurPos.StartPos=CurPos.EndPos-1) then begin
      case Src[CurPos.StartPos] of
      '.': Result:=atPoint;
      '^': Result:=atUp;
      '(': Result:=atRoundBracketOpen;
      ')': Result:=atRoundBracketClose;
      '[': Result:=atEdgedBracketOpen;
      ']': Result:=atEdgedBracketClose;
      else Result:=atNone;
      end;
    end
    else if UpAtomIs('INHERITED') then
      Result:=atINHERITED
    else if UpAtomIs('AS') then
      Result:=atAS
    else
      Result:=atNone;
  end;


var CurAtom, NextAtom: TAtomPosition;
  OldInput: TFindDeclarationInput;
  NextAtomType, CurAtomType: TAtomType;
  ProcNode: TCodeTreeNode;
begin
  // start parsing the expression from right to left
  NextAtom:=CurPos;
  NextAtomType:=GetCurrentAtomType;
  ReadPriorAtom;
  CurAtom:=CurPos;
  CurAtomType:=GetCurrentAtomType;
write('[TFindDeclarationTool.FindContextNodeAtCursor] A ',
  ' Context=',Params.ContextNode.DescAsString,
  ' CurAtom=',AtomTypeNames[CurAtomType],
  ' "',copy(Src,CurAtom.StartPos,CurAtom.EndPos-CurAtom.StartPos),'"',
  ' NextAtom=',AtomTypeNames[NextAtomType]
  );
writeln('');
  if not (CurAtomType in [atIdentifier,atPoint,atUp,atAs,atEdgedBracketClose,
    atRoundBracketClose,atRead,atWrite,atINHERITED])
  then begin
    // no special context found -> the context node is the deepest node at
    // cursor, and this should already be in Params.ContextNode
    if (not (NextAtomType in [atSpace,atIdentifier,atRoundBracketOpen,
      atEdgedBracketOpen])) then
    begin
      MoveCursorToCleanPos(NextAtom.StartPos);
      ReadNextAtom;
      RaiseException('identifier expected, but '
                      +GetAtom+' found');
    end;
    Result:=CreateFindContext(Self,Params.ContextNode);
    exit;
  end;
  if (CurAtomType in [atRoundBracketClose,atEdgedBracketClose]) then begin
    ReadBackTilBracketClose(true);
    CurAtom.StartPos:=CurPos.StartPos;
  end;
  if (not (CurAtomType in [atAS,atRead,atWrite,atINHERITED]))
  and ((CurAtomType<>atIdentifier) or (NextAtomType<>atIdentifier)) then
    Result:=FindContextNodeAtCursor(Params)
  else
    Result:=CreateFindContext(Self,Params.ContextNode);
  if Result.Node=nil then exit;
  
  // the left side has been parsed and
  // now the parsing goes from left to right
  
{$IFDEF CTDEBUG}
write('[TFindDeclarationTool.FindContextNodeAtCursor] B ',
  ' Context=',Params.ContextNode.DescAsString,
  ' CurAtom=',AtomTypeNames[CurAtomType],
  ' "',copy(Src,CurAtom.StartPos,CurAtom.EndPos-CurAtom.StartPos),'"',
  ' NextAtom=',AtomTypeNames[NextAtomType],
  ' Result=');
if Result.Node<>nil then write(Result.Node.DescAsString) else write('NIL');
writeln('');
{$ENDIF}

  case CurAtomType of

  atIdentifier:
    begin
      // for example  'AnObject[3]'
      if not (NextAtomType in [atSpace,atPoint,atUp,atAS,atRoundBracketOpen,
        atRoundBracketClose,atEdgedBracketOpen,atEdgedBracketClose]) then
      begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaiseException('illegal qualifier "'+GetAtom+'" found');
      end;
      if (Result.Node=Params.ContextNode) then begin
        if CompareSrcIdentifier(CurAtom.StartPos,'SELF') then begin
          // SELF in a method is the object itself
          // -> check if in a proc
          ProcNode:=Params.ContextNode;
          while (ProcNode<>nil) do begin
            if (ProcNode.Desc=ctnProcedure) then begin
              // in a proc -> find the class context
              if Result.Tool.FindClassOfMethod(ProcNode,Params,true) then begin
                Result:=CreateFindContext(Params);
                exit;
              end;
            end;
            ProcNode:=ProcNode.Parent;
          end;
        end else if CompareSrcIdentifier(CurAtom.StartPos,'RESULT') then begin
          // RESULT has a special meaning in a function
          // -> check if in a function
          ProcNode:=Params.ContextNode;
          while (ProcNode<>nil) do begin
            if (ProcNode.Desc=ctnProcedure) then begin
              Result:=Result.Tool.FindBaseTypeOfNode(Params,ProcNode);
              exit;
            end;
            ProcNode:=ProcNode.Parent;
          end;
        end;
      end;
      // find identifier
      Params.Save(OldInput);
      try
        Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound]
                      +fdfAllClassVisibilities
                      +(fdfGlobals*Params.Flags);
//writeln('  AAA ',Result.Node=Params.ContextNode,' ',Result.Node.DescAsString,',',Params.ContextNode.DescAsString);
        if Result.Node=Params.ContextNode then begin
          // there is no special context -> also search in parent contexts
          Params.Flags:=Params.Flags
                       +[fdfSearchInParentNodes,fdfIgnoreCurContextNode];
        end else
          // special context
          Params.ContextNode:=Result.Node;
        Params.Identifier:=@Src[CurAtom.StartPos];
        Result.Tool.FindIdentifierInContext(Params);
        Result:=CreateFindContext(Params);
      finally
        Params.Load(OldInput);
      end;
      Result:=Result.Tool.FindBaseTypeOfNode(Params,Result.Node);
    end;
    
  atPoint:
    begin
      // for example 'A.B'
      if Result.Node=Params.ContextNode then begin
        MoveCursorToCleanPos(CurAtom.StartPos);
        RaiseException('identifier expected, but . found');
      end;
      if (not (NextAtomType in [atSpace,atIdentifier])) then begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaiseException('identifier expected, but '+GetAtom+' found');
      end;
      if (Result.Node.Desc in AllUsableSoureTypes) then begin
        // identifier in front of the point is a unit name
        if Result.Tool<>Self then begin
          Result.Node:=Result.Tool.GetInterfaceNode;
        end else begin
          Result:=CreateFindContext(Self,Params.ContextNode);
        end;
      end;
      // there is no special left to do, since Result already points to
      // the type context node.
    end;

  atAS:
    begin
      // for example 'A as B'
      if (not (NextAtomType in [atSpace,atIdentifier])) then begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaiseException('identifier expected, but '+GetAtom+' found');
      end;
      // 'as' is a type cast, so the left side is irrelevant and was already
      // ignored in the code at the start of this proc
      // -> context is default context
    end;

  atUP:
    begin
      // for example:
      //   1. 'PInt = ^integer'  pointer type
      //   2. a^  dereferencing
      if not (NextAtomType in [atSpace,atPoint,atUp,atAS,atEdgedBracketClose,
        atEdgedBracketOpen,atRoundBracketClose]) then
      begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaiseException('illegal qualifier "'+GetAtom+'" found');
      end;
      if Result.Node<>Params.ContextNode then begin
        // left side of expression has defined a special context
        // => this '^' is a dereference
        if (not (NextAtomType in [atSpace,atPoint,atAS,atUP])) then begin
          MoveCursorToCleanPos(NextAtom.StartPos);
          ReadNextAtom;
          RaiseException('. expected, but '+GetAtom+' found');
        end;
        if Result.Node.Desc<>ctnPointerType then begin
          MoveCursorToCleanPos(CurAtom.StartPos);
          RaiseException('illegal qualifier ^');
        end;
        Result:=Result.Tool.FindBaseTypeOfNode(Params,Result.Node.FirstChild);
      end else if NodeHasParentOfType(Result.Node,ctnPointerType) then begin
        // this is a pointer type definition
        // -> the default context is ok
      end;
    end;

  atEdgedBracketClose:
    begin
      // for example:  a[]
      //   this could be:
      //     1. ranged array
      //     2. dynamic array
      //     3. indexed pointer
      //     4. default property
      if not (NextAtomType in [atSpace,atPoint,atAs,atUp,atRoundBracketClose,
        atRoundBracketOpen,atEdgedBracketClose,atEdgedBracketOpen]) then
      begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaiseException('illegal qualifier');
      end;
      if Result.Node<>Params.ContextNode then begin
        case Result.Node.Desc of
        
        ctnArrayType:
          // the array type is the last child node
          Result:=Result.Tool.FindBaseTypeOfNode(Params,Result.Node.LastChild);

        ctnPointerType:
          // the pointer type is the only child node
          Result:=Result.Tool.FindBaseTypeOfNode(Params,Result.Node.FirstChild);

        ctnClass:
          begin
            // search default property in class
            Params.Save(OldInput);
            Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound]
                          +fdfGlobals*Params.Flags;
            Params.Identifier:='['; // special identifier for default property
            Params.ContextNode:=Result.Node;
            Result.Tool.FindIdentifierInContext(Params);
            Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,Params.NewNode);
            Params.Load(OldInput);
          end;

        else
          MoveCursorToCleanPos(CurAtom.StartPos);
          RaiseException('illegal qualifier');
        end;
      end;
    end;

  atRoundBracketClose:
    begin
      { for example:
          (a+b)   expression bracket: the type is the result type of the
                                      expression.
          a()     typecast or function
      }
      if not (NextAtomType in [atSpace,atPoint,atAs,atUp,atRoundBracketClose,
        atRoundBracketOpen,atEdgedBracketClose,atEdgedBracketOpen]) then
      begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaiseException('illegal qualifier');
      end;
      if Result.Node<>Params.ContextNode then begin
        // typecast or function
        
        // ToDo: proc overloading, if parameter types incompatible, search next
        
      end else begin
        // expression
        Result:=FindExpressionResultType(Params,CurAtom.StartPos+1,
                                         CurAtom.EndPos-1);
      end;
    end;

  atINHERITED:
    begin
      // for example: inherited A;
      if not (NextAtomType in [atSpace,atIdentifier]) then begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaiseException('identifier expected, but '+GetAtom+' found');
      end;
      // find ancestor of class of method
      ProcNode:=Result.Node;
      while (ProcNode<>nil) do begin
        if not (ProcNode.Desc in [ctnProcedure,ctnProcedureHead,ctnBeginBlock,
           ctnAsmBlock,ctnWithVariable,ctnWithStatement,ctnCaseBlock,
           ctnCaseVariable,ctnCaseStatement]) then
        begin
          break;
        end;
        if ProcNode.Desc=ctnProcedure then begin
          Result.Tool.FindClassOfMethod(ProcNode,Params,true);
          // find class ancestor
          Params.NewCodeTool.FindAncestorOfClass(Params.NewNode,Params,true);
          Result:=CreateFindContext(Params);
          exit;
        end;
        ProcNode:=ProcNode.Parent;
      end;
      MoveCursorToCleanPos(CurAtom.StartPos);
      RaiseException('inherited keyword only allowed in methods');
    end;

  else
    // expression start found
    begin
      if (not (NextAtomType in [atSpace,atIdentifier,atRoundBracketOpen,
        atEdgedBracketOpen])) then
      begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        ReadNextAtom;
        RaiseException('identifier expected, but '+GetAtom+' found');
      end;
    end;
  end;
  
{$IFDEF CTDEBUG}
write('[TFindDeclarationTool.FindContextNodeAtCursor] END ',
  Params.ContextNode.DescAsString,' CurAtom=',AtomTypeNames[CurAtomType],
  ' NextAtom=',AtomTypeNames[NextAtomType],' Result=');
if Result.Node<>nil then write(Result.Node.DescAsString) else write('NIL');
writeln('');
{$ENDIF}
end;

function TFindDeclarationTool.FindBaseTypeOfNode(Params: TFindDeclarationParams;
  Node: TCodeTreeNode): TFindContext;
var OldInput: TFindDeclarationInput;
  ClassIdentNode: TCodeTreeNode;
begin
  Result.Node:=Node;
  Result.Tool:=Self;
  while (Result.Node<>nil) do begin
  
    // ToDo: check for circles
  
{$IFDEF ShowTriedContexts}
writeln('[TFindDeclarationTool.FindBaseTypeOfNode] A Result=',Result.Node.DescAsString);
{$ENDIF}
    if (Result.Node.Desc in AllIdentifierDefinitions) then begin
      // instead of variable/const/type definition, return the type
      Result.Node:=FindTypeNodeOfDefinition(Result.Node);
    end else
    if (Result.Node.Desc=ctnClass)
    and ((Result.Node.SubDesc and ctnsForwardDeclaration)>0) then
    begin
      // search the real class
      ClassIdentNode:=Result.Node.Parent;
      if (ClassIdentNode=nil) or (not (ClassIdentNode.Desc=ctnTypeDefinition))
      then begin
        MoveCursorToCleanPos(Result.Node.StartPos);
        RaiseException('[TFindDeclarationTool.FindBaseTypeOfNode] '
                      +'forward class node without name');
      end;
      Params.Save(OldInput);
      try
        Params.Identifier:=@Src[ClassIdentNode.StartPos];
        Params.Flags:=[fdfSearchInParentNodes,fdfSearchForward,
                       fdfIgnoreUsedUnits,fdfExceptionOnNotFound]
                      +(fdfGlobals*Params.Flags);
        Params.ContextNode:=ClassIdentNode;
        FindIdentifierInContext(Params);
        if (Params.NewNode.Desc<>ctnTypeDefinition)
        or (Params.NewCodeTool<>Self) then begin
          MoveCursorToCleanPos(Result.Node.StartPos);
          RaiseException('Forward class definition not resolved: '
              +copy(Src,ClassIdentNode.StartPos,
                  ClassIdentNode.EndPos-ClassIdentNode.StartPos));
        end;
        Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,Params.NewNode);
        exit;
      finally
        Params.Load(OldInput);
      end;
    end else
    if (Result.Node.Desc=ctnIdentifier) then begin
      // this type is just an alias for another type
      // -> search the basic type
      if Result.Node.Parent=nil then
        break;
      Params.Save(OldInput);
      try
        Params.Identifier:=@Src[Result.Node.StartPos];
        Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound]
                      +(fdfGlobals*Params.Flags);
        Params.ContextNode:=Result.Node.Parent;
        if Params.ContextNode.Desc=ctnParameterList then
          Params.ContextNode:=Params.ContextNode.Parent;
        if Params.ContextNode.Desc=ctnProcedureHead then
          Params.ContextNode:=Params.ContextNode.Parent;
        FindIdentifierInContext(Params);
        Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,Params.NewNode);
        exit;
      finally
        Params.Load(OldInput);
      end;
    end else
    if (Result.Node.Desc=ctnProperty) then begin
      // this is a property -> search the type definition of the property
      ReadTilTypeOfProperty(Result.Node);
      Params.Save(OldInput);
      try
        Params.Identifier:=@Src[CurPos.StartPos];
        Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound]
                      +(fdfGlobals*Params.Flags);
        Params.ContextNode:=Result.Node.Parent;
        FindIdentifierInContext(Params);
        if Result.Node.HasAsParent(Params.NewNode) then
          break;
        Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,Params.NewNode);
        exit;
      finally
        Params.Load(OldInput);
      end;
    end else
    if (Result.Node.Desc in [ctnProcedure,ctnProcedureHead]) then begin
      // a proc -> if this is a function return the result type
      if Result.Node.Desc=ctnProcedureHead then
        Result.Node:=Result.Node.Parent;
      MoveCursorToNodeStart(Result.Node);
      ReadNextAtom;
      if UpAtomIs('CLASS') then ReadNextAtom;
      if UpAtomIs('FUNCTION') then begin
        // in a function -> find the result type
        // build nodes for parameter list and result type
        BuildSubTreeForProcHead(Result.Node);
        // a proc node contains has as FirstChild a proc-head node
        // and a proc-head node has as childs the parameterlist and the result
        Result.Node:=Result.Node.FirstChild.FirstChild;
        if Result.Node.Desc=ctnParameterList then
          Result.Node:=Result.Node.NextBrother;
      end else
        break;
    end else
    if (Result.Node.Desc=ctnTypeType) then begin
      // a TypeType is for example 'MyInt = type integer;'
      // the context is not the 'type' keyword, but the identifier after it.
      Result.Node:=Result.Node.FirstChild;
    end else
      break;
  end;
  if (Result.Node=nil) and (fdfExceptionOnNotFound in Params.Flags) then begin
    MoveCursorToCleanPos(Params.Identifier);
    RaiseException('base type not found');
  end;
{$IFDEF CTDEBUG}
write('[TFindDeclarationTool.FindBaseTypeOfNode] END Node=');
if Node<>nil then write(Node.DescAsString) else write('NIL');
write(' Result=');
if Result.Node<>nil then write(Result.Node.DescAsString) else write('NIL');
writeln('');
{$ENDIF}
end;

function TFindDeclarationTool.FindIdentifierInProcContext(
  ProcContextNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
{ this function is internally used by FindIdentifierInContext
}
var
  NameAtom: TAtomPosition;
begin
  Result:=false;
  // if proc is a method, search in class
  // -> find class name
  MoveCursorToNodeStart(ProcContextNode);
  ReadNextAtom; // read keyword
  ReadNextAtom; // read name
  NameAtom:=CurPos;
  ReadNextAtom;
  if AtomIsChar('.') then begin
    // proc is a method
    // -> proceed the search normally ...
  end else begin
    // proc is not a method
    if CompareSrcIdentifiers(NameAtom.StartPos,Params.Identifier) then
    begin
      // proc identifier found
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInProcContext]  Proc Identifier found=',GetIdentifier(Params.Identifier));
{$ENDIF}
      Result:=true;
      Params.SetResult(Self,ProcContextNode,NameAtom.StartPos);
      exit;
    end else begin
      // proceed the search normally ...
    end;
  end;
end;

function TFindDeclarationTool.FindIdentifierInClassOfMethod(
  ProcContextNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
{ this function is internally used by FindIdentifierInContext
}
var
  ClassNameAtom: TAtomPosition;
  OldInput: TFindDeclarationInput;
  ClassContext: TFindContext;
begin
  Result:=false;
  // if proc is a method, search in class
  // -> find class name
  MoveCursorToNodeStart(ProcContextNode);
  ReadNextAtom; // read keyword
  ReadNextAtom; // read classname
  ClassNameAtom:=CurPos;
  ReadNextAtom;
  if AtomIsChar('.') then begin
    // proc is a method
    if CompareSrcIdentifiers(ClassNameAtom.StartPos,Params.Identifier) then
    begin
      // the class itself is searched
      // -> proceed the search normally ...
    end else begin
      // search the identifier in the class first
      // 1. search the class
      Params.Save(OldInput);
      try
        Params.Flags:=[fdfIgnoreCurContextNode,fdfSearchInParentNodes]
                      +(fdfGlobals*Params.Flags)
                      +[fdfExceptionOnNotFound,fdfIgnoreUsedUnits];
        Params.ContextNode:=ProcContextNode;
        Params.Identifier:=@Src[ClassNameAtom.StartPos];
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInProcContext]  Proc="',copy(src,ProcContextNode.StartPos,30),'" searching class of method   class="',GetIdentifier(ClassNameAtom.StartPos),'"');
{$ENDIF}
        FindIdentifierInContext(Params);
        ClassContext:=Params.NewCodeTool.FindBaseTypeOfNode(
                                                     Params,Params.NewNode);
        if (ClassContext.Node=nil)
        or (ClassContext.Node.Desc<>ctnClass) then begin
          MoveCursorToCleanPos(ClassNameAtom.StartPos);
          RaiseException('class identifier expected');
        end;
        // class context found
        // 2. -> search identifier in class
        Params.Load(OldInput);
        Params.Flags:=[fdfSearchInAncestors]+fdfAllClassVisibilities
                      +(fdfGlobals*Params.Flags)
                      -[fdfExceptionOnNotFound];
        Params.ContextNode:=ClassContext.Node;
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInProcContext]  searching identifier in class of method');
{$ENDIF}
        Result:=ClassContext.Tool.FindIdentifierInContext(Params);
        if Result then exit;
      finally
        Params.Load(OldInput);
      end;
    end;
  end else begin
    // proc is not a method
    if CompareSrcIdentifiers(ClassNameAtom.StartPos,Params.Identifier) then
    begin
      // proc identifier found
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInProcContext]  Proc Identifier found=',GetIdentifier(Params.Identifier));
{$ENDIF}
      Result:=true;
      Params.SetResult(Self,ProcContextNode,ClassNameAtom.StartPos);
      exit;
    end else begin
      // proceed the search normally ...
    end;
  end;
end;

function TFindDeclarationTool.FindClassOfMethod(ProcNode: TCodeTreeNode;
  Params: TFindDeclarationParams; FindClassContext: boolean): boolean;
var
  ClassNameAtom: TAtomPosition;
  OldInput: TFindDeclarationInput;
  ClassContext: TFindContext;
begin
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindClassOfMethod] A ');
{$ENDIF}
  Result:=false;
  MoveCursorToNodeStart(ProcNode);
  ReadNextAtom; // read keyword
  ReadNextAtom; // read classname
  ClassNameAtom:=CurPos;
  ReadNextAtom;
  if AtomIsChar('.') then begin
    // proc is a method
    // -> search the class
    Params.Save(OldInput);
    try
      Params.Flags:=[fdfIgnoreCurContextNode,fdfSearchInParentNodes,
                     fdfExceptionOnNotFound,fdfIgnoreUsedUnits]
                    +(fdfGlobals*Params.Flags);
      Params.ContextNode:=ProcNode;
      Params.Identifier:=@Src[ClassNameAtom.StartPos];
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindClassOfMethod]  searching class of method   class="',GetIdentifier(ClassNameAtom.StartPos),'"');
{$ENDIF}
      FindIdentifierInContext(Params);
      if FindClassContext then begin
        // parse class and return class node
        ClassContext:=FindBaseTypeOfNode(Params,Params.NewNode);
        if (ClassContext.Node=nil)
        or (ClassContext.Node.Desc<>ctnClass) then begin
          MoveCursorToCleanPos(ClassNameAtom.StartPos);
          RaiseException('class identifier expected');
        end;
        // class of method found
        Params.SetResult(ClassContext);
        // parse class and return class node

        // ToDo: do no JIT parsing for PPU, PPW, DCU files

        ClassContext.Tool.BuildSubTreeForClass(ClassContext.Node);
      end;
      Result:=true;
    finally
      Params.Load(OldInput);
    end;
  end else begin
    // proc is not a method
  end;
end;

function TFindDeclarationTool.FindAncestorOfClass(ClassNode: TCodeTreeNode;
  Params: TFindDeclarationParams; FindClassContext: boolean): boolean;
var AncestorAtom: TAtomPosition;
  OldInput: TFindDeclarationInput;
  AncestorNode, ClassIdentNode: TCodeTreeNode;
  SearchTObject: boolean;
  AncestorContext: TFindContext;
begin
  if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) then
    RaiseException('[TFindDeclarationTool.FindAncestorOfClass] '
      +' invalid classnode');
  Result:=false;
  // search the ancestor name
  MoveCursorToNodeStart(ClassNode);
  ReadNextAtom; // read keyword 'class', 'object', 'interface', 'dispinterface'
  if UpAtomIs('PACKED') then ReadNextAtom;
  ReadNextAtom;
  if not AtomIsChar('(') then begin
    // no ancestor class specified
    // check class name
    ClassIdentNode:=ClassNode.Parent;
    if (ClassIdentNode=nil) or (ClassIdentNode.Desc<>ctnTypeDefinition) then
    begin
      MoveCursorToNodeStart(ClassNode);
      RaiseException('class without name');
    end;
    // if this class is not TObject, TObject is class ancestor
    SearchTObject:=not CompareSrcIdentifier(ClassIdentNode.StartPos,'TObject');
    if not SearchTObject then exit;
  end else begin
    ReadNextAtom;
    if not AtomIsIdentifier(false) then exit;
    // ancestor name found
    AncestorAtom:=CurPos;
    SearchTObject:=false;
  end;
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindAncestorOfClass] ',
' search ancestor class = ',GetAtom);
{$ENDIF}
  // search ancestor class context
  CurPos.StartPos:=CurPos.EndPos;
  Params.Save(OldInput);
  try
    Params.Flags:=[fdfSearchInParentNodes,fdfIgnoreCurContextNode,
                   fdfExceptionOnNotFound]
                  +(fdfGlobals*Params.Flags);
    if not SearchTObject then
      Params.Identifier:=@Src[AncestorAtom.StartPos]
    else begin
      Params.Identifier:='TObject';
      Exclude(Params.Flags,fdfExceptionOnNotFound);
    end;
    Params.ContextNode:=ClassNode;
    if not FindIdentifierInContext(Params) then begin
      MoveCursorToNodeStart(ClassNode);
//writeln('  AQ2*** ',TCodeBuffer(Scanner.MainCode).Filename,' ',CurPos.StartPos);
      RaiseException('default class ancestor TObject not found');
    end;
    if FindClassContext then begin
      AncestorNode:=Params.NewNode;
      AncestorContext:=Params.NewCodeTool.FindBaseTypeOfNode(Params,
                                                             AncestorNode);
      Params.SetResult(AncestorContext);
    end;
  finally
    Params.Load(OldInput);
  end;
end;

function TFindDeclarationTool.FindForwardIdentifier(
  Params: TFindDeclarationParams; var IsForward: boolean): boolean;
{ first search the identifier in the normal way via FindIdentifierInContext
  then search the other direction }
var
  OldInput: TFindDeclarationInput;
begin
  Params.Save(OldInput);
  Exclude(Params.Flags,fdfExceptionOnNotFound);
  Result:=FindIdentifierInContext(Params);
  if not Result then begin
    Params.Load(OldInput);
    Include(Params.Flags,fdfSearchForward);
    Result:=FindIdentifierInContext(Params);
    IsForward:=true;
  end else begin
    IsForward:=false;
    Params.Load(OldInput);
  end;
end;

function TFindDeclarationTool.FindIdentifierInWithVarContext(
  WithVarNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
{ this function is internally used by FindIdentifierInContext
}
var
  WithVarContext: TFindContext;
  OldInput: TFindDeclarationInput;
begin
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInWithVarContext] ',
GetIdentifier(Params.Identifier)
);
{$ENDIF}
  Result:=false;
  // find the base type of the with variable
  // move cursor to end of with-expression
  if (WithVarNode.FirstChild<>nil) then begin
    // this is the last with-variable
    MoveCursorToCleanPos(WithVarNode.FirstChild.StartPos);
    ReadPriorAtom; // read 'do'
    CurPos.EndPos:=CurPos.StartPos; // make the 'do' unread,
                                    // because 'do' is not part of the expr
  end else begin
    // this is not the last with variable, so the expr end is equal to node end
    MoveCursorToCleanPos(WithVarNode.EndPos);
  end;
  Params.Save(OldInput);
  Params.ContextNode:=WithVarNode;
  Include(Params.Flags,fdfExceptionOnNotFound);
  WithVarContext:=FindContextNodeAtCursor(Params);
  if (WithVarContext.Node=nil) or (WithVarContext.Node=OldInput.ContextNode)
  or (not (WithVarContext.Node.Desc in [ctnClass,ctnRecordType])) then begin
    MoveCursorToCleanPos(WithVarNode.StartPos);
    RaiseException('expression type must be class or record type');
  end;
  // search identifier in with context
  Params.Load(OldInput);
  Exclude(Params.Flags,fdfExceptionOnNotFound);
  Params.ContextNode:=WithVarContext.Node;
  if WithVarContext.Tool.FindIdentifierInContext(Params) then begin
    // identifier found in with context
    Result:=true;
  end else
    Params.Load(OldInput);
end;

function TFindDeclarationTool.FindIdentifierInAncestors(
  ClassNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
{ this function is internally used by FindIdentifierInContext
}
var AncestorAtom: TAtomPosition;
  OldInput: TFindDeclarationInput;
  AncestorNode, ClassIdentNode: TCodeTreeNode;
  SearchTObject: boolean;
  AncestorContext: TFindContext;
begin
  if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) then
    RaiseException('[TFindDeclarationTool.FindIdentifierInAncestors] '
      +' invalid classnode');
  Result:=false;
  if not (fdfSearchInAncestors in Params.Flags) then exit;
  // search the ancestor name
  MoveCursorToNodeStart(ClassNode);
  ReadNextAtom; // read keyword 'class', 'object', 'interface', 'dispinterface'
  if UpAtomIs('PACKED') then ReadNextAtom;
  ReadNextAtom;
  if not AtomIsChar('(') then begin
    // no ancestor class specified
    // check class name
    ClassIdentNode:=ClassNode.Parent;
    if (ClassIdentNode=nil) or (ClassIdentNode.Desc<>ctnTypeDefinition) then
    begin
      MoveCursorToNodeStart(ClassNode);
      RaiseException('class without name');
    end;
    // if this class is not TObject, TObject is class ancestor
    SearchTObject:=not CompareSrcIdentifier(ClassIdentNode.StartPos,'TObject');
    if not SearchTObject then exit;
  end else begin
    ReadNextAtom;
    if not AtomIsIdentifier(false) then exit;
    // ancestor name found
    AncestorAtom:=CurPos;
    SearchTObject:=false;
  end;
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInAncestors] ',
' Ident=',GetIdentifier(Params.Identifier),
' search ancestor class = ',GetAtom);
{$ENDIF}
  // search ancestor class context
  CurPos.StartPos:=CurPos.EndPos;
  Params.Save(OldInput);
  try
    Params.Flags:=[fdfSearchInParentNodes,fdfIgnoreCurContextNode,
                   fdfExceptionOnNotFound]
                  +(fdfGlobals*Params.Flags);
    if not SearchTObject then
      Params.Identifier:=@Src[AncestorAtom.StartPos]
    else begin
      Params.Identifier:='TObject';
      Exclude(Params.Flags,fdfExceptionOnNotFound);
    end;
    Params.ContextNode:=ClassNode;
    if not FindIdentifierInContext(Params) then begin
      MoveCursorToNodeStart(ClassNode);
//writeln('  AQ*** ',TCodeBuffer(Scanner.MainCode).Filename,' ',CurPos.StartPos);
      RaiseException('default class ancestor TObject not found');
    end;
    AncestorNode:=Params.NewNode;
    AncestorContext:=Params.NewCodeTool.FindBaseTypeOfNode(Params,AncestorNode);
    Params.Load(OldInput);
    Exclude(Params.Flags,fdfExceptionOnNotFound);
    Params.ContextNode:=AncestorContext.Node;
    if (AncestorContext.Tool<>Self)
    and (not (fdfIgnoreClassVisibility in Params.Flags)) then
      Params.Flags:=Params.Flags-[fdfClassPrivate];
    Result:=AncestorContext.Tool.FindIdentifierInContext(Params);
  finally
    Params.Load(OldInput);
  end;
end;

{$IFDEF CTDEBUG}
procedure TFindDeclarationTool.DecPrefix;
begin
  DebugPrefix:=copy(DebugPrefix,1,length(DebugPrefix)-2);
end;

procedure TFindDeclarationTool.IncPrefix;
begin
  DebugPrefix:=DebugPrefix+'  ';
end;
{$ENDIF}

function TFindDeclarationTool.FindExpressionResultType(
  Params: TFindDeclarationParams; StartPos, EndPos: integer): TFindContext;
{
  ToDo:
    - operators
        - mixing ansistring and shortstring gives ansistring
        - Pointer +,- Pointer gives Pointer
        - Sets:
            [enum1] gives  set of enumeration type
            set *,-,+ set   gives set of same type
            set <>,=,<,> set  gives boolean
        - precedence rules table:
            1. brackets
            2. not @ sign
            3. * / div mod and shl shr as
            4. + - or xor
            5. < <> > <= >= in is
        -
    
    - operator overloading
    - internal types. e.g. string[], ansistring[], shortstring[], pchar[] to char
    - the type of a subrange is the type of the first constant/enum/number/char
    - predefined types:
        ordinal:
          int64, cardinal, QWord, boolean, bytebool, longbool, char
          
        real:
          real, single, double, extended, comp
          
    - predefined functions:
        function pred(ordinal type): ordinal constant of same type;
        function succ(ordinal type): ordinal constant of same type;
        function ord(ordinal type): ordinal type;
        val?
        function low(array): type of leftmost index type in the array;
        function high(array): type of leftmost index type in the array;
        procedure dec(ordinal var);
        procedure dec(ordinal var; ordinal type);
        procedure dec(pointer var);
        procedure dec(pointer var; ordinal type);
        procedure inc(ordinal var);
        procedure inc(ordinal var; ordinal type);
        procedure inc(pointer var);
        procedure inc(pointer var; ordinal type);
        procedure write(...);
        procedure writeln(...);
        function SizeOf(type): ordinal constant;
        typeinfo?
        uniquestring?
        procedure include(set type,enum identifier);
        procedure exclude(set type,enum identifier);
}
begin
  // This is a quick hack: Just return the type of the last variable.
  MoveCursorToCleanPos(EndPos);
  Result:=FindContextNodeAtCursor(Params);
end;

function TFindDeclarationTool.FindIdentifierInUsesSection(
  UsesNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
{ this function is internally used by FindIdentifierInContext

   search backwards through the uses section
   compare first the unit name, then load the unit and search there

}
var InAtom, UnitNameAtom: TAtomPosition;
  NewCodeTool: TFindDeclarationTool;
  OldInput: TFindDeclarationInput;
begin
  Result:=false;
  if (UsesNode=nil) or (UsesNode.Desc<>ctnUsesSection) then
    RaiseException('[TFindDeclarationTool.FindIdentifierInUsesSection] '
      +'internal error: invalid UsesNode');
  // search backwards through the uses section
  MoveCursorToCleanPos(UsesNode.EndPos);
  ReadPriorAtom; // read ';'
  if not AtomIsChar(';') then
    RaiseException('; expected, but '+GetAtom+' found');
  repeat
    ReadPriorAtom; // read unitname
    if AtomIsStringConstant then begin
      InAtom:=CurPos;
      ReadPriorAtom; // read 'in'
      if not UpAtomIs('IN') then
        RaiseException('keyword "in" expected, but '+GetAtom+' found');
      ReadPriorAtom; // read unitname
    end else
      InAtom.StartPos:=-1;
    AtomIsIdentifier(true);
    UnitNameAtom:=CurPos;
    if (fdfIgnoreUsedUnits in Params.Flags) then begin
      if CompareSrcIdentifiers(UnitNameAtom.StartPos,Params.Identifier) then
      begin
        // the searched identifier was a uses unitname, but since the unit should
        // not be opened, point to identifier in the uses section
        Result:=true;
        Params.SetResult(Self,UsesNode,UnitNameAtom.StartPos);
        exit;
      end else begin
        // identifier not found
      end;
    end else begin
      // open the unit and search the identifier in the interface
      NewCodeTool:=FindCodeToolForUsedUnit(UnitNameAtom,InAtom,false);
      if NewCodeTool=nil then begin
        MoveCursorToCleanPos(UnitNameAtom.StartPos);
        RaiseException('unit not found: '+copy(Src,UnitNameAtom.StartPos,
           UnitNameAtom.EndPos-UnitNameAtom.StartPos));
      end else if NewCodeTool=Self then begin
        MoveCursorToCleanPos(UnitNameAtom.StartPos);
        RaiseException('illegal circle using unit: '+copy(Src,
           UnitNameAtom.StartPos,UnitNameAtom.EndPos-UnitNameAtom.StartPos));
      end;
      // search the identifier in the interface of the used unit
      Params.Save(OldInput);
      Params.Flags:=[fdfIgnoreUsedUnits]+(fdfGlobals*Params.Flags)
                   -[fdfExceptionOnNotFound];
      Result:=NewCodeTool.FindIdentifierInInterface(Self,Params);
      if Result then exit;
      Params.Load(OldInput);
      // restore the cursor
      MoveCursorToCleanPos(UnitNameAtom.StartPos);
    end;
    ReadPriorAtom; // read keyword 'uses' or comma
  until not AtomIsChar(',');
end;

function TFindDeclarationTool.FindCodeToolForUsedUnit(UnitNameAtom,
  UnitInFileAtom: TAtomPosition;
  ExceptionOnNotFound: boolean): TFindDeclarationTool;
var AnUnitName, AnUnitInFilename: string;
  NewCode: TCodeBuffer;
begin
  Result:=nil;
  if (UnitNameAtom.StartPos<1) or (UnitNameAtom.EndPos<=UnitNameAtom.StartPos)
  or (UnitNameAtom.EndPos>SrcLen+1) then
    RaiseException('[TFindDeclarationTool.FindCodeToolForUsedUnit] '
      +'internal error: invalid UnitNameAtom');
  AnUnitName:=copy(Src,UnitNameAtom.StartPos,
                 UnitNameAtom.EndPos-UnitNameAtom.StartPos);
  if UnitInFileAtom.StartPos>=1 then begin
    if (UnitInFileAtom.StartPos<1)
    or (UnitInFileAtom.EndPos<=UnitInFileAtom.StartPos)
    or (UnitInFileAtom.EndPos>SrcLen+1) then
      RaiseException('[TFindDeclarationTool.FindCodeToolForUsedUnit] '
        +'internal error: invalid UnitInFileAtom');
    AnUnitInFilename:=copy(Src,UnitInFileAtom.StartPos,
                   UnitInFileAtom.EndPos-UnitInFileAtom.StartPos);
  end else
    AnUnitInFilename:='';
  NewCode:=FindUnitSource(AnUnitName,AnUnitInFilename);
  if (NewCode=nil) then begin
    // no source found
    if ExceptionOnNotFound then
      RaiseException('unit '+AnUnitName+' not found');
  end else begin
    // source found -> get codetool for it
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindCodeToolForUsedUnit] ',
' This source is=',TCodeBuffer(Scanner.MainCode).Filename,
' NewCode=',NewCode.Filename);
{$ENDIF}
    if Assigned(FOnGetCodeToolForBuffer) then
      Result:=FOnGetCodeToolForBuffer(Self,NewCode)
    else if NewCode=TCodeBuffer(Scanner.MainCode) then
      Result:=Self;
  end;
end;

function TFindDeclarationTool.FindIdentifierInInterface(
  AskingTool: TFindDeclarationTool; Params: TFindDeclarationParams): boolean;
var InterfaceNode: TCodeTreeNode;
  SrcIsUsable: boolean;
  OldInput: TFindDeclarationInput;
begin
  Result:=false;
  // build code tree
{$IFDEF CTDEBUG}
writeln(DebugPrefix,'TFindDeclarationTool.FindIdentifierInInterface',
' Ident=',GetIdentifier(Params.Identifier),
' IgnoreUsedUnits=',fdfIgnoreUsedUnits in Params.Flags,
' Self=',TCodeBuffer(Scanner.MainCode).Filename
);
{$ENDIF}

  // ToDo: build codetree for ppu, ppw, dcu files
  
  // build tree for pascal source
  BuildTree(true);
  
  // check source name
  MoveCursorToNodeStart(Tree.Root);
  ReadNextAtom; // read keyword for source type, e.g. 'unit'
  SrcIsUsable:=UpAtomIs('UNIT');
  if not SrcIsUsable then
    RaiseException('source is not unit');
  ReadNextAtom; // read source name
  if CompareSrcIdentifiers(CurPos.StartPos,Params.Identifier) then begin
    // identifier is source name
    Params.SetResult(Self,Tree.Root,CurPos.StartPos);
    Result:=true;
    exit;
  end;
  
  // search identifier in interface
  InterfaceNode:=FindInterfaceNode;
  if InterfaceNode=nil then
    RaiseException('interface section not found');
  Params.Save(OldInput);
  try
    Params.Flags:=(fdfGlobals*Params.Flags)
                  -[fdfExceptionOnNotFound,fdfSearchInParentNodes];
    Params.ContextNode:=InterfaceNode;
    Result:=FindIdentifierInContext(Params);
  finally
    Params.Load(OldInput);
  end;
end;

function TFindDeclarationTool.CompareNodeIdentifier(Node: TCodeTreeNode;
  Params: TFindDeclarationParams): boolean;
begin
  Result:=false;
  if Node=nil then exit;
  if Node.Desc in AllSourceTypes then begin
    MoveCursorToNodeStart(Node);
    ReadNextAtom;
    ReadNextAtom;
    Result:=CompareSrcIdentifiers(CurPos.StartPos,Params.Identifier);
  end else if (Node.Desc in AllIdentifierDefinitions)
  or (Node.Desc=ctnIdentifier) then begin
    Result:=CompareSrcIdentifiers(Node.StartPos,Params.Identifier);
  end;
end;

function TFindDeclarationTool.GetInterfaceNode: TCodeTreeNode;
begin
  Result:=Tree.Root;
  if Result=nil then begin
    CurPos.StartPos:=-1;
    RaiseException('[TFindDeclarationTool.GetInterfaceNode] no code tree found');
  end;
  if not (Tree.Root.Desc in AllUsableSoureTypes) then begin
    CurPos.StartPos:=-1;
    RaiseException('used unit is not an pascal unit');
  end;
  Result:=FindInterfaceNode;
  if Result=nil then begin
    CurPos.StartPos:=-1;
    RaiseException('no interface section found');
  end;
end;

function TFindDeclarationTool.FindIdentifierInUsedUnit(
  const AnUnitName: string; Params: TFindDeclarationParams): boolean;
{ this function is internally used by FindIdentifierInUsesSection
  for hidden used units, like the system unit or the objpas unit
}
var
  NewCode: TCodeBuffer;
  NewCodeTool: TFindDeclarationTool;
  OldInput: TFindDeclarationInput;
begin
  Result:=false;
  // open the unit and search the identifier in the interface
  NewCode:=FindUnitSource(AnUnitName,'');
  if (NewCode=nil) then begin
    // no source found
    CurPos.StartPos:=-1;
    RaiseException('unit '+AnUnitName+' not found');
  end else begin
    // source found -> get codetool for it
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInUsedUnit] ',
' This source is=',TCodeBuffer(Scanner.MainCode).Filename,
' NewCode=',NewCode.Filename,' IgnoreUsedUnits=',fdfIgnoreUsedUnits in Params.Flags);
{$ENDIF}
    if Assigned(FOnGetCodeToolForBuffer) then begin
      NewCodeTool:=FOnGetCodeToolForBuffer(Self,NewCode);
      if NewCodeTool=nil then begin
        CurPos.StartPos:=-1;
        RaiseException('unit '+AnUnitName+' not found');
      end;
    end else if NewCode=TCodeBuffer(Scanner.MainCode) then begin
      NewCodeTool:=Self;
      CurPos.StartPos:=-1;
      RaiseException('illegal circle using unit: '+AnUnitName);
    end;
    // search the identifier in the interface of the used unit
    Params.Save(OldInput);
    Params.Flags:=[fdfIgnoreUsedUnits]+(fdfGlobals*Params.Flags)
                 -[fdfExceptionOnNotFound];
    Result:=NewCodeTool.FindIdentifierInInterface(Self,Params);
    if Result then exit;
    Params.Load(OldInput);
  end;
end;

function TFindDeclarationTool.FindIdentifierInHiddenUsedUnits(
  Params: TFindDeclarationParams): boolean;
const
  sutSystem   = 1;
  sutObjPas   = 2;
  sutLineInfo = 3;
  sutHeapTrc  = 4;
  sutNone     = 5;
var
  OldInput: TFindDeclarationInput;
  SystemUnitName: string;
  SpecialUnitType: integer;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInHiddenUsedUnits] ',
GetIdentifier(Params.Identifier),' IgnoreUsedUnits=',fdfIgnoreUsedUnits in Params.Flags);
{$ENDIF}
  if (Tree.Root<>nil) and (not (fdfIgnoreUsedUnits in Params.Flags)) then begin
    // check, if this is a special unit
    MoveCursorToNodeStart(Tree.Root);
    ReadNextAtom;
    ReadNextAtom;
    if Scanner.InitialValues.IsDefined('LINUX') then
      SystemUnitName:='SYSLINUX'
    else
      // ToDo: other OS than linux
      SystemUnitName:='SYSTEM';
    if UpAtomIs(SystemUnitName) then
      SpecialUnitType:=sutSystem
    else if UpAtomIs('OBJPAS') then
      SpecialUnitType:=sutObjPas
    else if UpAtomIs('LINEINFO') then
      SpecialUnitType:=sutLineInfo
    else if UpAtomIs('HEAPTRC') then
      SpecialUnitType:=sutHeapTrc
    else
      SpecialUnitType:=sutNone;
    // try hidden units
    if (SpecialUnitType>sutHeapTrc)
    and Scanner.InitialValues.IsDefined(ExternalMacroStart+'UseHeapTrcUnit')
    then begin
      // try hidden used unit 'heaptrc'
      Result:=FindIdentifierInUsedUnit('HeapTrc',Params);
      if Result then exit;
    end;
    if (SpecialUnitType>sutLineInfo)
    and Scanner.InitialValues.IsDefined(ExternalMacroStart+'UseLineInfo')
    then begin
      // try hidden used unit 'lineinfo'
      Result:=FindIdentifierInUsedUnit('LineInfo',Params);
      if Result then exit;
    end;
    if (SpecialUnitType>sutObjPas)
    and (Scanner.CompilerMode in [cmDELPHI,cmOBJFPC]) then begin
      // try hidden used unit 'objpas'
      Result:=FindIdentifierInUsedUnit('ObjPas',Params);
      if Result then exit;
    end;
    // try hidden used unit 'system'
    if (SpecialUnitType>sutSystem)
    and CompareSrcIdentifiers(Params.Identifier,PChar(SystemUnitName)) then begin
      // the system unit name itself is searched -> rename searched identifier
      Params.Save(OldInput);
      Params.Identifier:=PChar(SystemUnitName);
      Result:=FindIdentifierInUsedUnit(SystemUnitName,Params);
      Params.Load(OldInput);
    end else
      Result:=FindIdentifierInUsedUnit(SystemUnitName,Params);
    if Result then exit;
  end;
end;


{ TFindDeclarationParams }

constructor TFindDeclarationParams.Create;
begin
  inherited Create;
  Clear;
end;

procedure TFindDeclarationParams.Clear;
begin
  ClearInput;
  ClearResult;
end;

procedure TFindDeclarationParams.Load(var Input: TFindDeclarationInput);
begin
  Flags:=Input.Flags;
  Identifier:=Input.Identifier;
  ContextNode:=Input.ContextNode;
end;

procedure TFindDeclarationParams.Save(var Input: TFindDeclarationInput);
begin
  Input.Flags:=Flags;
  Input.Identifier:=Identifier;
  Input.ContextNode:=ContextNode;
end;

procedure TFindDeclarationParams.ClearResult;
begin
  NewPos.Code:=nil;
  NewPos.X:=-1;
  NewPos.Y:=-1;
  NewTopLine:=-1;
  NewNode:=nil;
  NewCleanPos:=-1;
  NewCodeTool:=nil;
end;

procedure TFindDeclarationParams.SetResult(ANewCodeTool: TFindDeclarationTool;
  ANewNode: TCodeTreeNode);
begin
  ClearResult;
  NewCodeTool:=ANewCodeTool;
  NewNode:=ANewNode;
end;

procedure TFindDeclarationParams.SetResult(ANewCodeTool: TFindDeclarationTool;
  ANewNode: TCodeTreeNode; ANewCleanPos: integer);
begin
  ClearResult;
  NewCodeTool:=ANewCodeTool;
  NewNode:=ANewNode;
  NewCleanPos:=ANewCleanPos;
end;

procedure TFindDeclarationParams.ConvertResultCleanPosToCaretPos;
begin
  NewPos.Code:=nil;
  if NewCodeTool<>nil then begin
    if (NewCleanPos>=1) then
      NewCodeTool.CleanPosToCaretAndTopLine(NewCleanPos,
                 NewPos,NewTopLine)
    else if (NewNode<>nil) then
      NewCodeTool.CleanPosToCaretAndTopLine(NewNode.StartPos,
                 NewPos,NewTopLine);
  end;
end;

procedure TFindDeclarationParams.ClearInput;
begin
  Flags:=[];
  Identifier:=nil;
  ContextNode:=nil;
end;

procedure TFindDeclarationParams.SetResult(AFindContext: TFindContext);
begin
  ClearResult;
  NewCodeTool:=AFindContext.Tool;
  NewNode:=AFindContext.Node;
end;


end.


