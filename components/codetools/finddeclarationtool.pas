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
       1. Searching recursively
            - ParentNodes
            - Ancestor Classes/Objects/Interfaces
            - with statements
            - operators: '.', '()', 'A()', '^', 'inherited'
       2. Searching enums must be searched in sub nodes
            -> all classes node trees must be built
       3. Searching in used units (interface USES and implementation USES)
       4. Searching forward for pointer types e.g. ^Tralala
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
           2. PPU, PPW, DFU, ...:
}
unit FindDeclarationTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

{ $DEFINE CTDEBUG}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeTree, CodeAtom, CustomCodeTool, SourceLog,
  KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, AVL_Tree, TypInfo,
  PascalParserTool, FileProcs, DefineTemplates;

type
  // searchpath delimiter is semicolon
  TOnGetSearchPath = function(Sender: TObject): string;
  
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
    fdfClassPublished,fdfClassPublic,fdfClassProtected,fdfClassPrivate);
  TFindDeclarationFlags = set of TFindDeclarationFlag;

  TFindDeclarationInput = record
    Flags: TFindDeclarationFlags;
    IdentifierStartPos: integer;
    IdentifierEndPos: integer;
    ContextNode: TCodeTreeNode;
  end;

  TFindDeclarationParams = class(TObject)
  public
    Flags: TFindDeclarationFlags;
    IdentifierStartPos: integer;
    IdentifierEndPos: integer;
    ContextNode: TCodeTreeNode;
    NewNode: TCodeTreeNode;
    NewCleanPos: integer;
    NewCodeTool: TCustomCodeTool;
    NewPos: TCodeXYPosition;
    NewTopLine: integer;
    constructor Create;
    procedure Clear;
    procedure Save(var Input: TFindDeclarationInput);
    procedure Load(var Input: TFindDeclarationInput);
    procedure SetResult(ANewCodeTool: TCustomCodeTool; ANewNode: TCodeTreeNode);
    procedure SetResult(ANewCodeTool: TCustomCodeTool; ANewNode: TCodeTreeNode;
      ANewCleanPos: integer);
    procedure ConvertResultCleanPosToCaretPos;
    procedure ClearResult;
  end;

  TFindDeclarationTool = class(TPascalParserTool)
  private
    FOnGetUnitSourceSearchPath: TOnGetSearchPath;
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
    function FindDeclarationOfIdentifier(
      Params: TFindDeclarationParams): boolean;
    function FindContextNodeAtCursor(Params: TFindDeclarationParams): TCodeTreeNode;
    function FindIdentifierInContext(Params: TFindDeclarationParams): boolean;
    function FindEnumInContext(Params: TFindDeclarationParams): boolean;
    function FindBaseTypeOfNode(Params: TFindDeclarationParams;
      Node: TCodeTreeNode): TCodeTreeNode;
    function FindIdentifierInProcContext(ProcContextNode: TCodeTreeNode;
      Params: TFindDeclarationParams): boolean;
    function FindIdentifierInWithVarContext(WithVarNode: TCodeTreeNode;
      Params: TFindDeclarationParams): boolean;
    function FindClassOfMethod(ProcNode: TCodeTreeNode;
      Params: TFindDeclarationParams; FindClassContext: boolean): boolean;
    function FindForwardIdentifier(Params: TFindDeclarationParams;
      var IsForward: boolean): boolean;
  public
    function FindDeclaration(CursorPos: TCodeXYPosition;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindUnitSource(const AnUnitName,
      AnUnitInFilename: string): TCodeBuffer;
    property OnGetUnitSourceSearchPath: TOnGetSearchPath
      read FOnGetUnitSourceSearchPath write FOnGetUnitSourceSearchPath;
  end;


implementation


const
  fdfAllClassVisibilities = [fdfClassPublished,fdfClassPublic,fdfClassProtected,
                            fdfClassPrivate];
  fdfGlobals = [fdfExceptionOnNotFound, fdfIgnoreUsedUnits];

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
      if ClassNode.SubDesc<>ctnsForwardDeclaration then begin
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
        Params.IdentifierStartPos:=CurPos.StartPos;
        Params.IdentifierEndPos:=CurPos.EndPos;
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
    RaiseException('syntax error: expected uses, but '+GetAtom+' found');
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
          'syntax error: string constant expected, but '+GetAtom+' found');
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
      RaiseException(
        'syntax error: ; expected, but '+GetAtom+' found')
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
{$IFDEF CTDEBUG}
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
{$IFDEF CTDEBUG}
writeln('TFindDeclarationTool.FindUnitSource.SearchUnitInUnitLinks');
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
{$IFDEF CTDEBUG}
writeln('  unit "',copy(UnitLinks,UnitLinkStart,UnitLinkEnd-UnitLinkStart),'"');
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
    IdentifierStartPos, IdentifierEndPos
    ContextNode  // = DeepestNode at Cursor
    
  Result:
    true, if NewPos+NewTopLine valid

  For example:
    A^.B().C[].Identifier
}
var NewContextNode, OldContextNode: TCodeTreeNode;
begin
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindDeclarationOfIdentifier] Identifier=',
  copy(Src,Params.IdentifierStartPos,Params.IdentifierEndPos-Params.IdentifierStartPos),
  ' ContextNode=',NodeDescriptionAsString(Params.ContextNode.Desc));
{$ENDIF}
  Result:=false;
  MoveCursorToCleanPos(Params.IdentifierStartPos);
  OldContextNode:=Params.ContextNode;
  NewContextNode:=FindContextNodeAtCursor(Params);
  Params.Flags:=[fdfSearchInAncestors,fdfIgnoreCurContextNode]
                +fdfAllClassVisibilities+(fdfGlobals*Params.Flags);
  if NewContextNode=OldContextNode then
    Include(Params.Flags,fdfSearchInParentNodes);
  if (OldContextNode.Desc=ctnTypeDefinition)
  and (OldContextNode.FirstChild<>nil)
  and (OldContextNode.FirstChild.Desc=ctnClass)
  and (OldContextNode.FirstChild.SubDesc=ctnsForwardDeclaration)
  then
    Include(Params.Flags,fdfSearchForward);

  Params.ContextNode:=NewContextNode;
  Result:=FindIdentifierInContext(Params);
end;

function TFindDeclarationTool.FindIdentifierInContext(
  Params: TFindDeclarationParams): boolean;
{ searches an identifier in context node
  It does not care about code in front of the identifier like 'a.Identifer'.
  
  Params:
    IdentifierStartPos, IdentifierEndPos
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
  if ContextNode<>nil then begin
    repeat
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInContext] A Ident=',
copy(Src,Params.IdentifierStartPos,Params.IdentifierEndPos-Params.IdentifierStartPos),
' Context=',ContextNode.DescAsString,'  ',
' ParentsAllowed=',fdfSearchInParentNodes in Params.Flags,
' AncestorsAllowed=',fdfSearchInAncestors in Params.Flags
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
        ctnRecordType, ctnRecordCase, ctnRecordVariant:
          if (ContextNode.LastChild<>nil) then begin
            if not (fdfSearchForward in Params.Flags) then
              ContextNode:=ContextNode.LastChild
            else
              ContextNode:=ContextNode.FirstChild;
          end;

        ctnTypeDefinition, ctnVarDefinition, ctnConstDefinition, ctnEnumType:
          begin
            if CompareSrcIdentifiers(Params.IdentifierStartPos,
                ContextNode.StartPos) then
            begin
{$IFDEF CTDEBUG}
writeln('  Definition Identifier found=',copy(Src,ContextNode.StartPos,Params.IdentifierEndPos-Params.IdentifierStartPos));
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

        ctnProgram, ctnPackage, ctnLibrary, ctnUnit:
          begin
            MoveCursorToNodeStart(ContextNode);
            ReadNextAtom; // read keyword
            ReadNextAtom; // read name
            if CompareSrcIdentifiers(Params.IdentifierStartPos,CurPos.StartPos)
            then begin
              // identifier found
              Result:=true;
              Params.SetResult(Self,ContextNode,CurPos.StartPos);
              exit;
            end;
          end;

        ctnProperty:
          begin
            MoveCursorToNodeStart(ContextNode);
            ReadNextAtom; // read keyword 'property'
            ReadNextAtom; // read name
            if CompareSrcIdentifiers(Params.IdentifierStartPos,CurPos.StartPos)
            then begin
              // identifier found
              Result:=true;
              Params.SetResult(Self,ContextNode,CurPos.StartPos);
              exit;
            end;
          end;

        ctnUsesSection:
          begin
            // search backwards through the uses section
            // compare first the unit name then load the unit and search there

            // ToDo:

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
{$IFDEF CTDEBUG}
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
            and (NodeHasParentOfType(ContextNode,ctnClass))) then begin
              // even searching in ancestors contexts is not permitted
              // -> there is no prior context accessible any more
              // -> identifier not found
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInContext] no prior node accessible   ContextNode=',ContextNode.DescAsString);
{$ENDIF}
              exit;
            end;
          end;
        end;

        repeat
          // search for prior node
{$IFDEF CTDEBUG}
//writeln('[TFindDeclarationTool.FindIdentifierInContext] Searching prior node of ',ContextNode.DescAsString);
{$ENDIF}
          if ((not (fdfSearchForward in Params.Flags))
              and (ContextNode.PriorBrother<>nil))
          or ((fdfSearchForward in Params.Flags)
              and (ContextNode.NextBrother<>nil)) then
          begin
            if not (fdfSearchForward in Params.Flags) then
              ContextNode:=ContextNode.PriorBrother
            else
              ContextNode:=ContextNode.NextBrother;
{$IFDEF CTDEBUG}
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
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInContext] Searching in Parent  ContextNode=',ContextNode.DescAsString);
{$ENDIF}
            case ContextNode.Desc of
            
            ctnTypeSection, ctnVarSection, ctnConstSection, ctnResStrSection,
            ctnInterface, ctnImplementation,
            ctnClassPublished,ctnClassPublic,ctnClassProtected, ctnClassPrivate,
            ctnRecordCase, ctnRecordVariant:
              // these codetreenodes build a parent-child-relationship, but
              // for pascal it is only a range, hence after searching in the
              // childs of the last node, it must be searched next in the childs
              // of the prior node
              ;

            ctnClass:
              begin
              // the prior search space of a class is its ancestors + interfaces
              
              // ToDo: search in the ancestors and interfaces

              // search in the parent (no code needed) ...
              end;
              
            ctnRecordType:
              // do not search again in this node, go on ...
              ;
              
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
    MoveCursorToCleanPos(Params.IdentifierStartPos);
    RaiseException('Identifier not found '+copy(Src,Params.IdentifierStartPos,
      Params.IdentifierEndPos-Params.IdentifierStartPos));
  end;
end;

function TFindDeclarationTool.FindEnumInContext(
  Params: TFindDeclarationParams): boolean;
{ search all subnodes for ctnEnumType

  Params:
    IdentifierStartPos, IdentifierEndPos
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
      if (Params.ContextNode.Desc in [ctnEnumType])
      and CompareSrcIdentifiers(Params.IdentifierStartPos,
        Params.ContextNode.StartPos)
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
  Params: TFindDeclarationParams): TCodeTreeNode;
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
               atEdgedBracketOpen, atEdgedBracketClose);
const
  AtomTypeNames: array[TAtomType] of string =
    ('<None>','Space','Ident','Point','AS','INHERITED','Up^',
     'Bracket(','Bracket)','Bracket[','Bracket]');

  function GetCurrentAtomType: TAtomType;
  begin
    if (CurPos.StartPos=CurPos.EndPos) then
      Result:=atSpace
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
    end else if UpAtomIs('INHERITED') then
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
  if CurAtomType=atNone then begin
    // no special context found -> the context node is the deepest node at
    // cursor, and this should already be in Params.ContextNode
    Result:=Params.ContextNode;
    exit;
  end;
  Result:=FindContextNodeAtCursor(Params);
  if Result=nil then exit;
  
  // coming back the left side has been parsed and
  // now the parsing goes from left to right
  
{$IFDEF CTDEBUG}
write('[TFindDeclarationTool.FindContextNodeAtCursor] B ',
  ' Context=',Params.ContextNode.DescAsString,
  ' CurAtom=',AtomTypeNames[CurAtomType],
  ' "',copy(Src,CurAtom.StartPos,CurAtom.EndPos-CurAtom.StartPos),'"',
  ' NextAtom=',AtomTypeNames[NextAtomType],
  ' Result=');
if Result<>nil then write(Result.DescAsString) else write('NIL');
writeln('');
{$ENDIF}

  case CurAtomType of

  atIdentifier:
    begin
      // for example  'AnObject[3]'
      if not (NextAtomType in [atSpace,atPoint,atUp,atAS,atRoundBracketOpen,
        atEdgedBracketOpen]) then
      begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        RaiseException('syntax error: "'+GetAtom+'" found');
      end;
      if (Result=Params.ContextNode)
      and (CompareSrcIdentifier(CurAtom.StartPos,'SELF')) then begin
        // SELF in a method is the object itself
        // -> check if in a proc
        ProcNode:=Params.ContextNode;
        while (ProcNode<>nil) do begin
          if (ProcNode.Desc=ctnProcedure) then begin
            // in a proc -> find the class context
            if FindClassOfMethod(ProcNode,Params,true) then begin
              Result:=Params.NewNode;
              exit;
            end;
          end;
          ProcNode:=ProcNode.Parent;
        end;
      end;
      // find identifier
      Params.Save(OldInput);
      try
        Params.Flags:=[fdfSearchInAncestors,fdfIgnoreCurContextNode]
                      +fdfAllClassVisibilities
                      +(fdfGlobals*Params.Flags)
                      +[fdfExceptionOnNotFound];
//writeln('  ',Result=Params.ContextNode,' ',Result.DescAsString,',',Params.ContextNode.DescAsString);
        if Result=Params.ContextNode then begin
          // there is no special context -> also search in parent contexts
          Include(Params.Flags,fdfSearchInParentNodes);
        end else
          Params.ContextNode:=Result;
        Params.IdentifierStartPos:=CurAtom.StartPos;
        Params.IdentifierEndPos:=CurAtom.EndPos;
        FindIdentifierInContext(Params);
        Result:=Params.NewNode;
      finally
        Params.Load(OldInput);
      end;
    end;
    
  atPoint:
    begin
      // for example 'A.B'
      if (not (NextAtomType in [atSpace,atIdentifier])) then begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        RaiseException('syntax error: identifier expected, but '
                        +GetAtom+' found');
      end;
      // there is nothing special to do here, because the '.' will only change
      // from an identifier to its type context. But this is always done.
    end;

  atAS:
    begin
      // for example 'A as B'
      if (not (NextAtomType in [atSpace,atIdentifier])) then begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        RaiseException('syntax error: identifier expected, but '
                        +GetAtom+' found');
      end;
      // 'as' is a type cast, so the left side is irrelevant
      // -> context is default context
      Result:=Params.ContextNode;
    end;

  atUP:
    begin
      // for example:
      //   1. 'PInt = ^integer'  pointer type
      //   2. a^  dereferencing
      if Result<>Params.ContextNode then begin
        // left side of expression has defined a special context
        // => this '^' is a dereference
        if (not (NextAtomType in [atSpace,atPoint,atAS,atUP])) then begin
          MoveCursorToCleanPos(NextAtom.StartPos);
          RaiseException('syntax error: . expected, but '+GetAtom+' found');
        end;
        if Result.Desc<>ctnPointerType then begin
          MoveCursorToCleanPos(CurAtom.StartPos);
          RaiseException('illegal qualifier ^');
        end;
        Result:=Result.FirstChild;
      end else if NodeHasParentOfType(Result,ctnPointerType) then begin
        // this is a pointer type definition
        // -> the default context is ok
      end;
    end;


  // ToDo: atINHERITED, atRoundBracketClose, atEdgedBracketClose

  else
    begin
      if (not (NextAtomType in [atSpace,atIdentifier,atRoundBracketOpen,
        atEdgedBracketOpen])) then
      begin
        MoveCursorToCleanPos(NextAtom.StartPos);
        RaiseException('syntax error: identifier expected, but '
                        +GetAtom+' found');
      end;
      Result:=Params.ContextNode;
    end;
  end;
  
  // try to get the base type of the found context
  Result:=FindBaseTypeOfNode(Params,Result);

{$IFDEF CTDEBUG}
write('[TFindDeclarationTool.FindContextNodeAtCursor] END ',
  Params.ContextNode.DescAsString,' CurAtom=',AtomTypeNames[CurAtomType],
  ' NextAtom=',AtomTypeNames[NextAtomType],' Result=');
if Result<>nil then write(Result.DescAsString) else write('NIL');
writeln('');
{$ENDIF}
end;

function TFindDeclarationTool.FindBaseTypeOfNode(Params: TFindDeclarationParams;
  Node: TCodeTreeNode): TCodeTreeNode;
var OldInput: TFindDeclarationInput;
  ClassIdentNode: TCodeTreeNode;
begin
  Result:=Node;
  while (Result<>nil) do begin
    if (Result.Desc in AllIdentifierDefinitions) then begin
      // instead of variable/const/type definition, return the type
      Result:=FindTypeNodeOfDefinition(Result);
    end else
    if (Result.Desc=ctnClass) and (Result.SubDesc=ctnsForwardDeclaration) then
    begin
      // search the real class
      ClassIdentNode:=Result.Parent;
      if (ClassIdentNode=nil) or (not (ClassIdentNode.Desc=ctnTypeDefinition))
      then begin
        MoveCursorToCleanPos(Result.StartPos);
        RaiseException('[TFindDeclarationTool.FindBaseTypeOfNode] '
                      +'forward class node without name');
      end;
      Params.Save(OldInput);
      try
        Params.IdentifierStartPos:=ClassIdentNode.StartPos;
        Params.IdentifierEndPos:=ClassIdentNode.EndPos;
        Params.Flags:=[fdfSearchInParentNodes,fdfSearchForward]
                      +(fdfGlobals*Params.Flags)
                      +[fdfExceptionOnNotFound];
        Params.ContextNode:=ClassIdentNode;
        FindIdentifierInContext(Params);
        if Params.NewNode.Desc<>ctnTypeDefinition then begin
          MoveCursorToCleanPos(Result.StartPos);
          RaiseException('Forward class definition not resolved: '
              +copy(Src,ClassIdentNode.StartPos,
                  ClassIdentNode.EndPos-ClassIdentNode.StartPos));
        end;
      finally
        Params.Load(OldInput);
      end;
    end else
    if (Result.Desc=ctnTypeType) then begin
      // a TypeType is for example 'MyInt = type integer;'
      // the context is not the 'type' keyword, but the identifier after it.
      Result:=Result.FirstChild;
    end else
    if (Result.Desc=ctnIdentifier) then begin
      // this type is just an alias for another type
      // -> search the basic type
      if Result.Parent=nil then
        break;
      Params.Save(OldInput);
      try
        Params.IdentifierStartPos:=Result.StartPos;
        Params.IdentifierEndPos:=Result.EndPos;
        Params.Flags:=[fdfSearchInParentNodes]+(fdfGlobals*Params.Flags)
                      -[fdfExceptionOnNotFound];
        Params.ContextNode:=Result.Parent;
        if FindIdentifierInContext(Params) then begin
          if Result.HasAsParent(Params.NewNode) then
            break
          else
            Result:=Params.NewNode;
        end else
          break;
      finally
        Params.Load(OldInput);
      end;
    end else
      break;
  end;
{$IFDEF CTDEBUG}
write('[TFindDeclarationTool.FindBaseTypeOfNode] END Node=');
if Node<>nil then write(Node.DescAsString) else write('NIL');
write(' Result=');
if Result<>nil then write(Result.DescAsString) else write('NIL');
writeln('');
{$ENDIF}
end;

function TFindDeclarationTool.FindIdentifierInProcContext(
  ProcContextNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
{ this function is internally used by FindIdentifierInContext
}
var
  ClassNameAtom: TAtomPosition;
  OldInput: TFindDeclarationInput;
  ClassContextNode: TCodeTreeNode;
begin
  Result:=false;
  MoveCursorToNodeStart(ProcContextNode);
  ReadNextAtom; // read keyword
  ReadNextAtom; // read classname
  ClassNameAtom:=CurPos;
  ReadNextAtom;
  if AtomIsChar('.') then begin
    // proc is a method
    if CompareSrcIdentifiers(ClassNameAtom.StartPos,
      Params.IdentifierStartPos) then
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
                      +[fdfExceptionOnNotFound];
        Params.ContextNode:=ProcContextNode;
        Params.IdentifierStartPos:=ClassNameAtom.StartPos;
        Params.IdentifierEndPos:=ClassNameAtom.EndPos;
{$IFDEF CTDEBUG}
writeln('  searching class of method   class="',copy(Src,ClassNameAtom.StartPos,ClassNameAtom.EndPos-ClassNameAtom.StartPos),'"');
{$ENDIF}
        FindIdentifierInContext(Params);
        ClassContextNode:=FindBaseTypeOfNode(Params,Params.NewNode);
        if (ClassContextNode=nil)
        or (ClassContextNode.Desc<>ctnClass) then begin
          MoveCursorToCleanPos(ClassNameAtom.StartPos);
          RaiseException('class identifier expected');
        end;
        // class of method found
        BuildSubTreeForClass(ClassContextNode);
        // class context found -> search identifier
        Params.Load(OldInput);
        Params.Flags:=[fdfSearchInAncestors]+fdfAllClassVisibilities
                      +(fdfGlobals*Params.Flags)
                      -[fdfExceptionOnNotFound];
        Params.ContextNode:=ClassContextNode;
{$IFDEF CTDEBUG}
writeln('  searching identifier in class of method');
{$ENDIF}
        Result:=FindIdentifierInContext(Params);
        if Result then exit;
      finally
        Params.Load(OldInput);
      end;
    end;
  end else begin
    // proc is not a method
    if CompareSrcIdentifiers(ClassNameAtom.StartPos,
      Params.IdentifierStartPos) then
    begin
      // proc identifier found
      Result:=true;
      Params.SetResult(Self,ProcContextNode);
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
begin
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindClassOfMethod] A');
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
      Params.Flags:=[fdfIgnoreCurContextNode,fdfSearchInParentNodes]
                    +(fdfGlobals*Params.Flags)
                    -[fdfExceptionOnNotFound];
      Params.ContextNode:=ProcNode;
      Params.IdentifierStartPos:=ClassNameAtom.StartPos;
      Params.IdentifierEndPos:=ClassNameAtom.EndPos;
{$IFDEF CTDEBUG}
writeln('  searching class of method   class="',copy(Src,ClassNameAtom.StartPos,ClassNameAtom.EndPos-ClassNameAtom.StartPos),'"');
{$ENDIF}
      FindIdentifierInContext(Params);
      if FindClassContext then begin
        // parse class and return class node
        Params.NewNode:=FindBaseTypeOfNode(Params,Params.NewNode);
        if (Params.NewNode=nil)
        or (Params.NewNode.Desc<>ctnClass) then begin
          MoveCursorToCleanPos(ClassNameAtom.StartPos);
          RaiseException('class identifier expected');
        end;
        // class of method found
        // parse class and return class node
        BuildSubTreeForClass(Params.NewNode);
      end;
      Result:=true;
    finally
      Params.Load(OldInput);
    end;
  end else begin
    // proc is not a method
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
  try
    Exclude(Params.Flags,fdfExceptionOnNotFound);
    Result:=FindIdentifierInContext(Params);
    if not Result then begin
      Params.Load(OldInput);
      Include(Params.Flags,fdfSearchForward);
      Result:=FindIdentifierInContext(Params);
      IsForward:=true;
    end else
      IsForward:=false;
  finally
    Params.Load(OldInput);
  end;
end;

function TFindDeclarationTool.FindIdentifierInWithVarContext(
  WithVarNode: TCodeTreeNode; Params: TFindDeclarationParams): boolean;
{ this function is internally used by FindIdentifierInContext
}
var
  WithVarContextNode: TCodeTreeNode;
  OldInput: TFindDeclarationInput;
begin
{$IFDEF CTDEBUG}
writeln('[TFindDeclarationTool.FindIdentifierInWithVarContext] ',
copy(Src,Params.IdentifierStartPos,Params.IdentifierEndPos-Params.IdentifierStartPos)
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
  try
    Params.ContextNode:=WithVarNode;
    Include(Params.Flags,fdfExceptionOnNotFound);
    WithVarContextNode:=FindContextNodeAtCursor(Params);
    if (WithVarContextNode=nil) or (WithVarContextNode=OldInput.ContextNode)
    or (not (WithVarContextNode.Desc in [ctnClass,ctnRecordType])) then begin
      MoveCursorToCleanPos(WithVarNode.StartPos);
      RaiseException('expression type must be class or record type');
    end;
    // search identifier in with context
    Params.Load(OldInput);
    Exclude(Params.Flags,fdfExceptionOnNotFound);
    Params.ContextNode:=WithVarContextNode;
    if FindIdentifierInContext(Params) then begin
      // identifier found in with context
      Result:=true;
    end;
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


{ TFindDeclarationParams }

constructor TFindDeclarationParams.Create;
begin
  inherited Create;
  Clear;
end;

procedure TFindDeclarationParams.Clear;
begin
  Flags:=[];
  IdentifierStartPos:=-1;
  IdentifierEndPos:=-1;
  ContextNode:=nil;
  ClearResult;
end;

procedure TFindDeclarationParams.Load(var Input: TFindDeclarationInput);
begin
  Flags:=Input.Flags;
  IdentifierStartPos:=Input.IdentifierStartPos;
  IdentifierEndPos:=Input.IdentifierEndPos;
  ContextNode:=Input.ContextNode;
end;

procedure TFindDeclarationParams.Save(var Input: TFindDeclarationInput);
begin
  Input.Flags:=Flags;
  Input.IdentifierStartPos:=IdentifierStartPos;
  Input.IdentifierEndPos:=IdentifierEndPos;
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

procedure TFindDeclarationParams.SetResult(ANewCodeTool: TCustomCodeTool;
  ANewNode: TCodeTreeNode);
begin
  ClearResult;
  NewCodeTool:=ANewCodeTool;
  NewNode:=ANewNode;
end;

procedure TFindDeclarationParams.SetResult(ANewCodeTool: TCustomCodeTool;
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

end.


