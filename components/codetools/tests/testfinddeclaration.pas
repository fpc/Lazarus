{
 Test with:
   ./testcodetools --suite=TTestFindDeclaration
   ./testcodetools --suite=TestFindDeclaration_Basic
   ./testcodetools --suite=TestFindDeclaration_ClassOf
   ./testcodetools --suite=TestFindDeclaration_With
   ./testcodetools --suite=TestFindDeclaration_NestedClasses
   ./testcodetools --suite=TestFindDeclaration_ClassHelper
   ./testcodetools --suite=TestFindDeclaration_TypeHelper
   ./testcodetools --suite=TestFindDeclaration_ObjCClass
   ./testcodetools --suite=TestFindDeclaration_ObjCCategory
   ./testcodetools --suite=TestFindDeclaration_Generics
   ./testcodetools --suite=TestFindDeclaration_FileAtCursor

 FPC tests:
   ./testcodetools --suite=TestFindDeclaration_FPCTests
   ./testcodetools --suite=TestFindDeclaration_FPCTests --filemask=t*.pp
   ./testcodetools --suite=TestFindDeclaration_FPCTests --filemask=tchlp41.pp
 Laz tests:
   ./testcodetools --suite=TestFindDeclaration_LazTests
   ./testcodetools --suite=TestFindDeclaration_LazTests --filemask=t*.pp
   ./testcodetools --suite=TestFindDeclaration_LazTests --filemask=tdefaultproperty1.pp
}

(* Expectation in test-files

  {%identcomplincludekeywords:on}
    Enables CodeToolBoss.IdentComplIncludeKeywords

  {SELECT:TESTS=TEST(|TEST)*}
    Each "{" comment starting with a..z is a test instruction, or a list of test instructions separated by |

  SELECT can be one of the following tests:

    {completion:TESTS}
      TEST=([+-]POS=)?ENTRY(;ENTRY)*
      Tests: CodeToolBoss.GatherIdentifiers

      Each TEST can start with an optional POS (integer positive/negative)
      The POS specifies the relative source-pos from the start of the identifier before the comment.
      ("start of the identifier": identifier does NOT include any "&" for this pos)

      Each ENTRY can start with a ! to test for a non-present completion

    {declaration:TEST
     declaration!:TEST
       TEST=[unitname/]nested.declaration.path[:linenumber]
      Tests: CodeToolBoss.FindDeclaration
      Also runs {completion:*} unless followed by !

    {guesstype:
      Tests: CodeToolBoss.GuessTypeOfIdentifier

    {findrefs:XYLIST
      XYLIST=x,y;x,y;...
      Tests: CodeToolBoss.FindReferences

      An exact list of all the location that codetool will find.
      if XYLIST starts with "C," then comments are searched too.

*)
unit TestFindDeclaration;

{$i runtestscodetools.inc}

{off $define VerboseFindDeclarationTests}

interface

uses
  Classes, SysUtils, contnrs, fpcunit, testregistry, StrUtils,
  FileProcs, LazFileUtils, LazLogger,
  CodeToolManager, ExprEval, CodeCache, BasicCodeTools,
  CustomCodeTool, CodeTree, FindDeclarationTool, KeywordFuncLists,
  IdentCompletionTool, DefineTemplates, DirectoryCacher, CTUnitGraph,
  TestGlobals, TestPascalParser;

const
  MarkDecl = '#'; // a declaration, must be unique
  MarkRef = '@'; // a reference to a declaration

type
  TFDMarker = class
  public
    Name: string;
    Kind: char;
    NameStartPos, NameEndPos: integer; // identifier in front of comment
    CleanPos: integer; // comment end
  end;

  { TCustomTestFindDeclaration }

  TCustomTestFindDeclaration = class(TCustomTestPascalParser)
  private
    FMainCode: TCodeBuffer;
    FMarkers: TObjectList;// list of TFDMarker
    FMainTool: TCodeTool;
    function GetMarkers(Index: integer): TFDMarker;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function MarkerCount: integer;
    property Markers[Index: integer]: TFDMarker read GetMarkers;
    function AddMarker(const aName: string; Kind: char; CleanPos: integer;
      NameStartPos, NameEndPos: integer): TFDMarker;
    function IndexOfMarker(const aName: string; Kind: char): integer;
    procedure ParseSimpleMarkers(aCode: TCodeBuffer);
    function FindMarker(const aName: string; Kind: char): TFDMarker;
    procedure CheckReferenceMarkers;
    procedure FindDeclarations(Filename: string; ExpandFile: boolean = true);
    procedure FindDeclarations(aCode: TCodeBuffer);
    procedure TestFiles(Directory: string; ADefaultFileMask: String = '');
    property MainCode: TCodeBuffer read FMainCode;
    property MainTool: TCodeTool read FMainTool;
  end;

  { TTestFindDeclaration }

  TTestFindDeclaration = class(TCustomTestFindDeclaration)
  published
    procedure TestFindDeclaration_Program;
    procedure TestFindDeclaration_Basic;
    procedure TestFindDeclaration_UnitIntfBeforeImplUses;
    // todo: test impl uses unit name hides unit intf identifier
    procedure TestFindDeclaration_Proc_BaseTypes;
    procedure TestFindDeclaration_ProcNested;
    procedure TestFindDeclaration_ResultType;
    procedure TestFindDeclaration_ResultField;
    procedure TestFindDeclaration_With;
    procedure TestFindDeclaration_WithResult; // todo
    procedure TestFindDeclaration_ClassOf;
    procedure TestFindDeclaration_NestedClasses;
    procedure TestFindDeclaration_NestedAliasClass;
    procedure TestFindDeclaration_ClassHelper;
    procedure TestFindDeclaration_TypeHelper;

    procedure TestFindDeclaration_Proc_ArgSet;
    procedure TestFindDeclaration_ForIn;
    procedure TestFindDeclaration_FileAtCursor;
    procedure TestFindDeclaration_Arrays;
    procedure TestFindDeclaration_ArrayMultiDimDot;
    procedure TestFindDeclaration_GuessType;
    procedure TestFindDeclaration_GuessType_Set;
    procedure TestFindDeclaration_Attributes;
    procedure TestFindDeclaration_BracketOpen;
    procedure TestFindDeclaration_AnonymProc;
    procedure TestFindDeclaration_AnonymProc_ExprDot;
    procedure TestFindDeclaration_VarArgsOfType;
    procedure TestFindDeclaration_ProcRef;
    procedure TestFindDeclaration_PointerForwardVsUses;
    procedure TestFindDeclaration_AutoDeref;
    procedure TestFindDeclaration_Variant;

    // darwin objc
    procedure TestFindDeclaration_ObjCClass;
    procedure TestFindDeclaration_ObjCCategory;
    procedure TestFindDeclaration_CBlocks;

    // generics
    procedure TestFindDeclaration_GenericFunction;
    procedure TestFindDeclaration_Generics_Enumerator;
    procedure TestFindDeclaration_Generics;
    procedure TestFindDeclaration_Generics_GuessType;
    procedure TestFindDeclaration_Generics_GuessType2;
    procedure TestFindDeclaration_Generics_FindDeclaration;
    procedure TestFindDeclaration_GenericsObjFpc_ClassAncestor;
    procedure TestFindDeclaration_GenericsDelphi_InterfaceAncestor;
    procedure TestFindDeclaration_GenericsDelphi_FuncParam;
    procedure TestFindDeclaration_GenericsDelphi_PublicProcType;
    procedure TestFindDeclaration_GenericsDelphi_MultiGenParams;

    // ampersands
    procedure TestFindDeclaration_Ampersand;
    procedure TestFindDeclaration_Ampersand_UnitName;
    procedure TestFindDeclaration_AmpersandArray;

    // unit/include search
    procedure TestFindDeclaration_UnitSearch_CurrentDir;
    procedure TestFindDeclaration_UnitSearch_StarStar;
    procedure TestFindDeclaration_IncludeSearch_DirectiveWithPath;
    procedure TestFindDeclaration_IncludeSearch_StarStar;
    procedure TestFindDeclaration_FindFPCSrcNameSpacedUnits;

    // unit namespaces and dotted unitnames
    procedure TestFindDeclaration_NS_Program;
    procedure TestFindDeclaration_NS_ProgLocalVsUses;
    procedure TestFindDeclaration_NS_UnitIntfVsUses;
    procedure TestFindDeclaration_NS_UnitImplVsIntfUses;
    procedure TestFindDeclaration_NS_UnitImplVsImplUses;
    procedure TestDirectoyCache_NS_FN_DottedUses;
    procedure TestFindDeclaration_NS_FN_DottedUses;
    procedure TestFindDeclaration_NS_MultiDottedUses;
    procedure TestFindDeclaration_NS_MultiDottedPrg;
    procedure TestGatherIdentifier_NS_MultiDottedUses;

    // directives
    procedure TestFindDeclaration_Directive_OperatorIn;

    // test all files in directories:
    procedure TestFindDeclaration_FPCTests;
    procedure TestFindDeclaration_LazTests;
    procedure TestFindDeclaration_LazTestsBugs;
  end;

implementation

{ TCustomTestFindDeclaration }

procedure TCustomTestFindDeclaration.CheckReferenceMarkers;
var
  i, FoundTopLine, FoundCleanPos, BlockTopLine, BlockBottomLine: Integer;
  Marker, DeclMarker: TFDMarker;
  CursorPos, FoundCursorPos: TCodeXYPosition;
  FoundTool: TFindDeclarationTool;
begin
  for i:=0 to MarkerCount-1 do begin
    Marker:=Markers[i];
    if Marker.Kind=MarkRef then begin
      DeclMarker:=FindMarker(Marker.Name,MarkDecl);
      if DeclMarker=nil then
        Fail('ref has no decl marker. ref "'+Marker.Name+'" at '+MainTool.CleanPosToStr(Marker.CleanPos));
      MainTool.CleanPosToCaret(Marker.NameStartPos,CursorPos);

      // test FindDeclaration
      if not CodeToolBoss.FindDeclaration(CursorPos.Code,CursorPos.X,CursorPos.Y,
        FoundCursorPos.Code,FoundCursorPos.X,FoundCursorPos.Y,FoundTopLine,
        BlockTopLine,BlockBottomLine)
      then begin
        WriteSource(CursorPos);
        Fail('find declaration failed at '+MainTool.CleanPosToStr(Marker.NameStartPos,true)+': '+CodeToolBoss.ErrorMessage);
      end else begin
        FoundTool:=CodeToolBoss.GetCodeToolForSource(FoundCursorPos.Code,true,true) as TFindDeclarationTool;
        if FoundTool<>MainTool then begin
          WriteSource(CursorPos);
          Fail('find declaration at '+MainTool.CleanPosToStr(Marker.NameStartPos,true)
            +' returned wrong tool "'+FoundTool.MainFilename+'" instead of "'+MainTool.MainFilename+'"');
        end;
        MainTool.CaretToCleanPos(FoundCursorPos,FoundCleanPos);
        if (FoundCleanPos<DeclMarker.NameStartPos)
        or (FoundCleanPos>DeclMarker.NameEndPos) then begin
          WriteSource(CursorPos);
          Fail('find declaration at '+MainTool.CleanPosToStr(Marker.NameStartPos,true)
            +' returned wrong position "'+MainTool.CleanPosToStr(FoundCleanPos)+'"'
            +' instead of "'+MainTool.CleanPosToStr(Marker.NameStartPos)+'"');
        end;
      end;
    end;
  end;
end;

procedure TCustomTestFindDeclaration.FindDeclarations(Filename: string;
  ExpandFile: boolean);
var
  aCode: TCodeBuffer;
begin
  if ExpandFile then
    Filename:=TrimAndExpandFilename(Filename);
  {$IFDEF VerboseFindDeclarationTests}
  debugln(['TTestFindDeclaration.FindDeclarations File=',Filename]);
  {$ENDIF}
  aCode:=CodeToolBoss.LoadFile(Filename,true,false);
  if aCode=nil then
    raise Exception.Create('unable to load '+Filename);
  FindDeclarations(aCode);
end;

type
TFindDeclarationToolHelper = class helper for TFindDeclarationTool
    procedure ClearNodeCaches;
  end;
procedure TFindDeclarationToolHelper.ClearNodeCaches;
begin
  inherited ClearNodeCaches;
end;

procedure TCustomTestFindDeclaration.FindDeclarations(aCode: TCodeBuffer);

  procedure PrependPath(Prefix: string; var Path: string);
  begin
    if Path<>'' then Path:='.'+Path;
    Path:=Prefix+Path;
  end;

  function NodeAsPath(Tool: TFindDeclarationTool; Node: TCodeTreeNode): string;
  var
    aName: String;
  begin
    Result:='';
    while Node<>nil do begin
      case Node.Desc of
      ctnTypeDefinition,ctnVarDefinition,ctnConstDefinition,ctnGenericParameter:
        PrependPath(GetIdentifier(@Tool.Src[Node.StartPos]),Result);
      ctnGenericType:
        PrependPath(GetIdentifier(@Tool.Src[Node.FirstChild.StartPos]),Result);
      ctnInterface,ctnUnit,ctnSrcName:
        PrependPath(Tool.GetSourceName(false),Result);
      ctnProcedure:
        begin
        aName:=Tool.ExtractProcName(Node,[]);
        if aName='' then
          aName:='$ano';
        PrependPath(aName,Result);
        end;
      ctnProperty:
        PrependPath(Tool.ExtractPropName(Node,false),Result);
      ctnUseUnit:
        PrependPath(Tool.ExtractUsedUnitName(Node),Result);
      ctnUseUnitNamespace,ctnUseUnitClearName:
        begin
          PrependPath(GetIdentifier(@Tool.Src[Node.StartPos]),Result);
          if Node.PriorBrother<>nil then begin
            Node:=Node.PriorBrother;
            continue;
          end else begin
            PrependPath(Tool.GetSourceName(false),Result); // prepend src name to distinguish uses from unit
            break;
          end;
        end;
      //else debugln(['NodeAsPath ',Node.DescAsString]);
      end;
      Node:=Node.Parent;
    end;
    //debugln(['NodeAsPath ',Result]);
  end;

var
  CommentP: Integer;
  p: Integer;
  Src, ExpectedPath, FoundPath: String;
  PathPos: Integer;
  CursorPos, FoundCursorPos: TCodeXYPosition;
  FoundTopLine: integer;
  FoundTool: TFindDeclarationTool;
  FoundCleanPos: Integer;
  FoundNode: TCodeTreeNode;
  NameStartPos, i, j, l, IdentifierStartPos, IdentifierEndPos,
    BlockTopLine, BlockBottomLine, CommentEnd, StartOffs, TestLoop: Integer;
  Marker, ExpectedType, NewType, ExpectedCompletion, ExpectedTerm,
    ExpectedCompletionPart, ExpectedTermPart, s: String;
  IdentItem: TIdentifierListItem;
  ItsAKeyword, IsSubIdentifier, ExpInvert, ExpComment: boolean;
  ExistingDefinition: TFindContext;
  ListOfPFindContext: TFPList;
  NewExprType: TExpressionType;
  ListOfPCodeXYPosition: TFPList;
  Cache: TFindIdentifierReferenceCache;
begin
  FMainCode:=aCode;
  DoParseModule(MainCode,FMainTool);
  Src:=MainTool.Src;

  CodeToolBoss.IdentComplIncludeKeywords := False;
  if pos('{%identcomplincludekeywords:on}', LowerCase(Src)) > 0 then
    CodeToolBoss.IdentComplIncludeKeywords := True;

  for TestLoop := 0 to 2 do begin
    // Pass 1: Eval with cache from previous run
    CommentP:=1;
    while CommentP<length(Src) do begin
      if TestLoop = 2 then begin  // Pass 2: Eval each test, with an empty cache
        CodeToolBoss.CurCodeTool.ClearNodeCaches;
        MainTool.ClearNodeCaches;
      end;

      CommentP:=FindNextComment(Src,CommentP);
      if CommentP>length(Src) then break;
      p:=CommentP;
      CommentP:=FindCommentEnd(Src,CommentP,MainTool.Scanner.NestedComments);
      if Src[p]<>'{' then continue;
      if Src[p+1] in ['$','%',' ',#0..#31] then continue;

      // allow spaces before the comment
      IdentifierEndPos:=p;
      while (IdentifierEndPos>1) and (IsSpaceChar[Src[IdentifierEndPos-1]]) do
        dec(IdentifierEndPos);
      IdentifierStartPos:=IdentifierEndPos;
      if (IdentifierStartPos>1) and (Src[IdentifierStartPos-1]='.') then begin
        // .{...} for completion
      end else begin
        // check identifier in front of comment
        while (IdentifierStartPos>1) and (IsIdentChar[Src[IdentifierStartPos-1]]) do
          dec(IdentifierStartPos);
        if IdentifierStartPos=p then begin
          WriteSource(p,MainTool);
          Fail('missing identifier in front of marker at '+MainTool.CleanPosToStr(p));
        end;
      end;
      inc(p);
      NameStartPos:=p;
      if Src[p] in ['#','@'] then begin
        {#name}  {@name}
        inc(p);
        if not IsIdentStartChar[Src[p]] then begin
          WriteSource(p,MainTool);
          Fail('Expected identifier at '+MainTool.CleanPosToStr(p,true));
        end;
        NameStartPos:=p;
        while IsIdentChar[Src[p]] do inc(p);
        Marker:=copy(Src,NameStartPos,p-NameStartPos);
        if TestLoop=0 then
          AddMarker(Marker,Src[NameStartPos],CommentP,IdentifierStartPos,IdentifierEndPos);
        continue;
      end;

      CommentEnd := CommentP;
      CommentP := p;
      repeat
        NameStartPos:=CommentP;
        p := NameStartPos;
        CommentP := PosEx('|', Src, NameStartPos) + 1;
        if (CommentP <= 1) or (CommentP > CommentEnd) then
          CommentP := CommentEnd;

        // check for specials:
        {declaration:path}
        {guesstype:type}
        if not IsIdentStartChar[Src[p]] then continue;
        while (p<=length(Src)) and (IsIdentChar[Src[p]]) do inc(p);
        if (p<=length(Src)) and (Src[p] = '!') then inc(p);
        Marker:=copy(Src,NameStartPos,p-NameStartPos);
        if (p>length(Src)) or (Src[p]<>':') then begin
          WriteSource(p,MainTool);
          AssertEquals('Expected : at '+MainTool.CleanPosToStr(p,true),'declaration',Marker);
          continue;
        end;
        inc(p);
        PathPos:=p;

        //debugln(['TTestFindDeclaration.FindDeclarations Marker="',Marker,'" params: ',dbgstr(MainTool.Src,p,CommentP-p)]);
        if (Marker='declaration') or (Marker='declaration!') or (Marker='completion') then begin
          ExpectedPath:=copy(Src,PathPos,CommentP-1-PathPos);
          {$IFDEF VerboseFindDeclarationTests}
          debugln(['TTestFindDeclaration.FindDeclarations searching "',Marker,'" at ',MainTool.CleanPosToStr(NameStartPos-1),' ExpectedPath=',ExpectedPath]);
          {$ENDIF}

          if (Marker='declaration') or (Marker='declaration!') then begin
            MainTool.CleanPosToCaret(IdentifierStartPos,CursorPos);

            // test FindDeclaration
            if not CodeToolBoss.FindDeclaration(CursorPos.Code,CursorPos.X,CursorPos.Y,
              FoundCursorPos.Code,FoundCursorPos.X,FoundCursorPos.Y,FoundTopLine,
              BlockTopLine,BlockBottomLine)
            then begin
              if ExpectedPath<>'' then begin
                //if (CodeToolBoss.ErrorCode<>nil) then begin
                  //ErrorTool:=CodeToolBoss.GetCodeToolForSource(CodeToolBoss.ErrorCode);
                  //if ErrorTool<>MainTool then
                   // WriteSource(,ErrorTool);
                WriteSource(IdentifierStartPos,MainTool);
                Fail('find declaration (Loop: '+IntToStr(TestLoop)+') failed at '+MainTool.CleanPosToStr(IdentifierStartPos,true)+': '+CodeToolBoss.ErrorMessage);
              end;
              continue;
            end else begin
              FoundTool:=CodeToolBoss.GetCodeToolForSource(FoundCursorPos.Code,true,true) as TFindDeclarationTool;
              FoundPath:='';
              FoundNode:=nil;
              i := pos('/', ExpectedPath);
              if i > 1 then begin
                FoundPath:=ExtractFileNameOnly(FoundCursorPos.Code.Filename);
                AssertEquals('find declaration (Loop: '+IntToStr(TestLoop)+') FILENAME wrong at '+MainTool.CleanPosToStr(IdentifierStartPos,true),copy(LowerCase(ExpectedPath), 1 , i-1),LowerCase(FoundPath));
                delete(ExpectedPath, 1, i);
              end;
              i := pos(':', ExpectedPath); // line number
              if i > 1 then begin
                TryStrToInt(copy(ExpectedPath, i+1, Length(ExpectedPath)), j);
                AssertEquals('find declaration (Loop: '+IntToStr(TestLoop)+') LINE wrong at '+MainTool.CleanPosToStr(IdentifierStartPos,true), j, FoundCursorPos.Y);
                delete(ExpectedPath,i,Length(ExpectedPath));
              end;
              if (FoundCursorPos.Y=1) and (FoundCursorPos.X=1) then begin
                // unit
                FoundPath:=ExtractFileNameOnly(FoundCursorPos.Code.Filename);
              end else begin
                FoundTool.CaretToCleanPos(FoundCursorPos,FoundCleanPos);
                if (FoundCleanPos>1) and (IsIdentChar[FoundTool.Src[FoundCleanPos-1]]) then
                  dec(FoundCleanPos);
                FoundNode:=FoundTool.FindDeepestNodeAtPos(FoundCleanPos,true);
                //debugln(['TTestFindDeclaration.FindDeclarations Found: ',FoundTool.CleanPosToStr(FoundNode.StartPos,true),' FoundNode=',FoundNode.DescAsString]);
                FoundPath:=NodeAsPath(FoundTool,FoundNode);
              end;
              //debugln(['TTestFindDeclaration.FindDeclarations FoundPath=',FoundPath]);
              if LowerCase(ExpectedPath)<>LowerCase(FoundPath) then begin
                WriteSource(IdentifierStartPos,MainTool);
                AssertEquals('find declaration (Loop: '+IntToStr(TestLoop)+') wrong at '+MainTool.CleanPosToStr(IdentifierStartPos,true),LowerCase(ExpectedPath),LowerCase(FoundPath));
              end;
            end;
          end;

          // test identifier completion
          if (ExpectedPath<>'') and (Marker<>'declaration!') then begin
            for ExpectedCompletionPart in ExpectedPath.Split(';') do begin
              ExpectedCompletion := ExpectedCompletionPart;
              StartOffs := 0;
              if (ExpectedCompletion <> '') and (ExpectedCompletion[1] in ['+','-']) then begin
                i := Pos('=', ExpectedCompletion);
                if i > 1 then begin
                  StartOffs := StrToIntDef(copy(ExpectedCompletion, 1, i-1), 0);
                  Delete(ExpectedCompletion, 1, i);
                end
                else
                  StartOffs := 0;
              end;
              StartOffs := StartOffs + IdentifierStartPos;
              MainTool.CleanPosToCaret(StartOffs,CursorPos);

              //debugln(['TCustomTestFindDeclaration.FindDeclarations (Loop: '+IntToStr(TestLoop)+') test GatherIdentifiers at ',dbgs(CursorPos)]);
              if not CodeToolBoss.GatherIdentifiers(CursorPos.Code,CursorPos.X,CursorPos.Y)
              then begin
                if ExpectedCompletion<>'' then begin
                  WriteSource(StartOffs,MainTool);
                  AssertEquals('GatherIdentifiers (Loop: '+IntToStr(TestLoop)+') failed at '+MainTool.CleanPosToStr(StartOffs,true)+': '+CodeToolBoss.ErrorMessage,false,true);
                end;
                continue;
              end else begin
                for ExpectedTermPart in ExpectedCompletion.Split(',') do begin
                  ExpectedTerm := ExpectedTermPart;
                  ExpInvert := (ExpectedTerm <> '') and (ExpectedTerm[1] = '!');
                  if ExpInvert then
                    Delete(ExpectedTerm, 1, 1);
                  i:=CodeToolBoss.IdentifierList.GetFilteredCount-1;
                  //debugln(['TCustomTestFindDeclaration.FindDeclarations Count=',i,' ExpectedTerm="',ExpectedTerm,'" Invert=',ExpInvert]);
                  while i>=0 do begin
                    IdentItem:=CodeToolBoss.IdentifierList.FilteredItems[i];
                    if IdentItem.Node<>nil then begin
                      FoundPath:=NodeAsPath(IdentItem.Tool,IdentItem.Node);
                      //debugln(['TTestFindDeclaration.FindDeclarations i=',i,' FoundPath="',FoundPath,'"']);
                      if SameText(ExpectedTerm,FoundPath) then begin
                        //debugln(['TTestFindDeclaration.FindDeclarations Found i=',i,' FoundPath="',FoundPath,'"']);
                        break;
                      end;
                    end;
                    //debugln(['TTestFindDeclaration.FindDeclarations i=',i,' Identifier=',IdentItem.Identifier]);
                    s := IdentItem.Identifier;
                    if (iliNeedsAmpersand in IdentItem.Flags)
                       and (Marker = 'completion') // declaration=path.ident does not include the &
                    then
                      if s[1]<>'&' then
                        s := '&' + s;
                    if CompareText(s,ExpectedTerm)=0 then break;
                    if Marker='declaration' then begin
                      // last identifier is enough
                      l:=length(s);
                      if ((l=length(ExpectedTerm)) or (ExpectedTerm[length(ExpectedTerm)-l]='.'))
                      and (CompareText(s,RightStr(ExpectedTerm,l))=0)
                      then break;
                    end;
                    dec(i);
                  end;
                  //debugln(['TCustomTestFindDeclaration.FindDeclarations i=',i]);
                  if (i<0) and not ExpInvert then begin
                    WriteSource(StartOffs,MainTool);
                    AssertEquals('GatherIdentifiers misses "'+ExpectedTerm+'" at '+MainTool.CleanPosToStr(StartOffs,true),true,i>=0);
                  end
                  else
                  if ExpInvert and (i>=0) then begin
                    WriteSource(StartOffs,MainTool);
                    AssertEquals('GatherIdentifiers should not have "'+ExpectedTerm+'" at '+MainTool.CleanPosToStr(StartOffs,true),true,i>=0);
                  end;
                end;
              end;
            end;
          end
        end else if Marker='guesstype' then begin
          ExpectedType:=copy(Src,PathPos,CommentP-1-PathPos);
          {$IFDEF VerboseFindDeclarationTests}
          debugln(['TTestFindDeclaration.FindDeclarations "',Marker,'" at ',MainTool.CleanPosToStr(NameStartPos-1),' ExpectedType=',ExpectedType]);
          {$ENDIF}
          MainTool.CleanPosToCaret(IdentifierStartPos,CursorPos);

          // test GuessTypeOfIdentifier
          ListOfPFindContext:=nil;
          try
            if not CodeToolBoss.GuessTypeOfIdentifier(CursorPos.Code,CursorPos.X,CursorPos.Y,
              ItsAKeyword, IsSubIdentifier, ExistingDefinition, ListOfPFindContext,
              NewExprType, NewType)
            then begin
              if ExpectedType<>'' then
                AssertEquals('GuessTypeOfIdentifier (Loop: '+IntToStr(TestLoop)+') failed at '+MainTool.CleanPosToStr(IdentifierStartPos,true)+': '+CodeToolBoss.ErrorMessage,false,true);
              continue;
            end else begin
              //debugln(['TTestFindDeclaration.FindDeclarations FoundPath=',FoundPath]);
              if pos('/', ExpectedType) > 0 then
                if NewExprType.Context.Tool <> nil then
                  NewType := NewExprType.Context.Tool.GetSourceName + '/' + NewType
                else
                  NewType := 'NOT-FOUND' + '/' + NewType;
              if LowerCase(ExpectedType)<>LowerCase(NewType) then begin
                WriteSource(IdentifierStartPos,MainTool);
                AssertEquals('GuessTypeOfIdentifier (Loop: '+IntToStr(TestLoop)+') wrong at '+MainTool.CleanPosToStr(IdentifierStartPos,true),LowerCase(ExpectedType),LowerCase(NewType));
              end;
            end;
          finally
            FreeListOfPFindContext(ListOfPFindContext);
          end;

        end else if Marker='findrefs' then begin
          ExpectedPath:=copy(Src,PathPos,CommentP-1-PathPos);
          ExpComment := copy(ExpectedPath,1,2) = 'C,';
          if ExpComment then delete(ExpectedPath, 1, 2);
          ListOfPCodeXYPosition:=nil;
          Cache:=nil;
          MainTool.CleanPosToCaret(IdentifierStartPos,CursorPos);
          if not CodeToolBoss.FindReferences(
            aCode,CursorPos.X,CursorPos.Y,
            aCode {TODO: iterate multiple files}, not ExpComment,
            ListOfPCodeXYPosition, Cache)
          then
            AssertTrue('FindReferences failed at '+MainTool.CleanPosToStr(IdentifierStartPos,true), False);

            s := '';
            if ListOfPCodeXYPosition <> nil then
              for i:=0 to ListOfPCodeXYPosition.Count-1 do begin
                if s <> '' then
                  s := s + ';';
                s := s + IntToStr(PCodeXYPosition(ListOfPCodeXYPosition[i])^.X) + ',' + IntToStr(PCodeXYPosition(ListOfPCodeXYPosition[i])^.Y);
              end;
          CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
          Cache.Free;

          AssertEquals('FindReferences failed at '+MainTool.CleanPosToStr(IdentifierStartPos,true), ExpectedPath, s);
        end else begin
          WriteSource(IdentifierStartPos,MainTool);
          AssertEquals('Unknown marker at '+MainTool.CleanPosToStr(IdentifierStartPos,true),'declaration',Marker);
          continue;
        end;
      until CommentP >= CommentEnd;
    end;
  end;
  CheckReferenceMarkers;
  CodeToolBoss.IdentComplIncludeKeywords := False;
end;

function TCustomTestFindDeclaration.GetMarkers(Index: integer): TFDMarker;
begin
  Result:=TFDMarker(FMarkers[Index]);
end;

procedure TCustomTestFindDeclaration.TestFiles(Directory: string;
  ADefaultFileMask: String);
const
  fmparam = '--filemask=';
var
  Info: TSearchRec;
  aFilename, Param, aFileMask: String;
  i: Integer;
  Verbose: Boolean;
begin
  aFileMask:='t*.p*';
  if ADefaultFileMask <> '' then
    aFileMask:=ADefaultFileMask;
  Verbose:=false;
  for i:=1 to ParamCount do begin
    Param:=ParamStr(i);
    if LeftStr(Param,length(fmparam))=fmparam then
      aFileMask:=copy(Param,length(fmparam)+1,100);
    if Param='-v' then
      Verbose:=true;
  end;
  Directory:=AppendPathDelim(Directory);

  if FindFirstUTF8(Directory+aFileMask,faAnyFile,Info)=0 then begin
    try
      repeat
        if faDirectory and Info.Attr>0 then continue;
        aFilename:=Info.Name;
        if not FilenameIsPascalUnit(aFilename) then continue;
        if Verbose then
          debugln(['TTestFindDeclaration.TestFiles File="',aFilename,'"']);
        FindDeclarations(Directory+aFilename);
      until FindNextUTF8(Info)<>0;
    finally
      FindCloseUTF8(Info);
    end;
  end;
end;

procedure TCustomTestFindDeclaration.SetUp;
begin
  inherited SetUp;
  FMarkers:=TObjectList.Create(true);
  CodeToolBoss.IdentComplIncludeKeywords := False;
end;

procedure TCustomTestFindDeclaration.TearDown;
begin
  FMainCode:=nil;
  FMainTool:=nil;
  FreeAndNil(FMarkers);
  inherited TearDown;
end;

function TCustomTestFindDeclaration.MarkerCount: integer;
begin
  if FMarkers=nil then
    Result:=0
  else
    Result:=FMarkers.Count;
end;

function TCustomTestFindDeclaration.AddMarker(const aName: string; Kind: char;
  CleanPos: integer; NameStartPos, NameEndPos: integer): TFDMarker;
begin
  if (Kind=MarkDecl) then begin
    Result:=FindMarker(aName,Kind);
    if Result<>nil then
      Fail('duplicate decl marker at '+MainTool.CleanPosToStr(CleanPos)+' and at '+MainTool.CleanPosToStr(Result.CleanPos));
  end;
  Result:=TFDMarker.Create;
  Result.Name:=aName;
  Result.Kind:=Kind;
  Result.CleanPos:=CleanPos;
  Result.NameStartPos:=NameStartPos;
  Result.NameEndPos:=NameEndPos;
  FMarkers.Add(Result);
end;

function TCustomTestFindDeclaration.IndexOfMarker(const aName: string; Kind: char
  ): integer;
var
  i: Integer;
  Marker: TFDMarker;
begin
  for i:=0 to MarkerCount-1 do begin
    Marker:=Markers[i];
    if (Marker.Kind=Kind) and (CompareText(Markers[i].Name,aName)=0) then
      exit(i);
  end;
  Result:=-1;
end;

procedure TCustomTestFindDeclaration.ParseSimpleMarkers(aCode: TCodeBuffer);
var
  CommentP, p, IdentifierStartPos, IdentifierEndPos, NameStartPos: Integer;
  Src, Marker: String;
begin
  FMainCode:=aCode;
  DoParseModule(MainCode,FMainTool);
  CommentP:=1;
  Src:=MainTool.Src;
  while CommentP<length(Src) do begin
    CommentP:=FindNextComment(Src,CommentP);
    if CommentP>length(Src) then break;
    p:=CommentP;
    CommentP:=FindCommentEnd(Src,CommentP,MainTool.Scanner.NestedComments);
    if Src[p]<>'{' then continue;
    if Src[p+1] in ['$','%',' ',#0..#31] then continue;

    IdentifierStartPos:=p;
    IdentifierEndPos:=p;
    while (IdentifierStartPos>1) and (IsIdentChar[Src[IdentifierStartPos-1]]) do
      dec(IdentifierStartPos);

    inc(p);
    NameStartPos:=p;
    if Src[p] in ['#','@'] then begin
      {#name}  {@name}
      inc(p);
      if not IsIdentStartChar[Src[p]] then begin
        WriteSource(p,MainTool);
        Fail('Expected identifier at '+MainTool.CleanPosToStr(p,true));
      end;
      NameStartPos:=p;
      while IsIdentChar[Src[p]] do inc(p);
      Marker:=copy(Src,NameStartPos,p-NameStartPos);
      AddMarker(Marker,Src[NameStartPos-1],CommentP,IdentifierStartPos,IdentifierEndPos);
    end else begin
      WriteSource(p,MainTool);
      Fail('invalid marker at '+MainTool.CleanPosToStr(p));
    end;
  end;
end;

function TCustomTestFindDeclaration.FindMarker(const aName: string; Kind: char
  ): TFDMarker;
var
  i: Integer;
begin
  i:=IndexOfMarker(aName,Kind);
  if i<0 then
    Result:=nil
  else
    Result:=Markers[i];
end;

procedure TTestFindDeclaration.TestFindDeclaration_Program;
begin
  StartProgram;
  Add([
  'var Cow: longint;',
  'begin',
  //'  cow{declaration:Cow}:=3;',
  '  test1{declaration:Test1}.cow{declaration:Cow}:=3;',
  'end.',
  '']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_Basic;
begin
  FindDeclarations('moduletests/fdt_basic.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_UnitIntfBeforeImplUses;
var
  Unit2: TCodeBuffer;
begin
  Unit2:=nil;
  try
    Unit2:=CodeToolBoss.CreateFile('unit2.pp');
    Unit2.Source:='unit unit2;'+LineEnding
      +'interface'+LineEnding
      +'type'+LineEnding
      +'  TBird = word;'+LineEnding
      +'  test1 = word;'+LineEnding
      +'implementation'+LineEnding
      +'end.';

    StartUnit;
    Add([
    'type TBird = boolean;',
    'implementation',
    'uses unit2;',
    'type'+LineEnding,
    '  TEagle = TBird{declaration:test1.TBird};',
    '  TRobin = test1.TBird{declaration:test1.TBird};',
    'end.',
    '']);
    FindDeclarations(Code);
  finally
    if Unit2<>nil then
      Unit2.IsDeleted:=true;
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_Proc_BaseTypes;
begin
  FindDeclarations('moduletests/fdt_proc_basetypes.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_ProcNested;
begin
  StartProgram;
  Add([
  '{$mode objfpc}',
  'procedure Fly(Size: word);',
  '',
  '  procedure Sub1;',
  '  var Size: byte;',
  '  begin',
  '    Size{declaration:Fly.Sub1.Size}:=3;',
  '  end;',
  '',
  '  procedure Sub2(Size: word);',
  '  begin',
  '    Size{declaration:Fly.Sub2.Size}:=4;',
  '  end;',
  'begin',
  '  Size{declaration:Fly.Size}:=Size{ declaration:Fly.Size}+1;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_ResultType;
begin
  StartProgram;
  Add([
  '{$mode objfpc}',
  'function Simple: boolean;',
  '',
  '  procedure Sub1;',
  '  begin',
  '    if Result{declaration:Simple} then ;',
  '  end;',
  '',
  '  function Sub2: word;',
  '  begin',
  '    Result{declaration:Simple.Sub2}:=3;',
  '  end;',
  'begin',
  '  Result{declaration:Simple}:=4;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_ResultField;
begin
  StartProgram;
  Add([
  '{$mode objfpc}',
  'type',
  '  TBird = class',
  '  public',
  '    Result: boolean;',
  '    function Fly: word;',
  '  end;',
  'function TBird.Fly: word;',
  'begin',
  '  Result{declaration:TBird.Fly}:=4;',
  '  Self.Result{declaration:TBird.Result}:=true;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_With;
begin
  FindDeclarations('moduletests/fdt_with.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_WithResult;
begin
  StartProgram;
  Add([
  '{$mode objfpc}',
  'type',
  '  TBird = record',
  '    Result: word;',
  '  end;',
  'var Bird: TBird;',
  'function Fly: word;',
  'begin',
  '  Result{declaration:Fly}:=1;',
  '  with Bird do begin',
  '    Result{ declaration:TBird.Result}:=3;',
  '  end;',
  'end;',
  'begin',
  'end.',
  '']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_ClassOf;
begin
  FindDeclarations('moduletests/fdt_classof.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_NestedClasses;
begin
  FindDeclarations('moduletests/fdt_nestedclasses.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_NestedAliasClass;
begin
  FindDeclarations('moduletests/fdt_nestedaliasclass.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_ClassHelper;
begin
  FindDeclarations('moduletests/fdt_classhelper.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_TypeHelper;
begin
  FindDeclarations('moduletests/fdt_typehelper.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_Proc_ArgSet;
begin
  StartProgram;
  Add([
  '{$mode objfpc}',
  'type',
  '  TColor = (red, gree, blue);',
  '  TColors = set of TColor;',
  'procedure Fly(w: word; Col: TColors);',
  'begin',
  'end;',
  'procedure Fly(b: boolean; Col: TColors);',
  'begin',
  'end;',
  'begin',
  //'  Fly{declaration:Fly}(3,[red]);',
  '  Fly{declaration:Fly}(3,[blue,red]);',
  //'  Fly{declaration:Fly}(true,[red,green]);',
  'end.',
  '']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_ObjCClass;
begin
  {$IFDEF Darwin}
  FindDeclarations('moduletests/fdt_objcclass.pas');
  {$ENDIF}
end;

procedure TTestFindDeclaration.TestFindDeclaration_ObjCCategory;
begin
  {$IFDEF Darwin}
  FindDeclarations('moduletests/fdt_objccategory.pas');
  {$ENDIF}
end;

procedure TTestFindDeclaration.TestFindDeclaration_GenericFunction;
begin
  StartProgram;
  Add([
  '{$mode objfpc}',
  'type',
  '  TBird = class',
  '    generic class function Fly<T>(const AValues:array of T):T;',
  '  end;',
  'generic function RandomFrom<T>(const AValues:array of T):T;',
  'begin',
  '  Result:=Avalue[1];',
  'end;',
  'generic class function TBird.Fly<T>(const AValues:array of T):T;',
  'begin',
  '  Result:=Avalue[1];',
  'end;',
  'begin',
  '  i:=RandomFrom<longint>([1,2,3]);',
  'end.',
  '']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_Generics_Enumerator;
begin
  StartProgram;
  Add([
  'type',
  '  integer = longint;',
  '  TOwnedCollection = class',
  '  end;',
  '  generic TMyOwnedCollection<T: class> = class(TOwnedCollection)',
  '  public type',
  '    TEnumerator = class',
  '    private',
  '      FIndex: Integer;',
  '      FCol: specialize TMyOwnedCollection<T>;',
  '    public',
  '      constructor Create(ACol: specialize TMyOwnedCollection<T>);',
  '      function GetCurrent: T;',
  '      function MoveNext: Boolean;',
  '      property Current: T read GetCurrent;',
  '    end;',
  '  public',
  '    function GetEnumerator: TEnumerator;',
  '    function GetItem(AIndex: Integer): T;',
  '  end;',
  'end.']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_Generics;
begin
  FindDeclarations('moduletests/fdt_generics.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_Generics_GuessType;
begin
  FindDeclarations('moduletests/fdt_generics_guesstype.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_Generics_GuessType2;
begin
  FindDeclarations('moduletests/fdt_generics_guesstype2.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_Generics_FindDeclaration;
begin
  FindDeclarations('moduletests/fdt_generics_finddeclaration.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_GenericsObjFpc_ClassAncestor;
begin
  StartProgram;
  Add([
    //'program Project1;{$Mode objfpc}',
    'type',
    'TX = class',
    'public type',
    'generic TNGen<A: TX> = class',
    'public type',
    'F = A;',
    'end;',
    'end;',
    '',
    'generic TGen<A: TX> = class',
    'public type',
    'F = A;',
    'end;',
    '',
    'TY = class(TX.specialize TNGen<TX{declaration:TX}>)',
    'end;',
    'TY2 = class(TX.specialize TNGen<TX>.F.specialize TNGen<TX{declaration:TX}>)',
    'end;',
    '',
    'TY3 = class(specialize TGen<TX>.F.specialize TNGen<TX{declaration:TX}>)',
    'end;',
    '',
    '',
    'begin',
    'end.']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_GenericsDelphi_InterfaceAncestor;
begin
  StartProgram;
  Add([
  '{$mode delphi}',
  'type',
  '  IParameters = interface',
  '  end;',
  '  IItem = class',
  '  end;',
  '  IBirdy = interface (IParameters<IItem>)',
  '    [''guid'']',
  '  end;',
  'end.']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_GenericsDelphi_FuncParam;
begin
  StartProgram;
  Add([
  '{$mode delphi}',
  'type',
  '  TAnt<T> = class',
  '  type TEvent = procedure(aSender: T);',
  '  end;',
  '  TBird = class',
  '    procedure Fly<T>(Event: TAnt<T>.TEvent; aSender: T)',
  '  end;',
  'procedure Run(Sender: TObject);',
  'begin',
  'end;',
  'var Bird: TBird;',
  'begin',
  '  Bird.Fly<TObject>(Run,Bird);',
  'end.']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_GenericsDelphi_PublicProcType;
begin
  StartProgram;
  Add([
  '{$mode delphi}',
  'type',
  '  TArray<T> = array of T;',
  '  TWing = class',
  '  end;',
  '  TBird = class',
  '  public type',
  '    TFlyFunc<T: TWing> = function (AType: TObject): TArray<T>;',
  '  public var Fly: TFlyFunc<TWing>;',
  '  end;',
  'function Run(Sender: TObject): TArray<TWing>;',
  'begin',
  'end;',
  'var Bird: TBird;',
  'begin',
  '  Bird.Fly(nil);',
  'end.']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_GenericsDelphi_MultiGenParams;
begin
  StartProgram;
  Add([
  '{$mode delphi}',
  'type',
  '  TBird = class',
  '    A: boolean;',
  '    function Fly: boolean;',
  '  end;',
  '  TBird<T> = class',
  '    B: T;',
  '    function Fly: T;',
  '  end;',
  'function TBird.Fly: boolean;',
  'begin',
  'end;',
  'function TBird<T>.Fly: T;',
  'begin',
  'end;',
  'var',
  '  One: TBird;',
  '  Two: TBird<word>;',
  'begin',
  '  One.Fly;',
  '  Two.Fly;',
  'end.']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_ForIn;
begin
  FindDeclarations('moduletests/fdt_for_in.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_FileAtCursor;
var
  SubUnit2Code, LFMCode: TCodeBuffer;
  Found: TFindFileAtCursorFlag;
  FoundFilename: string;
begin
  FMainCode:=CodeToolBoss.CreateFile('test1.lpr');
  MainCode.Source:='uses unit2 in ''sub/../unit2.pas'';'+LineEnding;
  SubUnit2Code:=CodeToolBoss.CreateFile('unit2.pas');
  LFMCode:=CodeToolBoss.CreateFile('test1.lfm');
  try
    // --- used unit ---
    // test cursor on 'unit2'
    if not CodeToolBoss.FindFileAtCursor(MainCode,6,1,Found,FoundFilename) then
      Fail('CodeToolBoss.FindFileAtCursor at uses unit2');
    AssertEquals('FindFileAtCursor at uses unit2 Found',ord(ffatUsedUnit),ord(Found));
    AssertEquals('FindFileAtCursor at uses unit2 FoundFilename','unit2.pas',FoundFilename);
    // test cursor on 'in'
    if not CodeToolBoss.FindFileAtCursor(MainCode,12,1,Found,FoundFilename) then
      Fail('CodeToolBoss.FindFileAtCursor at uses unit2-in');
    AssertEquals('FindFileAtCursor at uses unit2-in Found',ord(ffatUsedUnit),ord(Found));
    AssertEquals('FindFileAtCursor at uses unit2-in FoundFilename','unit2.pas',FoundFilename);
    // test cursor on in-file literal
    if not CodeToolBoss.FindFileAtCursor(MainCode,16,1,Found,FoundFilename) then
      Fail('CodeToolBoss.FindFileAtCursor at uses unit2-in-literal');
    AssertEquals('FindFileAtCursor at uses unit2-in-lit Found',ord(ffatUsedUnit),ord(Found));
    AssertEquals('FindFileAtCursor at uses unit2-in-lit FoundFilename','unit2.pas',FoundFilename);

    // --- enabled include directive ---
    // test cursor on enabled include directive of empty file
    MainCode.Source:='program test1;'+LineEnding
      +'{$i unit2.pas}'+LineEnding;
    SubUnit2Code.Source:='';
    if not CodeToolBoss.FindFileAtCursor(MainCode,1,2,Found,FoundFilename) then
      Fail('CodeToolBoss.FindFileAtCursor at enabled include directive of empty inc');
    AssertEquals('FindFileAtCursor at enabled include directive of empty Found',ord(ffatIncludeFile),ord(Found));
    AssertEquals('FindFileAtCursor at enabled include directive of empty FoundFilename','unit2.pas',FoundFilename);

    // test cursor on enabled include directive of not empty file
    SubUnit2Code.Source:='{$define a}';
    if not CodeToolBoss.FindFileAtCursor(MainCode,1,2,Found,FoundFilename) then
      Fail('CodeToolBoss.FindFileAtCursor at enabled include directive of non-empty inc');
    AssertEquals('FindFileAtCursor at enabled include directive of non-empty Found',ord(ffatIncludeFile),ord(Found));
    AssertEquals('FindFileAtCursor at enabled include directive of non-empty FoundFilename','unit2.pas',FoundFilename);

    // --- disabled include directive ---
    // test cursor on disabled include directive
    MainCode.Source:='program test1;'+LineEnding
      +'{$ifdef disabled}'+LineEnding
      +'{$i unit2.pas}'+LineEnding
      +'{$endif}'+LineEnding;
    SubUnit2Code.Source:='';
    if not CodeToolBoss.FindFileAtCursor(MainCode,1,3,Found,FoundFilename) then
      Fail('CodeToolBoss.FindFileAtCursor at disabled include directive');
    AssertEquals('FindFileAtCursor at disabled include directive Found',ord(ffatDisabledIncludeFile),ord(Found));
    AssertEquals('FindFileAtCursor at disabled include directive FoundFilename','unit2.pas',FoundFilename);

    // --- enabled resource directive ---
    MainCode.Source:='program test1;'+LineEnding
      +'{$R test1.lfm}'+LineEnding;
    if not CodeToolBoss.FindFileAtCursor(MainCode,1,2,Found,FoundFilename) then
      Fail('CodeToolBoss.FindFileAtCursor at enabled resource directive');
    AssertEquals('FindFileAtCursor at enabled resource directive Found',ord(ffatResource),ord(Found));
    AssertEquals('FindFileAtCursor at enabled resource directive FoundFilename','test1.lfm',FoundFilename);

    MainCode.Source:='program test1;'+LineEnding
      +'{$R *.lfm}'+LineEnding;
    if not CodeToolBoss.FindFileAtCursor(MainCode,1,2,Found,FoundFilename) then
      Fail('CodeToolBoss.FindFileAtCursor at enabled resource directive');
    AssertEquals('FindFileAtCursor at enabled resource directive Found',ord(ffatResource),ord(Found));
    AssertEquals('FindFileAtCursor at enabled resource directive FoundFilename','test1.lfm',FoundFilename);

    // --- disabled resource directive ---
    MainCode.Source:='program test1;'+LineEnding
      +'{$ifdef disabled}'+LineEnding
      +'{$R test1.lfm}'+LineEnding
      +'{$endif}'+LineEnding;
    if not CodeToolBoss.FindFileAtCursor(MainCode,1,3,Found,FoundFilename) then
      Fail('CodeToolBoss.FindFileAtCursor at disabled resource directive');
    AssertEquals('FindFileAtCursor at disabled resource directive Found',ord(ffatDisabledResource),ord(Found));
    AssertEquals('FindFileAtCursor at disabled resource directive FoundFilename','test1.lfm',FoundFilename);

    // --- literal ---
    MainCode.Source:='program test1;'+LineEnding
      +'const Cfg=''unit2.pas'';'+LineEnding;
    if not CodeToolBoss.FindFileAtCursor(MainCode,11,2,Found,FoundFilename) then
      Fail('CodeToolBoss.FindFileAtCursor in literal');
    AssertEquals('FindFileAtCursor in literal Found',ord(ffatLiteral),ord(Found));
    AssertEquals('FindFileAtCursor in literal FoundFilename','unit2.pas',FoundFilename);

    // --- comment ---
    MainCode.Source:='program test1;'+LineEnding
      +'{unit2.pas}'+LineEnding;
    if not CodeToolBoss.FindFileAtCursor(MainCode,3,2,Found,FoundFilename) then
      Fail('CodeToolBoss.FindFileAtCursor in comment');
    AssertEquals('FindFileAtCursor in comment Found',ord(ffatComment),ord(Found));
    AssertEquals('FindFileAtCursor in comment FoundFilename','unit2.pas',FoundFilename);

    // --- unit name search in comment ---
    MainCode.Source:='program test1;'+LineEnding
      +'{unit2}'+LineEnding;
    if not CodeToolBoss.FindFileAtCursor(MainCode,3,2,Found,FoundFilename) then
      Fail('CodeToolBoss.FindFileAtCursor in comment');
    AssertEquals('FindFileAtCursor in comment Found',ord(ffatUnit),ord(Found));
    AssertEquals('FindFileAtCursor in comment FoundFilename','unit2.pas',FoundFilename);

    // --- unit name search in MainCode ---
    MainCode.Source:='program test1;'+LineEnding
      +'begin'+LineEnding
      +'  unit2.Test;'+LineEnding;
    if not CodeToolBoss.FindFileAtCursor(MainCode,3,3,Found,FoundFilename) then
      Fail('CodeToolBoss.FindFileAtCursor in comment');
    AssertEquals('FindFileAtCursor in comment Found',ord(ffatUnit),ord(Found));
    AssertEquals('FindFileAtCursor in comment FoundFilename','unit2.pas',FoundFilename);

  finally
    MainCode.IsDeleted:=true;
    SubUnit2Code.IsDeleted:=true;
    LFMCode.IsDeleted:=true;
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_CBlocks;
begin
  StartProgram;
  Add([
    '{$modeswitch cblocks}',
    'type tblock = reference to procedure; cdecl;',
    'procedure test(b: tblock);',
    'begin',
    '  b;',
    'end;',
    'procedure proc;',
    'begin',
    'end;',
    'const bconst: tblock = @proc;',
    'var',
    '  b: tblock;',
    'begin',
    '  b:=@proc;',
    '  b;',
    '  test{declaration:test1.test}(@proc);',
    '  test{declaration:test1.test}(b);',
    '  bconst{declaration:test1.bconst};',
    '  test{declaration:test1.test}(bconst{declaration:test1.bconst});',
    'end.',
  '']);
  ParseModule;
end;

procedure TTestFindDeclaration.TestFindDeclaration_Arrays;
begin
  FindDeclarations('moduletests/fdt_arrays.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_ArrayMultiDimDot;
begin
  StartProgram;
  Add([
  'type',
  '  TmyClass = class',
  '    Field: integer;',
  '  end;',
  '  TArray1 = array of TmyClass;',
  '  TArray2 = array of TArray1;',
  'var',
  '  tmp: TArray2;',
  'begin',
  '  tmp[0,0].Field{declaration:tmyclass.field};',
  'end.']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_GuessType;
begin
  FindDeclarations('moduletests/fdt_guesstype1.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_GuessType_Set;
begin
  StartProgram;
  Add([
  'type',
  '  TColor = (red,green,blue);',
  '  TColors = set of TColor;',
  'const',
  '  Tomato = [red];',
  '  TomatoSalad = Tomato+[green];',
  'begin',
  '  Bla{guesstype:TColors} := TomatoSalad+[blue];',
  'end.']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_Attributes;
var
  Node: TCodeTreeNode;
  p: Integer;
  Src: String;
begin
  StartProgram;
  Add([
  '{$modeswitch prefixedattributes}',
  'type',
  '  TCustomAttribute = class',
  '  end;',
  '  BirdAttribute = class(TCustomAttribute)',
  '  end;',
  '  Bird = class(TCustomAttribute)',
  '  end;',
  '  [Bird{declaration:BirdAttribute}]',
  '  THawk = class',
  '    [Bird{declaration:BirdAttribute}(1)]',
  '    FField: integer;',
  '    [Bird(2)]',
  '    procedure DoSome;',
  '    [Bird(3)]',
  '    property  F: integer read FField;',
  '  end;',
  '  IMy = interface',
  '    [''guid'']',
  '    [Bird]',
  '    [Bird(12)]',
  '    function GetSome: integer;',
  '    [Bird(13)]',
  '    property  Some: integer read GetSome;',
  '  end;',
  '  IMy = dispinterface',
  '    [''guid'']',
  '    [Bird(21)]',
  '    function GetMore: integer;',
  '  end;',
  '[test1.bird]',
  '[bird(4)]',
  'procedure DoIt; forward;',
  '[bird(5)]',
  'procedure Fly(const [ref] Obj: TObject);',
  'begin',
  'end;',
  'var',
  '  [bird(1+2,3),bird]',
  '  Foo: TObject;',
  'begin',
  'end.',
  '']);
  FindDeclarations(Code);
  // check if all attributes were parsed
  Src:=MainTool.Src;
  for p:=1 to length(Src) do begin
    if (Src[p]='[') and (IsIdentStartChar[Src[p+1]]) then begin
      Node:=MainTool.FindDeepestNodeAtPos(p,false);
      if (Node=nil) then begin
        WriteSource(p,MainTool);
        Fail('missing node at '+MainTool.CleanPosToStr(p));
      end;
      if (Node.Desc<>ctnAttribute) then begin
        WriteSource(p,MainTool);
        Fail('missing attribute at '+MainTool.CleanPosToStr(p));
      end;
      if Node.NextBrother=nil then begin
        WriteSource(Node.StartPos,MainTool);
        Fail('Attribute without NextBrother');
      end;
      if not (Node.NextBrother.Desc in [ctnAttribute,ctnVarDefinition,ctnTypeDefinition,ctnProcedure,ctnProperty])
      then begin
        WriteSource(Node.StartPos,MainTool);
        Fail('Attribute invalid NextBrother '+Node.NextBrother.DescAsString);
      end;
    end;
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_BracketOpen;
begin
  StartProgram;
  Add([
  'var c: integer;',
  'procedure DoIt(i: integer);',
  '  procedure WriteStr(s: string);',
  '  begin',
  '  end;',
  'begin',
  '  begin',
  '    DoIt(c{declaration:c}',
  '  end;',
  '  begin',
  '    WriteStr(c{declaration:c}',
  '  end;',
  'end;',
  'begin',
  'end.',
  '']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_AnonymProc;
begin
  StartProgram;
  Add([
  '{$mode objfpc}',
  '{$modeswitch anonymousfunctions}',
  'type',
  '  int = word;',
  '  TFunc = function(i: int): int;',
  'var f: TFunc;',
  'procedure DoIt(a: int);',
  '  procedure Sub(b: int);',
  '  begin',
  '    f:=function(c: int{declaration:int}): int{declaration:int}',
  '      begin',
  '        f{declaration:f}:=nil;',
  '        a{declaration:doit.a}:=b{declaration:doit.sub.b}+c{declaration:doit.sub.$ano.c};',
  '      end;',
  '    DoIt(function(i: int{declaration:int}): int{declaration:int}',
  '      begin',
  '        a{declaration:doit.a}:=b{declaration:doit.sub.b}+i{declaration:doit.sub.$ano.i};',
  '      end);',
  '  end;',
  'begin',
  'end;',
  'begin',
  'end.',
  '']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_AnonymProc_ExprDot;
begin
  StartProgram;
  Add([
  '{$mode objfpc}',
  '{$modeswitch anonymousfunctions}',
  'type',
  '  int = word;',
  '  TFunc = function(i: int): int;',
  'var f: TFunc;',
  'function DoIt(f: TProc): TObject{declaration:system.tobject};',
  'begin',
  '  DoIt(nil).ClassInfo{declaration:system.tobject.classinfo};',
  '  DoIt(function(c: int{declaration:int}): int{declaration:int}',
  '      type t = record o:byte end;',
  '      var w: t;',
  '      const v = 4;',
  '      begin',
  '        repeat until true;',
  '        asm end;',
  '        try except end;',
  '      end).ClassInfo{declaration:system.tobject.classinfo};',
  'end;',
  'begin',
  'end.',
  '']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_VarArgsOfType;
begin
  StartProgram;
  Add([
  'procedure Run; varargs of word;',
  'begin',
  '  Run{declaration:run}(1,2);',
  'end;',
  'procedure Fly; varargs;',
  'begin',
  '  Run{declaration:run}(2,3);',
  'end;',
  'begin',
  '  Run{declaration:run}(3);',
  '  Fly{declaration:fly}(4);',
  'end.']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_ProcRef;
begin
  StartProgram;
  Add([
  'type',
  '  TProc = procedure of object;',
  '  TFoo = class',
  '  private',
  '    FTest: TClassProcedure;',
  '  public',
  '    procedure TestProc;',
  '    property Test: TProc read FTest write FTest;',
  '  end;',
  'procedure TFoo.TestProc;',
  'begin',
  '  Self.Test{declaration:TFoo.Test} := @TestProc{declaration:TFoo.TestProc};',
  '  Test{declaration:TFoo.Test} := @Self.TestProc{declaration:TFoo.TestProc};',
  '  // TestProc{declaration:TFoo.TestProc}',
  'end;',
  'var Foo: TFoo;',
  'begin',
  '  Foo.Test{declaration:TFoo.Test} := @Foo.TestProc{declaration:TFoo.TestProc};',
  '  with Foo do',
  '    Test{declaration:TFoo.Test} := @TestProc{declaration:TFoo.TestProc};',
  'end.']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_PointerForwardVsUses;
var
  Unit2: TCodeBuffer;
begin
  Unit2:=nil;
  try
    Unit2:=CodeToolBoss.CreateFile('unit2.pp');
    Unit2.Source:='unit unit2;'+LineEnding
      +'interface'+LineEnding
      +'type TBird = word;'+LineEnding
      +'implementation'+LineEnding
      +'end.';

    StartUnit;
    Add([
    'uses unit2;',
    'type',
    '  PBird = ^TBird{declaration:test1.tbird};',
    '  [attr]',
    '  TBird = record',
    '    Speed: word;',
    '  end;',
    'var Bird: PBird;',
    'implementation',
    'begin',
    '  Bird^.Speed{declaration:test1.tbird.Speed}:=3;',
    'end.',
    '']);
    FindDeclarations(Code);
  finally
    if Unit2<>nil then
      Unit2.IsDeleted:=true;
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_AutoDeref;
var
  Unit2: TCodeBuffer;
begin
  Unit2:=nil;
  try
    Unit2:=CodeToolBoss.CreateFile('unit2.pp');
    Unit2.Source:='unit unit2;'+LineEnding
      +'interface'+LineEnding
      +'type TBird = word;'+LineEnding
      +'implementation'+LineEnding
      +'end.';

    StartUnit;
    Add([
    '{$ModeSwitch autoderef}',
    '{$ModeSwitch typehelpers}',
    'uses unit2;',
    'type',
    '  PBird = ^TBird{declaration:test1.tbird};',
    '  TBird = record',
    '    Speed: word;',
    '  end;',
    '  THelp = type helper for TBird',
    '    Wing: integer;',
    '  end;',
    'var Bird: PBird;',
    'implementation',
    'begin',
    '  Bird^.Speed{declaration:test1.tbird.Speed}:=3;',
    '  Bird.Speed{declaration:test1.tbird.Speed}:=3;',
    '  Bird^.Wing{declaration:test1.tHelp.Wing}:=4;',
    '  Bird.Wing{declaration:test1.tHelp.Wing}:=4;',
    'end.',
    '']);
    FindDeclarations(Code);
  finally
    if Unit2<>nil then
      Unit2.IsDeleted:=true;
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_Variant;
begin
  StartUnit;
  Add([
  'type',
  '  TBird = variant;',
  '  TOxe = olevariant;',
  'var',
  '  Bird: TBird;',
  '  Oxe: TOxe;',
  'implementation',
  'procedure Run(v: Variant);',
  'begin',
  'end;',
  'procedure Run(v: integer);',
  'begin',
  'end;',
  'procedure Pull(o: OLEVariant);',
  'begin',
  'end;',
  'initialization',
  '  Run{declaration:Run}(Bird);',
  '  Run{declaration:Run}(Oxe);',
  '  Pull{declaration:Pull}(Oxe);',
  'end.']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_Ampersand;
begin
  StartUnit;
  Add([
  'const',
  '  &true = 1;',
  'type',
  '  TBird = record',
  '    &type: word;',
  '  end;',
  'var',
  '  Bird: TBird;',
  'implementation',
  'initialization',
  '  Bird.&Type{declaration:test1.TBird.Type} = &True{declaration:test1.True};',
  'end.']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_Ampersand_UnitName;
var
  UnitType: TCodeBuffer;
begin
  UnitType:=CodeToolBoss.CreateFile('type.pas');
  try
    UnitType.Source:=
      'unit &Type;'+sLineBreak
      +'interface'+sLineBreak
      +'var r: word;'+sLineBreak
      +'implementation'+sLineBreak
      +'end.';

    StartProgram;
    Add([
    'uses &Type;',
    'begin',
    '  r{declaration:&type.r}:=3;',
    'end.']);
    FindDeclarations(Code);
  finally
    UnitType.IsDeleted:=true;
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_AmpersandArray;
begin
  StartProgram;
  Add([
  'type',
  '  TFoo = record',
  '    abc: integer;',
  '    &array:',
  '      record',
  '        x:integer;',
  '      end',
  '  end;',
  'var',
  '  AnFoo: TFoo;',
  '  &array: TFoo{declaration:TFoo};',
  'begin',
  '  AnFoo.abc{declaration:TFoo.abc} :=2;',
  '  AnFoo.&array{declaration:TFoo.array}.x{declaration:TFoo.array.x} := 3;',
  '  &Array{declaration:array}.abc{declaration:TFoo.abc} :=4;',
  '  &Array.&array{declaration:TFoo.array}.x{declaration:TFoo.array.x} := 5;',
  'end.']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_UnitSearch_CurrentDir;
var
  Unit1A, Unit1B: TCodeBuffer;
  DefTemp: TDefineTemplate;
begin
  Unit1A:=CodeToolBoss.CreateFile('unit1.pas');
  Unit1A.Source:=
    'unit unit1;'+sLineBreak
    +'interface'+sLineBreak
    +'var r: word;'+sLineBreak
    +'implementation'+sLineBreak
    +'end.';
  Unit1B:=CodeToolBoss.CreateFile('sub'+PathDelim+'unit1.pas');
  Unit1B.Source:=
    'unit unit1;'+sLineBreak
    +'interface'+sLineBreak
    +'implementation'+sLineBreak
    +'end.';

  DefTemp:=TDefineTemplate.Create('unitpath','add sub',UnitPathMacroName,'sub',da_Define);
  try
    StartProgram;
    Add([
    'uses unit1;',
    'begin',
    '  r{declaration:unit1.r}:=3;',
    'end.']);
    CodeToolBoss.DefineTree.Add(DefTemp);

    //debugln(['TTestFindDeclaration.TestFindDeclaration_UnitSearch_CurrentDir ',CodeToolBoss.GetUnitPathForDirectory('')]);

    FindDeclarations(Code);
  finally
    Unit1A.IsDeleted:=true;
    Unit1B.IsDeleted:=true;
    CodeToolBoss.DefineTree.RemoveDefineTemplate(DefTemp);
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_UnitSearch_StarStar;
var
  StarDir, UnitPath, Expected, anUnitName, InFilename,
    FoundFilename: String;
  DirDef, UnitPathDef: TDefineTemplate;
  DirCache: TCTDirectoryCachePool;
  Cache: TCTStarDirectoryCache;
  i: Integer;
begin
  StarDir:=ExpandFileNameUTF8(SetDirSeparators('moduletests/star'));

  DirCache:=CodeToolBoss.DirectoryCachePool;
  if DirCache.StarDirectoryExcludes.IndexOf('ignore')<0 then
    DirCache.StarDirectoryExcludes.Add('ignore');

  DirDef:=TDefineTemplate.Create('TTestFindDeclaration_UnitSearch','','',StarDir,da_Directory);
  try
    CodeToolBoss.DefineTree.Add(DirDef);
    UnitPathDef:=TDefineTemplate.Create('UnitPath','',UnitPathMacroName,DefinePathMacro+PathDelim+'**',da_DefineRecurse);
    DirDef.AddChild(UnitPathDef);
    CodeToolBoss.DefineTree.ClearCache;

    // check unit path in some directories
    Expected:=StarDir+PathDelim+'**';

    UnitPath:=CodeToolBoss.GetUnitPathForDirectory(StarDir);
    AssertEquals('unit path',Expected,UnitPath);

    UnitPath:=CodeToolBoss.GetUnitPathForDirectory(StarDir+PathDelim+'green');
    AssertEquals('unit path',Expected,UnitPath);

    // searching a lowercase unit
    anUnitName:='Star.Red1';
    InFilename:='';
    FoundFilename:=DirCache.FindUnitSourceInCompletePath(StarDir,anUnitName,InFilename);
    Expected:=StarDir+PathDelim+'red'+PathDelim+'star.red1.pas';
    AssertEquals('searching '+anUnitName,Expected,FoundFilename);

    // searching a mixedcase unit
    anUnitName:='Star.Green3';
    InFilename:='';
    FoundFilename:=DirCache.FindUnitSourceInCompletePath(StarDir,anUnitName,InFilename);
    Expected:=StarDir+PathDelim+'green'+PathDelim+'Star.Green3.pas';
    AssertEquals('searching '+anUnitName,Expected,FoundFilename);

    // searching an anycase unit
    anUnitName:='star.green3';
    InFilename:='';
    FoundFilename:=DirCache.FindUnitSourceInCompletePath(StarDir,anUnitName,InFilename,true);
    Expected:=StarDir+PathDelim+'green'+PathDelim+'Star.Green3.pas';
    AssertEquals('searching '+anUnitName,Expected,FoundFilename);

    // check excludes
    Cache:=DirCache.GetStarCache(StarDir,ctsdStarStar);
    if Cache<>nil then begin
      Cache.UpdateListing;
      for i:=0 to Cache.Listing.Count-1 do begin
        FoundFilename:=Cache.Listing.GetSubDirFilename(i);
        if (FoundFilename[1]='.')
            or (Pos(PathDelim+'.',FoundFilename)>0)
            or (Pos('ignore',FoundFilename)>0) then
          Fail('Failed to exclude "'+FoundFilename+'"');
      end;
    end;

  finally
    CodeToolBoss.DefineTree.RemoveDefineTemplate(DirDef);
  end;
end;

procedure TTestFindDeclaration.
  TestFindDeclaration_IncludeSearch_DirectiveWithPath;
var
  aFilename: String;
  StarCode: TCodeBuffer;
  Tool: TCodeTool;
begin
  aFilename:=ExpandFileNameUTF8(SetDirSeparators('moduletests/star/star.main.pas'));
  StarCode:=CodeToolBoss.LoadFile(aFilename,true,false);
  if not CodeToolBoss.Explore(STarCode,Tool,true) then begin
    debugln('Error: '+CodeToolBoss.ErrorDbgMsg);
    Fail('Explore failed: '+CodeToolBoss.ErrorMessage);
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_IncludeSearch_StarStar;
var
  StarDir, IncPath, Expected, IncFilename, FoundFilename: String;
  DirDef, IncPathDef: TDefineTemplate;
  DirCache: TCTDirectoryCachePool;
begin
  StarDir:=ExpandFileNameUTF8(SetDirSeparators('moduletests/star'));

  DirCache:=CodeToolBoss.DirectoryCachePool;
  if DirCache.StarDirectoryExcludes.IndexOf('ignore')<0 then
    DirCache.StarDirectoryExcludes.Add('ignore');

  DirDef:=TDefineTemplate.Create('TTestFindDeclaration_IncudeSearch','','',StarDir,da_Directory);
  try
    CodeToolBoss.DefineTree.Add(DirDef);
    IncPathDef:=TDefineTemplate.Create('IncPath','',IncludePathMacroName,DefinePathMacro+PathDelim+'**',da_DefineRecurse);
    DirDef.AddChild(IncPathDef);
    CodeToolBoss.DefineTree.ClearCache;

    // check include search path in some directories
    Expected:=StarDir+PathDelim+'**';

    IncPath:=CodeToolBoss.GetIncludePathForDirectory(StarDir);
    AssertEquals('include path',Expected,IncPath);

    IncPath:=CodeToolBoss.GetIncludePathForDirectory(StarDir+PathDelim+'green');
    AssertEquals('include path',Expected,IncPath);

    // searching a lowercase include
    IncFilename:='Star.inc';
    FoundFilename:=DirCache.FindIncludeFileInCompletePath(StarDir,IncFilename);
    Expected:=StarDir+PathDelim+'star.inc';
    AssertEquals('searching '+IncFilename,Expected,FoundFilename);

    // searching a mixedcase include
    IncFilename:='Green.inc';
    FoundFilename:=DirCache.FindIncludeFileInCompletePath(StarDir,IncFilename);
    Expected:=StarDir+PathDelim+'green'+PathDelim+'Green.inc';
    AssertEquals('searching '+IncFilename,Expected,FoundFilename);

    // searching an include file without extension
    IncFilename:='Green';
    FoundFilename:=DirCache.FindIncludeFileInCompletePath(StarDir,IncFilename);
    Expected:=StarDir+PathDelim+'green'+PathDelim+'Green.inc';
    AssertEquals('searching '+IncFilename,Expected,FoundFilename);

  finally
    CodeToolBoss.DefineTree.RemoveDefineTemplate(DirDef);
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_FindFPCSrcNameSpacedUnits;
var
  UnitSet: TFPCUnitSetCache;
  FPCSrcDir: String;

  procedure Traverse(const Dir: string; Lvl, NamespacedLvl: integer);
  var
    Cache: TCTDirectoryCache;
    Listing: TCTDirectoryListing;
    i, p, AtomStart, IncludeStart, IncludeEnd, NextNamespacedLvl: Integer;
    CurFilename, Ext, FullFilename, Src, IncFilename, FullIncFilename,
      FoundFilename, CurUnitName, DirectiveName: String;
    CurCode: TCodeBuffer;
  begin
    Cache:=CodeToolBoss.DirectoryCachePool.GetCache(Dir,true,false);
    Cache.UpdateListing;
    Listing:=Cache.Listing;
    for i:=0 to Listing.Count-1 do begin
      CurFilename:=Listing.GetFilename(i);
      FullFilename:=Dir+PathDelim+CurFilename;
      if Listing.GetAttr(i) and faDirectory>0 then begin
        // search recursive
        NextNamespacedLvl:=NamespacedLvl;
        if NamespacedLvl>=0 then
          inc(NextNamespacedLvl)
        else if CurFilename='namespaced' then
          NextNamespacedLvl:=Lvl;
        Traverse(FullFilename,Lvl+1,NextNamespacedLvl);
      end else begin
        Ext:=ExtractFileExt(CurFilename);
        if (NamespacedLvl>=0) and ((Ext='.pp') or (Ext='.pas')) then begin
          CurCode:=CodeToolBoss.LoadFile(FullFilename,true,false);
          if CurCode=nil then begin
            debugln(['TTestFindDeclaration.TestFindDeclaration_FindFPCSrcNameSpacedUnits failed loading "'+FullFilename+'"']);
            continue;
          end;
          Src:=CurCode.Source;
          // check if this file is an unit
          p:=1;
          AtomStart:=1;
          ReadRawNextPascalAtom(Src,p,AtomStart,false,true);
          if p=AtomStart then begin
            debugln(['TTestFindDeclaration.TestFindDeclaration_FindFPCSrcNameSpacedUnits no Pascal found in "'+FullFilename+'"']);
            continue;
          end;
          if CompareIdentifiers(@Src[AtomStart],'unit')<>0 then begin
            debugln(['TTestFindDeclaration.TestFindDeclaration_FindFPCSrcNameSpacedUnits is not a Pascal unit "'+FullFilename+'"']);
            continue;
          end;
          // search include directive
          if not FindIncludeDirective(Src,'unit',1,IncludeStart,IncludeEnd) then begin
            debugln(['TTestFindDeclaration.TestFindDeclaration_FindFPCSrcNameSpacedUnits missing include directive in "'+FullFilename+'"',NamespacedLvl]);
            continue;
          end;
          ExtractLongParamDirective(Src,IncludeStart,DirectiveName,IncFilename);
          DoDirSeparators(IncFilename);
          if ExtractFilePath(IncFilename)<>'' then begin
            FullIncFilename:=ResolveDots(Dir+PathDelim+IncFilename);
            if not CodeToolBoss.DirectoryCachePool.FileExists(FullIncFilename) then begin
              Fail('[20231230132715] Namespaced unit "'+FullFilename+'" includes missing "'+IncFilename+'"');
            end;
          end else begin
            FoundFilename:=CodeToolBoss.DirectoryCachePool.FindIncludeFileInDirectory(Dir,IncFilename);
            if FoundFilename='' then
              FoundFilename:=CodeToolBoss.DirectoryCachePool.FindIncludeFileInCompletePath(Dir,IncFilename);
            if FoundFilename<>'' then
              continue;
            if not FilenameIsPascalUnit(IncFilename) then begin
              Fail('[20231230132721] Namespaced unit "'+FullFilename+'" includes missing "'+IncFilename+'"');
            end;
            CurUnitName:=ExtractFileNameOnly(IncFilename);
            FoundFilename:=CodeToolBoss.DirectoryCachePool.FindUnitInUnitLinks('',CurUnitName);
            if FoundFilename<>'' then begin
              Fail('Namespaced unit "'+FullFilename+'" includes file "'+IncFilename+'", not found via FindIncludeFileInCompletePath, but found via UnitLinks');
            end else begin
              debugln('Note: Namespaced unit "'+FullFilename+'" includes missing file "'+IncFilename+'"');
            end;
          end;
        end;
      end;
    end;
  end;

begin
  UnitSet:=CodeToolBoss.GetUnitSetForDirectory('');
  if UnitSet=nil then Fail('GetUnitSetForDirectory returned nil');
  FPCSrcDir:=ChompPathDelim(UnitSet.FPCSourceDirectory);
  if FPCSrcDir='' then Fail('UnitSet.FPCSourceDirectory empty');
  if not DirectoryExists(FPCSrcDir) then
    Fail('UnitSet.FPCSourceDirectory not found: "'+FPCSrcDir+'"');
  Traverse(FPCSrcDir,0,-1);
end;

procedure TTestFindDeclaration.TestFindDeclaration_NS_Program;
begin
  Add([
  'program nsA.dots;',
  'var Red: TBird;',
  'begin',
  '  NSA . dots . Red{declaration:red}:=3',
  'end.',
  '']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_NS_ProgLocalVsUses;
var
  DotsUnit: TCodeBuffer;
begin
  DotsUnit:=nil;
  try
    DotsUnit:=CodeToolBoss.CreateFile('nsA.dots.pp');
    DotsUnit.Source:='unit nsa.dots;'+LineEnding
      +'interface'+LineEnding
      +'type Size = word;'+LineEnding
      +'implementation'+LineEnding
      +'end.';

    StartProgram;
    Add([
    'uses nsA.dots;',
    'type',
    '  TWing = record',
    '    Size: word;',
    '  end;',
    '  TBird = record',
    '    DoTs: TWing;',
    '  end;',
    'var NSA: TBird;',
    'begin',
    '  NSA.dots.Size{declaration:twing.Size}:=3',
    'end.',
    '']);
    FindDeclarations(Code);
  finally
    if DotsUnit<>nil then
      DotsUnit.IsDeleted:=true;
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_NS_UnitIntfVsUses;
var
  DotsUnit: TCodeBuffer;
begin
  DotsUnit:=nil;
  try
    DotsUnit:=CodeToolBoss.CreateFile('nsA.dots.pp');
    DotsUnit.Source:='unit nsa.dots;'+LineEnding
      +'interface'+LineEnding
      +'type Size = word;'+LineEnding
      +'implementation'+LineEnding
      +'end.';

    StartUnit;
    Add([
    'uses nsA.dots;',
    'type',
    '  TWing = record',
    '    Size: word;',
    '  end;',
    '  TBird = record',
    '    DoTs: TWing;',
    '  end;',
    'var NSA: TBird;',
    'implementation',
    'begin',
    '  NSA.dots.Size{declaration:test1.twing.Size}:=3',
    'end.',
    '']);
    FindDeclarations(Code);
  finally
    if DotsUnit<>nil then
      DotsUnit.IsDeleted:=true;
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_NS_UnitImplVsIntfUses;
var
  DotsUnit: TCodeBuffer;
begin
  DotsUnit:=nil;
  try
    DotsUnit:=CodeToolBoss.CreateFile('nsA.dots.pp');
    DotsUnit.Source:='unit nsa.dots;'+LineEnding
      +'interface'+LineEnding
      +'type Size = word;'+LineEnding
      +'implementation'+LineEnding
      +'end.';

    StartUnit;
    Add([
    'uses nsA.dots;',
    'implementation',
    'type',
    '  TWing = record',
    '    Size: word;',
    '  end;',
    '  TBird = record',
    '    DoTs: TWing;',
    '  end;',
    'var NSA: TBird;',
    'begin',
    '  NSA.dots.Size{declaration:twing.Size}:=3',
    'end.',
    '']);
    FindDeclarations(Code);
  finally
    if DotsUnit<>nil then
      DotsUnit.IsDeleted:=true;
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_NS_UnitImplVsImplUses;
var
  DotsUnit: TCodeBuffer;
begin
  DotsUnit:=nil;
  try
    DotsUnit:=CodeToolBoss.CreateFile('nsA.dots.pp');
    DotsUnit.Source:='unit nsa.dots;'+LineEnding
      +'interface'+LineEnding
      +'type Size = word;'+LineEnding
      +'implementation'+LineEnding
      +'end.';

    StartUnit;
    Add([
    'implementation',
    'uses nsA.dots;',
    'type',
    '  TWing = record',
    '    Size: word;',
    '  end;',
    '  TBird = record',
    '    DoTs: TWing;',
    '  end;',
    'var NSA: TBird;',
    'begin',
    '  NSA.dots.Size{declaration:twing.Size}:=3',
    'end.',
    '']);
    FindDeclarations(Code);
  finally
    if DotsUnit<>nil then
      DotsUnit.IsDeleted:=true;
  end;
end;

procedure TTestFindDeclaration.TestDirectoyCache_NS_FN_DottedUses;
var
  DotsUnit: TCodeBuffer;
  aUnitName, InFile, Filename: String;
begin
  AddNameSpace('nsA');
  DotsUnit:=CodeToolBoss.CreateFile('nsA.foo.bar.pp');
  try
    aUnitName:='foo.bar';
    InFile:='';
    Filename:=CodeToolBoss.DirectoryCachePool.FindUnitSourceInCompletePath('',aUnitName,InFile);
    AssertEquals('nsA.foo.bar.pp',Filename);
  finally
    DotsUnit.IsDeleted:=true;
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_NS_FN_DottedUses;
var
  DotsUnit: TCodeBuffer;
begin
  AddNameSpace('nsA');
  DotsUnit:=CodeToolBoss.CreateFile('nsA.foo.bar.pp');
  try
    DotsUnit.Source:=LinesToStr([
      'unit nsA.Foo.Bar;',
      'interface',
      'var Size: word;',
      'implementation',
      'end.']);

    StartUnit;
    Add([
    'implementation',
    'uses foo.bar;',
    'begin',
    '  Size{declaration:nsA.foo.bar.Size}:=1',
    '  foo.bar.Size{declaration:nsA.foo.bar.Size}:=2',
    'end.',
    '']);
    FindDeclarations(Code);
  finally
    DotsUnit.IsDeleted:=true;
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_NS_MultiDottedUses;
var
  RedGreenUnit, RedGreenBlueUnit, RedGreenBlueGrayUnit: TCodeBuffer;
begin
  RedGreenUnit:=CodeToolBoss.CreateFile('red.green.pp');
  RedGreenBlueUnit:=CodeToolBoss.CreateFile('red.green.blue.pp');
  RedGreenBlueGrayUnit:=CodeToolBoss.CreateFile('red.green.blue.gray.pp');
  try
    RedGreenUnit.Source:=LinesToStr([
      'unit Red.Green;',
      'interface',
      'var Two, Size, Red, Green, Blue, Gray: word;',
      'implementation',
      'end.']);
    RedGreenBlueUnit.Source:=LinesToStr([
      'unit Red.Green.Blue;',
      'interface',
      'var Three, Size, Red, Green, Blue, Gray: word;',
      'implementation',
      'end.']);
    RedGreenBlueGrayUnit.Source:=LinesToStr([
      'unit Red.Green.Blue.Gray;',
      'interface',
      'var One, Size, Red, Green, Blue, Gray: word;',
      'implementation',
      'end.']);

    Add([
    'unit Red;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses',
    '  Red.Green,',
    '  Red.Green.Blue,',
    '  Red.Green.Blue.Gray;',
    'var Size: word;',
    'implementation',
    'begin',
    '  Red{declaration:red.red:1}.Size:=1;',
    '  Red.Green{declaration!:red.red.green:5}.Size:=2;',
    '  Red{declaration!:red.red:5}.Green.Size:=3;',
    '  Red.Green.Blue{declaration:red.red.green.blue:6}.Size:=4;',
    '  Red.Green{declaration!:red.red.green:6}.Blue.Size:=5;',
    '  Red{declaration!:red.red:6}.Green.Blue.Size:=6;',
    '  Red.Green.Blue.Gray{declaration:red.red.green.blue.gray:7}.Size:=4;',
    'end.',
    '']);
    FindDeclarations(Code);
  finally
    RedGreenUnit.IsDeleted:=true;
    RedGreenBlueUnit.IsDeleted:=true;
    RedGreenBlueGrayUnit.IsDeleted:=true;
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_NS_MultiDottedPrg;
var
  RedUnit, RedRedUnit: TCodeBuffer;
begin
  RedUnit:=CodeToolBoss.CreateFile('red.pp');
  RedRedUnit:=CodeToolBoss.CreateFile('red.red.pp');
  try
    RedUnit.Source:=LinesToStr([
      'unit Red;',
      'interface',
      'var Red, RedCol: word;',
      'implementation',
      'end.']);
    RedRedUnit.Source:=LinesToStr([
      'unit Red.Red;',
      'interface',
      'var Red, RedCol: word;',
      'implementation',
      'end.']);

    Add([
    'program Red.',
    '  Red.',
    '  Red;',
    '{$mode objfpc}{$H+}',
    'uses',
    '  Red,',
    '  Red.',
    '    Red;',
    'var RedCol: word;',
    'begin',
    '  Red{declaration!:red.red.red.red:6}.RedCol{declaration!:red.redcol}:=1;',
    '  Red.Red{declaration!:red.red.red.red.red:8}.RedCol{declaration!:red.red.redcol}:=2;',
    '  Red{declaration!:red.red.red.red:7}.Red.RedCol:=3;',
    '  Red.Red.Red{declaration!:red.red.red:3}.RedCol{declaration!:redcol}:=4;',
    '  Red.Red{declaration!:red.red.red:2}.Red.RedCol:=5;',
    '  Red{declaration!:red.red.red:1}.Red.Red.RedCol:=6;',
    'end.',
    '']);
    FindDeclarations(Code);
  finally
    RedUnit.IsDeleted:=true;
    RedRedUnit.IsDeleted:=true;
  end;
end;

procedure TTestFindDeclaration.TestGatherIdentifier_NS_MultiDottedUses;
var
  RedUnit, RedRedUnit: TCodeBuffer;
begin
  RedUnit:=CodeToolBoss.CreateFile('red.pp');
  RedRedUnit:=CodeToolBoss.CreateFile('red.red.pp');
  try
    RedUnit.Source:=LinesToStr([
      'unit Red;',
      'interface',
      'var Red, RedCol, Roll: word;',
      'implementation',
      'end.']);
    RedRedUnit.Source:=LinesToStr([
      'unit Red.Red;',
      'interface',
      'var Red, RedCol, Run: word;',
      'implementation',
      'end.']);

    Add([
    'program Red.',
    '  Red.',
    '  Red;',
    '{$mode objfpc}{$H+}',
    'uses',
    '  Red,',
    '  Red.',
    '    Red;',
    'var RedCol, Rap: word;',
    'begin',
    '  R{completion:Red.Red.Red,Red,Red.Red,RedCol,Rap};',
    '  Red.R{completion:Red.Red,Red,RedCol,Roll};',
    '  Red.Red.R{completion:Red,RedCol,Run};',
    '  Red.Red.Red.R{completion:RedCol,Rap};',
    'end.',
    '']);
    FindDeclarations(Code);
  finally
    RedUnit.IsDeleted:=true;
    RedRedUnit.IsDeleted:=true;
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_Directive_OperatorIn;
begin
  StartProgram;
  Add([
  'type',
  '  { optimizer }',
  '  toptimizerswitch = (cs_opt_none,',
  '    cs_opt_use_load_modify_store',
  '  );',
  '  toptimizerswitches = set of toptimizerswitch;',
  'const',
  '  supported_optimizerswitches = [cs_opt_use_load_modify_store];',
  'var',
  '  o: TObject;',
  'begin',
  '{$if (cs_opt_use_load_modify_store in supported_optimizerswitches)}',
  '{$endif}',
  '  o.classinfo{declaration:System.TObject.ClassInfo}',
  'end.',
  '']);
  FindDeclarations(Code);
end;

procedure TTestFindDeclaration.TestFindDeclaration_FPCTests;
begin
  TestFiles('fpctests');
end;

procedure TTestFindDeclaration.TestFindDeclaration_LazTests;
begin
  TestFiles('laztests');
end;

procedure TTestFindDeclaration.TestFindDeclaration_LazTestsBugs;
begin
  TestFiles('laztests', 'b*.p*');
end;

initialization
  RegisterTests([TTestFindDeclaration]);
end.

