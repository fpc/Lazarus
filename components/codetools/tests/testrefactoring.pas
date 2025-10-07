{
 Test with:
   ./testcodetools --format=plain --suite=TTestRefactoring
   ./testcodetools --format=plain --suite=TestExplodeWith
}
unit TestRefactoring;

{$i runtestscodetools.inc}

interface

uses
  Classes, SysUtils, Contnrs, fpcunit, AVL_Tree,
  LazLogger, LazFileUtils, testregistry,
  CodeToolManager, CodeCache, CodeTree, BasicCodeTools, CTUnitGraph,
  FindDeclarationTool, ChangeDeclarationTool, CustomCodeTool, LinkScanner, TestGlobals,
  TestFinddeclaration;

const
  ExplodeWithMarker = 'explodewith:';
type

  { TCustomTestRefactoring }

  TCustomTestRefactoring = class(TCustomTestFindDeclaration)
  protected
    procedure RenameReferences(NewIdentifier: string; const Flags: TFindRefsFlags = []);
    procedure RenameSourceName(NewName, NewFilename: string);
    procedure RenameSourceName(NewName, NewFilename: string; const AddFiles: array of string);
    procedure RenameUsedUnitRefs(UsedUnit: TCodeBuffer; NewName, NewFilename: string); // only in Code, not in UsedUnit
    procedure RenameUsedUnitRefs(UsedUnit: TCodeBuffer; NewName, NewFilename: string; const AddFiles: array of string);
    procedure CheckDiff(CurCode: TCodeBuffer; const ExpLines: array of string);
    procedure CheckDiffStr(CurCode: TCodeBuffer; const ExpSrc: string);
  end;

  { TTestRefactoring }

  TTestRefactoring = class(TCustomTestRefactoring)
  private
  protected
    procedure TestRenameAlsoLFM(const RedUnitIntf, Form1IntfSrc, NewIdentifier,
      LFMSrc: string; const ExpLFMLines: array of string); virtual;
  published
    procedure TestExplodeWith;

    procedure TestIdentifierHasKeywords;

    procedure TestRenameVarReferences;
    procedure TestRenameProcReferences;
    procedure TestRenameProcedureArg;
    procedure TestRenameProcedureArgCaseSensitive;
    procedure TestRenameForwardProcedureArgDown;
    procedure TestRenameForwardProcedureArgUp;

    procedure TestRenameMethodArgDown;
    procedure TestRenameMethodArgUp;
    procedure TestRenameMethodInherited;
    procedure TestRenameMethodWithOverrides;
    procedure TestRenameMethodWithOverridesOtherUnit;
    procedure TestRenameClassMethodWithOverrides;

    procedure TestRenameNestedProgramProcDown;
    procedure TestRenameNestedProgramProcUp;
    procedure TestRenameNestedUnitProcDown;

    procedure TestRenameTypeToAmp;

    // rename program
    procedure TestRenameProgramName_Amp;
    procedure TestRenameProgramName_DottedSameCount;
    procedure TestRenameProgramName_MakeDotted;
    procedure TestRenameProgramName_DottedAppendThird;
    procedure TestRenameProgramName_DottedPrependThird;
    procedure TestRenameProgramName_DottedInsertThird;
    procedure TestRenameProgramName_DottedShortenStart;
    procedure TestRenameProgramName_DottedShortenMiddle;
    procedure TestRenameProgramName_DottedShortenEnd;
    procedure TestRenameProgramName_ToraToraTora;

    // rename unit
    procedure TestRenameUnitName_IncludeUsedTwiceInOneUnit;
    procedure TestRenameUnitName_IncludeUsedInTwoUnits;

    // rename uses
    procedure TestUseOmittedNamespace;
    procedure TestRenameUsedUnit_Amp;
    procedure TestRenameUsedUnit_Impl;
    procedure TestRenameUsedUnit_FN_KeepShort;
    procedure TestRenameUsedUnit_InFilename;
    procedure TestRenameUsedUnit_LongestUnitnameWins;

    // rename also in lfm
    procedure TestRenameAlsoLFM_Empty;
    procedure TestRenameAlsoLFM_Garbage;
    procedure TestRenameAlsoLFM_Variable;
    procedure TestRenameAlsoLFM_Event;
    procedure TestRenameAlsoLFM_SkipBinaryData;
    procedure TestRenameAlsoLFM_Property;
    procedure TestRenameAlsoLFM_Property_Typeless;
    procedure TestRenameAlsoLFM_DottedProperty;
    procedure TestRenameAlsoLFM_EnumProperty;
    procedure TestRenameAlsoLFM_SetProperty;
    procedure TestRenameAlsoLFM_AnonymousSetProperty;
    procedure TestRenameAlsoLFM_ComponentProperty;
    procedure TestRenameAlsoLFM_Property_FromOtherUnit;
    // todo: procedure TestRenameAlsoLFM_ComponentProperty_Foreign;  from another root component, e.g. DataModule
    // todo: procedure TestRenameAlsoLFM_CollectionProperty;
    procedure TestRenameAlsoLFM_ListProperty;
    procedure TestRenameAlsoLFM_ComponentClass;
    procedure TestRenameAlsoLFM_RootComponentClass;
    procedure TestRenameAlsoLFM_RootComponentName;
  end;

implementation

{ TCustomTestRefactoring }

procedure TCustomTestRefactoring.RenameReferences(NewIdentifier: string; const Flags: TFindRefsFlags
  );
var
  Marker: TFDMarker;
  Tool: TCodeTool;
  DeclX, DeclY, DeclTopLine, i, BlockTopLine, BlockBottomLine: integer;
  DeclCode, LFMCode, CurCode: TCodeBuffer;
  Files: TStringList;
  Graph: TUsesGraph;
  Completed: boolean;
  Node: TAVLTreeNode;
  UGUnit: TUGUnit;
  DeclarationCaretXY: TPoint;
  PascalReferences, LFMTreeOfPCodeXYPosition: TAVLTree;
  OldIdentifier, LFMFilename: string;
  LFMFindRefCache: TFindIdentifierReferenceCache;
  LFMReferences: TCodeXYPositions;
begin
  if not IsDottedIdentifier(NewIdentifier) then
    Fail('TCustomTestRefactoring.RenameReferences invalid NewName="'+NewIdentifier+'"');
  // find marker #Rename
  ParseSimpleMarkers(Code);
  if MarkerCount<1 then
    Fail('missing marker');
  if MarkerCount>1 then
    Fail('too many markers');
  Marker:=Markers[0];
  if Marker.Kind<>'#' then
    Fail('expected # marker, but found '+Marker.Kind);
  if not SameText(Marker.Name,'Rename') then
    Fail('expected marker #Rename, but found #'+Marker.Name);

  // find the main declaration
  if not CodeToolBoss.Explore(Code,Tool,true,false) then
    Fail('CodeToolBoss.Explore failed');
  Code.AbsoluteToLineCol(Marker.NameStartPos,DeclarationCaretXY.Y,DeclarationCaretXY.X);
  if not CodeToolBoss.FindDeclaration(Code,
    DeclarationCaretXY.X,DeclarationCaretXY.Y,
    DeclCode,DeclX,DeclY,DeclTopLine,BlockTopLine,BlockBottomLine,
    [fsfFindMainDeclaration,fsfSkipPropertyWithoutType]) then
  begin
    Fail('CodeToolBoss.FindDeclaration failed '+dbgs(DeclarationCaretXY)+' File='+Code.Filename);
  end;

  debugln(['TCustomTestRefactoring.RenameReferences X=',DeclX,' Y=',DeclY,' "',DeclCode.GetLine(DeclY-1,false),'"']);

  DeclarationCaretXY:=Point(DeclX,DeclY);

  CodeToolBoss.GetIdentifierAt(DeclCode,DeclarationCaretXY.X,DeclarationCaretXY.Y,OldIdentifier);

  // create the file list
  Files:=TStringList.Create;
  Graph:=nil;
  PascalReferences:=nil;
  LFMReferences:=nil;
  LFMFindRefCache:=nil;
  LFMTreeOfPCodeXYPosition:=nil;
  try
    Files.Add(DeclCode.Filename);
    if CompareFilenames(DeclCode.Filename,Code.Filename)<>0 then
      Files.Add(Code.Filename);

    Graph:=CodeToolBoss.CreateUsesGraph;
    Graph.AddStartUnit(Code.Filename);
    Graph.AddTargetUnit(DeclCode.Filename);
    Graph.Parse(true,Completed);
    Node:=Graph.FilesTree.FindLowest;
    Files.Clear;
    while Node<>nil do begin
      UGUnit:=TUGUnit(Node.Data);
      //debugln(['TCustomTestRefactoring.RenameReferences ',UGUnit.Filename]);
      Files.Add(UGUnit.Filename);
      Node:=Node.Successor;
    end;

    // search pascal source references
    if not CodeToolBoss.FindReferencesInFiles(Files,DeclCode,
        DeclarationCaretXY,true,PascalReferences,Flags) then begin
      Fail('CodeToolBoss.FindReferencesInFiles 20250515155115 failed at '+dbgs(DeclarationCaretXY)+' File='+Code.Filename);
    end;

    // todo: check for conflicts

    if frfIncludingLFM in Flags then begin
      if not CodeToolBoss.UpdateFindIdentifierRefCache(
             DeclCode,DeclarationCaretXY.X,DeclarationCaretXY.Y,LFMFindRefCache)
          or (LFMFindRefCache.NewNode=nil) then begin
        Fail('CodeToolBoss.UpdateFindIdentifierRefCache 20250515155111 failed at '+dbgs(DeclarationCaretXY)+' File='+Code.Filename);
      end;

      for i:=0 to Files.Count-1 do begin
        CurCode:=CodeToolBoss.FindFile(Files[i]);
        if CurCode=nil then
          Fail('CodeToolBoss.FindReferencesInFiles 20250515144047 source lost: "'+Files[i]+'"');
        LFMFilename:=ChangeFileExt(CurCode.Filename,'.lfm');
        LFMCode:=CodeToolBoss.FindFile(LFMFilename);
        if (LFMCode=nil) or LFMCode.IsDeleted then continue;

        if not CodeToolBoss.FindLFMReferences(LFMFindRefCache.NewPos.Code,LFMFindRefCache.NewPos.X,LFMFindRefCache.NewPos.Y,
            CurCode,LFMCode,LFMReferences,LFMFindRefCache) then
          Fail('CodeToolBoss.FindLFMReferences 20250515155330 failed for lfm: "'+LFMCode.Filename+'"');
      end;

      if (LFMReferences<>nil) and (LFMReferences.Count>0) then begin
        LFMTreeOfPCodeXYPosition:=CreateTreeOfPCodeXYPosition;
        for i:=0 to LFMReferences.Count-1 do
          LFMTreeOfPCodeXYPosition.Add(LFMReferences.Items[i]);

        if not CodeToolBoss.RenameIdentifierInLFMs(LFMTreeOfPCodeXYPosition,
          OldIdentifier, NewIdentifier) then
            Fail('TCustomTestRefactoring.RenameReferences in LFM failed');
      end;
    end;

    if not CodeToolBoss.RenameIdentifier(PascalReferences,
      OldIdentifier, NewIdentifier, DeclCode, @DeclarationCaretXY)
    then
      Fail('CodeToolBoss.RenameIdentifier failed');

  finally
    CodeToolBoss.FreeTreeOfPCodeXYPosition(PascalReferences);
    Graph.Free;
    Files.Free;
    LFMFindRefCache.Free;
    LFMTreeOfPCodeXYPosition.Free;
  end;
end;

procedure TCustomTestRefactoring.RenameSourceName(NewName, NewFilename: string);
begin
  RenameSourceName(NewName,NewFilename,[]);
end;

procedure TCustomTestRefactoring.RenameSourceName(NewName, NewFilename: string;
  const AddFiles: array of string);
var
  Files: TStringList;
  ListOfSrcNameRefs: TObjectList;
  i: Integer;
begin
  // create the file list
  ListOfSrcNameRefs:=nil;
  Files:=TStringList.Create;
  try
    // search pascal source references in Code
    Files.Add(Code.Filename);
    for i:=0 to length(AddFiles)-1 do
      Files.Add(AddFiles[i]);

    if not CodeToolBoss.FindSourceNameReferences(Code.Filename,Files,false,ListOfSrcNameRefs) then
    begin
      Fail('CodeToolBoss.FindSourceNameReferences failed File='+Code.Filename);
    end;
    // rename
    if not CodeToolBoss.RenameSourceNameReferences(Code.Filename,NewFilename,NewName,ListOfSrcNameRefs)
    then
      Fail('CodeToolBoss.RenameSourceNameReferences failed');
  finally
    ListOfSrcNameRefs.Free;
    Files.Free;
  end;
end;

procedure TCustomTestRefactoring.RenameUsedUnitRefs(UsedUnit: TCodeBuffer; NewName,
  NewFilename: string);
var
  Files: TStringList;
  ListOfSrcNameRefs: TObjectList;
begin
  // create the file list
  ListOfSrcNameRefs:=nil;
  Files:=TStringList.Create;
  try
    // search pascal source references in Code
    Files.Add(Code.Filename);
    if not CodeToolBoss.FindSourceNameReferences(UsedUnit.Filename,Files,false,ListOfSrcNameRefs) then
    begin
      Fail('CodeToolBoss.FindSourceNameReferences failed File='+Code.Filename);
    end;
    // rename
    if not CodeToolBoss.RenameSourceNameReferences(UsedUnit.Filename,NewFilename,NewName,ListOfSrcNameRefs)
    then
      Fail('CodeToolBoss.RenameSourceNameReferences failed');
  finally
    ListOfSrcNameRefs.Free;
    Files.Free;
  end;
end;

procedure TCustomTestRefactoring.RenameUsedUnitRefs(UsedUnit: TCodeBuffer; NewName,
  NewFilename: string; const AddFiles: array of string);
var
  Files: TStringList;
  ListOfSrcNameRefs: TObjectList;
  i: Integer;
begin
  // create the file list
  ListOfSrcNameRefs:=nil;
  Files:=TStringList.Create;
  try
    // search pascal source references in Code
    Files.Add(UsedUnit.Filename);
    Files.Add(Code.Filename);
    for i:=0 to length(AddFiles)-1 do
      Files.Add(AddFiles[i]);
    if not CodeToolBoss.FindSourceNameReferences(UsedUnit.Filename,Files,false,ListOfSrcNameRefs) then
    begin
      Fail('CodeToolBoss.FindSourceNameReferences failed File='+Code.Filename);
    end;
    // rename
    if not CodeToolBoss.RenameSourceNameReferences(UsedUnit.Filename,NewFilename,NewName,ListOfSrcNameRefs)
    then
      Fail('CodeToolBoss.RenameSourceNameReferences failed');
  finally
    ListOfSrcNameRefs.Free;
    Files.Free;
  end;
end;

procedure TCustomTestRefactoring.CheckDiff(CurCode: TCodeBuffer;
  const ExpLines: array of string);
var
  CurLine: String;
  i: Integer;
  Differ: Boolean;
begin
  //debugln(['TCustomTestRefactoring.CheckDiff ',CurCode.Filename,' ',length(ExpLines)]);
  if High(ExpLines)=CurCode.LineCount-1 then begin
    Differ:=false;
    for i:=0 to High(ExpLines) do begin
      if ExpLines[i]<>CurCode.GetLine(i,false) then
        Differ:=true;
    end;
    if not Differ then exit;
  end;

  debugln('TCustomTestRefactoring.CheckDiff Expected=');
  for i:=0 to High(ExpLines) do
    debugln('  ',ExpLines[i]);
  debugln('TCustomTestRefactoring.CheckDiff Found=');
  for i:=0 to CurCode.LineCount-1 do
    debugln('  ',CurCode.GetLine(i,false));

  debugln('TCustomTestRefactoring.CheckDiff Diff=');
  for i:=0 to High(ExpLines) do begin
    if i>=CurCode.LineCount then begin
      debugln('  Expec: ',ExpLines[i]);
      debugln('  Found: ');
    end else begin
      CurLine:=CurCode.GetLine(i,false);
      if ExpLines[i]<>CurLine then begin
        debugln('  Expec: ',ExpLines[i]);
        debugln('  Found: ',CurLine);
      end else begin
        debugln('       : ',ExpLines[i]);
      end;
    end;
  end;
  for i:=High(ExpLines)+1 to CurCode.LineCount-1 do begin
    debugln('>>Expec: ');
    debugln('<<Found: ',CurCode.GetLine(i,false));
  end;

  Fail('TCustomTestRefactoring.CheckDiff ');
end;

procedure TCustomTestRefactoring.CheckDiffStr(CurCode: TCodeBuffer; const ExpSrc: string);
var
  ExpLines: TStringList;
  Differ: Boolean;
  i: Integer;
  CurLine: String;
begin
  if (ExpSrc='') and (CurCode.Source='') then exit;
  ExpLines:=TStringList.Create;
  try
    ExpLines.Text:=ExpSrc;
    //debugln(['TCustomTestRefactoring.CheckDiffStr ',CurCode.Filename,' ',length(ExpLines)]);
    if ExpLines.Count=CurCode.LineCount then begin
      Differ:=false;
      for i:=0 to ExpLines.Count-1 do begin
        if ExpLines[i]<>CurCode.GetLine(i,false) then
          Differ:=true;
      end;
      if not Differ then exit;
    end;

    debugln('TCustomTestRefactoring.CheckDiffStr Expected=');
    for i:=0 to ExpLines.Count-1 do
      debugln('  ',ExpLines[i]);
    debugln('TCustomTestRefactoring.CheckDiffStr Found=');
    for i:=0 to CurCode.LineCount-1 do
      debugln('  ',CurCode.GetLine(i,false));

    debugln('TCustomTestRefactoring.CheckDiffStr Diff=');
    for i:=0 to ExpLines.Count-1 do begin
      if i>=CurCode.LineCount then begin
        debugln('  Expec: ',ExpLines[i]);
        debugln('  Found: ');
      end else begin
        CurLine:=CurCode.GetLine(i,false);
        if ExpLines[i]<>CurLine then begin
          debugln('  Expec: ',ExpLines[i]);
          debugln('  Found: ',CurLine);
        end else begin
          debugln('       : ',ExpLines[i]);
        end;
      end;
    end;
    for i:=ExpLines.Count to CurCode.LineCount-1 do begin
      debugln('>>Expec: ');
      debugln('<<Found: ',CurCode.GetLine(i,false));
    end;

    Fail('TCustomTestRefactoring.CheckDiff ');
  finally
    ExpLines.Free;
  end;
end;

{ TTestRefactoring }

procedure TTestRefactoring.TestRenameAlsoLFM(const RedUnitIntf, Form1IntfSrc, NewIdentifier,
  LFMSrc: string; const ExpLFMLines: array of string);
var
  Test1LFM, RedUnit: TCodeBuffer;
begin
  RedUnit:=CodeToolBoss.CreateFile('red.pas');
  Test1LFM:=CodeToolBoss.CreateFile(ChangeFileExt(Code.Filename,'.lfm'));
  try
    RedUnit.Source:=
      'unit Red;'+LineEnding
      +'interface'+LineEnding
      +RedUnitIntf
      +'implementation'+LineEnding
      +'end.'+LineEnding;

    Test1LFM.Source:=LFMSrc;

    Add('unit Test1;'+LineEnding
      +'{$mode objfpc}{$H+}'+LineEnding
      +'interface'+LineEnding
      +'uses red;'+LineEnding
      +'type'+LineEnding
      +'  TForm1 = class(TForm)'+LineEnding
      +Form1IntfSrc
      +'  end;'+LineEnding
      +'implementation'+LineEnding
      +'end.'+LineEnding);

    RenameReferences(NewIdentifier,[frfIncludingLFM]);
    CheckDiff(Test1LFM,ExpLFMLines);

  finally
    RedUnit.IsDeleted:=true;
    Test1LFM.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestExplodeWith;
type
  TWithBlock = record
    CodeXYPos: TCodeXYPosition;
    WithExpr: string;
    StatementStartPos: integer;
    StatementEndPos: integer;
  end;
  PWithBlock = ^TWithBlock;
var
  CurCode: TCodeBuffer;
  Tool: TCodeTool;
  Node, StatementNode: TCodeTreeNode;
  CodeXYPos: TCodeXYPosition;
  ListOfWiths: array of TWithBlock;
  i, NewStartPos, NewEndPos, p, CommentStartPos, CommentEndPos: Integer;
  Filename, OldSource, Src, ID, ExpectedInsertion: String;
  aWith: PWithBlock;
begin
  Filename:=ExpandFileNameUTF8('moduletests/rt_explodewith.pas');
  CurCode:=CodeToolBoss.LoadFile(Filename,true,false);
  AssertEquals('Load file error: '+Filename,true,CurCode<>nil);
  if not CodeToolBoss.Explore(CurCode,Tool,true) then
    AssertEquals('Parse error: ','',CodeToolBoss.ErrorMessage);
  // collect all With-Blocks
  Node:=Tool.Tree.Root;
  SetLength(ListOfWiths{%H-},0);
  while Node<>nil do begin
    if Node.Desc=ctnWithVariable then begin
      Tool.CleanPosToCaret(Node.StartPos,CodeXYPos);
      StatementNode:=Tool.FindWithBlockStatement(Node);
      if StatementNode<>nil then begin
        SetLength(ListOfWiths,length(ListOfWiths)+1);
        aWith:=@ListOfWiths[High(ListOfWiths)];
        aWith^.CodeXYPos:=CodeXYPos;
        aWith^.WithExpr:=Tool.ExtractWithBlockExpression(Node,[]);
        aWith^.StatementStartPos:=FindPrevNonSpace(CurCode.Source,StatementNode.StartPos);
        aWith^.StatementEndPos:=StatementNode.EndPos;
      end;
    end;
    Node:=Node.Next;
  end;

  for i:=0 to High(ListOfWiths) do begin
    aWith:=@ListOfWiths[i];
    CodeXYPos:=aWith^.CodeXYPos;
    //debugln(['TTestRefactoring.TestExplodeWith ',dbgs(CodeXYPos)]);
    OldSource:=CurCode.Source;
    try
      if CodeToolBoss.RemoveWithBlock(CurCode,CodeXYPos.X,CodeXYPos.Y) then begin
        // success
        // => check changes
        // get new bounds
        NewStartPos:=aWith^.StatementStartPos;
        NewEndPos:=aWith^.StatementEndPos;
        CurCode.AdjustPosition(NewStartPos);
        CurCode.AdjustPosition(NewEndPos);
        if (NewStartPos<1) or (NewStartPos>CurCode.SourceLength)
        or (NewEndPos<1) or (NewEndPos>CurCode.SourceLength)
        or (NewEndPos<NewStartPos)
        then begin
          debugln(['TTestRefactoring.TestExplodeWith WrongCode: ']);
          debugln(CurCode.Source);
          Fail('CodeToolBoss.RemoveWithBlock failed at '+dbgs(CodeXYPos));
        end;
        // check each marker
        Src:=CurCode.Source;
        //debugln(['TTestRefactoring.TestExplodeWith NewBlock=',copy(Src,NewStartPos,NewEndPos-NewStartPos)]);
        p:=NewStartPos;
        repeat
          CommentStartPos:=FindNextComment(Src,p,NewEndPos);
          if CommentStartPos>=NewEndPos then break;
          p:=CommentStartPos;
          CommentEndPos:=FindCommentEnd(Src,CommentStartPos,Tool.Scanner.NestedComments);
          if Src[p]='{' then begin
            inc(p);
            if copy(Src,p,length(ExplodeWithMarker))=ExplodeWithMarker then begin
              inc(p,length(ExplodeWithMarker));
              ID:=copy(Src,p,CommentEndPos-p-1);
              if ID=aWith^.WithExpr then begin
                // this marker expects an insertion
                ExpectedInsertion:=Id+'.';
                if copy(Src,CommentEndPos,length(ExpectedInsertion))<>ExpectedInsertion
                then begin
                  Fail('CodeToolBoss.RemoveWithBlock failed at '+dbgs(CodeXYPos)
                    +': Expected insertion "'+ExpectedInsertion+'"'
                    +' at '+CurCode.AbsoluteToLineColStr(CommentEndPos)
                    +', but found "'+dbgstr(Src,CommentStartPos,20)+'"');
                end;
              end;
            end;
          end;
          p:=CommentEndPos;
        until false;


      end else begin
        Fail('CodeToolBoss.RemoveWithBlock failed at '+dbgs(CodeXYPos)+': '+CodeToolBoss.ErrorMessage);
      end;
    finally
      CurCode.Source:=OldSource;
    end;
  end;
end;

procedure TTestRefactoring.TestIdentifierHasKeywords;

  procedure t(const Identifier: string;
    cm: TCompilerMode; const ExpectedAmp: string);
  var
    AmpIdentifier: string;
    r: Boolean;
  begin
    r:=IdentifierHasKeywords(Identifier, cm, AmpIdentifier);
    if AmpIdentifier<>ExpectedAmp then
      Fail('Identifier="'+Identifier+'" cm='+CompilerModeNames[cm]+' expected "'+ExpectedAmp+'", but got "'+AmpIdentifier+'"');
    AssertEquals('Result',Identifier<>AmpIdentifier,r);
  end;

begin
  t('a',cmFPC,'a');
  t('a.b',cmFPC,'a.b');
  t('a.&b',cmFPC,'a.&b');
  t('a.Type',cmFPC,'a.&Type');
  t('End.Type',cmFPC,'&End.&Type');
end;

procedure TTestRefactoring.TestRenameVarReferences;
begin
  StartProgram;
  Add([
  'var Cow: longint;',
  'begin',
  '  cow{#Rename}:=3;',
  '  test1.cow:=4;',
  'end.',
  '']);
  RenameReferences('Bird');
  CheckDiff(Code,[
  'program test1;',
  '',
  '{$mode objfpc}{$H+}',
  '',
  'var Bird: longint;',
  'begin',
  '  Bird{#Rename}:=3;',
  '  test1.Bird:=4;',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameProcReferences;
begin
  StartProgram;
  Add([
  'procedure Cow;',
  'begin',
  'end;',
  '',
  'begin',
  '  cow{#Rename};',
  '  p:=@Cow;',
  '  test1.cow;',
  '  p:=@test1.Cow;',
  'end.',
  '']);
  RenameReferences('Bird');
  CheckDiff(Code,[
  'program test1;',
  '',
  '{$mode objfpc}{$H+}',
  '',
  'procedure Bird;',
  'begin',
  'end;',
  '',
  'begin',
  '  Bird{#Rename};',
  '  p:=@Bird;',
  '  test1.Bird;',
  '  p:=@test1.Bird;',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameProcedureArg;
begin
  StartProgram;
  Add([
  'procedure Fly(Size{#Rename}: word);',
  '',
  '  procedure Sub1;',
  '  var Size: byte;',
  '  begin',
  '    Size:=3;',
  '  end;',
  '',
  '  procedure Sub2(Size: word);',
  '  begin',
  '    Size:=4;',
  '  end;',
  'begin',
  '  Size:=Size+1;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
  RenameReferences('Bird');
  CheckDiff(Code,[
  'program test1;',
  '',
  '{$mode objfpc}{$H+}',
  '',
  'procedure Fly(Bird{#Rename}: word);',
  '',
  '  procedure Sub1;',
  '  var Size: byte;',
  '  begin',
  '    Size:=3;',
  '  end;',
  '',
  '  procedure Sub2(Size: word);',
  '  begin',
  '    Size:=4;',
  '  end;',
  'begin',
  '  Bird:=Bird+1;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameProcedureArgCaseSensitive;
begin
  StartProgram;
  Add([
  'procedure Fly(Size{#Rename}: word);',
  '',
  '  procedure Sub1;',
  '  var Size: byte;',
  '  begin',
  '    Size:=3;',
  '  end;',
  '',
  '  procedure Sub2(Size: word);',
  '  begin',
  '    Size:=4;',
  '  end;',
  'begin',
  '  Size:=Size+1;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
  RenameReferences('siZe');
  CheckDiff(Code,[
  'program test1;',
  '',
  '{$mode objfpc}{$H+}',
  '',
  'procedure Fly(siZe{#Rename}: word);',
  '',
  '  procedure Sub1;',
  '  var Size: byte;',
  '  begin',
  '    Size:=3;',
  '  end;',
  '',
  '  procedure Sub2(Size: word);',
  '  begin',
  '    Size:=4;',
  '  end;',
  'begin',
  '  siZe:=siZe+1;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameForwardProcedureArgDown;
begin
  StartProgram;
  Add([
  'procedure Fly(Size{#Rename}: word); forward;',
  '',
  'procedure Fly(Size: word);',
  'begin',
  '  Size:=Size+1;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
  RenameReferences('Bird');
  CheckDiff(Code,[
  'program test1;',
  '',
  '{$mode objfpc}{$H+}',
  '',
  'procedure Fly(Bird{#Rename}: word); forward;',
  '',
  'procedure Fly(Bird: word);',
  'begin',
  '  Bird:=Bird+1;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameForwardProcedureArgUp;
begin
  StartProgram;
  Add([
  'procedure Fly(Size: word); forward;',
  '',
  'procedure Fly(Size{#Rename}: word);',
  'begin',
  '  Size:=Size+1;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
  RenameReferences('Bird');
  CheckDiff(Code,[
  'program test1;',
  '',
  '{$mode objfpc}{$H+}',
  '',
  'procedure Fly(Bird: word); forward;',
  '',
  'procedure Fly(Bird{#Rename}: word);',
  'begin',
  '  Bird:=Bird+1;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameMethodArgDown;
begin
  StartProgram;
  Add([
  'type',
  '  TBird = class',
  '    procedure Fly(Size{#Rename}: word);',
  '  end;',
  '',
  'procedure TBird.Fly(Size: word);',
  'begin',
  '  Size:=Size+1;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
  RenameReferences('Width');
  CheckDiff(Code,[
  'program test1;',
  '',
  '{$mode objfpc}{$H+}',
  '',
  'type',
  '  TBird = class',
  '    procedure Fly(Width{#Rename}: word);',
  '  end;',
  '',
  'procedure TBird.Fly(Width: word);',
  'begin',
  '  Width:=Width+1;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameMethodArgUp;
begin
  StartProgram;
  Add([
  'type',
  '  TBird = class',
  '    procedure Fly(Size: word);',
  '  end;',
  '',
  'procedure TBird.Fly(Size{#Rename}: word);',
  'begin',
  '  Size:=Size+1;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
  RenameReferences('Width');
  CheckDiff(Code,[
  'program test1;',
  '',
  '{$mode objfpc}{$H+}',
  '',
  'type',
  '  TBird = class',
  '    procedure Fly(Width: word);',
  '  end;',
  '',
  'procedure TBird.Fly(Width{#Rename}: word);',
  'begin',
  '  Width:=Width+1;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameMethodInherited;
begin
  StartProgram;
  Add([
  'type',
  '  TAnimal = class',
  '    procedure Fly{#Rename}; virtual;',
  '  end;',
  '  TBird = class(TAnimal)',
  '    procedure Fly; override;',
  '  end;',
  '',
  'procedure TAnimal.Fly;',
  'begin',
  'end;',
  '',
  'procedure TBird.Fly;',
  'begin',
  '  inherited Fly;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
  RenameReferences('Run');
  CheckDiff(Code,[
  'program test1;',
  '',
  '{$mode objfpc}{$H+}',
  '',
  'type',
  '  TAnimal = class',
  '    procedure Run{#Rename}; virtual;',
  '  end;',
  '  TBird = class(TAnimal)',
  '    procedure Fly; override;',
  '  end;',
  '',
  'procedure TAnimal.Run;',
  'begin',
  'end;',
  '',
  'procedure TBird.Fly;',
  'begin',
  '  inherited Run;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameMethodWithOverrides;
begin
  StartProgram;
  Add([
  'type',
  '  TAnimal = class',
  '    procedure Fly{#Rename}; virtual;',
  '  end;',
  '  TFlying = class(TAnimal)',
  '  end;',
  '  TBird = class(TFlying)',
  '    procedure Eat;',
  '    procedure Fly; override;',
  '  end;',
  '',
  'procedure TAnimal.Fly;',
  'begin',
  'end;',
  '',
  'procedure TBird.Eat;',
  'begin',
  '  inherited Fly;',
  '  Fly;',
  '  // Fly',
  'end;',
  '',
  'procedure TBird.Fly;',
  'begin',
  '  inherited Fly;',
  '  Fly;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
  RenameReferences('Run',[frfMethodOverrides]);
  CheckDiff(Code,[
  'program test1;',
  '',
  '{$mode objfpc}{$H+}',
  '',
  'type',
  '  TAnimal = class',
  '    procedure Run{#Rename}; virtual;',
  '  end;',
  '  TFlying = class(TAnimal)',
  '  end;',
  '  TBird = class(TFlying)',
  '    procedure Eat;',
  '    procedure Run; override;',
  '  end;',
  '',
  'procedure TAnimal.Run;',
  'begin',
  'end;',
  '',
  'procedure TBird.Eat;',
  'begin',
  '  inherited Run;',
  '  Run;',
  '  // Run',
  'end;',
  '',
  'procedure TBird.Run;',
  'begin',
  '  inherited Run;',
  '  Run;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameMethodWithOverridesOtherUnit;
var
  DeclUnit: TCodeBuffer;
begin
  DeclUnit:=nil;
  try
    DeclUnit:=CodeToolBoss.CreateFile('decl.pp');
    DeclUnit.Source:='unit Decl;'+LineEnding
      +'interface'+LineEnding
      +'type'+LineEnding
      +'  TAnimal = class'+LineEnding
      +'    procedure Walk(a: word); virtual; abstract;'+LineEnding
      +'  end;'+LineEnding
      +'  TBird = class(TAnimal)'+LineEnding
      +'    procedure Walk(b: longint); virtual; abstract;'+LineEnding
      +'    procedure Walk(a: word); override;'+LineEnding
      +'  end;'+LineEnding
      +'implementation'+LineEnding
      +'procedure TBird.Walk(a: word);'+LineEnding
      +'begin end;'+LineEnding
      +'end.';

    StartUnit;
    Add([
    'uses Decl;',
    'type',
    '  TBear = class(TAnimal)',
    '    procedure Charge;',
    '  end;',
    '  TEagle = class(TBird)',
    '    procedure Walk(c: int64);',
    '    procedure Walk(a: word); override;',
    '  end;',
    '  TBig = class(TEagle)',
    '    procedure Walk(b: longint); override;',
    '    procedure Walk(a: word); override;',
    '  end;',
    'implementation',
    '',
    'procedure TBear.Charge;',
    'var aWord: word;',
    'begin',
    '  Walk{#Rename}(aWord);',
    'end;',
    '',
    'procedure TEagle.Walk(c: int64);',
    'begin',
    '  Walk(c);',
    '  Walk(word(c));',
    'end;',
    '',
    'procedure TEagle.Walk(a: word);',
    'begin',
    '  Walk(c);',
    '  Walk(word(c));',
    'end;',
    '',
    'procedure TBig.Walk(b: longint);',
    'begin',
    '  Walk(b);',
    '  Walk(word(b));',
    'end;',
    '',
    'procedure TBig.Walk(a: word);',
    'begin',
    '  Walk(a);',
    '  Walk(longint(a));',
    'end;',
    '',
    'end.',
    '']);
    RenameReferences('Run',[frfMethodOverrides]);
    CheckDiff(Code,[
    'unit test1;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses Decl;',
    'type',
    '  TBear = class(TAnimal)',
    '    procedure Charge;',
    '  end;',
    '  TEagle = class(TBird)',
    '    procedure Walk(c: int64);',
    '    procedure Run(a: word); override;',
    '  end;',
    '  TBig = class(TEagle)',
    '    procedure Walk(b: longint); override;',
    '    procedure Run(a: word); override;',
    '  end;',
    'implementation',
    '',
    'procedure TBear.Charge;',
    'var aWord: word;',
    'begin',
    '  Run{#Rename}(aWord);',
    'end;',
    '',
    'procedure TEagle.Walk(c: int64);',
    'begin',
    '  Walk(c);',
    '  Run(word(c));',
    'end;',
    '',
    'procedure TEagle.Run(a: word);',
    'begin',
    '  Walk(c);',
    '  Run(word(c));',
    'end;',
    '',
    'procedure TBig.Walk(b: longint);',
    'begin',
    '  Walk(b);',
    '  Run(word(b));',
    'end;',
    '',
    'procedure TBig.Run(a: word);',
    'begin',
    '  Run(a);',
    '  Walk(longint(a));',
    'end;',
    '',
    'end.',
    '']);
  finally
    if DeclUnit<>nil then
      DeclUnit.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameClassMethodWithOverrides;
begin
  StartProgram;
  Add([
  'type',
  '  TOuter = class',
  '  public type',
  '    TAnimal = class',
  '      class procedure Fly{#Rename}; virtual;',
  '    end;',
  '    TBird = class(TAnimal)',
  '      class procedure Eat;',
  '      class procedure Fly; override;',
  '    end;',
  '  end;',
  '',
  'class procedure TOuter.TAnimal.Fly;',
  'begin',
  'end;',
  '',
  'class procedure TOuter.TBird.Eat;',
  'begin',
  '  TOuter.TAnimal.Fly;',
  '  TOuter.TBird.Fly;',
  '  Test1.TOuter.TAnimal.Fly;',
  '  Test1.TOuter.TBird.Fly;',
  '  // TOuter.TAnimal.Fly',
  '  // TOuter.TBird.Fly',
  '  // Test1.TOuter.TAnimal.Fly;',
  '  // Test1.TOuter.TBird.Fly;',
  'end;',
  '',
  'class procedure TOuter.TBird.Fly;',
  'begin',
  'end;',
  '',
  'begin',
  'end.',
  '']);
  RenameReferences('Run',[frfMethodOverrides]);
  CheckDiff(Code,[
  'program test1;',
  '',
  '{$mode objfpc}{$H+}',
  '',
  'type',
  '  TOuter = class',
  '  public type',
  '    TAnimal = class',
  '      class procedure Run{#Rename}; virtual;',
  '    end;',
  '    TBird = class(TAnimal)',
  '      class procedure Eat;',
  '      class procedure Run; override;',
  '    end;',
  '  end;',
  '',
  'class procedure TOuter.TAnimal.Run;',
  'begin',
  'end;',
  '',
  'class procedure TOuter.TBird.Eat;',
  'begin',
  '  TOuter.TAnimal.Run;',
  '  TOuter.TBird.Run;',
  '  Test1.TOuter.TAnimal.Run;',
  '  Test1.TOuter.TBird.Run;',
  '  // TOuter.TAnimal.Run',
  '  // TOuter.TBird.Run',
  '  // Test1.TOuter.TAnimal.Run;',
  '  // Test1.TOuter.TBird.Run;',
  'end;',
  '',
  'class procedure TOuter.TBird.Run;',
  'begin',
  'end;',
  '',
  'begin',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameNestedProgramProcDown;
begin
  StartProgram;
  Add([
  'type',
  '  TBird = class',
  '    procedure Fly;',
  '    procedure Run;',
  '  end;',
  '',
  'procedure TBird.Fly;',
  '  procedure Sub{#Rename}; forward;',
  '  procedure Sub;',
  '  begin',
  '  end;',
  'begin',
  '  Sub;',
  'end;',
  '',
  'procedure TBird.Run;',
  '  procedure Sub;',
  '  begin',
  '  end;',
  'begin',
  '  Sub;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
  RenameReferences('Glide');
  CheckDiff(Code,[
  'program test1;',
  '',
  '{$mode objfpc}{$H+}',
  '',
  'type',
  '  TBird = class',
  '    procedure Fly;',
  '    procedure Run;',
  '  end;',
  '',
  'procedure TBird.Fly;',
  '  procedure Glide{#Rename}; forward;',
  '  procedure Glide;',
  '  begin',
  '  end;',
  'begin',
  '  Glide;',
  'end;',
  '',
  'procedure TBird.Run;',
  '  procedure Sub;',
  '  begin',
  '  end;',
  'begin',
  '  Sub;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameNestedProgramProcUp;
begin
  StartProgram;
  Add([
  'type',
  '  TBird = class',
  '    procedure Fly;',
  '    procedure Run;',
  '  end;',
  '',
  'procedure TBird.Fly;',
  '',
  '  procedure Sub; forward;',
  '  procedure Sub{#Rename};',
  '  begin',
  '  end;',
  'begin',
  '  Sub;',
  'end;',
  '',
  'procedure TBird.Run;',
  '  procedure Sub;',
  '  begin',
  '  end;',
  'begin',
  '  Sub;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
  RenameReferences('Glide');
  CheckDiff(Code,[
  'program test1;',
  '',
  '{$mode objfpc}{$H+}',
  '',
  'type',
  '  TBird = class',
  '    procedure Fly;',
  '    procedure Run;',
  '  end;',
  '',
  'procedure TBird.Fly;',
  '',
  '  procedure Glide; forward;',
  '  procedure Glide{#Rename};',
  '  begin',
  '  end;',
  'begin',
  '  Glide;',
  'end;',
  '',
  'procedure TBird.Run;',
  '  procedure Sub;',
  '  begin',
  '  end;',
  'begin',
  '  Sub;',
  'end;',
  '',
  'begin',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameNestedUnitProcDown;
begin
  StartUnit;
  Add([
  'type',
  '  TBird = class',
  '    procedure Fly;',
  '    procedure Run;',
  '  end;',
  '',
  'implementation',
  '',
  'procedure TBird.Fly;',
  '',
  '  procedure Sub; forward;',
  '  procedure Sub{#Rename};',
  '  begin',
  '  end;',
  'begin',
  '  Sub;',
  'end;',
  '',
  'procedure TBird.Run;',
  '  procedure Sub;',
  '  begin',
  '  end;',
  'begin',
  '  Sub;',
  'end;',
  '',
  'begin',
  'end.',
  '',
  'end.',
  '']);
  RenameReferences('Glide');
  CheckDiff(Code,[
  'unit test1;',
  '',
  '{$mode objfpc}{$H+}',
  '',
  'interface',
  '',
  'type',
  '  TBird = class',
  '    procedure Fly;',
  '    procedure Run;',
  '  end;',
  '',
  'implementation',
  '',
  'procedure TBird.Fly;',
  '',
  '  procedure Glide; forward;',
  '  procedure Glide{#Rename};',
  '  begin',
  '  end;',
  'begin',
  '  Glide;',
  'end;',
  '',
  'procedure TBird.Run;',
  '  procedure Sub;',
  '  begin',
  '  end;',
  'begin',
  '  Sub;',
  'end;',
  '',
  'begin',
  'end.',
  '',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameTypeToAmp;
begin
  StartUnit;
  Add([
  'type',
  '  TFoo{#Rename} = word;',
  '  TBar = low(TFoo)..high(TFoo);',
  'implementation',
  'type',
  '  TBird = low(TFoo)..high(TFoo);',
  'end.',
  '']);
  RenameReferences('&End');
  CheckDiff(Code,[
  'unit test1;',
  '',
  '{$mode objfpc}{$H+}',
  '',
  'interface',
  '',
  'type',
  '  &End{#Rename} = word;',
  '  TBar = low(&End)..high(&End);',
  'implementation',
  'type',
  '  TBird = low(&End)..high(&End);',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameProgramName_Amp;
begin
  Add([
  'program test1;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: test1 . TRed;',
  'begin',
  '  test1.c:=&test1 . &c;',
  'end.',
  '']);
  RenameSourceName('&End','end.pas');
  CheckDiff(Code,[
  'program &End;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: &End . TRed;',
  'begin',
  '  &End.c:=&End . &c;',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameProgramName_DottedSameCount;
begin
  Add([
  'program Foo.Bar;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: foo . bar . TRed;',
  'begin',
  '  foo.bar.c:=&foo . &bar . &c;',
  'end.',
  '']);
  RenameSourceName('Foo.&End','foo.end.pas');
  CheckDiff(Code,[
  'program Foo.&End;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: Foo . &End . TRed;',
  'begin',
  '  Foo.&End.c:=Foo . &End . &c;',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameProgramName_MakeDotted;
begin
  Add([
  'program &Type;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: &Type . TRed;',
  'begin',
  '  &type.c:=&type . &c;',
  'end.',
  '']);
  RenameSourceName('Foo.&End','foo.end.pas');
  CheckDiff(Code,[
  'program Foo.&End;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: Foo.&End . TRed;',
  'begin',
  '  Foo.&End.c:=Foo.&End . &c;',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameProgramName_DottedAppendThird;
begin
  Add([
  'program Foo . Bar;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: Foo . Bar . TRed;',
  'begin',
  '  foo.bar.c:=&foo . bar . &c;',
  'end.',
  '']);
  RenameSourceName('Foo.Bar.&End','foo.bar.end.pas');
  CheckDiff(Code,[
  'program Foo . Bar.&End;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: Foo . Bar.&End . TRed;',
  'begin',
  '  Foo.Bar.&End.c:=Foo . Bar.&End . &c;',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameProgramName_DottedPrependThird;
begin
  Add([
  'program Foo . Bar;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: Foo . Bar . TRed;',
  'begin',
  '  foo.bar.c:=&foo . bar . &c;',
  'end.',
  '']);
  RenameSourceName('&Unit.Foo.Bar','unit.foo.bar.pas');
  CheckDiff(Code,[
  'program &Unit.Foo . Bar;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: &Unit.Foo . Bar . TRed;',
  'begin',
  '  &Unit.Foo.Bar.c:=&Unit.Foo . Bar . &c;',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameProgramName_DottedInsertThird;
begin
  Add([
  'program Foo . Bar;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: Foo . Bar . TRed;',
  'begin',
  '  foo.bar.c:=&foo . bar . &c;',
  'end.',
  '']);
  RenameSourceName('Foo.&Unit.Bar','foo.unit.bar.pas');
  CheckDiff(Code,[
  'program Foo . &Unit.Bar;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: Foo . &Unit.Bar . TRed;',
  'begin',
  '  Foo.&Unit.Bar.c:=Foo . &Unit.Bar . &c;',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameProgramName_DottedShortenStart;
begin
  Add([
  'program &Type . Foo . Bar;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: &Type . Foo . Bar . TRed;',
  'begin',
  '  &TYpe.foo.bar.c:=&Type . &foo . bar . &c;',
  '  {$IFDEF FPC}&Type.{$ENDIF}foo.bar:={$IFDEF FPC}&Type.Foo.{$ENDIF}bar;',
  'end.',
  '']);
  RenameSourceName('Foo.Bar','foo.bar.pas');
  CheckDiff(Code,[
  'program Foo . Bar;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: Foo . Bar . TRed;',
  'begin',
  '  Foo.Bar.c:=Foo . Bar . &c;',
  '  {$IFDEF FPC}{$ENDIF}Foo.Bar:={$IFDEF FPC}Foo.{$ENDIF}Bar;',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameProgramName_DottedShortenMiddle;
begin
  Add([
  'program &Type . Foo . Bar;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: &Type . Foo . Bar . TRed;',
  'begin',
  '  &TYpe.foo.bar.c:=&Type . &foo . bar . &c;',
  '  {$ifdef fpc}&type.{$endif}foo{$ifdef fpc}.bar{$endif};',
  'end.',
  '']);
  RenameSourceName('&Type.Bar','type.bar.pas');
  CheckDiff(Code,[
  'program &Type .Bar;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: &Type .Bar . TRed;',
  'begin',
  '  &Type.Bar.c:=&Type .Bar . &c;',
  '  {$ifdef fpc}&Type.{$endif}{$ifdef fpc}Bar{$endif};',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameProgramName_DottedShortenEnd;
begin
  Add([
  'program Foo . Bar.&End;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: Foo . Bar . &End . TRed;',
  'begin',
  '  foo.bar.&end.c:=&foo . bar.&end . &c;',
  'end.',
  '']);
  RenameSourceName('Foo.Bar','foo.bar.pas');
  CheckDiff(Code,[
  'program Foo . Bar;',
  '{$mode objfpc}{$H+}',
  'type TRed = word;',
  'var c: Foo . Bar . TRed;',
  'begin',
  '  Foo.Bar.c:=Foo . Bar . &c;',
  'end.',
  '']);
end;

procedure TTestRefactoring.TestRenameProgramName_ToraToraTora;
var
  ToraUnit: TCodeBuffer;
begin
  ToraUnit:=CodeToolBoss.CreateFile('tora.pas');
  try
    ToraUnit.Source:=LinesToStr([
    'unit Tora;',
    'interface',
    'implementation',
    'end.']);

    Add([
    'program tora.tora.{comment}tora;',
    '{$mode objFPC}',
    'uses tora;',
    'var Toranaga: longint;',
    'begin',
    '  Toranaga:=3;',
    '  tora.tora.tora.Toranaga:=3*Toranaga;',
    '  tora.{}tora.{comment}tora.{}Toranaga:=3*tora.tora.tora.Toranaga;',
    'end.',
    '']);
    RenameSourceName('Red.Green.Blue','red.green.blue.pas');
    CheckDiff(Code,[
    'program Red.Green.{comment}Blue;',
    '{$mode objFPC}',
    'uses tora;',
    'var Toranaga: longint;',
    'begin',
    '  Toranaga:=3;',
    '  Red.Green.Blue.Toranaga:=3*Toranaga;',
    '  Red.{}Green.{comment}Blue.{}Toranaga:=3*Red.Green.Blue.Toranaga;',
    'end.',
    '']);
  finally
    ToraUnit.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameUnitName_IncludeUsedTwiceInOneUnit;
var
  RedInc: TCodeBuffer;
begin
  RedInc:=CodeToolBoss.CreateFile('red.inc');
  try
    RedInc.Source:=
       '{$IFDEF EnableIntf}'+LineEnding
      +'function Fly: Test1.TBird;'+LineEnding
      +'{$ENDIF}'+LineEnding
      +'{$IFDEF EnableImpl}'+LineEnding
      +'function Fly: Test1.TBird;'+LineEnding
      +'begin'+LineEnding
      +'  Test1.Ant:=test1.ant;'+LineEnding
      +'end;'+LineEnding
      +'{$ENDIF}'+LineEnding;

    Add([
    'unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type TAnt = word;',
    '{$define EnableIntf}',
    '{$i red.inc}',
    '{$undefine EnableIntf}',
    'implementation',
    '{$define EnableImpl}',
    '{$i red.inc}',
    '{$undefine EnableIntf}',
    'end.',
    '']);
    RenameSourceName('&End','End.pas');
    CheckDiff(Code,[
    'unit &End;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type TAnt = word;',
    '{$define EnableIntf}',
    '{$i red.inc}',
    '{$undefine EnableIntf}',
    'implementation',
    '{$define EnableImpl}',
    '{$i red.inc}',
    '{$undefine EnableIntf}',
    'end.',
    '']);
    CheckDiff(RedInc,[
    '{$IFDEF EnableIntf}',
    'function Fly: &End.TBird;',
    '{$ENDIF}',
    '{$IFDEF EnableImpl}',
    'function Fly: &End.TBird;',
    'begin',
    '  &End.Ant:=&End.ant;',
    'end;',
    '{$ENDIF}']);

  finally
    RedInc.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameUnitName_IncludeUsedInTwoUnits;
var
  RedInc, RedGreenUnit: TCodeBuffer;
begin
  RedInc:=CodeToolBoss.CreateFile('red.inc');
  RedGreenUnit:=CodeToolBoss.CreateFile('red.green.pas');
  try
    RedInc.Source:=LinesToStr([
    'function Fly: Red.Green.TAnt;',
    'begin',
    '  red.green.Ant:=3;',
    'end;']);

    RedGreenUnit.Source:=LinesToStr([
    'unit Red.Green;',
    'interface',
    'type TAnt = word;',
    'var Ant: TAnt;',
    'implementation',
    '{$I red.inc}',
    'var Hop: red.green.TAnt;',
    'end.']);

    Add([
    'unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses Red.Green;',
    'implementation',
    '{$I red.inc}',
    'begin',
    '  red.green.ant:=2;',
    'end.',
    '']);
    RenameUsedUnitRefs(RedGreenUnit,'&End','end.pas',[]);
    CheckDiff(Code,[
    'unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses &End;',
    'implementation',
    '{$I red.inc}',
    'begin',
    '  &End.ant:=2;',
    'end.',
    '']);

    CheckDiff(RedGreenUnit,[
      'unit &End;',
      'interface',
      'type TAnt = word;',
      'var Ant: TAnt;',
      'implementation',
      '{$I red.inc}',
      'var Hop: &End.TAnt;',
      'end.']);

    CheckDiff(RedInc,[
      'function Fly: &End.TAnt;',
      'begin',
      '  &End.Ant:=3;',
      'end;']);

  finally
    RedInc.IsDeleted:=true;
    RedGreenUnit.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestUseOmittedNamespace;

  procedure t(const OldShort, OldFull, NewFull, Expected: string);
  var
    Actual: String;
  begin
    Actual:=TChangeDeclarationTool.UseOmittedNamespace(OldShort, OldFull, NewFull);
    if Actual=Expected then exit;
    Fail('OldShort="'+OldShort+'" OldFull="'+OldFull+'" NewFull="'+NewFull+'": expected "'+Expected+'", but got "'+Actual+'"');
  end;

begin
  t('','','','');
  t('a','a','b.a','b.a');
  t('b','a.b','c','c');
  t('b','a.b','a.c','c');
  t('b','a.b','b.c','b.c');
  t('b','a.b','d.c','d.c');
  t('a.b','&Foo.a.b','Foo.a.c','a.c');
  t('a.b','&Foo.a.b','&Foo.A.c','A.c');
  t('a.b','Foo.a.b','foO.a.c','a.c');
  t('a.b','Foo.Bar.a.b','Foo.Bar.d','d');
  t('a.b','Foo.Bar.a.b','Foo.Bar.&End.&Of','&End.&Of');
end;

procedure TTestRefactoring.TestRenameUsedUnit_Amp;
var
  UsedUnit: TCodeBuffer;
begin
  UsedUnit:=nil;
  try
    UsedUnit:=CodeToolBoss.CreateFile('type.pas');
    UsedUnit.Source:='unit &Type;'+LineEnding
      +'interface'+LineEnding
      +'type'+LineEnding
      +'  TAnt = word;'+LineEnding
      +'  Ant: TAnt;'+LineEnding
      +'implementation'+LineEnding
      +'end.';

    Add([
    'unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses &Type;',
    'var c: &Type . TAnt;',
    'implementation',
    'initialization',
    '  &type.ant:=&Type . &ant;',
    'end.',
    '']);
    RenameUsedUnitRefs(UsedUnit,'&End','end.pas');
    CheckDiff(Code,[
    'unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses &End;',
    'var c: &End . TAnt;',
    'implementation',
    'initialization',
    '  &End.ant:=&End . &ant;',
    'end.',
    '']);

  finally
    if UsedUnit<>nil then
      UsedUnit.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameUsedUnit_Impl;
var
  UsedUnit: TCodeBuffer;
begin
  UsedUnit:=nil;
  try
    UsedUnit:=CodeToolBoss.CreateFile('type.pp');
    UsedUnit.Source:='unit &Type;'+LineEnding
      +'interface'+LineEnding
      +'type'+LineEnding
      +'  TAnt = word;'+LineEnding
      +'  Ant: TAnt;'+LineEnding
      +'implementation'+LineEnding
      +'end.';

    Add([
    'unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'uses &Type;',
    'var c: &Type . TAnt;',
    'initialization',
    '  &type.ant:=&Type . &ant;',
    'end.',
    '']);
    RenameUsedUnitRefs(UsedUnit,'&End','end.pas');
    CheckDiff(Code,[
    'unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'uses &End;',
    'var c: &End . TAnt;',
    'initialization',
    '  &End.ant:=&End . &ant;',
    'end.',
    '']);

  finally
    if UsedUnit<>nil then
      UsedUnit.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameUsedUnit_FN_KeepShort;
var
  UsedUnit: TCodeBuffer;
begin
  AddNameSpace('foo');

  UsedUnit:=nil;
  try
    UsedUnit:=CodeToolBoss.CreateFile('foo.bar.pp');
    UsedUnit.Source:='unit Foo.Bar;'+LineEnding
      +'interface'+LineEnding
      +'type'+LineEnding
      +'  TAnt = word;'+LineEnding
      +'  Ant: TAnt;'+LineEnding
      +'implementation'+LineEnding
      +'end.';

    Add([
    'unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses Bar;',
    'var c: bar . TAnt;',
    'implementation',
    'initialization',
    '  bar.ant:=bar . &ant;',
    'end.',
    '']);
    RenameUsedUnitRefs(UsedUnit,'foo.&End','foo.end.pas');
    CheckDiff(Code,[
    'unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses &End;',
    'var c: &End . TAnt;',
    'implementation',
    'initialization',
    '  &End.ant:=&End . &ant;',
    'end.',
    '']);

  finally
    if UsedUnit<>nil then
      UsedUnit.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameUsedUnit_InFilename;
var
  UsedUnit: TCodeBuffer;
begin
  UsedUnit:=nil;
  try
    UsedUnit:=CodeToolBoss.CreateFile('foo.bar.pp');
    UsedUnit.Source:='unit Foo.Bar;'+LineEnding
      +'interface'+LineEnding
      +'type'+LineEnding
      +'  TAnt = word;'+LineEnding
      +'implementation'+LineEnding
      +'end.';

    Add([
    'program Test1;',
    '{$mode delphi}',
    'uses Foo.Bar in ''foo.bar.pp'';',
    'var c: foo.bar . TAnt;',
    'begin',
    'end.',
    '']);
    RenameUsedUnitRefs(UsedUnit,'Foo.&End','foo.end.pas');
    CheckDiff(Code,[
    'program Test1;',
    '{$mode delphi}',
    'uses Foo.&End in ''foo.end.pas'';',
    'var c: Foo.&End . TAnt;',
    'begin',
    'end.',
    '']);

  finally
    if UsedUnit<>nil then
      UsedUnit.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameUsedUnit_LongestUnitnameWins;
var
  RedUnit, RedGreenUnit, RedGreenBlueUnit: TCodeBuffer;
begin
  RedUnit:=CodeToolBoss.CreateFile('red.pas');
  RedGreenUnit:=CodeToolBoss.CreateFile('red.green.pas');
  RedGreenBlueUnit:=CodeToolBoss.CreateFile('red.green.blue.pas');
  try
    RedUnit.Source:='unit Red;'+LineEnding
      +'interface'+LineEnding
      +'var'+LineEnding
      +'  Red, Green: word;'+LineEnding
      +'implementation'+LineEnding
      +'end.';

    RedGreenUnit.Source:='unit Red.Green;'+LineEnding
      +'interface'+LineEnding
      +'var'+LineEnding
      +'  Green, Blue: word;'+LineEnding
      +'implementation'+LineEnding
      +'end.';

    RedGreenBlueUnit.Source:='unit Red.Green.Blue;'+LineEnding
      +'interface'+LineEnding
      +'var'+LineEnding
      +'  Blue: word;'+LineEnding
      +'implementation'+LineEnding
      +'end.';

    Add([
    'program test1;',
    '{$mode objfpc}{$H+}',
    'uses Red, Red.Green, Red.Green.Blue;',
    'begin',
    '  red.red:=1;',
    '  red.green.green:=2;',
    '  red.green.blue.blue:=3;',
    'end.',
    '']);
    RenameUsedUnitRefs(RedGreenUnit,'&End','end.pas');
    CheckDiff(Code,[
    'program test1;',
    '{$mode objfpc}{$H+}',
    'uses Red, &End, Red.Green.Blue;',
    'begin',
    '  red.red:=1;',
    '  &End.green:=2;',
    '  red.green.blue.blue:=3;',
    'end.',
    '']);

  finally
    RedUnit.IsDeleted:=true;
    RedGreenUnit.IsDeleted:=true;
    RedGreenBlueUnit.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameAlsoLFM_Empty;
begin
  TestRenameAlsoLFM(LinesToStr([ // red unit interface
  'type',
  '  TForm = class',
  '  end;',
  '  TButton = class',
  '  end;']),
  '    Button1{#Rename}: TButton;', // form interface
  'OkBtn', // new identifier
  '', // LFM source
  [] // expected LFM source after rename
  );
end;

procedure TTestRefactoring.TestRenameAlsoLFM_Garbage;
begin
  TestRenameAlsoLFM(LinesToStr([ // red unit interface
  'type',
  '  TForm = class',
  '  end;',
  '  TButton = class',
  '  end;']),
  '    Button1{#Rename}: TButton;', // form interface
  'OkBtn', // new identifier
  'bla'+LineEnding, // LFM source
  ['bla'] // expected LFM source after rename
  );
end;

procedure TTestRefactoring.TestRenameAlsoLFM_Variable;
begin
  TestRenameAlsoLFM(LinesToStr([ // red unit interface
  'type',
  '  TForm = class',
  '  end;',
  '  TButton = class',
  '  end;']),
  // form interface
  '    Button1{#Rename}: TButton;',
  'OkBtn', // new identifier
  LinesToStr([ // LFM source
  'object Form1: TForm1',
  '  Left = 353',
  '  object Button1: TButton',
  '  end',
  'end']),
  [  // expected LFM source after rename
  'object Form1: TForm1',
  '  Left = 353',
  '  object OkBtn: TButton',
  '  end',
  'end']
  );
end;

procedure TTestRefactoring.TestRenameAlsoLFM_Event;
begin
  TestRenameAlsoLFM(LinesToStr([ // red unit interface
  'type',
  '  TForm = class',
  '  end;',
  '  TButton = class',
  '  published',
  '    property OnClick: TNotifyEvent;',
  '  end;']),
  // TForm1 interface
  LinesToStr([
  '    Button1: TButton;',
  '    procedure Button1Click{#Rename}(Sender: TObject);']),
  // new identifier
  'OkClicked',
  // LFM source
  LinesToStr([
  'object Form1: TForm1',
  '  Left = 353',
  '  object Button1: TButton',
  '    OnClick = Button1Click',
  '  end',
  'end']),
  [  // expected LFM source after rename
  'object Form1: TForm1',
  '  Left = 353',
  '  object Button1: TButton',
  '    OnClick = OkClicked',
  '  end',
  'end']
  );
end;

procedure TTestRefactoring.TestRenameAlsoLFM_SkipBinaryData;
var
  Test1LFM, RedUnit: TCodeBuffer;
begin
  RedUnit:=CodeToolBoss.CreateFile('red.pas');
  Test1LFM:=CodeToolBoss.CreateFile(ChangeFileExt(Code.Filename,'.lfm'));
  try
    RedUnit.Source:='unit Red;'+LineEnding
      +'interface'+LineEnding
      +'type'+LineEnding
      +'  TForm = class'+LineEnding
      +'  end;'+LineEnding
      +'  TBitmap = class'+LineEnding
      +'  end;'+LineEnding
      +'  TButton = class'+LineEnding
      +'  published'+LineEnding
      +'    property Glyph: TBitmap;'+LineEnding
      +'  end;'+LineEnding
      +'implementation'+LineEnding
      +'end.';

    Test1LFM.Source:=LinesToStr([
      'object Form1: TForm1',
      '  object Button1: TButton',
      '    Glyph.Data = {',
      '      36040000424D3604000000000000360000002800000010000000100000000100',
      '      49EE000000000004000064000000640000000000000000000000000000000000',
      '    }',
      '  end',
      'end']);

    Add(['unit Test1;',
      '{$mode objfpc}{$H+}',
      'interface',
      'uses red;',
      'type',
      '  TForm1 = class(TForm)',
      '    Button1{#Rename}: TButton;',
      '  end;',
      'implementation',
      'end.']);
    RenameReferences('OkBtn',[frfIncludingLFM]);
    CheckDiff(Test1LFM,[
    'object Form1: TForm1',
    '  object OkBtn: TButton',
    '    Glyph.Data = {',
    '      36040000424D3604000000000000360000002800000010000000100000000100',
    '      49EE000000000004000064000000640000000000000000000000000000000000',
    '    }',
    '  end',
    'end']);

  finally
    RedUnit.IsDeleted:=true;
    Test1LFM.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameAlsoLFM_Property;
var
  Test1LFM: TCodeBuffer;
begin
  Test1LFM:=CodeToolBoss.CreateFile(ChangeFileExt(Code.Filename,'.lfm'));
  try
    Test1LFM.Source:=LinesToStr([
      'object Form1: TForm1',
      '  Checked = False',
      '  object Button1: TButton',
      '    Checked = True',
      '  end',
      'end']);

    Add(LinesToStr([
      'unit Test1;',
      '{$mode objfpc}{$H+}',
      'interface',
      'uses Classes;',
      'type',
      '  TForm = class(TComponent)',
      '  published',
      '    property Checked: boolean;',
      '  end;',
      '  TButton = class(TComponent)',
      '  published',
      '    property Checked{#Rename}: boolean;',
      '  end;',
      '  TForm1 = class(TForm)',
      '    Button1: TButton;',
      '  end;',
      'implementation',
      'end.']));

    RenameReferences('Activated',[frfIncludingLFM]);
    CheckDiff(Test1LFM,[
      'object Form1: TForm1',
      '  Checked = False',
      '  object Button1: TButton',
      '    Activated = True',
      '  end',
      'end']);

  finally
    Test1LFM.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameAlsoLFM_Property_Typeless;
var
  Test1LFM: TCodeBuffer;
begin
  Test1LFM:=CodeToolBoss.CreateFile(ChangeFileExt(Code.Filename,'.lfm'));
  try
    Test1LFM.Source:=LinesToStr([
      'object Form1: TForm1',
      '  object Button1: TButton',
      '    Checked = True',
      '  end',
      'end']);

    Add(LinesToStr([
      'unit Test1;',
      '{$mode objfpc}{$H+}',
      'interface',
      'uses Classes;',
      'type',
      '  TControl = class(TComponent)',
      '  public',
      '    property Checked{#Rename}: boolean;',
      '  end;',
      '  TButton = class(TControl)',
      '  published',
      '    property Checked;',
      '  end;',
      '  TForm1 = class(TComponent)',
      '    Button1: TButton;',
      '  end;',
      'implementation',
      'end.']));

    RenameReferences('Activated',[frfIncludingLFM]);
    CheckDiff(Test1LFM,[
      'object Form1: TForm1',
      '  object Button1: TButton',
      '    Activated = True',
      '  end',
      'end']);

  finally
    Test1LFM.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameAlsoLFM_DottedProperty;
var
  Test1LFM: TCodeBuffer;
begin
  Test1LFM:=CodeToolBoss.CreateFile(ChangeFileExt(Code.Filename,'.lfm'));
  try
    Test1LFM.Source:=LinesToStr([
      'object Form1: TForm1',
      '  object Button1: TButton',
      '    Prop.Prop = True',
      '  end',
      'end']);

    Add(LinesToStr([
      'unit Test1;',
      '{$mode objfpc}{$H+}',
      'interface',
      'uses Classes;',
      'type',
      '  TWing = class(TPersistent)',
      '  published',
      '    property Prop{#Rename}: boolean;',
      '  end;',
      '  TButton = class(TComponent)',
      '  published',
      '    property Prop: TWing;',
      '  end;',
      '  TForm1 = class(TComponent)',
      '    Button1: TButton;',
      '  end;',
      'implementation',
      'end.']));

    RenameReferences('Flying',[frfIncludingLFM]);
    CheckDiff(Test1LFM,[
      'object Form1: TForm1',
      '  object Button1: TButton',
      '    Prop.Flying = True',
      '  end',
      'end']);

  finally
    Test1LFM.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameAlsoLFM_EnumProperty;
var
  Test1LFM: TCodeBuffer;
begin
  Test1LFM:=CodeToolBoss.CreateFile(ChangeFileExt(Code.Filename,'.lfm'));
  try
    Test1LFM.Source:=LinesToStr([
      'object Form1: TForm1',
      '  object Button1: TButton',
      '    Color = green',
      '  end',
      'end']);

    Add(LinesToStr([
      'unit Test1;',
      '{$mode objfpc}{$H+}',
      'interface',
      'uses Classes;',
      'type',
      '  TColor = (red,green{#Rename},blue);',
      '  TButton = class(TComponent)',
      '  published',
      '    property Color: TColor;',
      '  end;',
      '  TForm1 = class(TComponent)',
      '    Button1: TButton;',
      '  end;',
      'implementation',
      'end.']));

    RenameReferences('Violet',[frfIncludingLFM]);
    CheckDiff(Test1LFM,[
      'object Form1: TForm1',
      '  object Button1: TButton',
      '    Color = Violet',
      '  end',
      'end']);

  finally
    Test1LFM.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameAlsoLFM_SetProperty;
var
  Test1LFM: TCodeBuffer;
begin
  Test1LFM:=CodeToolBoss.CreateFile(ChangeFileExt(Code.Filename,'.lfm'));
  try
    Test1LFM.Source:=LinesToStr([
      'object Form1: TForm1',
      '  object Button1: TButton',
      '    Colors = [red, green]',
      '  end',
      'end']);

    Add(LinesToStr([
      'unit Test1;',
      '{$mode objfpc}{$H+}',
      'interface',
      'uses Classes;',
      'type',
      '  TColor = (red,green{#Rename},blue);',
      '  TColors = set of TColor;',
      '  TButton = class(TComponent)',
      '  published',
      '    property Colors: TColors;',
      '  end;',
      '  TForm1 = class(TComponent)',
      '    Button1: TButton;',
      '  end;',
      'implementation',
      'end.']));

    RenameReferences('Violet',[frfIncludingLFM]);
    CheckDiff(Test1LFM,[
      'object Form1: TForm1',
      '  object Button1: TButton',
      '    Colors = [red, Violet]',
      '  end',
      'end']);

  finally
    Test1LFM.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameAlsoLFM_AnonymousSetProperty;
var
  Test1LFM: TCodeBuffer;
begin
  Test1LFM:=CodeToolBoss.CreateFile(ChangeFileExt(Code.Filename,'.lfm'));
  try
    Test1LFM.Source:=LinesToStr([
      'object Form1: TForm1',
      '  object Button1: TButton',
      '    Colors = [red, green]',
      '  end',
      'end']);

    Add(LinesToStr([
      'unit Test1;',
      '{$mode objfpc}{$H+}',
      'interface',
      'uses Classes;',
      'type',
      '  TColors = set of (red,green{#Rename},blue);',
      '  TButton = class(TComponent)',
      '  published',
      '    property Colors: TColors;',
      '  end;',
      '  TForm1 = class(TComponent)',
      '    Button1: TButton;',
      '  end;',
      'implementation',
      'end.']));

    RenameReferences('Violet',[frfIncludingLFM]);
    CheckDiff(Test1LFM,[
      'object Form1: TForm1',
      '  object Button1: TButton',
      '    Colors = [red, Violet]',
      '  end',
      'end']);

  finally
    Test1LFM.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameAlsoLFM_ComponentProperty;
var
  Test1LFM: TCodeBuffer;
begin
  Test1LFM:=CodeToolBoss.CreateFile(ChangeFileExt(Code.Filename,'.lfm'));
  try
    Test1LFM.Source:=LinesToStr([
      'object Form1: TForm1',
      '  object Button1: TButton',
      '    Link = Edit1',
      '  end',
      '  object Edit1: TEdit',
      '  end',
      'end']);

    Add(LinesToStr([
      'unit Test1;',
      '{$mode objfpc}{$H+}',
      'interface',
      'uses Classes;',
      'type',
      '  TButton = class(TComponent)',
      '  published',
      '    property Link: TComponent;',
      '  end;',
      '  TEdit = class(TComponent)',
      '  end;',
      '  TForm1 = class(TComponent)',
      '    Button1: TButton;',
      '    Edit1{#Rename}: TEdit;',
      '  end;',
      'implementation',
      'end.']));

    RenameReferences('Input1',[frfIncludingLFM]);
    CheckDiff(Test1LFM,[
      'object Form1: TForm1',
      '  object Button1: TButton',
      '    Link = Input1',
      '  end',
      '  object Input1: TEdit',
      '  end',
      'end']);
  finally
    Test1LFM.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameAlsoLFM_Property_FromOtherUnit;
var
  Test1LFM: TCodeBuffer;
  Unit2: TCodeBuffer;
begin
  Test1LFM:=CodeToolBoss.CreateFile(ChangeFileExt(Code.Filename,'.lfm'));
  Unit2:=CodeToolBoss.CreateFile(ExtractFilePath(Code.Filename)+'Unit2.pas');

  try
    Unit2.Source:=LinesToStr([
    'unit Unit2;',
      '{$mode objfpc}{$H+}',
      'interface',
      'type',
      '  TComponent = class end;',
      '  TForm2 = class(TComponent)',
      '    property Checked: boolean;',
      '  end;',
      '  TCustomButton = class(TComponent)',
      '    property Checked: boolean;',
      '  end;',
      '  TButton = class(TCustomButton)',
      '    property Checked stored true;',
      '  end;',
      '  TMyButton = class(TButton)',
      '    property Checked;',
      '  end;',
      'implementation',
      'end.']);

    Test1LFM.Source:=LinesToStr([
      'object Form1: TForm1',
      '  Checked = False',
      '  object Button1: TMyButton',
      '    Checked = True',
      '  end',
      'end']);

    Add(LinesToStr([
      'unit Test1;',
      '{$mode objfpc}{$H+}',
      'interface',
      'uses Unit2;',
      'type',
      '  TForm1 = class(TForm2)',
      '    Button1: TMyButton;',
      '    procedure ActivateMe(Sender: TObject);',
      '  end;',
      'var',
      '  Form1: TForm1;',
      'implementation',
      'procedure TForm1.ActivateMe(Sender: TObject);',
      'begin',
      '  Checked;',
      '  Button1.Checked{#Rename};',
      'end;',

      'end.']));

    RenameReferences('Activated',[frfIncludingLFM]);
    CheckDiff(Test1LFM,[
      'object Form1: TForm1',
      '  Checked = False',
      '  object Button1: TMyButton',
      '    Activated = True',
      '  end',
      'end']);

  finally
    Test1LFM.IsDeleted:=true;
    Unit2.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameAlsoLFM_ListProperty;
var
  Test1LFM: TCodeBuffer;
begin
  Test1LFM:=CodeToolBoss.CreateFile(ChangeFileExt(Code.Filename,'.lfm'));
  try
    Test1LFM.Source:=LinesToStr([
      'object Form1: TForm1',
      '  object Memo1: TMemo',
      '    Lines.Strings = (',
      '      ''Memo1''',
      '      ''Bla''',
      '    )',
      '  end',
      'end']);

    Add(LinesToStr([
      'unit Test1;',
      '{$mode objfpc}{$H+}',
      'interface',
      'uses Classes;',
      'type',
      '  TMemo = class(TComponent)',
      '  published',
      '    property Lines: TStrings;',
      '  end;',
      '  TForm1 = class(TComponent)',
      '    Memo1{#Rename}: TMemo;',
      '  end;',
      'implementation',
      'end.']));

    RenameReferences('Input1',[frfIncludingLFM]);
    CheckDiff(Test1LFM,[
      'object Form1: TForm1',
      '  object Input1: TMemo',
      '    Lines.Strings = (',
      '      ''Memo1''',
      '      ''Bla''',
      '    )',
      '  end',
      'end']);
  finally
    Test1LFM.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameAlsoLFM_ComponentClass;
var
  Test1LFM: TCodeBuffer;
begin
  Test1LFM:=CodeToolBoss.CreateFile(ChangeFileExt(Code.Filename,'.lfm'));
  try
    Test1LFM.Source:=LinesToStr([
      'object Form1: TForm1',
      '  object Div1: TDiv',
      '    object Div2: TDiv',
      '    end',
      '  end',
      '  object Div3: TDiv',
      '  end',
      'end']);

    Add(LinesToStr([
      'unit Test1;',
      '{$mode objfpc}{$H+}',
      'interface',
      'uses Classes;',
      'type',
      '  TDiv{#Rename} = class(TComponent)',
      '  published',
      '  end;',
      '  TForm1 = class(TComponent)',
      '    Div1: TDiv;',
      '    Div2: TDiv;',
      '    Div3: TDiv;',
      '  end;',
      'implementation',
      'end.']));

    RenameReferences('TControl',[frfIncludingLFM]);
    CheckDiff(Test1LFM,[
      'object Form1: TForm1',
      '  object Div1: TControl',
      '    object Div2: TControl',
      '    end',
      '  end',
      '  object Div3: TControl',
      '  end',
      'end']);
  finally
    Test1LFM.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameAlsoLFM_RootComponentClass;
var
  Test1LFM: TCodeBuffer;
begin
  Test1LFM:=CodeToolBoss.CreateFile(ChangeFileExt(Code.Filename,'.lfm'));
  try
    Test1LFM.Source:=LinesToStr([
      'object Form1: TForm1',
      '  object Div1: TDiv',
      '  end',
      'end']);

    Add(LinesToStr([
      'unit Test1;',
      '{$mode objfpc}{$H+}',
      'interface',
      'uses Classes;',
      'type',
      '  TDiv = class(TComponent)',
      '  published',
      '  end;',
      '  TForm1{#Rename} = class(TComponent)',
      '    Div1: TDiv;',
      '  end;',
      'implementation',
      'end.']));

    RenameReferences('TMyForm',[frfIncludingLFM]);
    CheckDiff(Test1LFM,[
      'object Form1: TMyForm',
      '  object Div1: TDiv',
      '  end',
      'end']);
  finally
    Test1LFM.IsDeleted:=true;
  end;
end;

procedure TTestRefactoring.TestRenameAlsoLFM_RootComponentName;
var
  Test1LFM: TCodeBuffer;
begin
  Test1LFM:=CodeToolBoss.CreateFile(ChangeFileExt(Code.Filename,'.lfm'));
  try
    Test1LFM.Source:=LinesToStr([
      'object Form1: TForm1',
      '  object Div1: TDiv',
      '  end',
      'end']);

    Add(LinesToStr([
      'unit Test1;',
      '{$mode objfpc}{$H+}',
      'interface',
      'uses Classes;',
      'type',
      '  TDiv = class(TComponent)',
      '  published',
      '  end;',
      '  TForm1 = class(TComponent)',
      '    Div1: TDiv;',
      '  end;',
      'var Form1{#Rename}: TForm1;',
      'implementation',
      'end.']));

    RenameReferences('MyForm',[frfIncludingLFM]);
    CheckDiff(Test1LFM,[
      'object MyForm: TForm1',
      '  object Div1: TDiv',
      '  end',
      'end']);
  finally
    Test1LFM.IsDeleted:=true;
  end;
end;

initialization
  RegisterTests([TTestRefactoring]);
end.

