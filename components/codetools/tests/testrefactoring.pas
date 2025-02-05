{
 Test with:
   ./testcodetools --format=plain --suite=TTestRefactoring
   ./testcodetools --format=plain --suite=TestExplodeWith
}
unit TestRefactoring;

{$i runtestscodetools.inc}

interface

uses
  Classes, SysUtils, CodeToolManager, CodeCache, CodeTree, BasicCodeTools, CTUnitGraph,
  FindDeclarationTool, ChangeDeclarationTool, LazLogger, LazFileUtils, AVL_Tree, Contnrs, fpcunit,
  testregistry, TestFinddeclaration;

const
  ExplodeWithMarker = 'explodewith:';
type

  { TCustomTestRefactoring }

  TCustomTestRefactoring = class(TCustomTestFindDeclaration)
  protected
    procedure RenameReferences(NewIdentifier: string; const Flags: TFindRefsFlags = []);
    procedure RenameSourceName(NewName, NewFilename: string);
    procedure RenameUsedUnitRefs(UsedUnit: TCodeBuffer; NewName, NewFilename: string);
    procedure CheckDiff(CurCode: TCodeBuffer; const ExpLines: array of string);
  end;

  { TTestRefactoring }

  TTestRefactoring = class(TCustomTestRefactoring)
  private
  protected
  published
    procedure TestExplodeWith;
    procedure TestRenameReferences;

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

    // rename uses
    procedure TestUseOmittedNamespace;
    procedure TestRenameUsedUnit_Amp;
    procedure TestRenameUsedUnit_Impl;
    procedure TestRenameUsedUnit_FN_KeepShort;
    procedure TestRenameUsedUnit_InFilename;
    procedure TestRenameUsedUnit_LongestUnitnameWins;
  end;

implementation

{ TCustomTestRefactoring }

procedure TCustomTestRefactoring.RenameReferences(NewIdentifier: string; const Flags: TFindRefsFlags
  );
var
  Marker: TFDMarker;
  Tool: TCodeTool;
  DeclX, DeclY, DeclTopLine: integer;
  DeclCode: TCodeBuffer;
  Files: TStringList;
  Graph: TUsesGraph;
  Completed: boolean;
  Node: TAVLTreeNode;
  UGUnit: TUGUnit;
  DeclarationCaretXY: TPoint;
  PascalReferences: TAVLTree;
  OldIdentifier: string;
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
  if not CodeToolBoss.FindMainDeclaration(Code,
    DeclarationCaretXY.X,DeclarationCaretXY.Y,
    DeclCode,DeclX,DeclY,DeclTopLine) then
  begin
    Fail('CodeToolBoss.FindMainDeclaration failed '+dbgs(DeclarationCaretXY)+' File='+Code.Filename);
  end;

  //debugln(['TCustomTestRefactoring.RenameReferences X=',DeclX,' Y=',DeclY,' "',DeclCode.GetLine(DeclY-1,false),'"']);

  DeclarationCaretXY:=Point(DeclX,DeclY);

  CodeToolBoss.GetIdentifierAt(DeclCode,DeclarationCaretXY.X,DeclarationCaretXY.Y,OldIdentifier);

  // create the file list
  Files:=TStringList.Create;
  Graph:=nil;
  PascalReferences:=nil;
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
      Fail('CodeToolBoss.FindReferencesInFiles failed at '+dbgs(DeclarationCaretXY)+' File='+Code.Filename);
    end;

    // todo: check for conflicts

    if not CodeToolBoss.RenameIdentifier(PascalReferences,
      OldIdentifier, NewIdentifier, DeclCode, @DeclarationCaretXY)
    then
      Fail('CodeToolBoss.RenameIdentifier failed');

  finally
    CodeToolBoss.FreeTreeOfPCodeXYPosition(PascalReferences);
    Graph.Free;
    Files.Free;
  end;
end;

procedure TCustomTestRefactoring.RenameSourceName(NewName, NewFilename: string);
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

{ TTestRefactoring }

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

procedure TTestRefactoring.TestRenameReferences;
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
  '  TBird = class(TAnimal)',
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
  '  TBird = class(TAnimal)',
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
begin
  Add([
  'program tora.tora.{comment}tora;',
  '{$mode objFPC}',
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
  'var Toranaga: longint;',
  'begin',
  '  Toranaga:=3;',
  '  Red.Green.Blue.Toranaga:=3*Toranaga;',
  '  Red.{}Green.{comment}Blue.{}Toranaga:=3*Red.Green.Blue.Toranaga;',
  'end.',
  '']);
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
    'var &Type: word;',
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
    'var &Type: word;',
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

initialization
  RegisterTests([TTestRefactoring]);
end.

