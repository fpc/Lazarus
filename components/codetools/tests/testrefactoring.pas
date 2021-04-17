{
 Test with:
   ./testcodetools --format=plain --suite=TTestRefactoring
   ./testcodetools --format=plain --suite=TestExplodeWith
}
unit TestRefactoring;

{$i runtestscodetools.inc}

interface

uses
  Classes, SysUtils, CodeToolManager, CodeCache, CodeTree, BasicCodeTools,
  CTUnitGraph, LazLogger, LazFileUtils, Laz_AVL_Tree, fpcunit, testregistry,
  TestFinddeclaration;

const
  ExplodeWithMarker = 'explodewith:';
type

  { TCustomTestRefactoring }

  TCustomTestRefactoring = class(TCustomTestFindDeclaration)
  protected
    procedure RenameReferences(NewIdentifier: string);
    procedure CheckDiff(CurCode: TCodeBuffer; const ExpLines: array of string);
  end;

  { TTestRefactoring }

  TTestRefactoring = class(TCustomTestRefactoring)
  private
  published
    procedure TestExplodeWith;
    procedure TestRenameReferences;
    procedure TestRenameProcReferences;
  end;

implementation

{ TCustomTestRefactoring }

procedure TCustomTestRefactoring.RenameReferences(NewIdentifier: string);
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
  if not IsValidIdent(NewIdentifier) then
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
  DeclarationCaretXY:=Point(DeclX,DeclY);

  CodeToolBoss.GetIdentifierAt(DeclCode,DeclarationCaretXY.X,DeclarationCaretXY.Y,OldIdentifier);

  // create the file list
  Files:=TStringList.Create;
  Graph:=nil;
  PascalReferences:=nil;
  try
    Files.Add(DeclCode.Filename);
    if CompareFilenames(DeclCode.Filename,Code.Filename)<>0 then
      Files.Add(DeclCode.Filename);

    Graph:=CodeToolBoss.CreateUsesGraph;
    Graph.AddStartUnit(Code.Filename);
    Graph.AddTargetUnit(DeclCode.Filename);
    Graph.Parse(true,Completed);
    Node:=Graph.FilesTree.FindLowest;
    Files.Clear;
    while Node<>nil do begin
      UGUnit:=TUGUnit(Node.Data);
      Files.Add(UGUnit.Filename);
      Node:=Node.Successor;
    end;

    // search pascal source references
    if not CodeToolBoss.FindReferencesInFiles(Files,DeclCode,
        DeclarationCaretXY,true,PascalReferences) then begin
      Fail('CodeToolBoss.FindReferencesInFiles failed at '+dbgs(DeclarationCaretXY)+' File='+Code.Filename);
    end;

    if not CodeToolBoss.RenameIdentifier(PascalReferences,
      OldIdentifier, NewIdentifier, DeclCode, @DeclarationCaretXY)
    then begin
      Fail('CodeToolBoss.RenameIdentifier failed');
    end;

  finally
    CodeToolBoss.FreeTreeOfPCodeXYPosition(PascalReferences);
    Graph.Free;
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

initialization
  RegisterTests([TTestRefactoring]);
end.

