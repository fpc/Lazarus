{
 Test with:
   ./testcodetools --format=plain --suite=TTestRefactoring
   ./testcodetools --format=plain --suite=TestExplodeWith
}
unit RefactoringTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CodeToolManager, CodeCache, CodeTree, LazLogger,
  LazFileUtils, fpcunit, testregistry, FindDeclarationTests;

type

  { TTestRefactoring }

  TTestRefactoring = class(TTestCase)
  private
  published
    procedure TestExplodeWith;
  end;

var
  RefactoringTestSuite: TTestSuite;

implementation

{ TTestRefactoring }

procedure TTestRefactoring.TestExplodeWith;
var
  Code: TCodeBuffer;
  Tool: TCodeTool;
  Node: TCodeTreeNode;
  CodeXYPos: TCodeXYPosition;
  ListOfPCodeXYPosition: TFPList;
  i: Integer;
  Filename, OldSource: String;
begin
  Filename:=ExpandFileNameUTF8('rt_explodewith.pas');
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  AssertEquals('Load file error: '+Filename,true,Code<>nil);
  if not CodeToolBoss.Explore(Code,Tool,true) then
    AssertEquals('Parse error: ','',CodeToolBoss.ErrorMessage);
  ListOfPCodeXYPosition:=nil;
  try
    // collect all With-Blocks
    Node:=Tool.Tree.Root;
    while Node<>nil do begin
      if Node.Desc=ctnWithVariable then begin
        Tool.CleanPosToCaret(Node.StartPos,CodeXYPos);
        AddCodePosition(ListOfPCodeXYPosition,CodeXYPos);
      end;
      Node:=Node.Next;
    end;

    for i:=0 to ListOfPCodeXYPosition.Count-1 do begin
      CodeXYPos:=PCodeXYPosition(ListOfPCodeXYPosition[i])^;
      debugln(['TTestRefactoring.TestExplodeWith ',dbgs(CodeXYPos)]);
      OldSource:=Code.Source;
      try
        if CodeToolBoss.RemoveWithBlock(Code,CodeXYPos.X,CodeXYPos.Y) then begin
          // check changes

        end else begin
          AssertEquals('CodeToolBoss.RemoveWithBlock failed at '+dbgs(CodeXYPos),'',CodeToolBoss.ErrorMessage);
        end;
      finally
        Code.Source:=OldSource;
      end;
    end;
  finally
    FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
  end;
end;

initialization
  RefactoringTestSuite := TTestSuite.Create('Refactoring');
  GetTestRegistry.AddTest(RefactoringTestSuite);

  RefactoringTestSuite.AddTestSuiteFromClass(TTestRefactoring);
end.

