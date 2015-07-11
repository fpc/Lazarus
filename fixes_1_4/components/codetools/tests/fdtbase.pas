{
 Test with:
     ./finddeclarationtest --format=plain --suite=TTestFindDeclarationClassHelper
     ./finddeclarationtest --format=plain --suite=TestFindDeclaration_Base
     ./finddeclarationtest --format=plain --suite=TestFindDeclaration_NestedClasses
     ./finddeclarationtest --format=plain --suite=TestFindDeclaration_ClassHelper
}
unit fdtbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CodeToolManager, ExprEval, CodeCache, BasicCodeTools,
  CustomCodeTool, CodeTree, FindDeclarationTool, LazLogger, LazFileUtils,
  fpcunit, testregistry;

type

  { TTestFindDeclaration }

  TTestFindDeclaration = class(TTestCase)
  private
    procedure FindDeclarations(Filename, Marker: string);
  published
    procedure TestFindDeclaration_Base;
    procedure TestFindDeclaration_NestedClasses;
    procedure TestFindDeclaration_ClassHelper;
  end;

var
  BugsTestSuite: TTestSuite;
  FindDeclarationTestSuite: TTestSuite;

procedure AddToBugsTestSuite(ATest: TTest);
procedure AddToFindDeclarationTestSuite(ATestClass: TClass);

implementation

procedure AddToBugsTestSuite(ATest: TTest);
begin
  BugsTestSuite.AddTest(ATest);
end;

procedure AddToFindDeclarationTestSuite(ATestClass: TClass);
begin
  FindDeclarationTestSuite.AddTestSuiteFromClass(ATestClass);
end;

{ TTestFindDeclaration }

procedure TTestFindDeclaration.FindDeclarations(Filename,
  Marker: string);

  procedure PrependPath(Prefix: string; var Path: string);
  begin
    if Path<>'' then Path:='.'+Path;
    Path:=Prefix+Path;
  end;

var
  Code: TCodeBuffer;
  Tool: TCodeTool;
  p: Integer;
  StartPos: Integer;
  ExpectedPath: String;
  PathPos: Integer;
  CursorPos, FoundCursorPos: TCodeXYPosition;
  FoundTopLine: integer;
  FoundTool: TFindDeclarationTool;
  FoundCleanPos: Integer;
  FoundNode: TCodeTreeNode;
  FoundPath: String;
begin
  Filename:=TrimAndExpandFilename(Filename);
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  if Code=nil then
    raise Exception.Create('unable to load '+Filename);
  if not CodeToolBoss.Explore(Code,Tool,true) then begin
    AssertEquals('parse error '+CodeToolBoss.ErrorMessage,false,true);
    exit;
  end;
  p:=1;
  while p<Tool.SrcLen do begin
    p:=FindNextComment(Tool.Src,p);
    if p>Tool.SrcLen then break;
    StartPos:=p;
    p:=FindCommentEnd(Tool.Src,p,Tool.Scanner.NestedComments);
    if Tool.Src[StartPos]<>'{' then continue;
    PathPos:=StartPos+1;
    //debugln(['TTestFindDeclaration.FindDeclarations Comment: ',dbgstr(Tool.Src,StartPos,p-StartPos)]);
    if copy(Tool.Src,PathPos,length(Marker))<>Marker then continue;
    PathPos+=length(Marker);
    ExpectedPath:=copy(Tool.Src,PathPos,p-1-PathPos);
    //debugln(['TTestFindDeclaration.FindDeclarations ExpectedPath=',ExpectedPath]);
    Tool.CleanPosToCaret(StartPos-1,CursorPos);
    if not CodeToolBoss.FindDeclaration(CursorPos.Code,CursorPos.X,CursorPos.Y,
      FoundCursorPos.Code,FoundCursorPos.X,FoundCursorPos.Y,FoundTopLine)
    then begin
      AssertEquals('find declaration failed at '+Tool.CleanPosToStr(StartPos-1)+': '+CodeToolBoss.ErrorMessage,false,true);
      continue;
    end else begin
      FoundTool:=CodeToolBoss.GetCodeToolForSource(FoundCursorPos.Code,true,true) as TFindDeclarationTool;
      FoundTool.CaretToCleanPos(FoundCursorPos,FoundCleanPos);
      FoundNode:=FoundTool.FindDeepestNodeAtPos(FoundCleanPos,true);
      FoundPath:='';
      while FoundNode<>nil do begin
        case FoundNode.Desc of
        ctnTypeDefinition,ctnVarDefinition,ctnConstDefinition:
          PrependPath(GetIdentifier(@FoundTool.Src[FoundNode.StartPos]),FoundPath);
        ctnInterface,ctnUnit:
          PrependPath(FoundTool.GetSourceName(false),FoundPath);
        end;
        FoundNode:=FoundNode.Parent;
      end;
      //debugln(['TTestFindDeclaration.FindDeclarations FoundPath=',FoundPath]);
      AssertEquals('find declaration wrong at '+Tool.CleanPosToStr(StartPos-1),LowerCase(ExpectedPath),LowerCase(FoundPath));
    end;
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_Base;
begin
  FindDeclarations('fdt_classhelper.pas','declaration:');
end;

procedure TTestFindDeclaration.TestFindDeclaration_NestedClasses;
begin
  FindDeclarations('fdt_nestedclasses.pas','declaration:');
end;

procedure TTestFindDeclaration.TestFindDeclaration_ClassHelper;
begin
  FindDeclarations('fdt_classhelper.pas','declaration-classhelper:');
end;

initialization
  GetTestRegistry.TestName := 'All tests';
  BugsTestSuite := TTestSuite.Create('Bugs');
  GetTestRegistry.AddTest(BugsTestSuite);
  FindDeclarationTestSuite := TTestSuite.Create('Parser');
  GetTestRegistry.AddTest(FindDeclarationTestSuite);

  AddToFindDeclarationTestSuite(TTestFindDeclaration);
end.

