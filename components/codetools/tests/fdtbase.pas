{
 Test with:
     ./finddeclarationtest --format=plain --suite=TTestFindDeclarationClassHelper
     ./finddeclarationtest --format=plain --suite=TestFindDeclaration_base
     ./finddeclarationtest --format=plain --suite=TestFindDeclaration_classhelper
}
unit fdtbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CodeToolManager, ExprEval, CodeCache, BasicCodeTools,
  CustomCodeTool, CodeTree, FindDeclarationTool, LazLogger, LazFileUtils,
  fpcunit, testregistry;

type

  { TTestFindDeclarationClassHelper }

  TTestFindDeclarationClassHelper = class(TTestCase)
  private
    procedure FindDeclarations(Filename, Marker: string);
  published
    procedure TestFindDeclaration_base;
    procedure TestFindDeclaration_classhelper;
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

{ TTestFindDeclarationClassHelper }

procedure TTestFindDeclarationClassHelper.FindDeclarations(Filename,
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
  CursorPos, TargetCursorPos: TCodeXYPosition;
  TargetTopLine: integer;
  TargetTool: TFindDeclarationTool;
  TargetCleanPos: Integer;
  TargetNode: TCodeTreeNode;
  TargetPath: String;
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
    if copy(Tool.Src,PathPos,length(Marker))<>Marker then continue;
    PathPos+=length(Marker);
    ExpectedPath:=copy(Tool.Src,PathPos,p-1-PathPos);
    //debugln(['TTestFindDeclarationClassHelper.FindDeclarations ',ExpectedPath]);
    Tool.CleanPosToCaret(StartPos-1,CursorPos);
    if not CodeToolBoss.FindDeclaration(CursorPos.Code,CursorPos.X,CursorPos.Y,
      TargetCursorPos.Code,TargetCursorPos.X,TargetCursorPos.Y,TargetTopLine)
    then begin
      AssertEquals('find declaration failed at '+Tool.CleanPosToStr(StartPos-1)+': '+CodeToolBoss.ErrorMessage,false,true);
      continue;
    end else begin
      TargetTool:=CodeToolBoss.GetCodeToolForSource(TargetCursorPos.Code,true,true) as TFindDeclarationTool;
      TargetTool.CaretToCleanPos(TargetCursorPos,TargetCleanPos);
      TargetNode:=TargetTool.FindDeepestNodeAtPos(TargetCleanPos,true);
      TargetPath:='';
      while TargetNode<>nil do begin
        case TargetNode.Desc of
        ctnTypeDefinition,ctnVarDefinition,ctnConstDefinition:
          PrependPath(GetIdentifier(@TargetTool.Src[TargetNode.StartPos]),TargetPath);
        ctnInterface,ctnUnit:
          PrependPath(TargetTool.GetSourceName(false),TargetPath);
        end;
        TargetNode:=TargetNode.Parent;
      end;
      AssertEquals('find declaration wrong at '+Tool.CleanPosToStr(StartPos-1),LowerCase(ExpectedPath),LowerCase(TargetPath));
    end;
  end;
end;

procedure TTestFindDeclarationClassHelper.TestFindDeclaration_base;
begin
  FindDeclarations('fdt_classhelper.pas','declaration:');
end;

procedure TTestFindDeclarationClassHelper.TestFindDeclaration_classhelper;
begin
  FindDeclarations('fdt_classhelper.pas','declaration-classhelper:');
end;

initialization
  GetTestRegistry.TestName := 'All tests';
  BugsTestSuite := TTestSuite.Create('Bugs');
  GetTestRegistry.AddTest(BugsTestSuite);
  FindDeclarationTestSuite := TTestSuite.Create('Parser');
  GetTestRegistry.AddTest(FindDeclarationTestSuite);

  AddToFindDeclarationTestSuite(TTestFindDeclarationClassHelper);
end.

