{
 Test with:
   ./finddeclarationtest --format=plain --suite=TTestFindDeclaration
   ./finddeclarationtest --format=plain --suite=TestFindDeclaration_Base
   ./finddeclarationtest --format=plain --suite=TestFindDeclaration_NestedClasses
   ./finddeclarationtest --format=plain --suite=TestFindDeclaration_ClassHelper
   ./finddeclarationtest --format=plain --suite=TestFindDeclaration_TypeHelper
   ./finddeclarationtest --format=plain --suite=TestFindDeclaration_ObjCCategory
}
unit fdtbase;

{$mode objfpc}{$H+}

{off $define VerboseFindDeclarationTests}

interface

uses
  Classes, SysUtils, CodeToolManager, ExprEval, CodeCache, BasicCodeTools,
  CustomCodeTool, CodeTree, FindDeclarationTool, KeywordFuncLists, LazLogger,
  LazFileUtils, fpcunit, testregistry;

type

  { TTestFindDeclaration }

  TTestFindDeclaration = class(TTestCase)
  private
    procedure FindDeclarations(Filename: string);
  published
    procedure TestFindDeclaration_Base;
    procedure TestFindDeclaration_NestedClasses;
    procedure TestFindDeclaration_ClassHelper;
    procedure TestFindDeclaration_TypeHelper;
    procedure TestFindDeclaration_ObjCCategory;
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

procedure TTestFindDeclaration.FindDeclarations(Filename: string);

  procedure PrependPath(Prefix: string; var Path: string);
  begin
    if Path<>'' then Path:='.'+Path;
    Path:=Prefix+Path;
  end;

var
  Code: TCodeBuffer;
  Tool: TCodeTool;
  CommentP: Integer;
  p: Integer;
  ExpectedPath: String;
  PathPos: Integer;
  CursorPos, FoundCursorPos: TCodeXYPosition;
  FoundTopLine: integer;
  FoundTool: TFindDeclarationTool;
  FoundCleanPos: Integer;
  FoundNode: TCodeTreeNode;
  FoundPath: String;
  Src: String;
  NameStartPos: Integer;
  Marker: String;
begin
  Filename:=TrimAndExpandFilename(Filename);
  {$IFDEF VerboseFindDeclarationTests}
  debugln(['TTestFindDeclaration.FindDeclarations File=',Filename]);
  {$ENDIF}
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  if Code=nil then
    raise Exception.Create('unable to load '+Filename);
  if not CodeToolBoss.Explore(Code,Tool,true) then begin
    AssertEquals('parse error '+CodeToolBoss.ErrorMessage,false,true);
    exit;
  end;
  CommentP:=1;
  Src:=Tool.Src;
  while CommentP<length(Src) do begin
    CommentP:=FindNextComment(Src,CommentP);
    if CommentP>length(Src) then break;
    p:=CommentP;
    CommentP:=FindCommentEnd(Src,CommentP,Tool.Scanner.NestedComments);
    if Src[p]<>'{' then continue;
    inc(p);
    NameStartPos:=p;
    if not IsIdentStartChar[Src[p]] then continue;
    while (p<=length(Src)) and (IsIdentChar[Src[p]]) do inc(p);
    Marker:=copy(Src,NameStartPos,p-NameStartPos);
    if (p>length(Src)) or (Src[p]<>':') then begin
      AssertEquals('Expected : at '+Tool.CleanPosToStr(p,true),'declaration',Marker);
      continue;
    end;
    inc(p);
    PathPos:=p;
    //debugln(['TTestFindDeclaration.FindDeclarations params: ',dbgstr(Tool.Src,p,CommentP-p)]);
    if Marker='declaration' then begin
      ExpectedPath:=copy(Src,PathPos,CommentP-1-PathPos);
      //debugln(['TTestFindDeclaration.FindDeclarations ExpectedPath=',ExpectedPath]);
      {$IFDEF VerboseFindDeclarationTests}
      debugln(['TTestFindDeclaration.FindDeclarations searching "',Marker,'" at ',Tool.CleanPosToStr(NameStartPos-1),' ExpectedPath=',ExpectedPath]);
      {$ENDIF}
      Tool.CleanPosToCaret(NameStartPos-1,CursorPos);
      if not CodeToolBoss.FindDeclaration(CursorPos.Code,CursorPos.X,CursorPos.Y,
        FoundCursorPos.Code,FoundCursorPos.X,FoundCursorPos.Y,FoundTopLine)
      then begin
        if ExpectedPath<>'' then
          AssertEquals('find declaration failed at '+Tool.CleanPosToStr(NameStartPos-1)+': '+CodeToolBoss.ErrorMessage,false,true);
        continue;
      end else begin
        FoundTool:=CodeToolBoss.GetCodeToolForSource(FoundCursorPos.Code,true,true) as TFindDeclarationTool;
        FoundPath:='';
        if (FoundCursorPos.Y=1) and (FoundCursorPos.X=1) then begin
          // unit
          FoundPath:=ExtractFileNameOnly(FoundCursorPos.Code.Filename);
        end else begin
          FoundTool.CaretToCleanPos(FoundCursorPos,FoundCleanPos);
          FoundNode:=FoundTool.FindDeepestNodeAtPos(FoundCleanPos,true);
          while FoundNode<>nil do begin
            case FoundNode.Desc of
            ctnTypeDefinition,ctnVarDefinition,ctnConstDefinition:
              PrependPath(GetIdentifier(@FoundTool.Src[FoundNode.StartPos]),FoundPath);
            ctnInterface,ctnUnit:
              PrependPath(FoundTool.GetSourceName(false),FoundPath);
            ctnProcedureHead:
              PrependPath(FoundTool.ExtractProcName(FoundNode,[]),FoundPath);
            end;
            FoundNode:=FoundNode.Parent;
          end;
        end;
        //debugln(['TTestFindDeclaration.FindDeclarations FoundPath=',FoundPath]);
        AssertEquals('find declaration wrong at '+Tool.CleanPosToStr(NameStartPos-1),LowerCase(ExpectedPath),LowerCase(FoundPath));
      end;
    end else begin
      AssertEquals('Unknown marker at '+Tool.CleanPosToStr(NameStartPos,true),'declaration',Marker);
      continue;
    end;
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_Base;
begin
  FindDeclarations('fdt_basic.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_NestedClasses;
begin
  FindDeclarations('fdt_nestedclasses.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_ClassHelper;
begin
  FindDeclarations('fdt_classhelper.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_TypeHelper;
begin
  FindDeclarations('fdt_typehelper.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_ObjCCategory;
begin
  FindDeclarations('fdt_objccategory.pas');
end;

initialization
  GetTestRegistry.TestName := 'All tests';
  BugsTestSuite := TTestSuite.Create('Bugs');
  GetTestRegistry.AddTest(BugsTestSuite);
  FindDeclarationTestSuite := TTestSuite.Create('Parser');
  GetTestRegistry.AddTest(FindDeclarationTestSuite);

  AddToFindDeclarationTestSuite(TTestFindDeclaration);
end.

