{
 Test with:
   ./finddeclarationtest --format=plain --suite=TTestFindDeclaration
   ./finddeclarationtest --format=plain --suite=TestFindDeclaration_Basic
   ./finddeclarationtest --format=plain --suite=TestFindDeclaration_NestedClasses
   ./finddeclarationtest --format=plain --suite=TestFindDeclaration_ClassHelper
   ./finddeclarationtest --format=plain --suite=TestFindDeclaration_TypeHelper
   ./finddeclarationtest --format=plain --suite=TestFindDeclaration_ObjCClass
   ./finddeclarationtest --format=plain --suite=TestFindDeclaration_ObjCCategory
}
unit fdtbase;

{$mode objfpc}{$H+}

{off $define VerboseFindDeclarationTests}

interface

uses
  Classes, SysUtils, CodeToolManager, ExprEval, CodeCache, BasicCodeTools,
  CustomCodeTool, CodeTree, FindDeclarationTool, KeywordFuncLists,
  IdentCompletionTool, LazLogger, LazFileUtils, fpcunit, testregistry;

type

  { TTestFindDeclaration }

  TTestFindDeclaration = class(TTestCase)
  private
    procedure FindDeclarations(Filename: string);
  published
    procedure TestFindDeclaration_Basic;
    procedure TestFindDeclaration_NestedClasses;
    procedure TestFindDeclaration_ClassHelper;
    procedure TestFindDeclaration_TypeHelper;
    {$IFDEF Darwin}
    procedure TestFindDeclaration_ObjCClass;
    procedure TestFindDeclaration_ObjCCategory;
    {$ENDIF}
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

  function NodeAsPath(Tool: TFindDeclarationTool; Node: TCodeTreeNode): string;
  begin
    Result:='';
    while Node<>nil do begin
      case Node.Desc of
      ctnTypeDefinition,ctnVarDefinition,ctnConstDefinition:
        PrependPath(GetIdentifier(@Tool.Src[Node.StartPos]),Result);
      ctnInterface,ctnUnit:
        PrependPath(Tool.GetSourceName(false),Result);
      ctnProcedureHead:
        PrependPath(Tool.ExtractProcName(Node,[]),Result);
      end;
      Node:=Node.Parent;
    end;
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
  NameStartPos, i, l: Integer;
  Marker: String;
  IdentItem: TIdentifierListItem;
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

      // test FindDeclaration
      if not CodeToolBoss.FindDeclaration(CursorPos.Code,CursorPos.X,CursorPos.Y,
        FoundCursorPos.Code,FoundCursorPos.X,FoundCursorPos.Y,FoundTopLine)
      then begin
        if ExpectedPath<>'' then
          AssertEquals('find declaration failed at '+Tool.CleanPosToStr(NameStartPos-1,true)+': '+CodeToolBoss.ErrorMessage,false,true);
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
          FoundPath:=NodeAsPath(FoundTool,FoundNode);
        end;
        //debugln(['TTestFindDeclaration.FindDeclarations FoundPath=',FoundPath]);
        AssertEquals('find declaration wrong at '+Tool.CleanPosToStr(NameStartPos-1,true),LowerCase(ExpectedPath),LowerCase(FoundPath));
      end;

      // test identifier completion
      if ExpectedPath<>'' then begin
        if not CodeToolBoss.GatherIdentifiers(CursorPos.Code,CursorPos.X,CursorPos.Y)
        then begin
          if ExpectedPath<>'' then
            AssertEquals('GatherIdentifiers failed at '+Tool.CleanPosToStr(NameStartPos-1,true)+': '+CodeToolBoss.ErrorMessage,false,true);
          continue;
        end else begin
          i:=CodeToolBoss.IdentifierList.GetFilteredCount-1;
          while i>=0 do begin
            IdentItem:=CodeToolBoss.IdentifierList.FilteredItems[i];
            l:=length(IdentItem.Identifier);
            if ((l=length(ExpectedPath)) or (ExpectedPath[length(ExpectedPath)-l]='.'))
            and (CompareText(IdentItem.Identifier,RightStr(ExpectedPath,l))=0)
            then break;
            dec(i);
          end;
          AssertEquals('GatherIdentifiers misses "'+ExpectedPath+'" at '+Tool.CleanPosToStr(NameStartPos-1,true),true,i>=0);
        end;
      end;
    end else begin
      AssertEquals('Unknown marker at '+Tool.CleanPosToStr(NameStartPos-1,true),'declaration',Marker);
      continue;
    end;
  end;
end;

procedure TTestFindDeclaration.TestFindDeclaration_Basic;
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

{$IFDEF Darwin}
procedure TTestFindDeclaration.TestFindDeclaration_ObjCClass;
begin
  FindDeclarations('fdt_objcclass.pas');
end;

procedure TTestFindDeclaration.TestFindDeclaration_ObjCCategory;
begin
  FindDeclarations('fdt_objccategory.pas');
end;
{$ENDIF}

initialization
  GetTestRegistry.TestName := 'All tests';
  BugsTestSuite := TTestSuite.Create('Bugs');
  GetTestRegistry.AddTest(BugsTestSuite);
  FindDeclarationTestSuite := TTestSuite.Create('Parser');
  GetTestRegistry.AddTest(FindDeclarationTestSuite);

  AddToFindDeclarationTestSuite(TTestFindDeclaration);
end.

