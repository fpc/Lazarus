{
 Test with:
     ./runtests --format=plain --suite=TTestCTStdCodetools
     ./runtests --format=plain --suite=TestCTStdFindBlockStart
}
unit TestStdCodetools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, testglobals, fpcunit,
  CodeToolManager, StdCodeTools, CodeCache, LinkScanner;

type

  { TTestCTStdCodetools }

  TTestCTStdCodetools = class(TTestCase)
  published
    procedure TestCTStdFindBlockStart;
  end;

implementation

{ TTestCTStdCodetools }

procedure TTestCTStdCodetools.TestCTStdFindBlockStart;
var
  Code: TCodeBuffer;

  function GetSource: string;
  begin
    Result:=
     'program TestStdCodeTools;'+LineEnding
    +'begin'+LineEnding
    +'  if true then {begin1}begin'+LineEnding
    +'    {try}try'+LineEnding
    +'      writeln;'+LineEnding
    +'    finally'+LineEnding
    +'      writeln;'+LineEnding
    +'    {try1end}end;'+LineEnding
    +'    writeln;'+LineEnding
    +'  {begin1end}end;'+LineEnding
    +'end.'+LineEnding;
  end;

  function GetMarker(Comment: string): TPoint;
  var
    p: SizeInt;
  begin
    Result:=Point(0,0);
    if Comment[1]<>'{' then
      Comment:='{'+Comment+'}';
    p:=System.Pos(Comment,Code.Source);
    if p<1 then
      AssertEquals('searching marker: '+Comment,true,p>=1);
    Code.AbsoluteToLineCol(p+length(Comment),Result.Y,Result.X);
    if Result.Y<1 then
      AssertEquals('Code.AbsoluteToLineCol: '+Comment,true,Result.Y>=1);
  end;

  function GetInfo(XY: TPoint): string;
  var
    Line: String;
  begin
    Line:=Code.GetLine(XY.Y-1);
    Result:=dbgs(XY)+': '+copy(Line,1,XY.X-1)+'|'+copy(Line,XY.X,length(Line));
  end;

var
  Tool: TCodeTool;
  BlockStart: TPoint;
  BlockEnd: TPoint;
  NewCode: TCodeBuffer;
  NewX: integer;
  NewY: integer;
  NewTopline: integer;
begin
  Code:=CodeToolBoss.CreateFile('TestStdCodeTools.pas');
  Tool:=CodeToolBoss.GetCodeToolForSource(Code,false,true) as TCodeTool;

  // scan source
  Code.Source:=GetSource();
  Tool.BuildTree(lsrEnd);

  BlockStart:=GetMarker('{begin1}');
  BlockEnd:=GetMarker('{begin1end}');
  debugln(['TTestCTStdCodetools.TestCTStdFindBlockStart BlockStart=',GetInfo(BlockStart),' BlockEnd=',GetInfo(BlockEnd)]);
  if not CodeToolBoss.FindBlockStart(Code,BlockEnd.X,BlockEnd.Y,NewCode,NewX,NewY,NewTopline)
  then
    AssertEquals('CodeToolBoss.FindBlockStart: begin,try,finally,end|end: '+CodeToolBoss.ErrorMessage,true,false)
  else
    AssertEquals('CodeToolBoss.FindBlockStart: begin,try,finally,end|end:',GetInfo(BlockStart),GetInfo(Point(NewX,NewY)))

end;

initialization
  AddToCodetoolsTestSuite(TTestCTStdCodetools);

end.

