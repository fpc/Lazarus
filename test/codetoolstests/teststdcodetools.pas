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
    +'    {try1}try'+LineEnding
    +'      writeln;'+LineEnding
    +'    {try1finally}finally'+LineEnding
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

  procedure Test(aTitle, StartMarker,EndMarker: string);
  var
    BlockStart: TPoint;
    BlockEnd: TPoint;
    NewCode: TCodeBuffer;
    NewX: integer;
    NewY: integer;
    NewTopline: integer;
  begin
    BlockStart:=GetMarker(StartMarker);
    BlockEnd:=GetMarker(EndMarker);
    //debugln(['TTestCTStdCodetools.TestCTStdFindBlockStart BlockStart=',GetInfo(BlockStart),' BlockEnd=',GetInfo(BlockEnd)]);
    if not CodeToolBoss.FindBlockStart(Code,BlockEnd.X,BlockEnd.Y,NewCode,NewX,NewY,NewTopline)
    then
      AssertEquals(aTitle+': '+CodeToolBoss.ErrorMessage,true,false)
    else
      AssertEquals(aTitle,GetInfo(BlockStart),GetInfo(Point(NewX,NewY)))
  end;

begin
  Code:=CodeToolBoss.CreateFile('TestStdCodeTools.pas');

  // scan source
  Code.Source:=GetSource();

  Test('begin,try,finally,end|end','begin1','begin1end');
  Test('begin,try,finally,|end,end','try1finally','try1end');
  Test('begin,try,finally,|end,end','try1','try1finally');
end;

initialization
  AddToCodetoolsTestSuite(TTestCTStdCodetools);

end.

