{
 Test with:
     ./runtests --format=plain --suite=TTestCTMethodJumpTool
     ./runtests --format=plain --suite=TestCTFindJumpPointIncFilewithIntfAndImpl
}
unit TestMethodJumpTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, testglobals, fpcunit,
  CodeToolManager, StdCodeTools, CodeCache, LinkScanner;

type

  { TTestCTMethodJumpTool }

  TTestCTMethodJumpTool = class(TTestCase)
  private
    function GetCTMarker(Code: TCodeBuffer; Comment: string; out Position: TPoint;
      LeftOfComment: boolean = true): boolean;
    function GetInfo(Code: TCodeBuffer; XY: TPoint): string;
  published
    procedure TestCTFindJumpPointIncFilewithIntfAndImpl;
  end;


implementation

{ TTestCTMethodJumpTool }

function TTestCTMethodJumpTool.GetCTMarker(Code: TCodeBuffer; Comment: string;
  out Position: TPoint; LeftOfComment: boolean): boolean;
var
  p: SizeInt;
begin
  Result:=false;
  Position:=Point(0,0);
  if Comment[1]<>'{' then
    Comment:='{'+Comment+'}';
  p:=System.Pos(Comment,Code.Source);
  if p<1 then
    AssertEquals('searching marker: '+Comment,true,p>=1);
  if not LeftOfComment then
    inc(p,length(Comment));
  Code.AbsoluteToLineCol(p,Position.Y,Position.X);
  if Position.Y<1 then
    AssertEquals('Code.AbsoluteToLineCol: '+Comment,true,Position.Y>=1)
  else
    Result:=true;
end;

function TTestCTMethodJumpTool.GetInfo(Code: TCodeBuffer; XY: TPoint): string;
var
  Line: String;
begin
  Line:=Code.GetLine(XY.Y-1);
  Result:=dbgs(XY)+': '+copy(Line,1,XY.X-1)+'|'+copy(Line,XY.X,length(Line));
end;

procedure TTestCTMethodJumpTool.TestCTFindJumpPointIncFilewithIntfAndImpl;

  procedure Test(aTitle: string; Code: TCodeBuffer;
    StartMarker: string; LeftOfStart: boolean;
    EndMarker: string; LeftOfEnd: boolean);
  var
    BlockStart: TPoint;
    BlockEnd: TPoint;
    NewCode: TCodeBuffer;
    NewX: integer;
    NewY: integer;
    NewTopline: integer;
    RevertableJump: boolean;
  begin
    if not GetCTMarker(Code,StartMarker,BlockStart,LeftOfStart) then exit;
    if not GetCTMarker(Code,EndMarker,BlockEnd,LeftOfEnd) then exit;
    //debugln(['TTestCTStdCodetools.TestCTStdFindBlockStart BlockStart=',GetInfo(BlockStart),' BlockEnd=',GetInfo(BlockEnd)]);
    if not CodeToolBoss.JumpToMethod(Code,BlockStart.X,BlockStart.Y,
      NewCode,NewX,NewY,NewTopline,RevertableJump)
    then
      AssertEquals(aTitle+': '+CodeToolBoss.ErrorMessage,true,false)
    else
      AssertEquals(aTitle,GetInfo(Code,BlockEnd),GetInfo(NewCode,Point(NewX,NewY)))
  end;

var
  UnitCode: TCodeBuffer;
  IncCode: TCodeBuffer;
begin
  UnitCode:=CodeToolBoss.CreateFile('TestMethodJumpTool1.pas');
  IncCode:=CodeToolBoss.CreateFile('TestMethodJumpTool2.inc');
  UnitCode.Source:=''
    +'unit TestMethodJumpTool1;'+LineEnding
    +'interface'+LineEnding
    +'{$DEFINE UseInterface}'
    +'{$I TestMethodJumpTool2.inc}'+LineEnding
    +'{$UNDEF UseInterface}'+LineEnding
    +'implementation'+LineEnding
    +'{$DEFINE UseImplementation}'
    +'{$I TestMethodJumpTool2.inc}'+LineEnding
    +'end.'+LineEnding;
  IncCode.Source:=''
    +'{%MainUnit TestMethodJumpTool1.pas}'+LineEnding
    +'{$IFDEF UseInterface}'+LineEnding
    +'procedure {ProcHeader}DoSomething;'+LineEnding
    +'{$ENDIF}'+LineEnding
    +'{$IFDEF UseImplementation}'+LineEnding
    +'procedure DoSomething;'+LineEnding
    +'begin'+LineEnding
    +'  {ProcBody}writeln;'+LineEnding
    +'end;'+LineEnding
    +'{$ENDIF}'+LineEnding;

  Test('Method jump from interface to implementation in one include file',
       IncCode,'ProcHeader',false,'ProcBody',true);
  Test('Method jump from implementation to interface in one include file',
       IncCode,'ProcBody',false,'ProcHeader',false);
end;

initialization
  AddToPascalTestSuite(TTestCTMethodJumpTool);

end.

