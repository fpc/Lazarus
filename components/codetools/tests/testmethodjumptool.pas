{
 Test with:
     ./runtests --format=plain --suite=TTestMethodJumpTool
     ./runtests --format=plain --suite=TestFindJumpPointIncFilewithIntfAndImpl
}
unit TestMethodJumpTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, LazLoggerBase,
  CodeToolManager, StdCodeTools, CodeCache, LinkScanner, TestPascalParser;

type

  { TTestMethodJumpTool }

  TTestMethodJumpTool = class(TCustomTestPascalParser)
  private
    procedure GetCTMarker(aCode: TCodeBuffer; Comment: string; out Position: TPoint;
      LeftOfComment: boolean = true);
    function GetInfo(aCode: TCodeBuffer; XY: TPoint): string;
  published
    procedure TestFindJumpPointIncFilewithIntfAndImpl;
    procedure TestMethodJump_IntfToImplSingleProc;
  end;


implementation

{ TTestMethodJumpTool }

procedure TTestMethodJumpTool.GetCTMarker(aCode: TCodeBuffer; Comment: string;
  out Position: TPoint; LeftOfComment: boolean);
var
  p: SizeInt;
begin
  Position:=Point(0,0);
  if Comment[1]<>'{' then
    Comment:='{'+Comment+'}';
  p:=System.Pos(Comment,aCode.Source);
  if p<1 then begin
    WriteSource(CodeXYPosition(1,1,Code));
    Fail('TTestMethodJumpTool.GetCTMarker, missing marker "'+Comment+'" in "'+Code.Filename+'"');
  end;
  if not LeftOfComment then
    inc(p,length(Comment));
  aCode.AbsoluteToLineCol(p,Position.Y,Position.X);
  if Position.Y<1 then begin
    WriteSource(CodeXYPosition(Position.X,Position.Y,Code));
    Fail('TTestMethodJumpTool.GetCTMarker, Code.AbsoluteToLineCol: "'+Comment+'" Pos='+dbgs(Position)+' in "'+Code.Filename+'"');
  end;
end;

function TTestMethodJumpTool.GetInfo(aCode: TCodeBuffer; XY: TPoint): string;
var
  Line: String;
begin
  Line:=aCode.GetLine(XY.Y-1);
  Result:=dbgs(XY)+': '+copy(Line,1,XY.X-1)+'|'+copy(Line,XY.X,length(Line));
end;

procedure TTestMethodJumpTool.TestFindJumpPointIncFilewithIntfAndImpl;

  procedure Test(aTitle: string; Code: TCodeBuffer;
    StartMarker: string; LeftOfStart: boolean;
    EndMarker: string; LeftOfEnd: boolean);
  var
    BlockStart: TPoint;
    BlockEnd: TPoint;
    NewCode: TCodeBuffer;
    NewX: integer;
    NewY: integer;
    NewTopline, BlockTopLine, BlockBottomLine: integer;
    RevertableJump: boolean;
  begin
    GetCTMarker(Code,StartMarker,BlockStart,LeftOfStart);
    GetCTMarker(Code,EndMarker,BlockEnd,LeftOfEnd);
    //debugln(['TTestCTStdCodetools.TestCTStdFindBlockStart BlockStart=',GetInfo(BlockStart),' BlockEnd=',GetInfo(BlockEnd)]);
    if not CodeToolBoss.JumpToMethod(Code,BlockStart.X,BlockStart.Y,
      NewCode,NewX,NewY,NewTopline,BlockTopLine,BlockBottomLine,RevertableJump)
    then
      AssertEquals(aTitle+': '+CodeToolBoss.ErrorMessage,true,false)
    else
      AssertEquals(aTitle,GetInfo(Code,BlockEnd),GetInfo(NewCode,Point(NewX,NewY)))
  end;

var
  IncCode: TCodeBuffer;
begin
  Code.Source:=''
    +'unit Test1;'+LineEnding
    +'interface'+LineEnding
    +'{$DEFINE UseInterface}'
    +'{$I TestMethodJumpTool2.inc}'+LineEnding
    +'{$UNDEF UseInterface}'+LineEnding
    +'implementation'+LineEnding
    +'{$DEFINE UseImplementation}'
    +'{$I TestMethodJumpTool2.inc}'+LineEnding
    +'end.'+LineEnding;
  IncCode:=CodeToolBoss.CreateFile('TestMethodJumpTool2.inc');
  IncCode.Source:=''
    +'{%MainUnit test1.pas}'+LineEnding
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

procedure TTestMethodJumpTool.TestMethodJump_IntfToImplSingleProc;

  procedure TestJumpToMethod(FromMarker: string; LeftFrom: boolean;
    ToMarker: string; LeftTo: boolean);
  var
    FromPos, ToPos: TPoint;
    NewCode: TCodeBuffer;
    NewX, NewY, NewTopLine, BlockTopLine, BlockBottomLine: integer;
    RevertableJump: boolean;
  begin
    GetCTMarker(Code,FromMarker,FromPos,LeftFrom);
    GetCTMarker(Code,ToMarker,ToPos,LeftTo);
    CodeToolBoss.JumpToMethod(Code,FromPos.X,FromPos.Y,NewCode, NewX, NewY,
      NewTopLine, BlockTopLine, BlockBottomLine, RevertableJump);
    if NewCode<>Code then
      AssertEquals('JumpToMethod jumped to wrong file, From line='+IntToStr(FromPos.Y)+' col='+IntToStr(FromPos.X),
        Code.Filename,NewCode.Filename);
    if NewY<>ToPos.Y then
      AssertEquals('JumpToMethod jumped to wrong line, From line='+IntToStr(FromPos.Y)+' col='+IntToStr(FromPos.X),
        ToPos.Y,NewY);
    if NewX<>ToPos.X then
      AssertEquals('JumpToMethod jumped to wrong line, From line='+IntToStr(FromPos.Y)+' col='+IntToStr(FromPos.X),
        ToPos.X,NewX);
  end;

begin
  Add([
  'unit Test1;',
  '{$mode objfpc}{$H+}',
  'interface',
  'procedure {a}DoIt;',
  'implementation',
  'procedure DoIt;',
  'begin',
  '  {b}',
  'end;',
  'end.']);
  TestJumpToMethod('a',false,'b',true);
end;

initialization
  RegisterTest(TTestMethodJumpTool);

end.

