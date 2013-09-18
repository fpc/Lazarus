{
 Test with:
     ./runtests --format=plain --suite=TTestCTStdCodetools
     ./runtests --format=plain --suite=TestCTStdFindBlockStart
     ./runtests --format=plain --suite=TestCTRemoveUnitFromAllUsesSections
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
  private
    function GetCTMarker(Code: TCodeBuffer; Comment: string; out Position: TPoint): boolean;
  published
    procedure TestCTStdFindBlockStart;
    procedure TestCTRemoveUnitFromAllUsesSections;
  end;

implementation

{ TTestCTStdCodetools }

function TTestCTStdCodetools.GetCTMarker(Code: TCodeBuffer; Comment: string;
  out Position: TPoint): boolean;
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
  Code.AbsoluteToLineCol(p+length(Comment),Position.Y,Position.X);
  if Position.Y<1 then
    AssertEquals('Code.AbsoluteToLineCol: '+Comment,true,Position.Y>=1)
  else
    Result:=true;
end;

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
    if not GetCTMarker(Code,StartMarker,BlockStart) then exit;
    if not GetCTMarker(Code,EndMarker,BlockEnd) then exit;
    //debugln(['TTestCTStdCodetools.TestCTStdFindBlockStart BlockStart=',GetInfo(BlockStart),' BlockEnd=',GetInfo(BlockEnd)]);
    if not CodeToolBoss.FindBlockStart(Code,BlockEnd.X,BlockEnd.Y,NewCode,NewX,NewY,NewTopline)
    then
      AssertEquals(aTitle+': '+CodeToolBoss.ErrorMessage,true,false)
    else
      AssertEquals(aTitle,GetInfo(BlockStart),GetInfo(Point(NewX,NewY)))
  end;

begin
  Code:=CodeToolBoss.CreateFile('TestStdCodeTools.pas');
  Code.Source:=GetSource();

  Test('begin,try,finally,end|end','begin1','begin1end');
  Test('begin,try,finally,|end,end','try1finally','try1end');
  Test('begin,try,finally,|end,end','try1','try1finally');
end;

procedure TTestCTStdCodetools.TestCTRemoveUnitFromAllUsesSections;

  function GetSource(UsesSrc: string): string;
  begin
    Result:='program TestStdCodeTools;'+LineEnding
      +UsesSrc
  end;

  procedure Test(RemoveUnit, UsesSrc, ExpectedUsesSrc: string);
  var
    Header: String;
    Footer: String;
    Code: TCodeBuffer;
    Src: String;
  begin
    Header:='program TestStdCodeTools;'+LineEnding;
    Footer:=LineEnding
      +'begin'+LineEnding
      +'end.'+LineEnding;
    Code:=CodeToolBoss.CreateFile('TestStdCodeTools.pas');
    Code.Source:=Header+UsesSrc+Footer;
    if not CodeToolBoss.RemoveUnitFromAllUsesSections(Code,RemoveUnit) then
    begin
      AssertEquals('RemoveUnitFromAllUsesSections failed: '+CodeToolBoss.ErrorMessage,true,false);
    end else begin
      Src:=Code.Source;
      AssertEquals('RemoveUnitFromAllUsesSections altered header: ',Header,LeftStr(Src,length(Header)));
      System.Delete(Src,1,length(Header));
      AssertEquals('RemoveUnitFromAllUsesSections altered footer: ',Footer,RightStr(Src,length(Footer)));
      System.Delete(Src,length(Src)-length(Footer)+1,length(Footer));
      AssertEquals('RemoveUnitFromAllUsesSections: ',ExpectedUsesSrc,Src);
    end;
  end;

begin
  Test('shellapi',
   'uses'+LineEnding
  +'   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,'+LineEnding
  +'   Dialogs, shellAPI, StdCtrls, ExtCtrls, ComCtrls, strutils, Buttons, inifiles;'+LineEnding
  ,
   'uses'+LineEnding
  +'   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,'+LineEnding
  +'   Dialogs, StdCtrls, ExtCtrls, ComCtrls, strutils, Buttons, inifiles;'+LineEnding
  );
end;

initialization
  AddToCodetoolsTestSuite(TTestCTStdCodetools);

end.

