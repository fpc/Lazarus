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
  Classes, SysUtils, LazLoggerBase, fpcunit, testregistry,
  CodeToolManager, StdCodeTools, CodeCache, LinkScanner, SourceChanger;

type

  { TCustomTestCTStdCodetools }

  TCustomTestCTStdCodetools = class(TTestCase)
  private
    function GetCTMarker(Code: TCodeBuffer; Comment: string; out Position: TPoint): boolean;
  protected
    procedure CheckDiff(Msg, Expected, Actual: string);
    procedure WriteSource(aFilename: string; Line, Col: integer);
  end;

  { TTestCTStdCodetools }

  TTestCTStdCodetools = class(TCustomTestCTStdCodetools)
  private
    procedure DoTestAddUnitWarn(Title: string; Src, Expected: array of string;
      WarnID, Comment: string; TurnOn: boolean);
    procedure DoTestAddUnitToMainUses(NewUnitName, NewUnitInFilename,
      UsesSrc, ExpectedUsesSrc: string; const Flags: TAddUsesFlags);
  published
    procedure TestCTStdFindBlockStart;
    procedure TestCTUses_AddUses_Start;
    procedure TestCTUses_AddUses_Append;
    procedure TestCTUses_AddUses_AppendKeepSpaces;
    procedure TestCTUses_AddUses_AppendKeepComment; // ToDo
    procedure TestCTUses_AddUses_Append_DottedNoBreak;
    procedure TestCTUses_RemoveFromAllUsesSections;
    procedure TestCTAddWarn5025_Program;
    procedure TestCTAddWarn5025_ProgramNoName;
    procedure TestCTAddWarn5025_Unit;
  end;

implementation

{ TTestCTStdCodetools }

function TCustomTestCTStdCodetools.GetCTMarker(Code: TCodeBuffer; Comment: string;
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

procedure TCustomTestCTStdCodetools.CheckDiff(Msg, Expected, Actual: string);
// search diff, ignore changes in spaces
const
  SpaceChars = [#9,#10,#13,' '];
var
  ExpectedP, ActualP: PChar;

  function FindLineEnd(p: PChar): PChar;
  begin
    Result:=p;
    while not (Result^ in [#0,#10,#13]) do inc(Result);
  end;

  function FindLineStart(p, MinP: PChar): PChar;
  begin
    while (p>MinP) and not (p[-1] in [#10,#13]) do dec(p);
    Result:=p;
  end;

  procedure DiffFound;
  var
    ActLineStartP, ActLineEndP, p, StartPos: PChar;
    ExpLine, ActLine: String;
    i: Integer;
  begin
    writeln('Diff found "',Msg,'". Lines:');
    // write correct lines
    p:=PChar(Expected);
    repeat
      StartPos:=p;
      while not (p^ in [#0,#10,#13]) do inc(p);
      ExpLine:=copy(Expected,StartPos-PChar(Expected)+1,p-StartPos);
      if p^ in [#10,#13] then begin
        if (p[1] in [#10,#13]) and (p^<>p[1]) then
          inc(p,2)
        else
          inc(p);
      end;
      if (p<=ExpectedP) and (p^<>#0) then begin
        writeln('= ',ExpLine);
      end else begin
        // diff line
        // write actual line
        ActLineStartP:=FindLineStart(ActualP,PChar(Actual));
        ActLineEndP:=FindLineEnd(ActualP);
        ActLine:=copy(Actual,ActLineStartP-PChar(Actual)+1,ActLineEndP-ActLineStartP);
        writeln('- ',ActLine);
        // write expected line
        writeln('+ ',ExpLine);
        // write empty line with pointer ^
        for i:=1 to 2+ExpectedP-StartPos do write(' ');
        writeln('^');
        AssertEquals(Msg,ExpLine,ActLine);
        break;
      end;
    until p^=#0;

    writeln('DiffFound Actual:-----------------------');
    writeln(Actual);
    writeln('DiffFound Expected:---------------------');
    writeln(Expected);
    writeln('DiffFound ------------------------------');
    Fail('diff found, but lines are the same, internal error');
  end;

var
  IsSpaceNeeded: Boolean;
  LastChar: Char;
begin
  if Expected='' then Expected:=' ';
  if Actual='' then Actual:=' ';
  ExpectedP:=PChar(Expected);
  ActualP:=PChar(Actual);
  repeat
    //writeln('TTestCodeCompletion.CheckDiff Exp="',ExpectedP^,'" Act="',ActualP^,'"');
    case ExpectedP^ of
    #0:
      begin
      // check that rest of Actual has only spaces
      while ActualP^ in SpaceChars do inc(ActualP);
      if ActualP^<>#0 then
        DiffFound;
      exit;
      end;
    ' ',#9,#10,#13:
      begin
      // skip space in Expected
      IsSpaceNeeded:=false;
      if ExpectedP>PChar(Expected) then
        LastChar:=ExpectedP[-1]
      else
        LastChar:=#0;
      while ExpectedP^ in SpaceChars do inc(ExpectedP);
      if (LastChar in ['a'..'z','A'..'Z','0'..'9','_','$'])
          and (ExpectedP^ in ['a'..'z','A'..'Z','0'..'9','_','$']) then
        IsSpaceNeeded:=true;
      if IsSpaceNeeded and (not (ActualP^ in SpaceChars)) then
        DiffFound;
      while ActualP^ in SpaceChars do inc(ActualP);
      end;
    else
      while ActualP^ in SpaceChars do inc(ActualP);
      if ExpectedP^<>ActualP^ then
        DiffFound;
      inc(ExpectedP);
      inc(ActualP);
    end;
  until false;
end;

procedure TCustomTestCTStdCodetools.WriteSource(aFilename: string; Line, Col: integer
  );
var
  Code: TCodeBuffer;
  s: String;
  i: Integer;
begin
  writeln('Testcode:-File="',aFilename,'"----------------------------------:');

  Code:=CodeToolBoss.FindFile(aFilename);
  if Code=nil then
    Fail('file was not loaded/created: "'+aFilename+'"');
  for i:=1 to Code.LineCount do begin
    s:=Code.GetLine(i-1,true);
    if i=Line then begin
      write('*');
      s:=LeftStr(s,Col-1)+'|'+copy(s,Col,length(s));
    end;
    if (s='') or not (s[length(s)] in [#10,#13]) then
      s+=LineEnding;
    write(Format('%:4d: ',[i]),s);
  end;
end;

procedure TTestCTStdCodetools.DoTestAddUnitWarn(Title: string; Src,
  Expected: array of string; WarnID, Comment: string; TurnOn: boolean);
var
  Code: TCodeBuffer;
  s: String;
  i: Integer;
begin
  Code:=CodeToolBoss.CreateFile('test1.pas');
  s:='';
  for i:=Low(Src) to High(Src) do
    s+=Src[i]+LineEnding;
  Code.Source:=s;

  if not CodeToolBoss.AddUnitWarnDirective(Code,WarnID,Comment,TurnOn) then begin
    WriteSource(Code.Filename,1,1);
    Fail(Title+'call AddUnitWarnDirective failed: '+CodeToolBoss.ErrorDbgMsg);
  end;

  //debugln(['TTestCTStdCodetools.DoTestAddUnitWarn NewSrc=',Code.Source]);
  s:='';
  for i:=Low(Expected) to High(Expected) do
    s+=Expected[i]+LineEnding;
  CheckDiff(Title,s,Code.Source);
end;

procedure TTestCTStdCodetools.DoTestAddUnitToMainUses(NewUnitName, NewUnitInFilename, UsesSrc, ExpectedUsesSrc: string;
  const Flags: TAddUsesFlags);
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
  if not CodeToolBoss.AddUnitToMainUsesSectionIfNeeded(Code,NewUnitName,NewUnitInFilename,Flags) then
  begin
    AssertEquals('AddUnitToMainUsesSectionIfNeeded failed: '+CodeToolBoss.ErrorMessage,true,false);
  end else begin
    Src:=Code.Source;
    AssertEquals('AddUnitToMainUsesSectionIfNeeded altered header: ',Header,LeftStr(Src,length(Header)));
    System.Delete(Src,1,length(Header));
    AssertEquals('AddUnitToMainUsesSectionIfNeeded altered footer: ',Footer,RightStr(Src,length(Footer)));
    System.Delete(Src,length(Src)-length(Footer)+1,length(Footer));
    if ExpectedUsesSrc<>Src then
      debugln(Code.Source);
    AssertEquals('AddUnitToMainUsesSectionIfNeeded: ',ExpectedUsesSrc,Src);
  end;
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

procedure TTestCTStdCodetools.TestCTUses_AddUses_Start;
begin
  DoTestAddUnitToMainUses('Foo','',
    '',
    LineEnding+'uses Foo;'+LineEnding,
   []);
end;

procedure TTestCTStdCodetools.TestCTUses_AddUses_Append;
begin
  DoTestAddUnitToMainUses('Foo','',
    'uses Abc;'+LineEnding,
    'uses Abc, Foo;'+LineEnding,
   []);
end;

procedure TTestCTStdCodetools.TestCTUses_AddUses_AppendKeepSpaces;
begin
  DoTestAddUnitToMainUses('Foo','',
    'uses Go,    Bla;'+LineEnding,
    'uses Go,    Bla, Foo;'+LineEnding,
   []);
end;

procedure TTestCTStdCodetools.TestCTUses_AddUses_AppendKeepComment;
begin
  exit;

  DoTestAddUnitToMainUses('Foo','',
    'uses Go, {Comment} Bla;'+LineEnding,
    'uses Go, {Comment} Bla, Foo;'+LineEnding,
   []);
end;

procedure TTestCTStdCodetools.TestCTUses_AddUses_Append_DottedNoBreak;
var
  Beauty: TBeautifyCodeOptions;
  OldLineLength: Integer;
  OldDoNotSplitLineInFront: TAtomTypes;
begin
  Beauty:=CodeToolBoss.SourceChangeCache.BeautifyCodeOptions;
  OldLineLength:=Beauty.LineLength;
  OldDoNotSplitLineInFront:=Beauty.DoNotSplitLineInFront;
  try
    Beauty.LineLength:=35;
    Beauty.DoNotSplitLineInFront:=Beauty.DoNotSplitLineInFront+[atPoint];// test that atPoint has no effect
    DoTestAddUnitToMainUses('System.SysUtils','',
      'uses System.Classes;'+LineEnding,
      'uses System.Classes,'+LineEnding
      +'  System.SysUtils;'+LineEnding,
     []);
  finally
    Beauty.LineLength:=OldLineLength;
    Beauty.DoNotSplitLineInFront:=OldDoNotSplitLineInFront;
  end;
end;

procedure TTestCTStdCodetools.TestCTUses_RemoveFromAllUsesSections;

  function GetSource(UsesSrc: string): string;
  begin
    Result:='program TestStdCodeTools;'+LineEnding
      +UsesSrc;
  end;

  procedure Test(RemoveUnit, UsesSrc, ExpectedUsesSrc: string);
  var
    Header: String;
    Footer: String;
    Code: TCodeBuffer;
    Src: String;
  begin
    Header:=GetSource('');
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
  // remove first unit
  Test('windows',
   'uses'+LineEnding
  +'   Windows, Messages, Forms,'+LineEnding
  +'   Dialogs, inifiles;'+LineEnding
  ,
   'uses'+LineEnding
  +'   Messages, Forms,'+LineEnding
  +'   Dialogs, inifiles;'+LineEnding
  );

  // remove middle unit
  Test('shellapi',
   'uses'+LineEnding
  +'   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,'+LineEnding
  +'   Dialogs, shellAPI, StdCtrls, ExtCtrls, ComCtrls, strutils, Buttons, inifiles;'+LineEnding
  ,
   'uses'+LineEnding
  +'   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,'+LineEnding
  +'   Dialogs, StdCtrls, ExtCtrls, ComCtrls, strutils, Buttons, inifiles;'+LineEnding
  );

  // remove first unit in second line
  Test('shellapi',
   'uses'+LineEnding
  +'   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,'+LineEnding
  +'   shellAPI, StdCtrls, ExtCtrls, ComCtrls, strutils, Buttons, inifiles;'+LineEnding
  ,
   'uses'+LineEnding
  +'   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,'+LineEnding
  +'   StdCtrls, ExtCtrls, ComCtrls, strutils, Buttons, inifiles;'+LineEnding
  );

  // remove last unit in first line
  Test('forms',
   'uses'+LineEnding
  +'   Windows, Messages, Forms,'+LineEnding
  +'   Dialogs, inifiles;'+LineEnding
  ,
   'uses'+LineEnding
  +'   Windows, Messages,'+LineEnding
  +'   Dialogs, inifiles;'+LineEnding
  );

  // remove last unit
  Test('inifiles',
   'uses'+LineEnding
  +'   Windows, Messages, Forms,'+LineEnding
  +'   Dialogs, inifiles;'+LineEnding
  ,
   'uses'+LineEnding
  +'   Windows, Messages, Forms,'+LineEnding
  +'   Dialogs;'+LineEnding
  );
end;

procedure TTestCTStdCodetools.TestCTAddWarn5025_Program;
begin
  DoTestAddUnitWarn(
  'TestCTAddUnitWarn',
  ['program test1;'
  ,'begin'
  ,'end.'],
  ['program test1;'
  ,'{$WARN 5025 off}'
  ,'begin'
  ,'end.'],'5025','',false);
end;

procedure TTestCTStdCodetools.TestCTAddWarn5025_ProgramNoName;
begin
  DoTestAddUnitWarn(
  'TestCTAddUnitWarn',
  ['begin'
  ,'end.'],
  ['{$WARN 5025 off}'
  ,'begin'
  ,'end.'],'5025','',false);
end;

procedure TTestCTStdCodetools.TestCTAddWarn5025_Unit;
begin
  DoTestAddUnitWarn(
  'TestCTAddUnitWarn',
  ['unit test1;'
  ,'interface'
  ,'end.'],
  ['unit test1;'
  ,'{$WARN 5025 off}'
  ,'interface'
  ,'end.'],'5025','',false);
end;

initialization
  RegisterTest(TTestCTStdCodetools);

end.

