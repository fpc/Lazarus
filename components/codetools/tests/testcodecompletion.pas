unit TestCodeCompletion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CodeToolManager, CodeCache,
  LazLogger, LazFileUtils, fpcunit, testregistry,
  TestFinddeclaration;

type

  { TTestCodeCompletion }

  TTestCodeCompletion = class(TTestCase)
  private
    procedure CheckDiff(Msg, Expected, Actual: string);
    procedure WriteSource(aFilename: string; Line, Col: integer);
    procedure Test(Title: string; Src: array of string; Line, Col: integer;
      Expected: array of string);
  published
    procedure TestIntfProcUpdateArgName;
  end;

implementation

{ TTestCodeCompletion }

procedure TTestCodeCompletion.CheckDiff(Msg, Expected, Actual: string);
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

procedure TTestCodeCompletion.WriteSource(aFilename: string; Line, Col: integer
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

procedure TTestCodeCompletion.Test(Title: string; Src: array of string; Line,
  Col: integer; Expected: array of string);
var
  i, NewX, NewY, NewTopLine, BlockTopLine, BlockBottomLine: Integer;
  s: String;
  NewCode, Code: TCodeBuffer;
begin
  Code:=CodeToolBoss.CreateFile('test1.pas');
  s:='';
  for i:=Low(Src) to High(Src) do
    s+=Src[i]+LineEnding;
  Code.Source:=s;

  if not CodeToolBoss.CompleteCode(Code,Col,Line,Line,NewCode,NewX,NewY,NewTopLine,
    BlockTopLine,BlockBottomLine,false) then
  begin
    WriteSource(Code.Filename,Line,Col);
    Fail(Title+'call CompleteCode failed: '+CodeToolBoss.ErrorDbgMsg);
  end;
  s:='';
  for i:=Low(Expected) to High(Expected) do
    s+=Expected[i]+LineEnding;
  CheckDiff(Title,s,Code.Source);
end;

procedure TTestCodeCompletion.TestIntfProcUpdateArgName;
begin
  Test('TestIntfProcUpdateArgName',
    ['unit test1;'
    ,'interface'
    ,'procedure DoIt(a: longint);'
    ,'implementation'
    ,'procedure DoIt(b: longint);'
    ,'begin end;'
    ,'end.'],
    3,1,
    ['unit test1;'
    ,'interface'
    ,'procedure DoIt(a: longint);'
    ,'implementation'
    ,'procedure DoIt(a: longint);'
    ,'begin end;'
    ,'end.']);
end;

initialization
  RegisterTests([TTestCodeCompletion]);
end.

