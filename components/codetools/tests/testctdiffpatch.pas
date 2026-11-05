{
 Test all with:
     ./runtestscodetools --format=plain --suite=TTestCTDiffPatch

 Test individually:
     ./runtestscodetools --format=plain --suite=TestDiffAdd
}
unit TestCTDiffPatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // FPCUnit
  FPCUnit, TestRegistry,
  // LazUtils
  LazLoggerBase,
  // CodeTools
  DiffPatch;

type

  { TTestCTDiffPatch }

  TTestCTDiffPatch = class(TTestCase)
  protected
    function LinesToText(const Lines: array of string; const LineEnd: string = #10): string;
    procedure CheckDiff(const Title, OldText, NewText: string;
      const Expected: string; Flags: TTextDiffFlags = []);
    procedure CheckTextDiff(const Title, OldText, NewText: string;
      OutputType: TTextDiffOutputType; const Expected: string);
  published
    procedure TestDiffEmpty;
    procedure TestDiffEqual;
    procedure TestDiffAdd;
    procedure TestDiffDelete;
    procedure TestDiffModify;
    procedure TestDiffMultiple;
    procedure TestDiffNoTrailingNewline;
    procedure TestDiffHistogram;
    procedure TestDiffFlags;
    procedure TestDiffIgnoreEmptyLines;
    procedure TestDiffTextOutputRCS;
    procedure TestDiffTextOutputContext;
  end;

implementation

{ TTestCTDiffPatch }

function TTestCTDiffPatch.LinesToText(const Lines: array of string;
  const LineEnd: string): string;
var
  i: Integer;
begin
  Result:='';
  for i:=0 to high(Lines) do
    Result:=Result+Lines[i]+LineEnd;
end;

procedure TTestCTDiffPatch.CheckDiff(const Title, OldText, NewText: string;
  const Expected: string; Flags: TTextDiffFlags);
var
  Diff: TDiffOutput;
  Actual: String;
  i: Integer;
begin
  Diff:=TDiffOutput.Create(OldText,NewText,Flags);
  try
    Actual:='';
    for i:=0 to Diff.Count-1 do begin
      if i>0 then
        Actual:=Actual+', ';
      Actual:=Actual+Diff[i].AsString;
    end;
    AssertEquals(Title+': Old='+DbgStr(OldText)+' New='+DbgStr(NewText),
                 Expected,Actual);
  finally
    Diff.Free;
  end;
end;

procedure TTestCTDiffPatch.CheckTextDiff(const Title, OldText, NewText: string;
  OutputType: TTextDiffOutputType; const Expected: string);
var
  Diff: TDiffOutput;
  Actual: String;
begin
  Diff:=TDiffOutput.Create(OldText,NewText,[]);
  try
    Diff.OutputType:=OutputType;
    Actual:=Diff.CreateTextDiff;
    AssertEquals(Title,DbgStr(Expected),DbgStr(Actual));
  finally
    Diff.Free;
  end;
end;

procedure TTestCTDiffPatch.TestDiffEmpty;
begin
  CheckDiff('both empty','','','');
  CheckDiff('old empty','',LinesToText(['a']),'Add 1-0 1-1');
  CheckDiff('new empty',LinesToText(['a','b']),'','Delete 1-2 1-0');
end;

procedure TTestCTDiffPatch.TestDiffEqual;
begin
  CheckDiff('one line',LinesToText(['a']),LinesToText(['a']),'None 1-1 1-1');
  CheckDiff('lines',LinesToText(['a','b','c']),LinesToText(['a','b','c']),
            'None 1-3 1-3');
  CheckDiff('empty line',#10,#10,'None 1-1 1-1');
end;

procedure TTestCTDiffPatch.TestDiffAdd;
begin
  CheckDiff('add at start',LinesToText(['b','c']),LinesToText(['a','b','c']),
            'Add 1-0 1-1, None 1-2 2-3');
  CheckDiff('add in middle',LinesToText(['a','c']),LinesToText(['a','b','c']),
            'None 1-1 1-1, Add 2-1 2-2, None 2-2 3-3');
  CheckDiff('add at end',LinesToText(['a','b']),LinesToText(['a','b','c']),
            'None 1-2 1-2, Add 3-2 3-3');
  CheckDiff('add two lines',LinesToText(['a','b','c']),
            LinesToText(['a','b','c','N1','N2']),
            'None 1-3 1-3, Add 4-3 4-5');
end;

procedure TTestCTDiffPatch.TestDiffDelete;
begin
  CheckDiff('delete at start',LinesToText(['a','b','c']),LinesToText(['b','c']),
            'Delete 1-1 1-0, None 2-3 1-2');
  CheckDiff('delete in middle',LinesToText(['a','b','c']),LinesToText(['a','c']),
            'None 1-1 1-1, Delete 2-2 2-1, None 3-3 2-2');
  CheckDiff('delete at end',LinesToText(['a','b','c']),LinesToText(['a','b']),
            'None 1-2 1-2, Delete 3-3 3-2');
end;

procedure TTestCTDiffPatch.TestDiffModify;
begin
  CheckDiff('modify one line',LinesToText(['a','b','c']),LinesToText(['a','X','c']),
            'None 1-1 1-1, Modify 2-2 2-2, None 3-3 3-3');
  CheckDiff('modify several lines',LinesToText(['a','b','c','d']),
            LinesToText(['a','X','Y','Z','d']),
            'None 1-1 1-1, Modify 2-3 2-4, None 4-4 5-5');
  CheckDiff('modify all',LinesToText(['a','b']),LinesToText(['c']),
            'Modify 1-2 1-1');
end;

procedure TTestCTDiffPatch.TestDiffMultiple;
begin
  CheckDiff('modify, delete, add',
            LinesToText(['a','b','c','d','e','f','g']),
            LinesToText(['a','X','c','d','f','g','h']),
            'None 1-1 1-1, Modify 2-2 2-2, None 3-4 3-4, Delete 5-5 5-4,'
            +' None 6-7 5-6, Add 8-7 7-7');
end;

procedure TTestCTDiffPatch.TestDiffNoTrailingNewline;
begin
  CheckDiff('both without',#10'a'#10'b',#10'a'#10'b','None 1-3 1-3');
  CheckDiff('newline added','a'#10'b','a'#10'b'#10,'None 1-1 1-1, Modify 2-2 2-2');
  CheckDiff('newline added, ignore line ends','a'#10'b','a'#10'b'#10,
            'None 1-2 1-2',[tdfIgnoreLineEnds]);
  CheckDiff('last line changed','a'#10'b','a'#10'c','None 1-1 1-1, Modify 2-2 2-2');
end;

procedure TTestCTDiffPatch.TestDiffHistogram;
var
  OldText, NewText: String;
begin
  // a function is added in front, a line deleted, a function deleted and the
  // main program changed.
  // git diff --histogram matches old line 16 'begin' instead of 24 and gives
  // Delete 15-15, None 16-16, Modify 17-25. Both are minimal.
  OldText:=LinesToText([
    'uses SysUtils;',
    '',
    '// Frobs foo heartily',
    'procedure Frobnitz(Foo: integer);',
    'var',
    '  i: integer;',
    'begin',
    '  for i:=0 to 9 do',
    '  begin',
    '    write(''Your answer is: '');',
    '    writeln(Foo);',
    '  end;',
    'end;',
    '',
    'function Fact(n: integer): integer;',
    'begin',
    '  if n>1 then',
    '  begin',
    '    exit(Fact(n-1)*n);',
    '  end;',
    '  Result:=1;',
    'end;',
    '',
    'begin',
    '  Frobnitz(Fact(10));',
    'end.']);
  NewText:=LinesToText([
    'uses SysUtils;',
    '',
    'function Fib(n: integer): integer;',
    'begin',
    '  if n>2 then',
    '  begin',
    '    exit(Fib(n-1)+Fib(n-2));',
    '  end;',
    '  Result:=1;',
    'end;',
    '',
    '// Frobs foo heartily',
    'procedure Frobnitz(Foo: integer);',
    'var',
    '  i: integer;',
    'begin',
    '  for i:=0 to 9 do',
    '  begin',
    '    writeln(Foo);',
    '  end;',
    'end;',
    '',
    'begin',
    '  Frobnitz(Fib(10));',
    'end.']);
  CheckDiff('add function, delete line, change main',OldText,NewText,
    'None 1-2 1-2, Add 3-2 3-11, None 3-9 12-18, Delete 10-10 19-18,'
    +' None 11-14 19-22, Delete 15-23 23-22, None 24-24 23-23,'
    +' Modify 25-25 24-24, None 26-26 25-25');

  // unique lines are anchors, on equal occurrence the longer region wins
  CheckDiff('unique anchor',
    LinesToText(['end;','A','end;','B','end;']),
    LinesToText(['B','end;','A','end;']),
    'Add 1-0 1-1, None 1-2 2-3, Delete 3-4 4-3, None 5-5 4-4');
end;

procedure TTestCTDiffPatch.TestDiffFlags;
begin
  CheckDiff('case','Abc'#10,'aBC'#10,'Modify 1-1 1-1');
  CheckDiff('ignore case','Abc'#10,'aBC'#10,'None 1-1 1-1',[tdfIgnoreCase]);

  CheckDiff('spaces','a b'#10,'ab'#10,'Modify 1-1 1-1');
  CheckDiff('ignore spaces','a b'#10,' ab  '#10,'None 1-1 1-1',[tdfIgnoreSpaceChars]);

  CheckDiff('ignore space amount','a  b'#10,'a'#9'b'#10,'None 1-1 1-1',
            [tdfIgnoreSpaceCharAmount]);
  CheckDiff('ignore space amount, all removed','a b'#10,'ab'#10,'Modify 1-1 1-1',
            [tdfIgnoreSpaceCharAmount]);

  CheckDiff('ignore heading spaces','  a'#10,'a'#10,'None 1-1 1-1',
            [tdfIgnoreHeadingSpaces]);
  CheckDiff('ignore heading spaces, not trailing','a  '#10,'a'#10,'Modify 1-1 1-1',
            [tdfIgnoreHeadingSpaces]);
  CheckDiff('ignore trailing spaces','a  '#10,'a'#10,'None 1-1 1-1',
            [tdfIgnoreTrailingSpaces]);
  CheckDiff('ignore trailing spaces, not heading','  a'#10,'a'#10,'Modify 1-1 1-1',
            [tdfIgnoreTrailingSpaces]);

  CheckDiff('line ends','a'#13#10'b'#13#10,'a'#10'b'#10,'Modify 1-2 1-2');
  CheckDiff('ignore line ends','a'#13#10'b'#13#10,'a'#10'b'#10,'None 1-2 1-2',
            [tdfIgnoreLineEnds]);
  CheckDiff('mixed line ends','a'#13'b'#10#13'c','a'#10'b'#10'c','None 1-3 1-3',
            [tdfIgnoreLineEnds]);
end;

procedure TTestCTDiffPatch.TestDiffIgnoreEmptyLines;
begin
  CheckDiff('empty line added',LinesToText(['a','b']),LinesToText(['a','','b']),
            'None 1-1 1-1, Add 2-1 2-2, None 2-2 3-3');
  CheckDiff('ignore empty line added',LinesToText(['a','b']),LinesToText(['a','','b']),
            'None 1-2 1-3',[tdfIgnoreEmptyLineChanges]);
  CheckDiff('ignore empty lines at start and end',LinesToText(['','a']),
            LinesToText(['a','','']),
            'None 1-2 1-3',[tdfIgnoreEmptyLineChanges]);
  CheckDiff('ignore empty lines, empty line next to change',
            LinesToText(['a','','b']),LinesToText(['a','X']),
            'None 1-1 1-1, Modify 2-3 2-2',[tdfIgnoreEmptyLineChanges]);
  CheckDiff('ignore empty lines with spaces',LinesToText(['a','b']),
            LinesToText(['a','  ','b']),
            'None 1-2 1-3',[tdfIgnoreEmptyLineChanges,tdfIgnoreTrailingSpaces]);
end;

procedure TTestCTDiffPatch.TestDiffTextOutputRCS;
var
  OldText: String;
begin
  OldText:=LinesToText(['a','b','c','d','e']);
  CheckTextDiff('modify',OldText,LinesToText(['a','b','X','d','e']),tdoRCS,
    '3c3'#10
    +'< c'#10
    +'---'#10
    +'> X'#10);
  CheckTextDiff('delete',OldText,LinesToText(['a','b','d','e']),tdoRCS,
    '3d2'#10
    +'< c'#10);
  CheckTextDiff('add',OldText,LinesToText(['a','b','c','N1','N2','d','e']),tdoRCS,
    '3a4,5'#10
    +'> N1'#10
    +'> N2'#10);
  CheckTextDiff('no newline at end','a'#10'b','a'#10'c',tdoRCS,
    '2c2'#10
    +'< b'#10
    +'\ No newline at end'#10
    +'---'#10
    +'> c'#10
    +'\ No newline at end'#10);
  CheckTextDiff('equal',OldText,OldText,tdoRCS,'');
end;

procedure TTestCTDiffPatch.TestDiffTextOutputContext;
var
  OldText, NewText: String;
  i: Integer;
begin
  OldText:=LinesToText(['a','b','c','d','e']);
  CheckTextDiff('modify',OldText,LinesToText(['a','b','X','d','e']),tdoContext,
    '***************'#10
    +'*** 1,5 ****'#10
    +'  a'#10
    +'  b'#10
    +'! c'#10
    +'  d'#10
    +'  e'#10
    +'--- 1,5 ----'#10
    +'  a'#10
    +'  b'#10
    +'! X'#10
    +'  d'#10
    +'  e'#10);
  CheckTextDiff('delete',OldText,LinesToText(['a','b','d','e']),tdoContext,
    '***************'#10
    +'*** 1,5 ****'#10
    +'  a'#10
    +'  b'#10
    +'- c'#10
    +'  d'#10
    +'  e'#10
    +'--- 1,4 ----'#10);
  CheckTextDiff('add',OldText,LinesToText(['a','b','c','N1','N2','d','e']),tdoContext,
    '***************'#10
    +'*** 1,5 ****'#10
    +'--- 1,7 ----'#10
    +'  a'#10
    +'  b'#10
    +'  c'#10
    +'+ N1'#10
    +'+ N2'#10
    +'  d'#10
    +'  e'#10);
  CheckTextDiff('no newline at end','a'#10'b','a'#10'c',tdoContext,
    '***************'#10
    +'*** 1,2 ****'#10
    +'  a'#10
    +'! b'#10
    +'\ No newline at end'#10
    +'--- 1,2 ----'#10
    +'  a'#10
    +'! c'#10
    +'\ No newline at end'#10);
  CheckTextDiff('equal',OldText,OldText,tdoContext,'');

  // two hunks: L3 modified, L15 deleted
  OldText:='';
  NewText:='';
  for i:=1 to 20 do begin
    OldText:=OldText+'L'+IntToStr(i)+#10;
    if i=3 then
      NewText:=NewText+'X3'#10
    else if i<>15 then
      NewText:=NewText+'L'+IntToStr(i)+#10;
  end;
  CheckTextDiff('two hunks',OldText,NewText,tdoContext,
    '***************'#10
    +'*** 1,6 ****'#10
    +'  L1'#10
    +'  L2'#10
    +'! L3'#10
    +'  L4'#10
    +'  L5'#10
    +'  L6'#10
    +'--- 1,6 ----'#10
    +'  L1'#10
    +'  L2'#10
    +'! X3'#10
    +'  L4'#10
    +'  L5'#10
    +'  L6'#10
    +'***************'#10
    +'*** 12,18 ****'#10
    +'  L12'#10
    +'  L13'#10
    +'  L14'#10
    +'- L15'#10
    +'  L16'#10
    +'  L17'#10
    +'  L18'#10
    +'--- 12,17 ----'#10);

  // two changes close together: one hunk
  CheckTextDiff('one hunk',
    LinesToText(['a','b','c','d','e','f','g','h','i']),
    LinesToText(['a','X','c','d','e','f','g','Y','i']),tdoContext,
    '***************'#10
    +'*** 1,9 ****'#10
    +'  a'#10
    +'! b'#10
    +'  c'#10
    +'  d'#10
    +'  e'#10
    +'  f'#10
    +'  g'#10
    +'! h'#10
    +'  i'#10
    +'--- 1,9 ----'#10
    +'  a'#10
    +'! X'#10
    +'  c'#10
    +'  d'#10
    +'  e'#10
    +'  f'#10
    +'  g'#10
    +'! Y'#10
    +'  i'#10);
end;

initialization
  RegisterTest(TTestCTDiffPatch);

end.

