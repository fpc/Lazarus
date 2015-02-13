{
 Test all with:
     ./runtests --format=plain --suite=TTestLazUtils

 Test specific with:
     ./runtests --format=plain --suite=TestReplaceSubstring
     ./runtests --format=plain --suite=TestSplitCmdLineParams
     ./runtests --format=plain --suite=TestExpandFilename
     ./runtests --format=plain --suite=TestMergeCmdLineParams
}
unit TestLazUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testglobals, LazLogger, LazUTF8, LazFileUtils;

type

  { TTestLazUtils }

  TTestLazUtils = class(TTestCase)
  public
  published
    procedure TestReplaceSubstring;
    procedure TestSplitCmdLineParams;
    procedure TestExpandFilename;
    procedure TestMergeCmdLineParams;
  end;

implementation

{ TTestLazUTF8 }

procedure TTestLazUtils.TestReplaceSubstring;

  function r(const s: string; StartPos, Count: SizeInt;
             const Insertion: string): string;
  var
    OldS: String;
  begin
    Result:=s;
    OldS:=s;
    UniqueString(OldS);
    ReplaceSubstring(Result,StartPos,Count,Insertion);
    AssertEquals('s unchanged',OldS,s);
  end;

begin
  AssertEquals('empty string','',r('',1,1,''));
  AssertEquals('empty string insert a','a',r('',1,1,'a'));
  AssertEquals('empty string negative startpos','a',r('',-1,1,'a'));
  AssertEquals('empty string count too big','a',r('',-1,10,'a'));
  AssertEquals('empty string beyond length','a',r('',10,10,'a'));
  AssertEquals('whole','a',r('a',1,1,'a'));
  AssertEquals('whole','b',r('a',1,1,'b'));
  AssertEquals('whole','abc',r('a',1,1,'abc'));
  AssertEquals('first char','abcbc',r('abc',1,1,'abc'));
  AssertEquals('last char single','aba',r('abc',3,1,'a'));
  AssertEquals('last char multi','ababc',r('abc',3,1,'abc'));
  AssertEquals('middle char same','abc',r('abc',2,1,'b'));
  AssertEquals('middle char single','adc',r('abc',2,1,'d'));
  AssertEquals('middle char longen','acdec',r('abc',2,1,'cde'));
  AssertEquals('last multi','adef',r('abc',2,2,'def'));
  AssertEquals('middle chars same','abcde',r('abcde',2,3,'bcd'));
  AssertEquals('middle chars shorten','axe',r('abcde',2,3,'x'));
  AssertEquals('after chars','abcx',r('abc',4,3,'x'));
end;

procedure TTestLazUtils.TestSplitCmdLineParams;

  function r(Params: string; ReadBackslash: boolean = false): string;
  var
    ParamList: TStringList;
    i: Integer;
  begin
    Result:='';
    ParamList:=TStringList.Create;
    try
      SplitCmdLineParams(Params,ParamList,ReadBackslash);
      for i:=0 to ParamList.Count-1 do begin
        if i>0 then Result+='|';
        Result+=ParamList[i];
      end;
    finally
      ParamList.Free;
    end;
  end;

begin
  // using | as separator for parameters
  AssertEquals('empty','',r(''));
  AssertEquals('simple','a',r('a'));
  AssertEquals('two simple','a|b',r('a b'));
  AssertEquals('one quote "','a b',r('"a b"'));
  AssertEquals('one quote ''','a b',r('''a b'''));
  AssertEquals('two with backslash disabled','a\|b',r('a\ b'));
  AssertEquals('two with backslash enabled','a b',r('a\ b',true));
  AssertEquals('two with backslashed quote','a"b',r('"a\"b"',true));
  AssertEquals('two with backslashed apos','a''b',r('"a\''b"',true));
  AssertEquals('two with backslashed backslash','a\b',r('"a\\b"',true));
  AssertEquals('quoted quote','''|"',r('"''" ''"''',true));
  AssertEquals('empty params','|',r('"" '''''));
end;

procedure TTestLazUtils.TestExpandFilename;
begin
  {$IFDEF Unix}
  AssertEquals('basedir','/opt/ide',ExpandFileNameUTF8('ide','/opt/'));
  AssertEquals('basedir','/opt/ide',ExpandFileNameUTF8('ide','/opt'));
  AssertEquals('basedir','/ide',ExpandFileNameUTF8('/ide','/opt'));
  {$ENDIF}
  {$IFDEF Windows}
  AssertEquals('basedir','C:\opt\ide',ExpandFileNameUTF8('ide','C:\opt\'));
  AssertEquals('basedir','D:\opt\ide',ExpandFileNameUTF8('ide','D:\opt'));
  AssertEquals('basedir','E:\ide',ExpandFileNameUTF8('E:\ide','D:\opt'));
  {$ENDIF}
end;

procedure TTestLazUtils.TestMergeCmdLineParams;

  procedure t(Title, Param, Expected: string);
  var
    l: TStringList;
    Actual: String;
  begin
    l:=TStringList.Create;
    try
      l.Add(Param);
      Actual:=MergeCmdLineParams(l);
      AssertEquals(Title,'['+Expected+']','['+Actual+']');
    finally
      l.Free;
    end;
  end;

begin
  t('empty','','''''');
  t('word','a','a');
  t('space',' ',''' ''');
  t('two words','a b','''a b''');
  t('single quot','"','''"''');
  t('two quots','"a"','''"a"''');
  t('single apos','''','"''"');
  t('two apos','''a''','"''a''"');
  t('quot apos','"''','''"''"''"');
  t('wordquot','a"','''a"''');
  t('null#0char','null'#0'char','null');
end;

initialization
  AddToLazUtilsTestSuite(TTestLazUtils);

end.

