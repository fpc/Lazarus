{
 Test with:
   ./testcodetools --format=plain --suite=TTestBasicCodeTools
   ./testcodetools --format=plain --suite=TestFindLineEndOrCodeInFrontOfPosition
   ./testcodetools --format=plain --suite=TestHasTxtWord
   ./testcodetools --format=plain --suite=TestBasicFindCommentEnd
   ./testcodetools --format=plain --suite=TestBasicFindNextComment
   ./testcodetools --format=plain --suite=TestCompareTextIgnoringSpace
   ./testcodetools --format=plain --suite=TestCleanCodeFromComments
   ./testcodetools --format=plain --suite=TestGuessIndentSize
   ./testcodetools --format=plain --suite=TestReindent
   ./testcodetools --format=plain --suite=TestSimpleFormat
   ./testcodetools --format=plain --suite=TestStringToPascalConst

   ./testcodetools --format=plain --suite=TestDateToCfgStr
   ./testcodetools --format=plain --suite=TestFilenameIsMatching
   ./testcodetools --format=plain --suite=TestExtractFileUnitname
   ./testcodetools --format=plain --suite=TestParseFPCParameters

   ./testcodetools --format=plain --suite=TestChangeLineEndings
}
unit TestBasicCodetools;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, contnrs, Classes, SysUtils,
  // LazUtils
  LazStringUtils,
  // CodeTools
  FileProcs, BasicCodeTools, DefineTemplates;

type
  { TTestBasicCodeTools }

  TTestBasicCodeTools = class(TTestCase)
  protected
  published
    // BasicCodeTools
    procedure TestFindLineEndOrCodeInFrontOfPosition;
    procedure TestHasTxtWord;
    procedure TestBasicFindCommentEnd;
    procedure TestBasicFindNextComment;
    procedure TestCompareTextIgnoringSpace;
    procedure TestCleanCodeFromComments;
    procedure TestGuessIndentSize;
    procedure TestReIndent;
    procedure TestSimpleFormat;
    procedure TestStringToPascalConst;
    // FileProcs
    procedure TestDateToCfgStr;
    procedure TestFilenameIsMatching;
    procedure TestExtractFileUnitname;
    procedure TestParseFPCParameters;
    // SourceLog
    procedure TestChangeLineEndings;
  end;

implementation

{ TTestBasicCodeTools }

procedure TTestBasicCodeTools.TestFindLineEndOrCodeInFrontOfPosition;

  procedure t(Src: string; SkipSemicolonComma: boolean = true;
    StopAtDirectives: boolean = true;
    SkipEmptyLines: boolean = false; NestedComments: boolean = true);
  var
    Position: integer;
    Expected: integer;
    Actual: LongInt;
    OrigSrc: String;
  begin
    OrigSrc:=Src;
    Expected:=Pos('$',Src);
    if Expected<1 then
      raise Exception.Create('TTestBasicCodeTools.TestFindLineEndOrCodeInFrontOfPosition missing # expected position');
    Delete(Src,Expected,1);
    Position:=Pos('|',Src);
    if Position<1 then
      raise Exception.Create('TTestBasicCodeTools.TestFindLineEndOrCodeInFrontOfPosition missing | start position');
    Delete(Src,Position,1);
    Actual:=FindLineEndOrCodeInFrontOfPosition(Src,Position,1,NestedComments,
      StopAtDirectives,SkipSemicolonComma,SkipEmptyLines);
    AssertEquals('['+dbgstr(OrigSrc)+']',Expected,Actual);
  end;

var
  e: String;
begin
  //writeln('TTestBasicCodeTools.TestFindLineEndOrCodeInFrontOfPosition ');
  e:=#13#10; // use windows line endings, they are more complicated
  t(' $'+e+'|a:=1;');
  t('a$'+e+'// comment'+e+' { comment } '+e+'|');
  t('$'+e+' (* *)'+e+' |a:=1');
  t('a:=1;(* comment of first line'+e+' *)$ |b:=1');
  t('a:=1; // comment$'+e+'|b:=1');
  t('a:=1; (* comment'+e+' *) $'+e+'|b:=1');
  t('a:=1; (* comment'+e+' *) { comment'+e+' } $'+e+'|b:=1');
end;

procedure TTestBasicCodeTools.TestHasTxtWord;

  procedure t(aWord,aText: PChar; ExpectedWholeWord: boolean; ExpectedCount: SizeInt);
  var
    WholeWord: boolean;
    Count: SizeInt;
  begin
    HasTxtWord(aWord,aText,WholeWord,Count);
    AssertEquals('Word="'+aWord+'" Text="'+aText+'" WholeWord',ExpectedWholeWord,WholeWord);
    AssertEquals('Word="'+aWord+'" Text="'+aText+'" Count',ExpectedCount,Count);
  end;

begin
  t(nil,nil,false,0);
  t('a','a',true,1);
  t('ab','a',false,0); // ab not in a
  t('a','ab',false,1); // a in ab
  t('a','aba',false,2);
  t('a','a a',true,2);
  t('a','ab a',true,1);
  t('abc','ab abcd',false,1);
  t('ab','abab',false,2);
end;

procedure TTestBasicCodeTools.TestBasicFindCommentEnd;

  procedure TestFindCommentEnd(Src: string;
    NestedComments: boolean; ExpectedEndPos: integer);
  var
    EndPos: Integer;
    p: PChar;
    EndP: PChar;
  begin
    EndPos:=FindCommentEnd(Src,1,NestedComments);
    AssertEquals('FindCommentEnd(string) '+dbgstr(Src),ExpectedEndPos,EndPos);
    p:=PChar(Src);
    EndP:=FindCommentEnd(p,NestedComments);
    EndPos:=EndP-PChar(Src)+1;
    AssertEquals('FindCommentEnd(pchar) '+dbgstr(Src),ExpectedEndPos,EndPos);
  end;

begin
  TestFindCommentEnd(' ',false,1);
  TestFindCommentEnd('{}',false,3);
  TestFindCommentEnd('(**)',false,5);
  TestFindCommentEnd('(*)',false,4);
  TestFindCommentEnd('{{}}',false,4);
  TestFindCommentEnd('{{}}',true,5);
  TestFindCommentEnd('{'#3#3'}',false,5);
  TestFindCommentEnd('{'#3'}'#3'}',false,6);
  TestFindCommentEnd('//',false,3);
end;

procedure TTestBasicCodeTools.TestBasicFindNextComment;

  procedure TestFindNextComment(Src: string; ExpectedPos: integer);
  var
    StartPos: Integer;
  begin
    StartPos:=FindNextComment(Src,1);
    AssertEquals('FindNextComment '+dbgstr(Src),ExpectedPos,StartPos);
  end;

begin
  TestFindNextComment('{',1);
  TestFindNextComment('//',1);
  TestFindNextComment('(*',1);
  TestFindNextComment('(',2);
  TestFindNextComment('/',2);
end;

procedure TTestBasicCodeTools.TestCompareTextIgnoringSpace;

  procedure t(Txt1, Txt2: string; Expected: integer; CaseSensitive: boolean = false);
  var
    r: Integer;
  begin
    r:=CompareTextIgnoringSpace(Txt1,Txt2,CaseSensitive);
    AssertEquals('Txt1="'+dbgstr(Txt1)+'",Txt2="'+dbgstr(Txt2)+'"',Expected,r);
  end;

begin
  t('a','a',0);
  t('a','A',0);
  t('a',' a',0);
  t(' a','a',0);
  t(' a','a ',0);
  t(' a: b','a:b',0);
  t('procedure TCustomSynEdit.LineCountChanged(Sender: TSynEditStrings; AIndex,'#13#10'  ACount: Integer); ',
    'procedure TCustomSynEdit.LineCountChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);',0);
end;

procedure TTestBasicCodeTools.TestCleanCodeFromComments;

  procedure t(const Src, Expected: string;
    NestedComments, KeepDirectives, KeepVerbosityDirectives: boolean);
  var
    r: string;
  begin
    r:=CleanCodeFromComments(Src,NestedComments,KeepDirectives,KeepVerbosityDirectives);
    AssertEquals('Src="'+dbgstr(Src)+'",Expected="'+dbgstr(Expected)+'" Nested='+dbgs(NestedComments)+' KeepDir='+dbgs(KeepDirectives)+' KeepVerbose='+dbgs(KeepVerbosityDirectives),Expected,r);
  end;

begin
  t('','',true,true,true);
  t(' ',' ',true,true,true);
  t('  ','  ',true,true,true);
  t(' a ',' a ',true,true,true);
  t(' a ',' a ',true,true,true);
  t(' {} ','  ',true,true,true);
  t(' (**) ','  ',true,true,true);
  t(' // ',' ',true,true,true);
  t(' // '#10' ',' '#10' ',true,true,true);
  t(' {$} ',' {$} ',true,true,true);
  t(' {$} ','  ',true,false,true);
  t(' {$hints} ','  ',true,false,true);
  t(' {$hints} ','  ',true,false,false);
  t(' {$hints} ',' {$hints} ',true,true,false);
  t(' {$hints} ',' {$hints} ',true,true,true);
  t(' {$warn} ','  ',true,false,false);
  t(' {$warn} ','  ',true,false,true);
  t(' {$warn} ',' {$warn} ',true,true,true);
  t(' {$warn} ','  ',true,true,false);
  t(' {$hint} ','  ',true,false,false);
  t(' {$hint} ','  ',true,false,true);
  t(' {$hint} ',' {$hint} ',true,true,true);
  t(' {$hint} ','  ',true,true,false);
end;

procedure TTestBasicCodeTools.TestGuessIndentSize;

  procedure t(Src: string; ExpectedIndent: integer; DefaultIndent: integer = 2; TabWidth: integer = 2);
  var
    ActualIndent: Integer;
  begin
    ActualIndent:=DefaultIndent;
    GuessIndentSize(Src,ActualIndent,TabWidth);
    if ActualIndent=ExpectedIndent then exit;
    AssertEquals('"'+DbgStr(Src)+'"',ExpectedIndent,ActualIndent);
  end;

begin
  t('',2,2);
  t(' a',1);
  t('  a',2);
  t('   a',3);
  t('  a'#10'b',2);
  t('  a'#10#13'  b',2);
  t('  a'#10'    b'#10'  c',2);
end;

procedure TTestBasicCodeTools.TestReIndent;

  procedure t(Src: string; OldIndent,OldTabWidth, NewIndentStep, NewTabWidth: integer; ExpectedSrc: string);
  var
    ActualSrc: String;
  begin
    ActualSrc:=ReIndent(Src,OldIndent,OldTabWidth, NewIndentStep, NewTabWidth);
    if ExpectedSrc=ActualSrc then exit;
    writeln(dbgsDiff(ExpectedSrc,ActualSrc));
    AssertEquals('"'+DbgStr(Src)+'"',true,false);
  end;

begin
  t('',2,4, 4,4,'');
  t('A',2,4, 4,4,'A');
  t(' A',2,4, 4,4,#9'A');
  t('  A',2,4, 4,4,#9'A');
  t('   A',2,4, 4,4,#9#9'A');
  t('    A',2,4, 4,4,#9#9'A');
  t('     A',2,4, 4,4,#9#9#9'A');
  t('      A',2,4, 4,4,#9#9#9'A');
  t(#9'A',4,4, 2,0,'  A');
  t('A'#10'  B'#10,2,4, 3,0,'A'#10'   B'#10);
end;

procedure TTestBasicCodeTools.TestSimpleFormat;

  procedure t(const Fmt: string; Args: array of const; const Expected: string);
  var
    Actual: String;
  begin
    Actual:=SimpleFormat(Fmt,Args);
    if Expected=Actual then exit;
    writeln(dbgsDiff(Expected,Actual));
    AssertEquals('"'+DbgStr(Fmt)+'"('+dbgs(High(Args)-Low(Args)+1)+')',true,false);
  end;

begin
  t('A',['Foo'],'A,Foo');
  t('A%sB',['Foo'],'AFooB');
  t('A%sB%sC',['Foo'],'AFooB%sC');
  t('A%sB',['Foo','Bar'],'AFooB,Bar');
  t('A%0B',['Foo','Bar'],'AFooB,Bar');
  t('A%1B',['Foo','Bar'],'ABarB,Foo');
  t('A%1%0B',['Foo','Bar'],'ABarFooB');
  t('A%1:s%0:sB',['Foo','Bar'],'ABarFooB');
end;

procedure TTestBasicCodeTools.TestStringToPascalConst;

  procedure t(s, Expected: string);
  var
    Actual: String;
  begin
    Actual:=StringToPascalConst(s);
    AssertEquals('StringToPascalConst <'+DbgStr(s)+'>',Expected,Actual);
  end;

begin
  t('','''''');
  t('Foo','''Foo''');
  t('Foo"','''Foo"''');
  t('Foo''','''Foo''''''');
  t('Fo''o','''Fo''''o''');
  t('''Foo','''''''Foo''');
  t('Foo'#10,'''Foo''#10');
  t('Foo'#10'Bar','''Foo''#10''Bar''');
end;

procedure TTestBasicCodeTools.TestDateToCfgStr;

  procedure t(const Date: TDateTime; const aFormat, Expected: string);
  var
    Actual: String;
    ActualDate: TDateTime;
  begin
    Actual:=DateToCfgStr(Date,aFormat);
    if Actual<>Expected then begin
      writeln(dbgsDiff(Expected,Actual));
      AssertEquals('DateToCfgStr failed: Format="'+aFormat+'"',Expected,Actual);
      exit;
    end;
    if (not CfgStrToDate(Actual,ActualDate,aFormat)) then begin
      AssertEquals('CfgStrToDate failed: Format="'+aFormat+'" Cfg="'+Actual+'"',false,true);
      exit;
    end;
    if ActualDate<>Date then begin
      AssertEquals('CfgStrToDate failed: Format="'+aFormat+'"',DateTimeToStr(ActualDate),DateTimeToStr(Date));
    end;
  end;

begin
  t(EncodeDate(1234,12,17),DateAsCfgStrFormat,'12341217');
  t(EncodeDate(1234,1,2),DateAsCfgStrFormat,'12340102');
  t(ComposeDateTime(EncodeDate(1234,1,2),EncodeTime(3,4,5,6)),DateTimeAsCfgStrFormat,'1234/01/02 03:04:05');
end;

procedure TTestBasicCodeTools.TestFilenameIsMatching;

  procedure t(aMask,aFilename: string; aExactly,Expected: boolean);
  var
    Actual: Boolean;
  begin
    DoDirSeparators(aMask);
    DoDirSeparators(aFilename);
    Actual:=FilenameIsMatching(aMask,aFilename,aExactly);
    if Actual=Expected then exit;
    AssertEquals('Mask="'+aMask+'" File="'+aFilename+'" Exactly='+dbgs(aExactly),Expected,Actual);
  end;

begin
  //  /abc           matches /abc, /abc/, /abc/p, /abc/xyz/filename
  //                 but not /abcd
  t('/abc','/abc',true,true);
  t('/abc','/abc',false,true);
  t('/abc','/abc/',true,true);
  t('/abc','/abc/',false,true);
  t('/abc','/abc/p',true,false);
  t('/abc','/abc/p',false,true);
  t('/abc','/abc/xyz/filename',false,true);
  t('/abc','/abc/xyz/filename',true,false);
  t('/abc','/abcd',true,false);
  t('/abc','/abcd',false,false);

  //  /abc/            matches /abc, /abc/, /abc//, but not /abc/.
  t('/abc/','/abc',true,true);
  t('/abc/','/abc',false,true);
  t('/abc/','/abc/',true,true);
  t('/abc/','/abc/',false,true);
  t('/abc/','/abc//',true,true);
  t('/abc/','/abc//',false,true);
  t('/abc/','/abc/.',true,false);
  t('/abc/','/abc/.',false,true);
  t('/abc/','/abc/d',true,false);
  t('/abc/','/abc/d',false,true);

  //  /abc/x?z/www   matches /abc/xyz/www, /abc/xaz/www
  //                 but not /abc/x/z/www
  t('/abc/x?z/www','/abc/xyz/www',true,true);
  t('/abc/x?z/www','/abc/xyz/www',false,true);
  t('/abc/x?z/www','/abc/xaz/www',true,true);
  t('/abc/x?z/www','/abc/x/z/www',true,false);
  t('/abc/x?z/www','/abc/x/z/www',false,false);

  //  /abc/x*z/www   matches /abc/xz/www, /abc/xyz/www, /abc/xAAAz/www
  //                 but not /abc/x/z/www
  t('/abc/x*z/www','/abc/xz/www',true,true);
  t('/abc/x*z/www','/abc/xz/www',false,true);
  t('/abc/x*z/www','/abc/xyz/www',true,true);
  t('/abc/x*z/www','/abc/xyz/www',false,true);
  t('/abc/x*z/www','/abc/xAAAz/www',true,true);
  t('/abc/x*z/www','/abc/xAAAz/www',false,true);
  t('/abc/x*z/www','/abc/x/z/www',true,false);
  t('/abc/x*z/www','/abc/x/z/www',false,false);

  //  /abc/x#*z/www  matches /abc/x*z/www, /abc/x*z/www/ttt
  t('/abc/x#*z/www','/abc/x*z/www',true,true);
  t('/abc/x#*z/www','/abc/x*z/www',false,true);
  t('/abc/x#*z/www','/abc/x*z/www/ttt',true,false);
  t('/abc/x#*z/www','/abc/x*z/www/ttt',false,true);

  //  /a{b,c,d}e     matches /abe, /ace, /ade, but not /aze
  t('/a{b,c,d}e','/abe',true,true);
  t('/a{b,c,d}e','/abe',false,true);
  t('/a{b,c,d}e','/ace',true,true);
  t('/a{b,c,d}e','/ace',false,true);
  t('/a{b,c,d}e','/ade',true,true);
  t('/a{b,c,d}e','/ade',false,true);
  t('/a{b,c,d}e','/aze',true,false);
  t('/a{b,c,d}e','/aze',false,false);

  // {*.pas,*.pp,*.p,*.inc,Makefile.fpc} matches math.pp
  t('{*.pas,*.pp,*.p,*.inc,Makefile.fpc}','math.pp',true,true);

  t('{*.pas}','Generics.Collections.pas',true,true);
  t('{*.pas,*.pp}','Generics.Collections.pas',true,true);
  t('{*.pas,*.pp,*.p}','Generics.Collections.pas',true,true);
  t('{*.pas,*.pp,*.p,*.inc}','Generics.Collections.pas',true,true);
  t('{*.pas,*.pp,*.p,*.inc,Makefile.fpc}','Generics.Collections.pas',true,true);

  // *{.p{as,p,}}   matches a.pas unit1.pp b.p but not b.inc
  t('*{.p{as,p,}}','a.pas',true,true);
  t('*{.p{as,p,}}','unit1.pp',true,true);
  t('*{.p{as,p,}}','b.p',true,true);
  t('*{.p{as,p,}}','b.inc',true,false);

  //  *.{p{as,p,},inc} matches a.pas unit1.pp b.p b.inc but not c.lfm
  t('*.{p{as,p,},inc}','a.pas',true,true);
  t('*.{p{as,p,},inc}','unit1.pp',true,true);
  t('*.{p{as,p,},inc}','b.p',true,true);
  t('*.{p{as,p,},inc}','b.inc',true,true);
  t('*.{p{as,p,},inc}','c.lfm',true,false);
end;

procedure TTestBasicCodeTools.TestExtractFileUnitname;

  procedure t(Filename: string; WithNameSpace: boolean; Expected: string);
  begin
    AssertEquals('ExtractFileUnitname('''+Filename+''','+BoolToStr(WithNameSpace,true)+')',Expected,ExtractFileUnitname(Filename,WithNameSpace));
  end;

begin
  t('a.pas',true,'a');
  t('a.pp',true,'a');
  t('a.p',true,'a');
  t('ab.pas',true,'ab');
  t('a.pas',false,'a');
  t('ab.pas',false,'ab');
  t('a.b.pas',true,'a.b');
  t('a.b.pas',false,'b');
  t('ab.c.pas',true,'ab.c');
  t('ab.c.pas',false,'c');
  t('ab.c.d.pas',true,'ab.c.d');
  t('ab.c.d.pas',false,'d');
end;

procedure TTestBasicCodeTools.TestParseFPCParameters;
var
  Parsed: TObjectList;

  procedure Init(CmdLineParams: string);
  begin
    Parsed.Clear;
    ParseFPCParameters(CmdLineParams,Parsed);
  end;

  procedure T(Params: string; ExpectedName, ExpectedValue: string;
    ExpectedKind: TFPCParamKind; ExpectedFlags: TFPCParamFlags);
  var
    Actual: TFPCParamValue;
  begin
    Init(Params);
    Actual:=GetFPCParamValue(Parsed,ExpectedName);
    AssertNotNull('Param "'+ExpectedName+'" not found in "'+Params+'"',Actual);
    AssertEquals('Params="'+Params+'" Param="'+ExpectedName+'" Value mismatch',ExpectedValue,Actual.Value);
    AssertEquals('Params="'+Params+'" Param="'+ExpectedName+'" Kind mismatch',dbgs(ExpectedKind),dbgs(Actual.Kind));
    AssertEquals('Params="'+Params+'" Param="'+ExpectedName+'" Flags mismatch',dbgs(ExpectedFlags),dbgs(Actual.Flags));
  end;

begin
  Parsed:=TObjectList.Create(true);
  try
    t('-a','a',FPCParamEnabled,fpkBoolean,[]);
    t('-al','al',FPCParamEnabled,fpkBoolean,[]);
    t('-aln','an',FPCParamEnabled,fpkBoolean,[]);
    t('-al-n','al','',fpkBoolean,[fpfUnset]);
    t('-al+l-n','al','',fpkBoolean,[fpfUnset,fpfSetTwice,fpfValueChanged]);
    t('-al-ln','al',FPCParamEnabled,fpkBoolean,[fpfSetTwice,fpfValueChanged]);
    t('-Adefault','Adefault',FPCParamEnabled,fpkBoolean,[]);
    t('-b-','b','',fpkBoolean,[fpfUnset]);
    t('-bl','bl',FPCParamEnabled,fpkBoolean,[]);
    t('-C3','C3',FPCParamEnabled,fpkBoolean,[]);
    t('-Cavalue','Ca','value',fpkValue,[]);
    t('-CPPACKSET=4','CPPACKSET','4',fpkValue,[]);
    t('-CrRt','CR',FPCParamEnabled,fpkBoolean,[]);
    t('-dMacro','Macro','',fpkDefine,[]);
    t('-Dddesc','Dd','desc',fpkValue,[]);
    t('-epath','e','path',fpkValue,[]);
    t('-E','E',FPCParamEnabled,fpkBoolean,[]);
    t('-fPIC','fPIC',FPCParamEnabled,fpkBoolean,[]);
    t('-Fabla','Fa','bla',fpkMultiValue,[]);
    t('-g','g',FPCParamEnabled,fpkBoolean,[]);
    t('-gchl','gh',FPCParamEnabled,fpkBoolean,[]);
    t('-goset','goset',FPCParamEnabled,fpkBoolean,[]);
    t('-gw2','gw','2',fpkValue,[]);
    t('-iD','iD',FPCParamEnabled,fpkBoolean,[]);
    t('-iSO','iSO',FPCParamEnabled,fpkBoolean,[]);
    t('-kbla','k','bla',fpkMultiValue,[]);
    t('-l-','l','',fpkBoolean,[fpfUnset]);
    t('-Mdelphi','M','delphi',fpkValue,[]);
    t('-n-','n','',fpkBoolean,[fpfUnset]);
    t('-opath','o','path',fpkValue,[]);
    t('-O1','O','1',fpkValue,[]);
    t('-O2','O','2',fpkValue,[]);
    t('-O3','O','3',fpkValue,[]);
    t('-O4','O','4',fpkValue,[]);
    t('-O1 -O-','O','',fpkValue,[fpfUnset,fpfSetTwice,fpfValueChanged]);
    t('-O1 -O2','O','2',fpkValue,[fpfSetTwice,fpfValueChanged]);
    t('-Oab=c','Oab','c',fpkValue,[]);
    t('-pg','pg',FPCParamEnabled,fpkBoolean,[]);
    t('-Rfoo','R','foo',fpkValue,[]);
    t('-S2','S2',FPCParamEnabled,fpkBoolean,[]);
    t('-Sefoo','Se','foo',fpkValue,[]);
    t('-s','s',FPCParamEnabled,fpkBoolean,[]);
    t('-sht-','st','',fpkBoolean,[fpfUnset]);
    t('-Tfoo','T','foo',fpkValue,[]);
    t('-uMacro','Macro','',fpkDefine,[fpfUnset]);
    t('-dMacro -uMacro','Macro','',fpkDefine,[fpfUnset,fpfSetTwice,fpfValueChanged]);
    t('-dmacro -uMACRO','macro','',fpkDefine,[fpfUnset,fpfSetTwice,fpfValueChanged]);
    t('-Uns-','Us','',fpkBoolean,[fpfUnset]);
    t('-vwne','ve',FPCParamEnabled,fpkBoolean,[]);
    t('-vmfoo','vm','foo',fpkMultiValue,[]);
    t('-Wbe','We',FPCParamEnabled,fpkBoolean,[]);
    t('-WMfoo','WM','foo',fpkValue,[]);
    t('-X9LA-LO-','XLO','',fpkBoolean,[fpfUnset]);
  finally
    Parsed.Free;
  end;
end;

procedure TTestBasicCodeTools.TestChangeLineEndings;

  procedure t(s, NewLineEnding, Expected: string);
  var
    Actual: string;
  begin
    Actual:=ChangeLineEndings(s,NewLineEnding);
    if Actual=Expected then exit;
    AssertEquals('s="'+DbgStr(s)+'" NewLineEnding="'+DbgStr(NewLineEnding)+'"',DbgStr(Expected),DbgStr(Actual));
  end;

begin
  t('','','');
  t('A',#10,'A');
  t('A'#10,#10,'A'#10);
  t(#10'A',#10,#10'A');
  t('A'#13,#13,'A'#13);
  t(#13'A',#13,#13'A');
  t('A'#13#10,#10,'A'#10);
  t(#13#10'A',#10,#10'A');
end;

initialization
  RegisterTest(TTestBasicCodeTools);

end.

