{
 Test with:
     ./runtests --format=plain --suite=TTestCodetoolsH2Pas
     ./runtests --format=plain --suite=TestCTH2PMergeHeaderFiles
     ./runtests --format=plain --suite=TestCTH2PReplaceMacros
     ./runtests --format=plain --suite=TestCTH2PConvertSimpleVars
     ./runtests --format=plain --suite=TestCTH2PConvertEnumsTypes
     ./runtests --format=plain --suite=TestCTH2PConvertConst
     ./runtests --format=plain --suite=TestCTH2PConvertSimpleTypedefs
     ./runtests --format=plain --suite=TestCTH2PConvertSimpleStructs
}
unit TestCTH2Pas;

{$mode objfpc}{$H+}

{$DEFINE VerboseTestCTH2Pas}

interface

uses
  Classes, SysUtils, fpcunit, testglobals, FileProcs, CodeToolManager,
  BasicCodeTools, CodeCache, CCodeParserTool, H2PasTool;

type

  { TTestCodetoolsH2Pas }

  TTestCodetoolsH2Pas = class(TTestCase)
  protected
    procedure Test(Title, CHeaderSrc, ExpectedPasSrc: string);
    procedure TestIntf(Title, CHeaderSrc, ExpectedPasSrc: string);
  published
    procedure TestCTH2PMergeHeaderFiles;
    procedure TestCTH2PReplaceMacros;
    procedure TestCTH2PConvertSimpleVars;
    procedure TestCTH2PConvertEnumsTypes;
    procedure TestCTH2PConvertConst;
    procedure TestCTH2PConvertSimpleTypedefs;
    procedure TestCTH2PConvertSimpleStructs;
  end;

implementation

{ TTestCodetoolsH2Pas }

procedure TTestCodetoolsH2Pas.Test(Title, CHeaderSrc, ExpectedPasSrc: string);
var
  Tool: TH2PasTool;
  Header1: TCodeBuffer;
  PasCode: TCodeBuffer;
begin
  Tool:=TH2PasTool.Create;
  Header1:=nil;
  PasCode:=nil;
  try
    Header1:=CodeToolBoss.CreateFile('header1.h');
    PasCode:=CodeToolBoss.CreateFile('header1.pas');
    Header1.Source:=CHeaderSrc;
    Tool.Convert(Header1,PasCode);
    if CompareTextIgnoringSpace(ExpectedPasSrc,PasCode.Source,true)<>0 then begin
      // failed
      debugln(['TTestCodetoolsH2Pas.Test C Source="',CHeaderSrc,'"']);
      debugln(['TTestCodetoolsH2Pas.Test Expected pas="',ExpectedPasSrc,'"']);
      debugln(['TTestCodetoolsH2Pas.Test Found pas="',PasCode.Source,'"']);
      Tool.WriteH2PNodeReport;
      Tool.WriteH2PDirectivesNodeReport;
      AssertEquals(Title,ExpectedPasSrc,PasCode.Source);
    end else begin
      //debugln(['TTestCodetoolsH2Pas.Test Found pas="',PasCode.Source,'"']);
      AssertEquals(Title,true,true);
    end;
  finally
    if Header1<>nil then Header1.IsDeleted:=true;
    if PasCode<>nil then PasCode.IsDeleted:=true;
    Tool.Free;
  end;
end;

procedure TTestCodetoolsH2Pas.TestIntf(Title, CHeaderSrc, ExpectedPasSrc: string
  );
var
  UsesCTypes: String;
  EmpytImplementation: String;
begin
  UsesCTypes:='uses ctypes;'+LineEnding;
  EmpytImplementation:=LineEnding+'implementation'+LineEnding+'end.';
  Test(Title,CHeaderSrc,UsesCTypes+ExpectedPasSrc+EmpytImplementation);
end;

procedure TTestCodetoolsH2Pas.TestCTH2PMergeHeaderFiles;
var
  Header1, Header2: TCodeBuffer;
  Merger: TCHeaderFileMerger;
  Filenames: TStringList;
  Src: String;
  Header1Start: LongInt;
  Header2Start: LongInt;
  Header1End: LongInt;
begin
  Header1:=CodeToolBoss.CreateFile('header1.h');
  Header2:=CodeToolBoss.CreateFile('header2.h');

  Header1.Source:='// header1.h'+LineEnding
          +'#include header2.h'+LineEnding
          +'// end of header1.h';
  Header2.Source:='// header2.h';
  Merger:=TCHeaderFileMerger.Create;
  Filenames:=TStringList.Create;
  try
    Filenames.Add('header1.h');
    Filenames.Add('header2.h');
    Merger.Merge(Filenames,CodeToolBoss.SourceCache,[]);
    Src:=Merger.CombinedSource.Source;
    Header1Start:=System.Pos('// header1.h',Src);
    Header2Start:=System.Pos('// header2.h',Src);
    Header1End:=System.Pos('// end of header1.h',Src);
    //debugln(['TTestCodetoolsH2Pas.TestCTH2PMergeHeaderFiles ',Src]);
    AssertEquals('start comment header1.h',1,Header1Start);
    AssertEquals('start comment header1.h in front of header2.h',true,Header1Start<Header2Start);
    AssertEquals('start comment header2.h in front of end of header1.h',true,Header2Start<Header1End);
  finally
    Merger.Free;
    Filenames.Free;
  end;
end;

procedure TTestCodetoolsH2Pas.TestCTH2PReplaceMacros;
var
  Header1: TCodeBuffer;
  Buffers: TFPList;
  Merger: TCHeaderFileMerger;

  procedure Check(Msg, Src, ExpectedSrc: string);
  var
    NewSrc: String;
  begin
    Header1.Source:=Src;
    Merger.Merge(Buffers,[]);
    NewSrc:=Trim(Merger.CombinedSource.Source);
    AssertEquals(Msg,dbgstr(ExpectedSrc),dbgstr(NewSrc));
  end;

begin
  Header1:=CodeToolBoss.CreateFile('header1.h');

  Merger:=TCHeaderFileMerger.Create;
  Buffers:=TFPList.Create;
  try
    Buffers.Add(Header1);
    // undefine
    Merger.Macros['remove1']:='';
    Check('undefine remove1','remove1','');
    // define
    Merger.Macros['macro1']:='newvalue';
    Check('define macro1','macro1','newvalue');
    // define macro function
    Merger.Macros['macrofunc1()']:='newvalue';
    Check('define macrofunc1','macrofunc1(param)','newvalue');
    // do not replace #define
    Merger.Macros['macro1']:='newvalue';
    Check('do not replace macros in #define','#define macro1','#define macro1');
    Check('do not replace macros in #undef','#undef macro1','#undef macro1');
    Check('do not replace macros in #ifdef','#ifdef macro1','#ifdef macro1');
    Check('do not replace macros in #ifndef','#ifndef macro1','#ifndef macro1');
  finally
    Buffers.Free;
    Merger.Free;
  end;
end;

procedure TTestCodetoolsH2Pas.TestCTH2PConvertSimpleVars;
begin
  TestIntf('convert int i;',  'int i;',  'var i: cint; cvar; external;');
  TestIntf('convert #define c 1', '#define c 1',  'const c=1;');
  TestIntf('convert int y = 7;',  'int y = 7;',  'var y:cint;cvar;external;');
  TestIntf('convert short signed int ssi_octal = 0123;',
       'short signed int ssi_octal = 0123;',
       'var ssi_octal:csshort;cvar;external;');
  TestIntf('convert unsigned short unsigned_short;',
       'unsigned short unsigned_short;',
       'var unsigned_short:cushort;cvar;external;');
  TestIntf('convert unsigned long long unsigned_long_long;',
       'unsigned long long unsigned_long_long;',
       'var unsigned_long_long:culonglong;cvar;external;');
end;

procedure TTestCodetoolsH2Pas.TestCTH2PConvertEnumsTypes;
begin
  TestIntf('convert anonymous enum{ENUM1};',
       'enum{ENUM1};',
       'type enumENUM1 = (ENUM1);');
  TestIntf('convert one named enum color{red};',
       'enum color{red};',
       'type color = (red);');
  TestIntf('convert named enum with id: color{red=1};',
       'enum color{red=1};',
       'type color = (red=1);');
  TestIntf('convert multi enums: color{red,green,blue};',
       'enum color{red,green,blue};',
       'type color = (red,green,blue);');
end;

procedure TTestCodetoolsH2Pas.TestCTH2PConvertConst;
begin
  // const char a;           // A constant character
  TestIntf('convert const char a;',
       'const char a;',
       'var a: cchar;cvar;external;');

  //char const b;           // A constant character (the same)
  TestIntf('convert char const b;',
       'char const b;',
       'var b: cchar;cvar;external;');

  //char *const c;          // A constant pointer to a character
  TestIntf('convert char *const c;',
       'char *const c;',
       'var c: pcchar;cvar;external;');

  //const char *const d;    // A constant pointer to a constant character
  TestIntf('convert char *const d;',
       'char *const d;',
       'var d: pcchar;cvar;external;');

  //const char *e;          // A pointer to a constant character. The pointer may be modified.
  TestIntf('convert const char *e;',
       'const char *e;',
       'var e: pcchar;cvar;external;');
end;

procedure TTestCodetoolsH2Pas.TestCTH2PConvertSimpleTypedefs;
begin
  TestIntf('convert typedef unsigned short sa_family_t;',
    'typedef unsigned short sa_family_t;',
    'type sa_family_t = cushort;');
end;

procedure TTestCodetoolsH2Pas.TestCTH2PConvertSimpleStructs;
begin
  TestIntf('convert struct SwsContext;',
    'struct SwsContext;',
    'type SwsContext = record end;');
end;

initialization
  AddToCodetoolsTestSuite(TTestCodetoolsH2Pas);

end.

