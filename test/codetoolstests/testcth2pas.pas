{
 Test with:
     ./runtests --format=plain --suite=TTestCodetoolsH2Pas
     ./runtests --format=plain --suite=TestCTH2PMergeHeaderFiles
     ./runtests --format=plain --suite=TestCTH2PReplaceMacros
     ./runtests --format=plain --suite=TestCTH2PConvertSimpleTypes
}
unit TestCTH2Pas;

{$mode objfpc}{$H+}

{$DEFINE VerboseTestCTH2Pas}

interface

uses
  Classes, SysUtils, fpcunit, testglobals, FileProcs, CodeToolManager,
  CodeCache, CCodeParserTool, H2PasTool;

type

  { TTestCodetoolsH2Pas }

  TTestCodetoolsH2Pas = class(TTestCase)
  protected
  published
    procedure TestCTH2PMergeHeaderFiles;
    procedure TestCTH2PReplaceMacros;
    procedure TestCTH2PConvertSimpleTypes;
  end;

implementation

{ TTestCodetoolsH2Pas }

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

procedure TTestCodetoolsH2Pas.TestCTH2PConvertSimpleTypes;
var
  Tool: TH2PasTool;
  Header1: TCodeBuffer;
  PasCode: TCodeBuffer;
begin
  Tool:=TH2PasTool.Create;
  try
    Header1:=CodeToolBoss.CreateFile('header1.h');
    PasCode:=CodeToolBoss.CreateFile('header1.pas');
    Header1.Source:='int i;';
    Tool.Convert(Header1,PasCode);
    Tool.WriteH2PNodeReport;
    Tool.WriteH2PDirectivesNodeReport;
    writeln;
    writeln('=============================================');
    writeln(PasCode.Source);
  finally
    Tool.Free;
  end;
end;

initialization
  AddToCodetoolsTestSuite(TTestCodetoolsH2Pas);

end.

