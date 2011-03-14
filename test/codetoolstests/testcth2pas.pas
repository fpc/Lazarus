{
 Test with:
     ./runtests --format=plain --suite=TTestCodetoolsH2Pas
     ./runtests --format=plain --suite=TestCTH2PMergeHeaderFiles
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

initialization
  AddToCodetoolsTestSuite(TTestCodetoolsH2Pas);

end.

