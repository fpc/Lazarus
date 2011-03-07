{
 Test with:
     ./runtests --format=plain --suite=TestCTScanRange
}
unit TestCTRangeScan;

{$mode objfpc}{$H+}

interface

{$IFNDEF EnableCTRange}
implementation
end.
{$ENDIF}

uses
  Classes, SysUtils, fpcunit, testglobals,
  CodeToolManager, CodeCache, CustomCodeTool, LinkScanner;

type

  { TTestCodetoolsRangeScan }

  TTestCodetoolsRangeScan = class(TTestCase)
  protected
  published
    procedure TestCTScanRange;
  end;

implementation

{ TTestCodetoolsRangeScan }

procedure TTestCodetoolsRangeScan.TestCTScanRange;
var
  Code: TCodeBuffer;
  Tool: TCodeTool;
begin
  Code:=CodeToolBoss.CreateFile('TestRangeScan.pas');
  Code.Source:=
     'unit TestRangeScan;'+LineEnding
    +'interface'+LineEnding
    +'uses'+LineEnding
    +'  Classes;'+LineEnding
    +'var i: integer;'+LineEnding
    +'implementation'+LineEnding
    +'uses'+LineEnding
    +'  Math;'+LineEnding
    +'const c = 3;'+LineEnding
    +'end.';
  Tool:=CodeToolBoss.GetCodeToolForSource(Code,false,true) as TCodeTool;
  Tool.BuildTree(lsrInitializationStart);
  Tool.WriteDebugTreeReport;
end;

initialization
  AddToCodetoolsTestSuite(TTestCodetoolsRangeScan);

end.

