{
 Test with:
     ./runtests --format=plain --suite=TestCTScanRange
     ./runtests --format=plain --suite=TestCTScanRangeAscending
     ./runtests --format=plain --suite=TestCTScanRangeProcModified
}
unit TestCTRangeScan;

{$mode objfpc}{$H+}

interface

{$IFNDEF EnableCTRange}
implementation
end.
{$ENDIF}

uses
  Classes, SysUtils, fpcunit, testglobals, FileProcs, CodeToolManager,
  CodeCache, CustomCodeTool, LinkScanner, CodeTree, EventCodeTool;

type
  TCTRgSrcFlag = (
    crsfWithProc1,
    crsfWithProc1Modified,
    crsfWithCommentAtEnd,
    crsfWithInitialization,
    crsfWithFinalization
    );
  TCTRgSrcFlags = set of TCTRgSrcFlag;

  { TTestCodetoolsRangeScan }

  TTestCodetoolsRangeScan = class(TTestCase)
  protected
    function GetSource(Flags: TCTRgSrcFlags): string;
  published
    procedure TestCTScanRange;
    procedure TestCTScanRangeAscending;
    procedure TestCTScanRangeDescending;
    procedure TestCTScanRangeProcModified;
  end;

implementation

{ TTestCodetoolsRangeScan }

function TTestCodetoolsRangeScan.GetSource(Flags: TCTRgSrcFlags): string;
begin
  Result:=
  'unit TestRangeScan;'+LineEnding
  +'interface'+LineEnding
  +'uses'+LineEnding
  +'  Classes;'+LineEnding
  +'var i: integer;'+LineEnding
  +'implementation'+LineEnding
  +'uses'+LineEnding
  +'  Math;'+LineEnding
  +'const c = 3;'+LineEnding;
  if crsfWithProc1 in Flags then
    Result:=Result+'procedure Proc1;'+LineEnding
      +'begin'+LineEnding
      +'end;'+LineEnding;
  if crsfWithProc1Modified in Flags then
    Result:=Result+'procedure Proc1;'+LineEnding
      +'begin'+LineEnding
      +'  // comment'+LineEnding
      +'end;'+LineEnding;
  if crsfWithInitialization in Flags then
    Result:=Result+'initialization'+LineEnding;
  if crsfWithFinalization in Flags then
    Result:=Result+'finalization'+LineEnding;
  Result:=Result+'end.';
  if crsfWithCommentAtEnd in Flags then
    Result:=Result+LineEnding
      +'// end comment';
end;

procedure TTestCodetoolsRangeScan.TestCTScanRange;
var
  Code: TCodeBuffer;
  Tool: TCodeTool;
  RootNode: TCodeTreeNode;
  TreeChangeStep: LongInt;
begin
  Code:=CodeToolBoss.CreateFile('TestRangeScan.pas');
  Tool:=CodeToolBoss.GetCodeToolForSource(Code,false,true) as TCodeTool;

  // scan source
  Code.Source:=GetSource([]);
  debugln(['TTestCodetoolsRangeScan.TestCTScanRange INITIAL SCAN']);
  Tool.BuildTree(lsrEnd);
  RootNode:=Tool.Tree.Root;
  TreeChangeStep:=Tool.TreeChangeStep;
  AssertEquals('Step1: RootNode<>nil',true,RootNode<>nil);
  //Tool.WriteDebugTreeReport;

  // append a comment at end and scan again => this should result in no tree change
  Code.Source:=GetSource([crsfWithCommentAtEnd]);
  debugln(['TTestCodetoolsRangeScan.TestCTScanRange SCAN with comment at end']);
  Tool.BuildTree(lsrEnd);
  AssertEquals('Step2: RootNode=Tree.Root',true,RootNode=Tool.Tree.Root);
  AssertEquals('Step2: TreeChangeStep=Tool.TreeChangeStep',true,TreeChangeStep=Tool.TreeChangeStep);
  Tool.WriteDebugTreeReport;

  // insert a procedure in the implementation and scan again
  // => this should result in a tree change, but the root node should be kept
  Code.Source:=GetSource([crsfWithProc1]);
  debugln(['TTestCodetoolsRangeScan.TestCTScanRange SCAN with new proc in implementation']);
  Tool.BuildTree(lsrEnd);
  AssertEquals('Step3: RootNode=Tree.Root',true,RootNode=Tool.Tree.Root);
  AssertEquals('Step3: TreeChangeStep<>Tool.TreeChangeStep',true,TreeChangeStep<>Tool.TreeChangeStep);
  Tool.WriteDebugTreeReport;
end;

procedure TTestCodetoolsRangeScan.TestCTScanRangeAscending;
var
  Code: TCodeBuffer;
  Tool: TEventsCodeTool;
  r: TLinkScannerRange;
  RootNode: TCodeTreeNode;
  MinRange: TLinkScannerRange;
  MaxRange: TLinkScannerRange;
begin
  Code:=CodeToolBoss.CreateFile('TestRangeScan.pas');
  Tool:=CodeToolBoss.GetCodeToolForSource(Code,false,true) as TCodeTool;

  // empty tool
  Code.Source:='';
  Tool.BuildTree(lsrInit);

  // scan source
  Code.Source:=GetSource([crsfWithInitialization,crsfWithFinalization]);
  RootNode:=nil;
  MinRange:=low(TLinkScannerRange);
  MaxRange:=high(TLinkScannerRange);
  for r:=MinRange to MaxRange do begin
    debugln(['TTestCodetoolsRangeScan.TestCTScanRangeAscending Range=',dbgs(r)]);
    Tool.BuildTree(r);
    if RootNode<>nil then begin
      AssertEquals('RootNode must stay for ascending range '+dbgs(r),true,RootNode=Tool.Tree.Root);
    end;
    RootNode:=Tool.Tree.Root;
    Tool.WriteDebugTreeReport;
    case r of
    lsrNone: ;
    lsrInit: ;
    lsrSourceType:
      AssertEquals('source type scanned',true,RootNode<>nil);
    lsrSourceName: ;
    lsrInterfaceStart:
      AssertEquals('interface start scanned',true,Tool.FindInterfaceNode<>nil);
    lsrMainUsesSectionStart:
      AssertEquals('main uses section start scanned',true,Tool.FindMainUsesSection<>nil);
    lsrMainUsesSectionEnd:
      AssertEquals('main uses section end scanned',true,Tool.FindMainUsesSection.FirstChild<>nil);
    lsrImplementationStart:
      AssertEquals('implementation start scanned',true,Tool.FindImplementationNode<>nil);
    lsrImplementationUsesSectionStart:
      AssertEquals('implementation uses section start scanned',true,Tool.FindImplementationUsesSection<>nil);
    lsrImplementationUsesSectionEnd:
      AssertEquals('implementation uses section end scanned',true,Tool.FindImplementationUsesSection.FirstChild<>nil);
    lsrInitializationStart:
      AssertEquals('initialization section start scanned',true,Tool.FindInitializationNode<>nil);
    lsrFinalizationStart:
      AssertEquals('finalization section start scanned',true,Tool.FindFinalizationNode<>nil);
    lsrEnd:
      AssertEquals('end. found',true,Tool.Tree.FindRootNode(ctnEndPoint)<>nil);
    end;
  end;
end;

procedure TTestCodetoolsRangeScan.TestCTScanRangeDescending;
var
  Code: TCodeBuffer;
  Tool: TEventsCodeTool;
  MinRange: TLinkScannerRange;
  MaxRange: TLinkScannerRange;
  r: TLinkScannerRange;
begin
  Code:=CodeToolBoss.CreateFile('TestRangeScan.pas');
  Tool:=CodeToolBoss.GetCodeToolForSource(Code,false,true) as TCodeTool;

  // scan source
  Code.Source:=GetSource([]);
  MinRange:=low(TLinkScannerRange);
  MaxRange:=high(TLinkScannerRange);
  for r:=MaxRange downto MinRange do begin
    debugln(['TTestCodetoolsRangeScan.TestCTScanRangeDescending Range=',dbgs(r)]);
    Tool.BuildTree(r);
    AssertEquals('RootNode must stay for descending range '+dbgs(r),true,Tool.Tree.Root<>nil);
    //Tool.WriteDebugTreeReport;
  end;
end;

procedure TTestCodetoolsRangeScan.TestCTScanRangeProcModified;
var
  Code: TCodeBuffer;
  Tool: TEventsCodeTool;
begin
  Code:=CodeToolBoss.CreateFile('TestRangeScan.pas');
  Tool:=CodeToolBoss.GetCodeToolForSource(Code,false,true) as TCodeTool;

  // scan source
  Code.Source:=GetSource([crsfWithProc1]);
  Tool.BuildTree(lsrEnd);
  Tool.WriteDebugTreeReport;
  AssertEquals('step1: end. found',true,Tool.Tree.FindRootNode(ctnEndPoint)<>nil);

  Code.Source:=GetSource([crsfWithProc1Modified]);
  Tool.BuildTree(lsrEnd);
  Tool.WriteDebugTreeReport;
  AssertEquals('step2: end. found',true,Tool.Tree.FindRootNode(ctnEndPoint)<>nil);
end;

initialization
  AddToCodetoolsTestSuite(TTestCodetoolsRangeScan);

end.

