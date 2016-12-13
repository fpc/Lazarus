{
 Test all with:
     ./runtests --format=plain --suite=TTestCodetoolsRangeScan

 Test specific with:
     ./runtests --format=plain --suite=TestCTScanRange
     ./runtests --format=plain --suite=TestCTScanRangeAscending
     ./runtests --format=plain --suite=TestCTScanRangeDescending
     ./runtests --format=plain --suite=TestCTScanRangeProcModified
     ./runtests --format=plain --suite=TestCTScanRangeImplementationToEnd
     ./runtests --format=plain --suite=TestCTScanRangeInitializationModified
     ./runtests --format=plain --suite=TestCTScanRangeLibraryInitializationModified
     ./runtests --format=plain --suite=TestCTScanRangeScannerAtEnd
}
unit TestCTRangeScan;

{$mode objfpc}{$H+}{$coperators on}

{off $DEFINE VerboseTestCTRangeScan}

interface

uses
  Classes, SysUtils, fpcunit, testglobals, FileProcs, CodeToolManager,
  CodeCache, CustomCodeTool, LinkScanner, CodeTree, EventCodeTool,
  PascalParserTool;

const
  ctrsCommentOfProc1 = 'comment of Proc1';
type
  TCTRgSrcFlag = (
    crsfWithProc1,
    crsfWithProc1Modified,
    crsfWithCommentAtEnd,
    crsfWithInitialization,
    crsfWithInitializationStatement1,
    crsfWithInitializationStatement2,
    crsfWithFinalization,
    crsLibrary
    );
  TCTRgSrcFlags = set of TCTRgSrcFlag;

  { TTestCodetoolsRangeScan }

  TTestCodetoolsRangeScan = class(TTestCase)
  protected
    function GetSource(Flags: TCTRgSrcFlags): string;
    procedure CheckTree(Tool: TCodeTool);
    function FindPos(Code: TCodeBuffer; SearchText: string; out Line,Col: integer; ErrorOnNotFound: boolean = true): boolean;
  published
    procedure TestCTScanRange;
    procedure TestCTScanRangeAscending;
    procedure TestCTScanRangeDescending;
    procedure TestCTScanRangeProcModified;
    procedure TestCTScanRangeImplementationToEnd;
    procedure TestCTScanRangeInitializationModified;
    procedure TestCTScanRangeLibraryInitializationModified;
    procedure TestCTScanRangeScannerAtEnd;
  end;

implementation

{ TTestCodetoolsRangeScan }

function TTestCodetoolsRangeScan.GetSource(Flags: TCTRgSrcFlags): string;
var
  IsUnit: Boolean;
begin
  IsUnit:=true;
  Result:='unit';
  if crsLibrary in Flags then begin
    Result:='library';
    IsUnit:=false;
  end;
  Result+=' TestRangeScan;'+LineEnding;
  if IsUnit then
    Result+='interface'+LineEnding;
  Result+='uses'+LineEnding
    +'  Classes;'+LineEnding
    +'var i: integer;'+LineEnding;
  if IsUnit then begin
    Result+='implementation'+LineEnding
      +'uses'+LineEnding
      +'  Math;'+LineEnding;
  end;
  Result+='const c = 3;'+LineEnding;
  if crsfWithProc1 in Flags then
    Result+='procedure Proc1;'+LineEnding
      +'begin'+LineEnding
      +'end;'+LineEnding;
  if crsfWithProc1Modified in Flags then
    Result+='procedure Proc1;'+LineEnding
      +'begin'+LineEnding
      +'  // '+ctrsCommentOfProc1+LineEnding
      +'end;'+LineEnding;
  if crsfWithInitialization in Flags then begin
    Result+='initialization'+LineEnding;
    if crsfWithInitializationStatement1 in Flags then
      Result+='i:=3;'+LineEnding;
    if crsfWithInitializationStatement2 in Flags then
      Result+='i:=5;'+LineEnding;
  end;
  if crsfWithFinalization in Flags then
    Result+='finalization'+LineEnding;
  Result+='end.';
  if crsfWithCommentAtEnd in Flags then
    Result+=LineEnding
      +'// end comment';
end;

procedure TTestCodetoolsRangeScan.CheckTree(Tool: TCodeTool);
var
  Node: TCodeTreeNode;
  LastSection: TCodeTreeNode;
begin
  if Tool=nil then exit;
  if Tool.Tree=nil then exit;
  Node:=Tool.Tree.Root;
  LastSection:=nil;
  while Node<>nil do begin
    if Node.Desc in AllCodeSections then begin
      if (LastSection<>nil) and (LastSection.Desc=Node.Desc) then
        AssertEquals('duplicate section '+Node.DescAsString+'.',false,true);
      LastSection:=Node;
    end;
    Node:=Node.Next;
  end;
end;

function TTestCodetoolsRangeScan.FindPos(Code: TCodeBuffer; SearchText: string;
  out Line, Col: integer; ErrorOnNotFound: boolean): boolean;
var
  p: SizeInt;
begin
  Line:=0;
  Col:=0;
  p:=Pos(SearchText,Code.Source);
  if p<1 then begin
    if ErrorOnNotFound then
      raise Exception.Create('TTestCodetoolsRangeScan.FindPos SearchText "'+SearchText+'" not found in "'+Code.Filename+'"');
    exit(false);
  end;
  Code.AbsoluteToLineCol(p,Line,Col);
  Result:=Line>=0;
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
  {$IFDEF VerboseTestCTRangeScan}
  debugln(['TTestCodetoolsRangeScan.TestCTScanRange INITIAL SCAN']);
  {$ENDIF}
  Tool.BuildTree(lsrEnd);
  RootNode:=Tool.Tree.Root;
  TreeChangeStep:=Tool.TreeChangeStep;
  AssertEquals('Step1: RootNode<>nil',true,RootNode<>nil);
  //Tool.WriteDebugTreeReport;

  // append a comment at end and scan again => this should result in no tree change
  Code.Source:=GetSource([crsfWithCommentAtEnd]);
  {$IFDEF VerboseTestCTRangeScan}
  debugln(['TTestCodetoolsRangeScan.TestCTScanRange SCAN with comment at end']);
  {$ENDIF}
  Tool.BuildTree(lsrEnd);
  AssertEquals('Step2: RootNode=Tree.Root',true,RootNode=Tool.Tree.Root);
  AssertEquals('Step2: TreeChangeStep=Tool.TreeChangeStep',true,TreeChangeStep=Tool.TreeChangeStep);
  //Tool.WriteDebugTreeReport;

  // insert a procedure in the implementation and scan again
  // => this should result in a tree change, but the root node should be kept
  Code.Source:=GetSource([crsfWithProc1]);
  {$IFDEF VerboseTestCTRangeScan}
  debugln(['TTestCodetoolsRangeScan.TestCTScanRange SCAN with new proc in implementation']);
  {$ENDIF}
  Tool.BuildTree(lsrEnd);
  AssertEquals('Step3: RootNode=Tree.Root',true,RootNode=Tool.Tree.Root);
  AssertEquals('Step3: TreeChangeStep<>Tool.TreeChangeStep',true,TreeChangeStep<>Tool.TreeChangeStep);
  //Tool.WriteDebugTreeReport;
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
    {$IFDEF VerboseTestCTRangeScan}
    debugln(['TTestCodetoolsRangeScan.TestCTScanRangeAscending Range=',dbgs(r)]);
    {$ENDIF}
    Tool.BuildTree(r);
    if RootNode<>nil then begin
      AssertEquals('RootNode must stay for ascending range '+dbgs(r),true,RootNode=Tool.Tree.Root);
    end;
    RootNode:=Tool.Tree.Root;
    //Tool.WriteDebugTreeReport;
    case r of
    lsrNone: ;
    lsrInit: ;
    lsrSourceType:
      AssertEquals('source type scanned',true,RootNode<>nil);
    lsrSourceName: ;
    lsrInterfaceStart:
      AssertEquals('interface start scanned',true,Tool.FindInterfaceNode<>nil);
    lsrMainUsesSectionStart:
      AssertEquals('main uses section start scanned',true,Tool.FindMainUsesNode<>nil);
    lsrMainUsesSectionEnd:
      AssertEquals('main uses section end scanned',true,Tool.FindMainUsesNode.FirstChild<>nil);
    lsrImplementationStart:
      AssertEquals('implementation start scanned',true,Tool.FindImplementationNode<>nil);
    lsrImplementationUsesSectionStart:
      AssertEquals('implementation uses section start scanned',true,Tool.FindImplementationUsesNode<>nil);
    lsrImplementationUsesSectionEnd:
      AssertEquals('implementation uses section end scanned',true,Tool.FindImplementationUsesNode.FirstChild<>nil);
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
  Code.Source:='begin end.';
  Tool.BuildTree(lsrEnd);
  Code.Source:=GetSource([]);
  MinRange:=low(TLinkScannerRange);
  MaxRange:=high(TLinkScannerRange);
  for r:=MaxRange downto MinRange do begin
    {$IFDEF VerboseTestCTRangeScan}
    debugln(['TTestCodetoolsRangeScan.TestCTScanRangeDescending Range=',dbgs(r)]);
    {$ENDIF}
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
  //Tool.WriteDebugTreeReport;
  AssertEquals('step1: end. found',true,Tool.Tree.FindRootNode(ctnEndPoint)<>nil);

  Code.Source:=GetSource([crsfWithProc1Modified]);
  Tool.BuildTree(lsrEnd);
  //Tool.WriteDebugTreeReport;
  AssertEquals('step2: end. found',true,Tool.Tree.FindRootNode(ctnEndPoint)<>nil);

  CheckTree(Tool);
end;

procedure TTestCodetoolsRangeScan.TestCTScanRangeImplementationToEnd;
var
  Code: TCodeBuffer;
  Tool: TEventsCodeTool;
begin
  Code:=CodeToolBoss.CreateFile('TestRangeScan.pas');
  Tool:=CodeToolBoss.GetCodeToolForSource(Code,false,true) as TCodeTool;

  Code.Source:='';
  Tool.BuildTree(lsrInit);

  // scan source
  Code.Source:=GetSource([crsfWithProc1]);
  Tool.BuildTree(lsrImplementationStart);
  //Tool.WriteDebugTreeReport;
  AssertEquals('step1: implementation found',true,Tool.FindImplementationNode<>nil);

  Tool.BuildTree(lsrEnd);
  //Tool.WriteDebugTreeReport;
  AssertEquals('step2: end. found',true,Tool.Tree.FindRootNode(ctnEndPoint)<>nil);

  CheckTree(Tool);
end;

procedure TTestCodetoolsRangeScan.TestCTScanRangeInitializationModified;
var
  Code: TCodeBuffer;
  Tool: TCodeTool;
begin
  Code:=CodeToolBoss.CreateFile('TestRangeScan.pas');
  Tool:=CodeToolBoss.GetCodeToolForSource(Code,false,true) as TCodeTool;

  // scan source with initialization
  Code.Source:=GetSource([crsfWithInitialization,crsfWithInitializationStatement1]);
  Tool.BuildTree(lsrEnd);
  AssertEquals('step1: end found',true,Tool.Tree.FindRootNode(ctnEndPoint)<>nil);

  // scan source with a modified initialization
  Code.Source:=GetSource([crsfWithInitialization,crsfWithInitializationStatement2]);
  Tool.BuildTree(lsrEnd);
  AssertEquals('step2: end found',true,Tool.Tree.FindRootNode(ctnEndPoint)<>nil);

  CheckTree(Tool);
  //Tool.WriteDebugTreeReport;
end;

procedure TTestCodetoolsRangeScan.TestCTScanRangeLibraryInitializationModified;
var
  Code: TCodeBuffer;
  Tool: TCodeTool;
begin
  Code:=CodeToolBoss.CreateFile('TestRangeScan.pas');
  Tool:=CodeToolBoss.GetCodeToolForSource(Code,false,true) as TCodeTool;

  // scan source with initialization
  Code.Source:=GetSource([crsLibrary,crsfWithInitialization,crsfWithInitializationStatement1]);
  Tool.BuildTree(lsrEnd);
  AssertEquals('step1: end found',true,Tool.Tree.FindRootNode(ctnEndPoint)<>nil);

  // scan source with a modified initialization
  Code.Source:=GetSource([crsLibrary,crsfWithInitialization,crsfWithInitializationStatement2]);
  Tool.BuildTree(lsrEnd);
  AssertEquals('step2: end found',true,Tool.Tree.FindRootNode(ctnEndPoint)<>nil);

  CheckTree(Tool);
  //Tool.WriteDebugTreeReport;
end;

procedure TTestCodetoolsRangeScan.TestCTScanRangeScannerAtEnd;
var
  Code: TCodeBuffer;
  Tool: TCodeTool;
  CursorPos: TCodeXYPosition;
  CleanCursorPos: integer;
begin
  Code:=CodeToolBoss.CreateFile('TestRangeScan.pas');
  Tool:=CodeToolBoss.GetCodeToolForSource(Code,false,true) as TCodeTool;

  // scan source til implementation uses end
  Code.Source:=GetSource([crsfWithProc1Modified]);
  Tool.BuildTree(lsrImplementationUsesSectionEnd);

  // let LinkScanner scan til end
  Tool.Scanner.Scan(lsrEnd,false);

  AssertEquals('scanner at end, tool at impl uses end',true,Tool.ScannedRange=lsrImplementationUsesSectionEnd);
  AssertEquals('scanner at end, tool at impl uses end',true,Tool.Scanner.ScannedRange=lsrEnd);
  // test BuildTreeAndGetCleanPos in implementation section
  CursorPos.Code:=Code;
  FindPos(Code,ctrsCommentOfProc1,CursorPos.Y,CursorPos.X);
  Tool.BuildTreeAndGetCleanPos(trTillCursor,lsrEnd,CursorPos,CleanCursorPos,
                          [btSetIgnoreErrorPos]);
  AssertEquals('scanner and tool at end',true,Tool.ScannedRange=lsrEnd);
  AssertEquals('scanner and tool at end',true,Tool.Scanner.ScannedRange=lsrEnd);
end;

initialization
  AddToPascalTestSuite(TTestCodetoolsRangeScan);

end.

