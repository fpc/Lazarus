unit TestMarkupIfDef;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, TestBase, TestHighlightFoldBase, LCLProc, SynEdit,
  SynEditMarkupIfDef, SynHighlighterPas, SynEditHighlighterFoldBase, SynEditMiscClasses;

type

  { TTestMarkupIfDef }

  TTestMarkupIfDef = class(TTestBaseHighlighterFoldBase)
  private
    function TestText1: TStringArray;
  protected
    function CreateTheHighLighter: TSynCustomFoldHighlighter; override;
    //procedure SetUp; override;
    //procedure TearDown; override;
    //procedure ReCreateEdit; reintroduce;
  published
    procedure TestIfDefTree;
  end;

implementation

{ TTestMarkupIfDef }

function TTestMarkupIfDef.TestText1: TStringArray;
  procedure AddLine(s: String);
  begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := s;
  end;
begin
  // 1
  AddLine('//'                                                    );
  AddLine('{$IFDEF a}'                                            );
  AddLine(''                                                      );
  AddLine('  // ifdef'                                            );
  // 5
  AddLine('  {$IFDEF b}'                                          );
  AddLine(''                                                      );
  AddLine('    {$IFDEF c} F1; {$else} F2; {$endif}   {$IFDEF d}'  );
  AddLine('      {$IFDEF e}  {$IFDEF f} F1; {$else} F2; {$endif} ');
  AddLine(''                                                      );
  // 10
  AddLine('      {$Endif}'                                        ); // close e from line 8
  AddLine('    {$Endif}'                                          ); // close d from line 7
  AddLine(''                                                      );
  AddLine('    {$IFDEF c2}'                                       );
  AddLine('    {$Endif}'                                          ); // close c2
  // 15
  AddLine('    {$IFDEF x} {$endif} '                              );
  AddLine(''                                                      );
  AddLine('  {$Endif}'                                            ); // close b from line 5
  AddLine(''                                                      );
  AddLine('  {$IFDEF b2}'                                         );
  // 20
  AddLine(''                                                      );
  AddLine('  {$Endif}'                                            ); // close b2
  AddLine(''                                                      );
  AddLine('{$Endif}'                                              ); // close a
  AddLine(''                                                      );
end;

function TTestMarkupIfDef.CreateTheHighLighter: TSynCustomFoldHighlighter;
begin
  Result := TSynPasSyn.Create(nil);
end;

procedure TTestMarkupIfDef.TestIfDefTree;
var
  t: TSynMarkupHighIfDefLinesTree;

  procedure CheckOuter(AName: String; ALine, ExpOuter: Integer);
  var
    n1, n2: TSynMarkupHighIfDefLinesNodeInfo;
  begin
    n1 := t.FindNodeAtPosition(ALine, afmNil);
    AName := Format('%s - %s L=%d Exp=%d ', [BaseTestName, AName, ALine, ExpOuter]);
    AssertTrue(AName + 'HasNode', n1.HasNode);
    AssertEquals(AName + 'Outer', ExpOuter, n1.OuterNestingLine);
    if ExpOuter = 0 then exit;
    n2 := t.FindNodeAtPosition(n1.OuterNestingLine, afmNil);
    AssertTrue(AName + 'Outer.High HasNode', n2.HasNode);
    AssertTrue(AName + 'Outer.High Value', n2.HighestValidNestedLine >= ExpOuter);
  end;
  procedure CheckOuter(AName: String; ExpOuter: Integer; ATestInner: Array of Integer);
  var
    i: Integer;
  begin
    for i := low(ATestInner) to high(ATestInner) do
      CheckOuter(AName, ATestInner[i], ExpOuter);
  end;

  procedure CheckPeer(AName: String; ALine, ACol: Integer;
    AType: SynMarkupIfDefNodeFlag; ExpLine, ExpCol: Integer);
  var
    n1: TSynMarkupHighIfDefLinesNodeInfo;
    p: TSynMarkupHighIfDefEntry;
  begin
    AName := Format('%s - %s L=%d Col=%d %s <=> %d, %d', [BaseTestName, AName, ALine, ACol, dbgs(AType), ExpLine, ExpCol]);
    n1 := t.FindNodeAtPosition(ALine, afmNil);
    AssertTrue(AName + 'HasNode', n1.HasNode);
    AssertTrue(AName + 'HasEntry', n1.EntryCount > ACol);
    case AType of
      idnIfdef: p := n1.Entry[ACol].IfDefPeer;
      idnElse:  p := n1.Entry[ACol].ElsePeer;
      idnEndIf: p := n1.Entry[ACol].EndIfPeer;
    end;
    AssertTrue(AName + 'Peer', p <> nil);
    AssertEquals(AName + 'Peer.Y', ExpLine, p.Line.GetPosition);
    AssertTrue(AName + 'Peer.X (1)', p.Line.EntryCount > ExpCol);
    AssertTrue(AName + 'Peer.X (2)', p.Line.Entry[ExpCol] = p);
  end;

var
  n: String;
begin
  ReCreateEdit;
  SetLines(TestText1);

  t := TSynMarkupHighIfDefLinesTree.Create;
  t.Lines       := SynEdit.ViewedTextBuffer;
  t.Highlighter := TSynPasSyn(SynEdit.Highlighter);

  n := 'ValidateRange(5,7)';
  t.ValidateRange(5,7);
  t.DebugPrint;DebugLn;
  t.DebugPrint(true);DebugLn;
  CheckPeer(n,   7,  0,    idnElse,   7, 1);
  CheckPeer(n,   7,  1,    idnEndIf,  7, 2);

  t.ValidateRange(5,7);
  t.DebugPrint(true); DebugLn;

  n := 'ValidateRange(5,7);(21,22)';
  t.ValidateRange(21,22);
  t.DebugPrint(true);DebugLn;
  CheckOuter(n,  0,  [2]);
  CheckOuter(n,  2,  [5, 19]);
  CheckOuter(n,  5,  [7]);
  CheckOuter(n, 19,  [21]);

  n := 'ValidateRange(5,7);(21,22);(15,19)';
  t.ValidateRange(15,19);
  t.DebugPrint(true);DebugLn;
  CheckOuter(n,  0,  [2]);
  CheckOuter(n,  2,  [5, 19]);
  CheckOuter(n,  5,  [7, 15, 17]);
  CheckOuter(n, 19,  [21]);


  n := 'ValidateRange 1-24';
  t.ValidateRange(1,24);
  t.DebugPrint(true);DebugLn;
  CheckOuter(n,  0,  [2]);
  CheckOuter(n,  2,  [5, 19]);
  CheckOuter(n,  5,  [7, 13, 15, 17]);
  CheckOuter(n,  7,  [8, 11]);
  CheckOuter(n,  8,  [10]);
  CheckOuter(n, 13,  [14]);
  CheckOuter(n, 19,  [21]);
  CheckOuter(n,  0,  [2]);
  CheckOuter(n,  2,  [5]);
  CheckOuter(n,  5,  [7]);
  CheckPeer(n,   2,  0,    idnEndIf, 23, 0);
  CheckPeer(n,   5,  0,    idnEndIf, 17, 0);
  CheckPeer(n,   7,  0,    idnElse,   7, 1);
  CheckPeer(n,   7,  1,    idnEndIf,  7, 2);
  CheckPeer(n,   7,  3,    idnEndIf, 11, 0);


  t.Free;
end;

initialization
  RegisterTest(TTestMarkupIfDef);

end.

