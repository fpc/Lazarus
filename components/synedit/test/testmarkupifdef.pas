unit TestMarkupIfDef;

{$mode objfpc}{$H+}

interface

uses windows,
  Classes, SysUtils, math, testregistry, TestBase, TestHighlightFoldBase, LCLProc,
  SynEdit, SynEditMarkupIfDef, SynHighlighterPas, SynEditHighlighterFoldBase,
  SynEditMiscClasses, SynEditFoldedView;

type

  { TTestMarkupIfDef }

  TTestMarkupIfDef = class(TTestBaseHighlighterFoldBase)
  private
    FTestTree: TSynMarkupHighIfDefLinesTree;
    FOpenings: TLazSynEditNestedFoldsList;
    function TestTextEmpty: TStringArray;
    function TestTextNoIfDef: TStringArray;
    function TestTextIfDef: TStringArray;
    function TestText1: TStringArray;
    function TestText2: TStringArray;
    function TestText3: TStringArray;
  protected
    function CreateTheHighLighter: TSynCustomFoldHighlighter; override;
    //procedure SetUp; override;
    //procedure TearDown; override;
    procedure ReCreateEditForTreeTest(Lines: Array of String); reintroduce;
  published
    procedure TestIfDefTree;
  end;

implementation

{ TTestMarkupIfDef }

function TTestMarkupIfDef.TestTextEmpty: TStringArray;
begin
  SetLength(Result, 0);
end;

function TTestMarkupIfDef.TestTextNoIfDef: TStringArray;
  procedure AddLine(s: String);
  begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := s;
  end;
begin
  AddLine('//'                                                    );
  AddLine('//'                                                    );
  AddLine('//'                                                    );
  AddLine('//'                                                    );
  AddLine('//'                                                    );
end;

function TTestMarkupIfDef.TestTextIfDef: TStringArray;
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
  AddLine(' {$IFDEF b}'                                           );
  // 5
  AddLine('  {$IFDEF cc}'                                         );
  AddLine(''                                                      );
  AddLine(' {$IFDEF d}{$IFDEF eee}{$IFDEF ffff}'                  );
  AddLine(''                                                      );
  AddLine('{$IFDEF g'                                             );
  // 10
  AddLine('}{$IFDEF h'                                            );
  AddLine(''                                                      );
  AddLine(' }'                                                    );
  AddLine(''                                                      );
  AddLine('{$IFDEF i'                                             );
  // 15
  AddLine('    }{$IFDEF h'                                        );
  AddLine(''                                                      );
  AddLine('           }'                                          );
  AddLine(''                                                      );
  AddLine(''                                                      );

end;

function TTestMarkupIfDef.TestText1: TStringArray;
  procedure AddLine(s: String);
  begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := s;
  end;
begin
  // 1
  AddLine('//'                                                    );
  AddLine('{$IFDEF a}'                                            ); // A >> else 29 / end 36 (1-11)
  AddLine(''                                                      ); // |
  AddLine('  // $ifdef'                                           ); // |
  // 5
  AddLine('  {$IFDEF b}'                                          ); // | B >> end 17 (3-13)
  AddLine(''                                                      ); // | |
  AddLine('    {$IFDEF c} F1; {$else} F2; {$endif}   {$IFDEF d}'  ); // | | --(5-15/20-27/32-40) // D >> 11 (43-53)
  AddLine('      {$IFDEF e}  {$IFDEF f} F1; {$else} F2; {$endif} '); // | | | e to 10 (7-17) // -- (19-29/34-41/46-54)
  AddLine(''                                                      ); // | | | |
  // 10
  AddLine('      {$Endif}'                                        ); // | | | e << from line 8 (7-15)
  AddLine('    {$Endif}'                                          ); // | | d << from line 7 (5-13)
  AddLine(''                                                      ); // | |
  AddLine('    {$IFDEF c2}'                                       ); // | | c2 >> (5-16)
  AddLine('    {$Endif}'                                          ); // | | c2 << from 14 (5-13)
  // 15
  AddLine('    {$IFDEF x} {$endif} '                              ); // | | -- (5-15/16-24)
  AddLine(''                                                      ); // | |
  AddLine('  {$Endif}'                                            ); // | close b from line 5 (3-11)
  AddLine(''                                                      ); // |
  AddLine('  {$IFDEF b2}'                                         ); // | b2 >> else 21 / end 27 (3-14)
  // 20
  AddLine(''                                                      ); // | |
  AddLine('  {$Else}'                                             ); // | else b2 from 19 to 27 (3-10)
  AddLine(''                                                      ); // | |
  AddLine('    {$IFDEF H}'                                        ); // | | H >> (5-15)
  AddLine(''                                                      ); // | | |
  // 25
  AddLine('    {$Endif}'                                          ); // | | H << (5-13)
  AddLine('    {$IFDEF Y} {$endif} '                              ); // | | -- (5-15/16-24)
  AddLine('  {$Endif}'                                            ); // | << b2 from 19 (else 21) (3-11)
  AddLine(''                                                      ); // |
  AddLine('{$Else}'                                               ); // A else from 2 to 36 (1-8)
  // 30
  AddLine(''                                                      ); // |
  AddLine('  {$IFDEF M}'                                          ); // | M (3-13)
  AddLine('  {$Else}'                                             ); // | else M (3-10)
  AddLine(''                                                      ); // | |
  AddLine('  {$Endif}'                                            ); // | close M (3-11)
  // 35
  AddLine(''                                                      ); // |
  AddLine('{$Endif}'                                              ); // A << from 1 / else 29 (1-9)
  AddLine(''                                                      ); //
end;

function TTestMarkupIfDef.TestText2: TStringArray;
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
  AddLine('{$Endif}'                                              );
  // 5
  AddLine(''                                                      );
  AddLine('{$Endif}'                                              ); // extra endif
  AddLine(''                                                      );
  AddLine(''                                                      );
  AddLine(''                                                      );
  //10
  AddLine('{$IFDEF a}'                                            );
  AddLine(''                                                      );
  AddLine('{$else}'                                               );
  AddLine(''                                                      );
  AddLine('{$else}'                                               );
  // 15
  AddLine(''                                                      );
  AddLine('{$Endif}'                                              );
  AddLine(''                                                      );
  AddLine(''                                                      );
end;

function TTestMarkupIfDef.TestText3: TStringArray;
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
  AddLine('{$Endif}'                                              );
  // 5
  AddLine(''                                                      );
  AddLine(''                                                      );
  AddLine(''                                                      );
  AddLine(''                                                      );
  AddLine(''                                                      );
  AddLine(''                                                      );
  AddLine(''                                                      );
end;

function TTestMarkupIfDef.CreateTheHighLighter: TSynCustomFoldHighlighter;
begin
  Result := TSynPasSyn.Create(nil);
end;

procedure TTestMarkupIfDef.ReCreateEditForTreeTest(Lines: array of String);
begin
  FreeAndNil(FTestTree);
  FTestTree.DiscardOpeningList(FOpenings);
  FOpenings := nil;;


  ReCreateEdit;
  SetLines(Lines);

  FTestTree := TSynMarkupHighIfDefLinesTree.Create;
  FTestTree.Lines       := SynEdit.ViewedTextBuffer;
  FTestTree.Highlighter := TSynPasSyn(SynEdit.Highlighter);

  FOpenings := FTestTree.CreateOpeningList;
end;

procedure TTestMarkupIfDef.TestIfDefTree;

type
  TSynMarkupIfdefNodeTypeTest = (idnIfdef, idnElse, idnEndIf, idnNone);
  TPeerExpect = record
    PeerType:  TSynMarkupIfdefNodeTypeTest;  // use idnDisabled as dummy
    PeerY, PeerX: Integer;
  end;
  PPeerExpect = ^TPeerExpect;

const
  NodeTypeMap: array [TSynMarkupIfdefNodeType] of TSynMarkupIfdefNodeTypeTest =
    (idnIfdef, idnElse, idnEndIf);

type
  TNodeExpect = record
    ExpStart, ExpEnd, ExpEndLineOffs: Integer;
    ExpType:  TSynMarkupIfdefNodeTypeTest;  // use idnDisabled as dummy
    Peer1, Peer2: TPeerExpect;
  end;

  function dbgs(AFlag: TSynMarkupIfdefNodeTypeTest): String; overload;
  begin
    Result := '';
    WriteStr(Result, AFlag);
  end;

  function ExpP(PeerType: TSynMarkupIfdefNodeTypeTest; PeerY, PeerX: Integer): PPeerExpect;
  begin
    New(Result);
    Result^.PeerType := PeerType;
    Result^.PeerY := PeerY;
    Result^.PeerX := PeerX;
  end;

  function EpIf(PeerY, PeerX: Integer): PPeerExpect;
  begin
    Result := ExpP(idnIfdef, PeerY, PeerX);
  end;
  function EpElse(PeerY, PeerX: Integer): PPeerExpect;
  begin
    Result := ExpP(idnElse, PeerY, PeerX);
  end;
  function EpEnd(PeerY, PeerX: Integer): PPeerExpect;
  begin
    Result := ExpP(idnEndIf, PeerY, PeerX);
  end;

  function ExpN(ExpStart: Integer; ExpEnd: Integer = -1;
    ExpEndLineOffs: Integer = 0; // Default to check for same line end
    ExpType: TSynMarkupIfdefNodeTypeTest = idnDisabled;
    Peer1: PPeerExpect = nil; Peer2: PPeerExpect = nil): TNodeExpect;
  begin
    Result.ExpStart       := ExpStart;
    Result.ExpEnd         := ExpEnd;
    Result.ExpEndLineOffs := ExpEndLineOffs;
    Result.ExpType        := ExpType;
    if Peer1 = nil then Peer1 := ExpP(idnNone, -1 , -1);
    if Peer2 = nil then Peer2 := ExpP(idnNone, -1 , -1);
    Result.Peer1          := Peer1^;
    Result.Peer2          := Peer2^;
    Dispose(Peer1);
    Dispose(Peer2);
  end;

  function ExpN(ExpStart, ExpEnd: Integer; ExpType: TSynMarkupIfdefNodeTypeTest;
    Peer1: PPeerExpect = nil; Peer2: PPeerExpect = nil): TNodeExpect;
  begin
    Result := ExpN(ExpStart, ExpEnd, 0, ExpType, Peer1, Peer2);
  end;

  function ExpN(ExpStart, ExpEnd: Integer; ExpEndLineOffs: Integer;
    Peer1: PPeerExpect; Peer2: PPeerExpect = nil): TNodeExpect;
  begin
    Result := ExpN(ExpStart, ExpEnd, ExpEndLineOffs, idnNone, Peer1, Peer2);
  end;

  function ExpN(ExpStart, ExpEnd: Integer; Peer1: PPeerExpect; Peer2: PPeerExpect = nil): TNodeExpect;
  begin
    Result := ExpN(ExpStart, ExpEnd, 0, idnNone, Peer1, Peer2);
  end;

  function ExpN(ExpStart: Integer; Peer1: PPeerExpect; Peer2: PPeerExpect = nil): TNodeExpect;
  begin
    Result := ExpN(ExpStart, -1, 0, idnNone, Peer1, Peer2);
  end;

  procedure CheckNodes(AName: String; ALine: Integer;
    AExp: array of TNodeExpect);
  var
    Node: TSynMarkupHighIfDefEntry;

    procedure CheckPeer(ExpPeer: TPeerExpect);
    var
      TestPeer: TSynMarkupHighIfDefEntry;
      PName: String;
    begin
      PName := '';
      WriteStr(PName, AName, ' ', ExpPeer.PeerType);
      case ExpPeer.PeerType of
        idnIfdef: TestPeer := Node.IfDefPeer;
        idnElse:  TestPeer := Node.ElsePeer;
        idnEndIf: TestPeer := Node.EndIfPeer;
      end;
      if ExpPeer.PeerY = -99 then begin // special check for existence only
        AssertTrue(PName + 'Has Peer', TestPeer <> nil);
        AssertTrue(PName+' PeerType', ExpPeer.PeerType = NodeTypeMap[TestPeer.NodeType]);
      end
      else
      if ExpPeer.PeerY < 0 then begin
        AssertTrue(PName + 'NO Peer', TestPeer = nil);
      end
      else begin
        AssertTrue(PName + 'Has Peer', TestPeer <> nil);
        AssertTrue(PName+' PeerType', ExpPeer.PeerType = NodeTypeMap[TestPeer.NodeType]);
        AssertEquals(PName + 'Peer.Y', ExpPeer.PeerY, TestPeer.Line.GetPosition);
        if ExpPeer.PeerX >= 0 then
          AssertEquals(PName + 'Peer.X', ExpPeer.PeerX, TestPeer.StartColumn);
      end;
    end;
  var
    i, c: Integer;
    LineNode: TSynMarkupHighIfDefLinesNodeInfo;
    ExpNode: TNodeExpect;
  begin
    AName := Format('%s - %s L=%d', [BaseTestName, AName, ALine]);
    LineNode := FTestTree.FindNodeAtPosition(ALine, afmNil);
    c := length(AExp);
    if (c = 0) and (not LineNode.HasNode) then
      exit;
    AssertTrue(AName + 'HasNode', LineNode.HasNode);
    AssertEquals(AName + 'EntryCount', c, LineNode.EntryCount);
    for i := 0 to c - 1 do begin
      ExpNode := AExp[i];
      Node := LineNode.Entry[i];
      AssertTrue('Node.Line = LineNode', Node.Line = LineNode.Node);
      AssertEquals(AName+'StartCol', ExpNode.ExpStart, Node.StartColumn);
      if ExpNode.ExpEnd >= 0 then
        AssertEquals(AName+'EndCol', ExpNode.ExpEnd, Node.EndColumn);
      if ExpNode.ExpEndLineOffs >= 0 then begin
        AssertEquals(AName+'EndLineOffs', ExpNode.ExpEndLineOffs, LineNode.LastEntryEndLineOffs);
//        AssertTrue(AName+'EndLineOffs flag', idnMultiLineTag in Node.NodeFlags);
      end;
      if ExpNode.ExpType <> idnNone then
        AssertTrue(AName+'NodeTypeflag', NodeTypeMap[Node.NodeType] = ExpNode.ExpType);
      if ExpNode.Peer1.PeerType <> idnNone then
        CheckPeer(ExpNode.Peer1);
      if ExpNode.Peer2.PeerType <> idnNone then
        CheckPeer(ExpNode.Peer2);
    end;

  end;

  procedure CheckNodesXY(AName: String; ALine: Integer;
    AExp: array of Integer; // [Start, end,  start, end, ....]
    AExpEndOffs: Integer);
  var
    i, c: Integer;
    n1: TSynMarkupHighIfDefLinesNodeInfo;
  begin
    AName := Format('%s - %s L=%d', [BaseTestName, AName, ALine]);
    n1 := FTestTree.FindNodeAtPosition(ALine, afmNil);
    c := length(AExp);
    if (c = 0) and (not n1.HasNode) then
      exit;
    AssertTrue(AName + 'HasNode', n1.HasNode);
    AssertEquals(AName + 'EntryCount', c div 2, n1.EntryCount);
    for i := 0 to (c div 2) - 1 do begin
      AssertTrue('Node.Line = LineNode', n1.Entry[i].Line = n1.Node);
      AssertEquals(AName+'StartCol', AExp[i*2], n1.Entry[i].StartColumn);
      AssertEquals(AName+'EndCol', AExp[i*2+1], n1.Entry[i].EndColumn);
    end;
    AssertEquals(AName+'EndLine', AExpEndOffs, n1.LastEntryEndLineOffs);
  end;

  procedure CheckPeer(AName: String; ALine, ACol: Integer;
    AType: TSynMarkupIfdefNodeTypeTest; ExpLine, ExpCol: Integer);
  var
    n1: TSynMarkupHighIfDefLinesNodeInfo;
    p: TSynMarkupHighIfDefEntry;
  begin
    AName := Format('%s - %s L=%d Col=%d %s <=> %d, %d', [BaseTestName, AName, ALine, ACol, dbgs(AType), ExpLine, ExpCol]);
    n1 := FTestTree.FindNodeAtPosition(ALine, afmNil);
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
  nd: TSynMarkupHighIfDefLinesNodeInfo;
begin
  FTestTree := nil;

  n := 'No modification';
  ReCreateEditForTreeTest(TestTextIfDef);
  FTestTree.ValidateRange(1, 14, FOpenings);
  CheckNodesXY(n+'', 1, [], 0);
  CheckNodesXY(n+'', 2, [1, 11], 0);
  CheckNodesXY(n+'', 3, [], 0);
  CheckNodesXY(n+'', 4, [2, 12], 0);
  CheckNodesXY(n+'', 5, [3, 14], 0);
  CheckNodes(n+'', 5, [ExpN(3,14, idnIfdef)]);

  {%region  one line edit}
  ReCreateEditForTreeTest(TestTextIfDef);
  FTestTree.ValidateRange(1, 10, FOpenings);

  n := 'Remove Space AT line-start, Node after space';
  CheckNodesXY(n+'', 7, [2,12,   12,24,   24,37], 0);
  SynEdit.TextBetweenPoints[point(1, 7),point(2, 7)] := '';
  CheckNodesXY(n+'', 7, [1,11,   11,23,   23,36], 0);

  n := 'Insert Space AT line-start, Node after space';
  SynEdit.TextBetweenPoints[point(1, 7),point(1, 7)] := ' ';
  CheckNodesXY(n+'', 7, [2,12,   12,24,   24,37], 0);

  n := 'Replace Space AT line-start, Node after space';
  SynEdit.TextBetweenPoints[point(1, 7),point(2, 7)] := #9;
  CheckNodesXY(n+'', 7, [2,12,   12,24,   24,37], 0);

  n := 'Insert Space AT line-start, node later on line';
  SynEdit.TextBetweenPoints[point(1, 7),point(1, 7)] := ' ';
  CheckNodesXY(n+'', 7, [3,13,   13,25,   25,38], 0);

  n := 'Insert Space before first node on line';
  SynEdit.TextBetweenPoints[point(3, 7),point(3, 7)] := ' ';
  CheckNodesXY(n+'', 7, [4,14,   14,26,   26,39], 0);

  n := 'Remove Space before first node on line';
  SynEdit.TextBetweenPoints[point(3, 7),point(4, 7)] := '';
  CheckNodesXY(n+'', 7, [3,13,   13,25,   25,38], 0);

  n := 'Insert Space between first and 2nd node on line';
  SynEdit.TextBetweenPoints[point(13, 7),point(13, 7)] := ' ';
  CheckNodesXY(n+'', 7, [3,13,   14,26,   26,39], 0);

  n := 'Remove Space between first and 2nd node on line';
  SynEdit.TextBetweenPoints[point(13, 7),point(14, 7)] := '';
  CheckNodesXY(n+'', 7, [3,13,   13,25,   25,38], 0);

  n := 'Insert Space after last node on line';
  SynEdit.TextBetweenPoints[point(38, 7),point(38, 7)] := ' ';
  CheckNodesXY(n+'', 7, [3,13,   13,25,   25,38], 0);

  n := 'Remove Space after last node on line';
  SynEdit.TextBetweenPoints[point(38, 7),point(39, 7)] := '';
  CheckNodesXY(n+'', 7, [3,13,   13,25,   25,38], 0);

  // Edit in node
  ReCreateEditForTreeTest(TestTextIfDef);
  FTestTree.ValidateRange(1, 19, FOpenings);
  n := 'Insert Space INSIDE node (1st node on line)';
  SynEdit.TextBetweenPoints[point(3, 7),point(3, 7)] := ' ';
  CheckNodesXY(n+'', 7, [2,13,   13,25,   25,38], 0);

  n := 'REMOVE Space INSIDE node (1st node on line)';
  SynEdit.TextBetweenPoints[point(3, 7),point(4, 7)] := '';
  CheckNodesXY(n+'', 7, [2,12,   12,24,   24,37], 0);

  n := 'Insert Space INSIDE node end (1st node on line)';
  SynEdit.TextBetweenPoints[point(11, 7),point(11, 7)] := ' ';
  CheckNodesXY(n+'', 7, [2,13,   13,25,   25,38], 0);

  n := 'REMOVE Space INSIDE node end (1st node on line)';
  SynEdit.TextBetweenPoints[point(11, 7),point(12, 7)] := '';
  CheckNodesXY(n+'', 7, [2,12,   12,24,   24,37], 0);

  n := 'Insert Space INSIDE node (last node on line)';
  SynEdit.TextBetweenPoints[point(26, 7),point(26, 7)] := ' ';
  CheckNodesXY(n+'', 7, [2,12,   12,24,   24,38], 0);

  n := 'REMOVE Space INSIDE node (last node on line)';
  SynEdit.TextBetweenPoints[point(26, 7),point(27, 7)] := '';
  CheckNodesXY(n+'', 7, [2,12,   12,24,   24,37], 0);

  n := 'Insert Space INSIDE node (last node on line, multiline)';
  SynEdit.TextBetweenPoints[point(3, 14),point(3, 14)] := ' ';
  CheckNodesXY(n+'', 14, [1,6], 1);

  n := 'REMOVE Space INSIDE node (last node on line, multiline)';
  SynEdit.TextBetweenPoints[point(3, 14),point(4, 14)] := '';
  CheckNodesXY(n+'', 14, [1,6], 1);


  // test 1,12 modify previous node
  n := 'Insert Space INSIDE node (last node on line, multiline, on end line)';
  SynEdit.TextBetweenPoints[point(1, 12),point(1, 12)] := ' ';
  CheckNodesXY(n+'', 10, [2,4], 2);

  n := 'REMOVE Space INSIDE node (last node on line, multiline, on end line)';
  SynEdit.TextBetweenPoints[point(1, 12),point(2, 12)] := '';
  CheckNodesXY(n+'', 10, [2,3], 2);

  // test 1,15 modify previous and current node
  n := 'Insert Space INSIDE node (last node on line, multiline, on end line with NEXT open)';
  SynEdit.TextBetweenPoints[point(2, 15),point(2, 15)] := ' ';
  CheckNodesXY(n+'', 14, [1, 7], 1);
  CheckNodesXY(n+'', 15, [7,13], 2);

    n := 'REMOVE Space INSIDE node (last node on line, multiline, on end line with NEXT open)';
  SynEdit.TextBetweenPoints[point(2, 15),point(3, 15)] := '';
  CheckNodesXY(n+'', 14, [1, 6], 1);
  CheckNodesXY(n+'', 15, [6,13], 2);

  n := 'Insert Space Between node (last node on line, multiline, on end line with NEXT open)';
  SynEdit.TextBetweenPoints[point(6, 15),point(6, 15)] := ' ';
  CheckNodesXY(n+'', 14, [1, 6], 1);
  CheckNodesXY(n+'', 15, [7,13], 2);

  n := 'REMOVE Space Between node (last node on line, multiline, on end line with NEXT open)';
  SynEdit.TextBetweenPoints[point(6, 15),point(7, 15)] := '';
  CheckNodesXY(n+'', 14, [1, 6], 1);
  CheckNodesXY(n+'', 15, [6,13], 2);

  n := 'Insert Space at end of node (IN node) (last node on line, multiline, on end line with NEXT open)';
  SynEdit.TextBetweenPoints[point(5, 15),point(5, 15)] := ' ';
  CheckNodesXY(n+'', 14, [1, 7], 1);
  CheckNodesXY(n+'', 15, [7,13], 2);

// delete an entire node
  n := 'Delete full node';
  SynEdit.TextBetweenPoints[point(10, 7),point(27, 7)] := '';
  CheckNodesXY(n+'', 7, [2,10,   10,20], 0);

  {%endregion one line}

  {%region  Line Breaks }

  n := 'not modified';
  ReCreateEditForTreeTest(TestTextIfDef);
  FTestTree.ValidateRange(1, 19, FOpenings);
DebugLn('###########');FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 6, [], 0);
  CheckNodesXY(n+'', 7, [2,12,   12,24,   24,37], 0);
  CheckNodesXY(n+'', 8, [], 0);
  CheckNodesXY(n+'', 9, [1,2], 1);


  n := 'Insert LineBreak at line start nodes';
  SynEdit.TextBetweenPoints[point(1, 7),point(1, 7)] := LineEnding;
DebugLn('###########');FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 6, [], 0);
  CheckNodesXY(n+'', 7, [], 0);
  CheckNodesXY(n+'', 8, [2,12,   12,24,   24,37], 0);
  CheckNodesXY(n+'', 9, [], 0);
  CheckNodesXY(n+'',10, [1,2], 1);

  n := 'Remove LineBreak at line start nodes';
  SynEdit.TextBetweenPoints[point(1, 7),point(1, 8)] := '';
FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 6, [], 0);
  CheckNodesXY(n+'', 7, [2,12,   12,24,   24,37], 0);
  CheckNodesXY(n+'', 8, [], 0);
  CheckNodesXY(n+'', 9, [1,2], 1);


  n := 'Insert LineBreak before nodes';
  SynEdit.TextBetweenPoints[point(2, 7),point(2, 7)] := LineEnding;
DebugLn('###########');FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 6, [], 0);
  CheckNodesXY(n+'', 7, [], 0);
  CheckNodesXY(n+'', 8, [1,11,   11,23,   23,36], 0);
  CheckNodesXY(n+'', 9, [], 0);
  CheckNodesXY(n+'',10, [1,2], 1);

  n := 'Remove LineBreak before nodes';
  SynEdit.TextBetweenPoints[point(2, 7),point(1, 8)] := '';
FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 6, [], 0);
  CheckNodesXY(n+'', 7, [2,12,   12,24,   24,37], 0);
  CheckNodesXY(n+'', 8, [], 0);
  CheckNodesXY(n+'', 9, [1,2], 1);



  n := 'Insert LineBreak between nodes';
  SynEdit.TextBetweenPoints[point(24, 7),point(24, 7)] := LineEnding;
DebugLn('###########');FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 6, [], 0);
  CheckNodesXY(n+'', 7, [2,12,   12,24], 0);
  CheckNodesXY(n+'', 8, [1,14], 0);
  CheckNodesXY(n+'', 9, [], 0);
  CheckNodesXY(n+'',10, [1,2], 1);

  n := 'Remove LineBreak between nodes';
  SynEdit.TextBetweenPoints[point(24, 7),point(1, 8)] := '';
FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 6, [], 0);
  CheckNodesXY(n+'', 7, [2,12,   12,24,   24,37], 0);
  CheckNodesXY(n+'', 8, [], 0);
  CheckNodesXY(n+'', 9, [1,2], 1);



  n := 'Insert LineBreak after nodes';
  SynEdit.TextBetweenPoints[point(37, 7),point(37, 7)] := LineEnding;
DebugLn('###########');FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 6, [], 0);
  CheckNodesXY(n+'', 7, [2,12,   12,24,   24,37], 0);
  CheckNodesXY(n+'', 8, [], 0);
  CheckNodesXY(n+'', 9, [], 0);
  CheckNodesXY(n+'',10, [1,2], 1);

  n := 'Remove LineBreak after nodes';
  SynEdit.TextBetweenPoints[point(37, 7),point(1, 8)] := '';
FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 6, [], 0);
  CheckNodesXY(n+'', 7, [2,12,   12,24,   24,37], 0);
  CheckNodesXY(n+'', 8, [], 0);
  CheckNodesXY(n+'', 9, [1,2], 1);



  n := 'Insert LineBreak INSIDE nodes';
  SynEdit.TextBetweenPoints[point(13, 7),point(13, 7)] := LineEnding;
DebugLn('###########');FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 6, [], 0);
  CheckNodesXY(n+'', 7, [2,12,   12,12], 1);
  CheckNodesXY(n+'', 8, [12,25], 0);
  CheckNodesXY(n+'', 9, [], 0);
  CheckNodesXY(n+'',10, [1,2], 1);

  n := 'Remove LineBreak INSIDE  nodes';
  SynEdit.TextBetweenPoints[point(13, 7),point(1, 8)] := '';
FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 6, [], 0);
  CheckNodesXY(n+'', 7, [2,12,   12,24,   24,37], 0);
  CheckNodesXY(n+'', 8, [], 0);
  CheckNodesXY(n+'', 9, [1,2], 1);



  n := 'Insert LineBreak INSIDE nodes (last node 1 line)';
  SynEdit.TextBetweenPoints[point(23, 7),point(23, 7)] := LineEnding;
DebugLn('###########');FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 6, [], 0);
  CheckNodesXY(n+'', 7, [2,12,   12,2], 1);
  CheckNodesXY(n+'', 8, [2,15], 0);
  CheckNodesXY(n+'', 9, [], 0);
  CheckNodesXY(n+'',10, [1,2], 1);

  n := 'Remove LineBreak INSIDE  nodes (last node 1 line)';
  SynEdit.TextBetweenPoints[point(23, 7),point(1, 8)] := '';
FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 6, [], 0);
  CheckNodesXY(n+'', 7, [2,12,   12,24,   24,37], 0);
  CheckNodesXY(n+'', 8, [], 0);
  CheckNodesXY(n+'', 9, [1,2], 1);



  n := 'Insert LineBreak INSIDE nodes (last node multi line)';
  SynEdit.TextBetweenPoints[point(9, 10),point(9, 10)] := LineEnding;
DebugLn('###########');FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'',  9, [1,2], 1);
  CheckNodesXY(n+'', 10, [2,3], 3);
  CheckNodesXY(n+'', 14, [], 0);
  CheckNodesXY(n+'', 15, [1,6], 1);

  n := 'Remove LineBreak INSIDE  nodes (last node multi line)';
  SynEdit.TextBetweenPoints[point(9, 10),point(1, 11)] := '';
FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'',  9, [1,2], 1);
  CheckNodesXY(n+'', 10, [2,3], 2);
  CheckNodesXY(n+'', 13, [], 0);
  CheckNodesXY(n+'', 14, [1,6], 1);


  n := 'Insert LineBreak INSIDE nodes (last node multi line)';
  SynEdit.TextBetweenPoints[point(2, 14),point(2, 14)] := LineEnding;
DebugLn('###########');FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 14, [1,6], 2);

  n := 'Remove LineBreak INSIDE  nodes (last node multi line)';
  SynEdit.TextBetweenPoints[point(2, 14),point(1, 15)] := '';
FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 14, [1,6], 1);



DebugLn('###########');FTestTree.DebugPrint(true);DebugLn;
  n := 'Insert LineBreak INSIDE nodes (previous node multi line)';
  SynEdit.TextBetweenPoints[point(1, 10),point(1, 10)] := LineEnding;
DebugLn('###########');FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'',  9, [1,2], 2);
  CheckNodesXY(n+'', 10, [], 0);
  CheckNodesXY(n+'', 11, [2,3], 2);

  n := 'Remove LineBreak INSIDE  nodes (previous node multi line)';
  SynEdit.TextBetweenPoints[point(1, 10),point(1, 11)] := '';
FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'',  9, [1,2], 1);
  CheckNodesXY(n+'', 10, [2,3], 2);



  n := 'Insert LineBreak INSIDE nodes (last node multi line - no next)';
  SynEdit.TextBetweenPoints[point(1, 17),point(1, 17)] := LineEnding;
DebugLn('###########');FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 14, [1, 6], 1);
  CheckNodesXY(n+'', 15, [6,13], 3);
  CheckNodesXY(n+'', 16, [], 0);
  CheckNodesXY(n+'', 17, [], 0);
  CheckNodesXY(n+'', 18, [], 0);

  n := 'Remove LineBreak INSIDE  nodes (last node multi line - no next)';
  SynEdit.TextBetweenPoints[point(1, 17),point(1, 18)] := '';
FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 14, [1, 6], 1);
  CheckNodesXY(n+'', 15, [6,13], 2);
  CheckNodesXY(n+'', 16, [], 0);
  CheckNodesXY(n+'', 17, [], 0);
  CheckNodesXY(n+'', 18, [], 0);


  n := 'Insert LineBreak INSIDE nodes (mid last node multi line - no next)';
  SynEdit.TextBetweenPoints[point(7, 16),point(7, 16)] := LineEnding;
DebugLn('###########');FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 14, [1, 6], 1);
  CheckNodesXY(n+'', 15, [6,13], 3);
  CheckNodesXY(n+'', 16, [], 0);
  CheckNodesXY(n+'', 17, [], 0);
  CheckNodesXY(n+'', 18, [], 0);

  n := 'Remove LineBreak INSIDE  nodes (mid last node multi line - no next)';
  SynEdit.TextBetweenPoints[point(7, 16),point(1, 17)] := '';
FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 14, [1, 6], 1);
  CheckNodesXY(n+'', 15, [6,13], 2);
  CheckNodesXY(n+'', 16, [], 0);
  CheckNodesXY(n+'', 17, [], 0);
  CheckNodesXY(n+'', 18, [], 0);





  n := 'Remove LineBreak INSIDE  nodes EndOffs';
  CheckNodesXY(n+'', 8, [], 0);
  CheckNodesXY(n+'', 9, [1,2], 1);
  CheckNodesXY(n+'',10, [2,3], 2);
  CheckNodesXY(n+'',11, [], 0);
  CheckNodesXY(n+'',12, [], 0);

  SynEdit.TextBetweenPoints[point(10, 9),point(1, 10)] := '';
FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY(n+'', 8, [], 0);
  CheckNodesXY(n+'', 9, [1,11,  11,3], 2);
  CheckNodesXY(n+'',10, [], 0);
  CheckNodesXY(n+'',11, [], 0);
  CheckNodesXY(n+'',12, [], 0);

  n := 'Insert LineBreak INSIDE  nodes EndOffs';
  SynEdit.TextBetweenPoints[point(10, 9),point(10, 9)] := LineEnding;
  CheckNodesXY(n+'', 8, [], 0);
  CheckNodesXY(n+'', 9, [1,2], 1);
  CheckNodesXY(n+'',10, [2,3], 2);
  CheckNodesXY(n+'',11, [], 0);
  CheckNodesXY(n+'',12, [], 0);

  {%endregion  Line Breaks }

  ReCreateEditForTreeTest(TestTextIfDef);
  FTestTree.ValidateRange(1, 14, FOpenings);
FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY('', 1, [], 0);
  CheckNodesXY('', 2, [1, 11], 0);
  CheckNodesXY('', 3, [], 0);
  CheckNodesXY('', 4, [2, 12], 0);
  CheckNodesXY('', 5, [3, 14], 0);

  // no modification
  SynEdit.TextBetweenPoints[point(1,1),point(1,2)] := LineEnding;
FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY('', 1, [], 0);
  CheckNodesXY('', 2, [1, 11], 0);
  CheckNodesXY('', 3, [], 0);
  CheckNodesXY('', 4, [2, 12], 0);
  CheckNodesXY('', 5, [3, 14], 0);

FTestTree.DebugPrint(true);DebugLn;
  SynEdit.TextBetweenPoints[point(1,1),point(1,1)] := LineEnding;
FTestTree.DebugPrint(true);DebugLn;
  CheckNodesXY('', 1, [], 0);
  CheckNodesXY('', 2, [], 0);
  CheckNodesXY('', 3, [1, 11], 0);
  CheckNodesXY('', 4, [], 0);
  CheckNodesXY('', 5, [2, 12], 0);
  CheckNodesXY('', 6, [3, 14], 0);

  SynEdit.TextBetweenPoints[point(1, 3),point(1, 3)] := ' ';
  CheckNodesXY('', 1, [], 0);
  CheckNodesXY('', 2, [], 0);
  CheckNodesXY('', 3, [2, 12], 0);
  CheckNodesXY('', 4, [], 0);
  CheckNodesXY('', 5, [2, 12], 0);
  CheckNodesXY('', 6, [3, 14], 0);

  SynEdit.TextBetweenPoints[point(1, 3),point(2, 3)] := '';
  CheckNodesXY('', 1, [], 0);
  CheckNodesXY('', 2, [], 0);
  CheckNodesXY('', 3, [1, 11], 0);
  CheckNodesXY('', 4, [], 0);
  CheckNodesXY('', 5, [2, 12], 0);
  CheckNodesXY('', 6, [3, 14], 0);


  {%region peers}
  n := 'Peers, TestText1: Validate all';
  ReCreateEditForTreeTest(TestText1);
  FTestTree.ValidateRange(1, 36, FOpenings);
DebugLn('--------');FTestTree.DebugPrint(true);
  CheckNodes(n, 2, [ ExpN( 1,11, idnIfdef, EpElse(29, 1)) ]);
  CheckNodes(n, 5, [ ExpN( 3,13, idnIfdef, EpEnd(17, 3)) ]);
  CheckNodes(n, 7, [ ExpN( 5,15, idnIfdef, EpElse(7, 20)),
                     ExpN(20,27, idnElse,  EpIf(7, 5), EpEnd(7, 32)),
                     ExpN(32,40, idnEndIf, EpElse(7, 20)),
                     ExpN(43,53, idnIfdef, EpEnd(11, 5))  ]);
  CheckNodes(n, 8, [ ExpN( 7,17, idnIfdef, EpEnd(10, 7)),
                     ExpN(19,29, idnIfdef, EpElse(8, 34)),
                     ExpN(34,41, idnElse,  EpIf(8, 19), EpEnd(8, 46)),
                     ExpN(46,54, idnEndIf, EpElse(8, 34))  ]);
  CheckNodes(n,10, [ ExpN( 7,15, idnEndIf, EpIf(8, 7)) ]);
  CheckNodes(n,11, [ ExpN( 5,13, idnEndIf, EpIf(7, 43)) ]);
  CheckNodes(n,13, [ ExpN( 5,16, idnIfdef, EpEnd(14, 5)) ]);
  CheckNodes(n,14, [ ExpN( 5,13, idnEndIf, EpIf(13, 5)) ]);
  CheckNodes(n,15, [ ExpN( 5,15, idnIfdef, EpEnd(15, 16)),
                     ExpN(16,24, idnEndIf, EpIf(15, 5))  ]);
  CheckNodes(n,17, [ ExpN( 3,11, idnEndIf, EpIf(5, 3)) ]);
  CheckNodes(n,19, [ ExpN( 3,14, idnIfdef, EpElse(21, 3)) ]);
  CheckNodes(n,21, [ ExpN( 3,10, idnElse,  EpIf(19, 3), EpEnd(27, 3)) ]);
  CheckNodes(n,23, [ ExpN( 5,15, idnIfdef, EpEnd(25, 5)) ]);
  CheckNodes(n,25, [ ExpN( 5,13, idnEndIf, EpIf(23, 5)) ]);
  CheckNodes(n,26, [ ExpN( 5,15, idnIfdef, EpEnd(26, 16)),
                     ExpN(16,24, idnEndIf, EpIf(26, 5))  ]);
  CheckNodes(n,27, [ ExpN( 3,11, idnEndIf, EpElse(21, 3)) ]);
  CheckNodes(n,29, [ ExpN( 1, 8, idnElse,  EpIf(2, 1), EpEnd(36,1)) ]);
  CheckNodes(n,31, [ ExpN( 3,13, idnIfdef, EpElse(32, 3)) ]);
  CheckNodes(n,32, [ ExpN( 3,10, idnElse,  EpIf(31, 3), EpEnd(34,3)) ]);
  CheckNodes(n,34, [ ExpN( 3,11, idnEndIf, EpElse(32, 3)) ]);
  CheckNodes(n,36, [ ExpN( 1, 9, idnEndIf, EpElse(29, 1)) ]);



  n := 'Peers, TestText1: Validate 8 - 36';
  ReCreateEditForTreeTest(TestText1);
  FTestTree.ValidateRange(8, 36, FOpenings);
DebugLn('---');FTestTree.DebugPrint(true);
  CheckNodes(n, 2, [ ExpN( 1,11, idnIfdef, EpElse(29, 1)) ]);
  CheckNodes(n, 5, [ ExpN( 3,13, idnIfdef, EpEnd(17, 3)) ]);
  CheckNodes(n, 7, [ ExpN( 5,15, idnIfdef ),
                     ExpN(20,27, idnElse  ),
                     ExpN(32,40, idnEndIf ),
                     ExpN(43,53, idnIfdef, EpEnd(11, 5))  ]);
  CheckNodes(n, 8, [ ExpN( 7,17, idnIfdef, EpEnd(10, 7)),
                     ExpN(19,29, idnIfdef, EpElse(8, 34)),
                     ExpN(34,41, idnElse,  EpIf(8, 19), EpEnd(8, 46)),
                     ExpN(46,54, idnEndIf, EpElse(8, 34))  ]);
  CheckNodes(n,10, [ ExpN( 7,15, idnEndIf, EpIf(8, 7)) ]);
  CheckNodes(n,11, [ ExpN( 5,13, idnEndIf, EpIf(7, 43)) ]);
  CheckNodes(n,13, [ ExpN( 5,16, idnIfdef, EpEnd(14, 5)) ]);
  CheckNodes(n,14, [ ExpN( 5,13, idnEndIf, EpIf(13, 5)) ]);
  CheckNodes(n,15, [ ExpN( 5,15, idnIfdef, EpEnd(15, 16)),
                     ExpN(16,24, idnEndIf, EpIf(15, 5))  ]);
  CheckNodes(n,17, [ ExpN( 3,11, idnEndIf, EpIf(5, 3)) ]);
  CheckNodes(n,19, [ ExpN( 3,14, idnIfdef, EpElse(21, 3)) ]);
  CheckNodes(n,21, [ ExpN( 3,10, idnElse,  EpIf(19, 3), EpEnd(27, 3)) ]);
  CheckNodes(n,23, [ ExpN( 5,15, idnIfdef, EpEnd(25, 5)) ]);
  CheckNodes(n,25, [ ExpN( 5,13, idnEndIf, EpIf(23, 5)) ]);
  CheckNodes(n,26, [ ExpN( 5,15, idnIfdef, EpEnd(26, 16)),
                     ExpN(16,24, idnEndIf, EpIf(26, 5))  ]);
  CheckNodes(n,27, [ ExpN( 3,11, idnEndIf, EpElse(21, 3)) ]);
  CheckNodes(n,29, [ ExpN( 1, 8, idnElse,  EpIf(2, 1), EpEnd(36,1)) ]);
  CheckNodes(n,31, [ ExpN( 3,13, idnIfdef, EpElse(32, 3)) ]);
  CheckNodes(n,32, [ ExpN( 3,10, idnElse,  EpIf(31, 3), EpEnd(34,3)) ]);
  CheckNodes(n,34, [ ExpN( 3,11, idnEndIf, EpElse(32, 3)) ]);
  CheckNodes(n,36, [ ExpN( 1, 9, idnEndIf, EpElse(29, 1)) ]);




  n := 'Peers, TestText1: Validate 33 - 36';
  ReCreateEditForTreeTest(TestText1);
  FTestTree.ValidateRange(33, 36, FOpenings);
DebugLn('---######');FTestTree.DebugPrint(true);
  AssertTrue('Scan start node Node at empty line', FTestTree.FindNodeAtPosition(33, afmNil).HasNode);
  CheckNodes(n,33, [ ]);
  CheckNodes(n, 2, [ ExpN( 1,11, idnIfdef, EpElse(29, 1)) ]);
  CheckNodes(n,29, [ ExpN( 1, 8, idnElse,  EpIf(2, 1), EpEnd(36,1)) ]);
  CheckNodes(n,31, [ ExpN( 3,13, idnIfdef, EpElse(32, 3)) ]);
  CheckNodes(n,32, [ ExpN( 3,10, idnElse,  EpIf(31, 3), EpEnd(34,3)) ]);
  CheckNodes(n,34, [ ExpN( 3,11, idnEndIf, EpElse(32, 3)) ]);
  CheckNodes(n,36, [ ExpN( 1, 9, idnEndIf, EpElse(29, 1)) ]);
  //
  n := 'Peers, TestText1: Validate all  AFTER 33-36';
  FTestTree.ValidateRange(1, 36, FOpenings);
DebugLn('--------#');FTestTree.DebugPrint(true);
  AssertFalse('Scan start node Node at empty line gone', FTestTree.FindNodeAtPosition(33, afmNil).HasNode);
  CheckNodes(n, 2, [ ExpN( 1,11, idnIfdef, EpElse(29, 1)) ]);
  CheckNodes(n, 5, [ ExpN( 3,13, idnIfdef, EpEnd(17, 3)) ]);
  CheckNodes(n, 7, [ ExpN( 5,15, idnIfdef, EpElse(7, 20)),
                     ExpN(20,27, idnElse,  EpIf(7, 5), EpEnd(7, 32)),
                     ExpN(32,40, idnEndIf, EpElse(7, 20)),
                     ExpN(43,53, idnIfdef, EpEnd(11, 5))  ]);
  CheckNodes(n, 8, [ ExpN( 7,17, idnIfdef, EpEnd(10, 7)),
                     ExpN(19,29, idnIfdef, EpElse(8, 34)),
                     ExpN(34,41, idnElse,  EpIf(8, 19), EpEnd(8, 46)),
                     ExpN(46,54, idnEndIf, EpElse(8, 34))  ]);
  CheckNodes(n,10, [ ExpN( 7,15, idnEndIf, EpIf(8, 7)) ]);
  CheckNodes(n,11, [ ExpN( 5,13, idnEndIf, EpIf(7, 43)) ]);
  CheckNodes(n,13, [ ExpN( 5,16, idnIfdef, EpEnd(14, 5)) ]);
  CheckNodes(n,14, [ ExpN( 5,13, idnEndIf, EpIf(13, 5)) ]);
  CheckNodes(n,15, [ ExpN( 5,15, idnIfdef, EpEnd(15, 16)),
                     ExpN(16,24, idnEndIf, EpIf(15, 5))  ]);
  CheckNodes(n,17, [ ExpN( 3,11, idnEndIf, EpIf(5, 3)) ]);
  CheckNodes(n,19, [ ExpN( 3,14, idnIfdef, EpElse(21, 3)) ]);
  CheckNodes(n,21, [ ExpN( 3,10, idnElse,  EpIf(19, 3), EpEnd(27, 3)) ]);
  CheckNodes(n,23, [ ExpN( 5,15, idnIfdef, EpEnd(25, 5)) ]);
  CheckNodes(n,25, [ ExpN( 5,13, idnEndIf, EpIf(23, 5)) ]);
  CheckNodes(n,26, [ ExpN( 5,15, idnIfdef, EpEnd(26, 16)),
                     ExpN(16,24, idnEndIf, EpIf(26, 5))  ]);
  CheckNodes(n,27, [ ExpN( 3,11, idnEndIf, EpElse(21, 3)) ]);
  CheckNodes(n,29, [ ExpN( 1, 8, idnElse,  EpIf(2, 1), EpEnd(36,1)) ]);
  CheckNodes(n,31, [ ExpN( 3,13, idnIfdef, EpElse(32, 3)) ]);
  CheckNodes(n,32, [ ExpN( 3,10, idnElse,  EpIf(31, 3), EpEnd(34,3)) ]);
  CheckNodes(n,34, [ ExpN( 3,11, idnEndIf, EpElse(32, 3)) ]);
  CheckNodes(n,36, [ ExpN( 1, 9, idnEndIf, EpElse(29, 1)) ]);




  n := 'Peers, TestText1: 32-36';
  ReCreateEditForTreeTest(TestText1);
  FTestTree.ValidateRange(32, 36, FOpenings);
DebugLn('--------');FTestTree.DebugPrint(true);
  CheckNodes(n, 2, [ ExpN( 1,11, idnIfdef, EpElse(29, 1)) ]);
  CheckNodes(n,29, [ ExpN( 1, 8, idnElse,  EpIf(2, 1), EpEnd(36,1)) ]);
  CheckNodes(n,31, [ ExpN( 3,13, idnIfdef, EpElse(32, 3)) ]);
  CheckNodes(n,32, [ ExpN( 3,10, idnElse,  EpIf(31, 3), EpEnd(34,3)) ]);
  CheckNodes(n,34, [ ExpN( 3,11, idnEndIf, EpElse(32, 3)) ]);
  CheckNodes(n,36, [ ExpN( 1, 9, idnEndIf, EpElse(29, 1)) ]);
  //
  n := 'Peers, TestText1: 16-20 AFTER 32-36';
  FTestTree.ValidateRange(16, 20, FOpenings);
DebugLn('--------');FTestTree.DebugPrint(true);
  CheckNodes(n, 2, [ ExpN( 1,11, idnIfdef, EpElse(29, 1)) ]);
  CheckNodes(n, 5, [ ExpN( 3,13, idnIfdef, EpEnd(17, 3)) ]);
  CheckNodes(n,17, [ ExpN( 3,11, idnEndIf, EpIf(5, 3)) ]);
  CheckNodes(n,19, [ ExpN( 3,14, idnIfdef) ]);
  CheckNodes(n,29, [ ExpN( 1, 8, idnElse,  EpIf(2, 1), EpEnd(36,1)) ]);
  CheckNodes(n,31, [ ExpN( 3,13, idnIfdef, EpElse(32, 3)) ]);
  CheckNodes(n,32, [ ExpN( 3,10, idnElse,  EpIf(31, 3), EpEnd(34,3)) ]);
  CheckNodes(n,34, [ ExpN( 3,11, idnEndIf, EpElse(32, 3)) ]);
  CheckNodes(n,36, [ ExpN( 1, 9, idnEndIf, EpElse(29, 1)) ]);
  //
  n := 'Peers, TestText1: all AFTER 16-20 AFTER 32-36';
  FTestTree.ValidateRange(1, 36, FOpenings);
DebugLn('--------');FTestTree.DebugPrint(true);
  CheckNodes(n, 2, [ ExpN( 1,11, idnIfdef, EpElse(29, 1)) ]);
  CheckNodes(n, 5, [ ExpN( 3,13, idnIfdef, EpEnd(17, 3)) ]);
  CheckNodes(n, 7, [ ExpN( 5,15, idnIfdef, EpElse(7, 20)),
                     ExpN(20,27, idnElse,  EpIf(7, 5), EpEnd(7, 32)),
                     ExpN(32,40, idnEndIf, EpElse(7, 20)),
                     ExpN(43,53, idnIfdef, EpEnd(11, 5))  ]);
  CheckNodes(n, 8, [ ExpN( 7,17, idnIfdef, EpEnd(10, 7)),
                     ExpN(19,29, idnIfdef, EpElse(8, 34)),
                     ExpN(34,41, idnElse,  EpIf(8, 19), EpEnd(8, 46)),
                     ExpN(46,54, idnEndIf, EpElse(8, 34))  ]);
  CheckNodes(n,10, [ ExpN( 7,15, idnEndIf, EpIf(8, 7)) ]);
  CheckNodes(n,11, [ ExpN( 5,13, idnEndIf, EpIf(7, 43)) ]);
  CheckNodes(n,13, [ ExpN( 5,16, idnIfdef, EpEnd(14, 5)) ]);
  CheckNodes(n,14, [ ExpN( 5,13, idnEndIf, EpIf(13, 5)) ]);
  CheckNodes(n,15, [ ExpN( 5,15, idnIfdef, EpEnd(15, 16)),
                     ExpN(16,24, idnEndIf, EpIf(15, 5))  ]);
  CheckNodes(n,17, [ ExpN( 3,11, idnEndIf, EpIf(5, 3)) ]);
  CheckNodes(n,19, [ ExpN( 3,14, idnIfdef, EpElse(21, 3)) ]);
  CheckNodes(n,21, [ ExpN( 3,10, idnElse,  EpIf(19, 3), EpEnd(27, 3)) ]);
  CheckNodes(n,23, [ ExpN( 5,15, idnIfdef, EpEnd(25, 5)) ]);
  CheckNodes(n,25, [ ExpN( 5,13, idnEndIf, EpIf(23, 5)) ]);
  CheckNodes(n,26, [ ExpN( 5,15, idnIfdef, EpEnd(26, 16)),
                     ExpN(16,24, idnEndIf, EpIf(26, 5))  ]);
  CheckNodes(n,27, [ ExpN( 3,11, idnEndIf, EpElse(21, 3)) ]);
  CheckNodes(n,29, [ ExpN( 1, 8, idnElse,  EpIf(2, 1), EpEnd(36,1)) ]);
  CheckNodes(n,31, [ ExpN( 3,13, idnIfdef, EpElse(32, 3)) ]);
  CheckNodes(n,32, [ ExpN( 3,10, idnElse,  EpIf(31, 3), EpEnd(34,3)) ]);
  CheckNodes(n,34, [ ExpN( 3,11, idnEndIf, EpElse(32, 3)) ]);
  CheckNodes(n,36, [ ExpN( 1, 9, idnEndIf, EpElse(29, 1)) ]);



  n := 'Peers, TestText2: Bad nodes';
  ReCreateEditForTreeTest(TestText2);
  FTestTree.ValidateRange(1, 18, FOpenings);
DebugLn('--------');FTestTree.DebugPrint(true);
  // ONe and only one of the 2 ends should have a peer
  nd := FTestTree.FindNodeAtPosition(33, afmNil);
  if nd.HasNode and (nd.EntryCount > 0) and (nd.Entry[0].EndIfPeer.Line.GetPosition = 4) then begin
    CheckNodes(n, 2, [ ExpN( 1,11, idnIfdef, EpEnd(4, 1)) ]);
    CheckNodes(n, 4, [ ExpN( 1, 9, idnEndIf, EpIf(2, 1)) ]);
    CheckNodes(n, 6, [ ExpN( 1, 9, idnEndIf, EpIf(-1, -1)) ]); // must not have a peer
  end else begin
    CheckNodes(n, 2, [ ExpN( 1,11, idnIfdef, EpEnd(6, 1)) ]);
    CheckNodes(n, 4, [ ExpN( 1, 9, idnEndIf, EpIf(-1, -1)) ]); // must not have a peer
    CheckNodes(n, 6, [ ExpN( 1, 9, idnEndIf, EpIf(2, 1)) ]);
  end;
  // One and only one else may be connected to if and one (but maybe the other) to endif
  CheckNodes(n,10, [ ExpN( 1,11, idnIfdef) ]); // EpElse(12, 1) // or 14
  CheckNodes(n,12, [ ExpN( 1, 8, idnElse) ]);
  CheckNodes(n,14, [ ExpN( 1, 8, idnElse) ]);
  CheckNodes(n,16, [ ExpN( 1, 9, idnEndIf, EpElse(14, 1)) ]);




  n := 'scan plain text, after closed node: step 1: node';
  ReCreateEditForTreeTest(TestText3);
  FTestTree.ValidateRange(3, 5, FOpenings);
DebugLn('--------');FTestTree.DebugPrint(true);
  CheckNodes(n, 2, [ ExpN( 1,11, idnIfdef, EpEnd(4, 1)) ]);
  CheckNodes(n, 4, [ ExpN( 1, 9, idnEndIf, EpIf(2, 1)) ]);

  n := 'scan plain text, after closed node: step 2: empty';
  FTestTree.ValidateRange(8, 9, FOpenings);
DebugLn('--------');FTestTree.DebugPrint(true);
  CheckNodes(n, 2, [ ExpN( 1,11, idnIfdef, EpEnd(4, 1)) ]);
  CheckNodes(n, 4, [ ExpN( 1, 9, idnEndIf, EpIf(2, 1)) ]);




  n := 'scan plain text, after closed node (overlap): step 1: node';
  ReCreateEditForTreeTest(TestText3);
  FTestTree.ValidateRange(3, 8, FOpenings); // ensure node is valid to begin-of-plain-line-scan
DebugLn('--------');FTestTree.DebugPrint(true);
  CheckNodes(n, 2, [ ExpN( 1,11, idnIfdef, EpEnd(4, 1)) ]);
  CheckNodes(n, 4, [ ExpN( 1, 9, idnEndIf, EpIf(2, 1)) ]);

  n := 'scan plain text, after closed node (overlap): step 2: empty';
  FTestTree.ValidateRange(7, 9, FOpenings);
DebugLn('--------');FTestTree.DebugPrint(true);
  CheckNodes(n, 2, [ ExpN( 1,11, idnIfdef, EpEnd(4, 1)) ]);
  CheckNodes(n, 4, [ ExpN( 1, 9, idnEndIf, EpIf(2, 1)) ]);




  n := 'scan plain text, after closed node (not scanned end): step 1: node';
  ReCreateEditForTreeTest(TestText3);
  FTestTree.ValidateRange(3, 3, FOpenings);
DebugLn('--------');FTestTree.DebugPrint(true);
  CheckNodes(n, 2, [ ExpN( 1,11, idnIfdef) ]);

  n := 'scan plain text, after closed node (not scanned end): step 2: empty';
  FTestTree.ValidateRange(8, 9, FOpenings);
DebugLn('--------');FTestTree.DebugPrint(true);


  {%endregion peers}

  {%region peers + editing}

  n := 'Peers, TestText1: Before Edit';
  ReCreateEditForTreeTest(TestText1);
  FTestTree.ValidateRange(1, 36, FOpenings);
DebugLn('--------');FTestTree.DebugPrint(true);
  CheckNodes(n, 2, [ ExpN( 1,11, idnIfdef, EpElse(29, 1)) ]);
  CheckNodes(n, 5, [ ExpN( 3,13, idnIfdef, EpEnd(17, 3)) ]);
  CheckNodes(n, 7, [ ExpN( 5,15, idnIfdef, EpElse(7, 20)),
                     ExpN(20,27, idnElse,  EpIf(7, 5), EpEnd(7, 32)),
                     ExpN(32,40, idnEndIf, EpElse(7, 20)),
                     ExpN(43,53, idnIfdef, EpEnd(11, 5))  ]);
  CheckNodes(n, 8, [ ExpN( 7,17, idnIfdef, EpEnd(10, 7)),
                     ExpN(19,29, idnIfdef, EpElse(8, 34)),
                     ExpN(34,41, idnElse,  EpIf(8, 19), EpEnd(8, 46)),
                     ExpN(46,54, idnEndIf, EpElse(8, 34))  ]);
  CheckNodes(n,10, [ ExpN( 7,15, idnEndIf, EpIf(8, 7)) ]);
  CheckNodes(n,11, [ ExpN( 5,13, idnEndIf, EpIf(7, 43)) ]);
  CheckNodes(n,13, [ ExpN( 5,16, idnIfdef, EpEnd(14, 5)) ]);
  CheckNodes(n,14, [ ExpN( 5,13, idnEndIf, EpIf(13, 5)) ]);
  CheckNodes(n,15, [ ExpN( 5,15, idnIfdef, EpEnd(15, 16)),
                     ExpN(16,24, idnEndIf, EpIf(15, 5))  ]);
  CheckNodes(n,17, [ ExpN( 3,11, idnEndIf, EpIf(5, 3)) ]);
  CheckNodes(n,19, [ ExpN( 3,14, idnIfdef, EpElse(21, 3)) ]);
  CheckNodes(n,21, [ ExpN( 3,10, idnElse,  EpIf(19, 3), EpEnd(27, 3)) ]);
  CheckNodes(n,23, [ ExpN( 5,15, idnIfdef, EpEnd(25, 5)) ]);
  CheckNodes(n,25, [ ExpN( 5,13, idnEndIf, EpIf(23, 5)) ]);
  CheckNodes(n,26, [ ExpN( 5,15, idnIfdef, EpEnd(26, 16)),
                     ExpN(16,24, idnEndIf, EpIf(26, 5))  ]);
  CheckNodes(n,27, [ ExpN( 3,11, idnEndIf, EpElse(21, 3)) ]);
  CheckNodes(n,29, [ ExpN( 1, 8, idnElse,  EpIf(2, 1), EpEnd(36,1)) ]);
  CheckNodes(n,31, [ ExpN( 3,13, idnIfdef, EpElse(32, 3)) ]);
  CheckNodes(n,32, [ ExpN( 3,10, idnElse,  EpIf(31, 3), EpEnd(34,3)) ]);
  CheckNodes(n,34, [ ExpN( 3,11, idnEndIf, EpElse(32, 3)) ]);
  CheckNodes(n,36, [ ExpN( 1, 9, idnEndIf, EpElse(29, 1)) ]);

  SynEdit.TextBetweenPoints[point(1, 9),point(1, 9)] := LineEnding;

  n := 'Peers, TestText1: Line inserted at 9';
  FTestTree.ValidateRange(1, 37, FOpenings);
DebugLn('--------');FTestTree.DebugPrint(true);
  CheckNodes(n, 2, [ ExpN( 1,11, idnIfdef, EpElse(30, 1)) ]);
  CheckNodes(n, 5, [ ExpN( 3,13, idnIfdef, EpEnd(18, 3)) ]);
  CheckNodes(n, 7, [ ExpN( 5,15, idnIfdef, EpElse(7, 20)),
                     ExpN(20,27, idnElse,  EpIf(7, 5), EpEnd(7, 32)),
                     ExpN(32,40, idnEndIf, EpElse(7, 20)),
                     ExpN(43,53, idnIfdef, EpEnd(12, 5))  ]);
  CheckNodes(n, 8, [ ExpN( 7,17, idnIfdef, EpEnd(11, 7)),
                     ExpN(19,29, idnIfdef, EpElse(8, 34)),
                     ExpN(34,41, idnElse,  EpIf(8, 19), EpEnd(8, 46)),
                     ExpN(46,54, idnEndIf, EpElse(8, 34))  ]);
  CheckNodes(n,11, [ ExpN( 7,15, idnEndIf, EpIf(8, 7)) ]);
  CheckNodes(n,12, [ ExpN( 5,13, idnEndIf, EpIf(7, 43)) ]);
  CheckNodes(n,14, [ ExpN( 5,16, idnIfdef, EpEnd(15, 5)) ]);
  CheckNodes(n,15, [ ExpN( 5,13, idnEndIf, EpIf(14, 5)) ]);
  CheckNodes(n,16, [ ExpN( 5,15, idnIfdef, EpEnd(16, 16)),
                     ExpN(16,24, idnEndIf, EpIf(16, 5))  ]);
  CheckNodes(n,18, [ ExpN( 3,11, idnEndIf, EpIf(5, 3)) ]);
  CheckNodes(n,20, [ ExpN( 3,14, idnIfdef, EpElse(22, 3)) ]);
  CheckNodes(n,22, [ ExpN( 3,10, idnElse,  EpIf(20, 3), EpEnd(28, 3)) ]);
  CheckNodes(n,24, [ ExpN( 5,15, idnIfdef, EpEnd(26, 5)) ]);
  CheckNodes(n,26, [ ExpN( 5,13, idnEndIf, EpIf(24, 5)) ]);
  CheckNodes(n,27, [ ExpN( 5,15, idnIfdef, EpEnd(27, 16)),
                     ExpN(16,24, idnEndIf, EpIf(27, 5))  ]);
  CheckNodes(n,28, [ ExpN( 3,11, idnEndIf, EpElse(22, 3)) ]);
  CheckNodes(n,30, [ ExpN( 1, 8, idnElse,  EpIf(2, 1), EpEnd(37,1)) ]);
  CheckNodes(n,32, [ ExpN( 3,13, idnIfdef, EpElse(33, 3)) ]);
  CheckNodes(n,33, [ ExpN( 3,10, idnElse,  EpIf(32, 3), EpEnd(35,3)) ]);
  CheckNodes(n,35, [ ExpN( 3,11, idnEndIf, EpElse(33, 3)) ]);
  CheckNodes(n,37, [ ExpN( 1, 9, idnEndIf, EpElse(30, 1)) ]);

  SynEdit.TextBetweenPoints[point(1, 9),point(1, 9)] := '{$EndIf}';
DebugLn('--------');FTestTree.DebugPrint(true);

  n := 'Peers, TestText1: ENDIF inserted at 9';
  FTestTree.ValidateRange(1, 37, FOpenings);
DebugLn('--------');FTestTree.DebugPrint(true);
  CheckNodes(n, 2, [ ExpN( 1,11, idnIfdef, EpEnd(18, 3)) ]);
  CheckNodes(n, 5, [ ExpN( 3,13, idnIfdef, EpEnd(12, 5)) ]);
  CheckNodes(n, 7, [ ExpN( 5,15, idnIfdef, EpElse(7, 20)),
                     ExpN(20,27, idnElse,  EpIf(7, 5), EpEnd(7, 32)),
                     ExpN(32,40, idnEndIf, EpElse(7, 20)),
                     ExpN(43,53, idnIfdef, EpEnd(11, 7))  ]);
  CheckNodes(n, 8, [ ExpN( 7,17, idnIfdef, EpEnd(9, 1)),
                     ExpN(19,29, idnIfdef, EpElse(8, 34)),
                     ExpN(34,41, idnElse,  EpIf(8, 19), EpEnd(8, 46)),
                     ExpN(46,54, idnEndIf, EpElse(8, 34))  ]);
  CheckNodes(n,11, [ ExpN( 7,15, idnEndIf, EpIf(7, 43)) ]);
  CheckNodes(n,12, [ ExpN( 5,13, idnEndIf, EpIf(5, 3)) ]);
  CheckNodes(n,14, [ ExpN( 5,16, idnIfdef, EpEnd(15, 5)) ]);
  CheckNodes(n,15, [ ExpN( 5,13, idnEndIf, EpIf(14, 5)) ]);
  CheckNodes(n,16, [ ExpN( 5,15, idnIfdef, EpEnd(16, 16)),
                     ExpN(16,24, idnEndIf, EpIf(16, 5))  ]);
  CheckNodes(n,18, [ ExpN( 3,11, idnEndIf, EpIf(2, 1)) ]);
  CheckNodes(n,20, [ ExpN( 3,14, idnIfdef, EpElse(22, 3)) ]);
  CheckNodes(n,22, [ ExpN( 3,10, idnElse,  EpIf(20, 3), EpEnd(28, 3)) ]);
  CheckNodes(n,24, [ ExpN( 5,15, idnIfdef, EpEnd(26, 5)) ]);
  CheckNodes(n,26, [ ExpN( 5,13, idnEndIf, EpIf(24, 5)) ]);
  CheckNodes(n,27, [ ExpN( 5,15, idnIfdef, EpEnd(27, 16)),
                     ExpN(16,24, idnEndIf, EpIf(27, 5))  ]);
  CheckNodes(n,28, [ ExpN( 3,11, idnEndIf, EpElse(22, 3)) ]);
  CheckNodes(n,30, [ ExpN( 1, 8, idnElse,  EpIf(-1, 1), EpEnd(37,1)) ]);
  CheckNodes(n,32, [ ExpN( 3,13, idnIfdef, EpElse(33, 3)) ]);
  CheckNodes(n,33, [ ExpN( 3,10, idnElse,  EpIf(32, 3), EpEnd(35,3)) ]);
  CheckNodes(n,35, [ ExpN( 3,11, idnEndIf, EpElse(33, 3)) ]);
  CheckNodes(n,37, [ ExpN( 1, 9, idnEndIf, EpElse(30, 1)) ]);






  {%endregion peers}

  FTestTree.DiscardOpeningList(FOpenings);
  FOpenings := nil;;
  FTestTree.Free;

end;

initialization
  RegisterTest(TTestMarkupIfDef);

end.

