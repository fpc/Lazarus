unit TestSynMultiCaret;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, SynPluginMultiCaret,
  SynEditTypes, LazLoggerBase;

type

  { TTestSynMultCaret }

  TTestSynMultCaret = class(TTestCase)
  published
    procedure TestAdjustAfterChange;
  end;

implementation

procedure TTestSynMultCaret.TestAdjustAfterChange;
  procedure CreateList(var AList: TSynPluginMultiCaretList; ACount: Integer; AMergeLock: Boolean);
  var
    i: Integer;
  begin
    FreeAndNil(AList);
    AList := TSynPluginMultiCaretList.Create;
    if AMergeLock then
      AList.IncMergeLock;

    for i := 1 to ACount do
      AList.AddCaret(i * 10, 1, 0);
  end;

  procedure AssertSorted(AName: String; AList: TSynPluginMultiCaretList; ACount: Integer; ADupValue: Integer = -1);
  var
    i: Integer;
  begin
      //for i := 0 to AList.Count-1 do DbgOut([AList.CaretFull[i].x, ' ']); debugln('');
      AssertEquals(AName + ' Count ', ACount, AList.Count);
      for i := 1 to ACount-1 do
        AssertTrue(AName + ' Ordered '+IntToStr(i),
          (AList.CaretFull[i].x > AList.CaretFull[i-1].x) or
          ( (ADupValue <> -1) and
            (AList.CaretFull[i].x = AList.CaretFull[i-1].x) and (AList.CaretFull[i].x = ADupValue) )
        );
  end;

var
  TestList: TSynPluginMultiCaretList;
  p: TLogCaretPoint;
  Len, MoveIdx, MoveTo, i, L2: Integer;
  s: String;
begin
  for Len := 1 to 10 do
  for MoveIdx := 0 to Len - 1 do
  for MoveTo := 0 to Len + 1 do
  try
    TestList := nil;
    s := Format('L: %d, From: %d, To: %d ', [Len, MoveIdx, MoveTo]);
    //DebugLn('TestAdjustAfterChange '+s);

    L2 := Len;
    If not( (MoveTo = MoveIdx+1) or (MoveTo = 0) or (MoveTo = Len + 1) ) then
      dec(L2);

    // Modify, no dups
    CreateList(TestList, Len, True);

    p := TestList.CaretFull[MoveIdx];
    p.X := MoveTo * 10 + 1;
    TestList.CaretFull[MoveIdx] := p;

    AssertSorted('Modify, no dup '+s, TestList, Len);



    // Modify, with dups
    CreateList(TestList, Len, True);

    p := TestList.CaretFull[MoveIdx];
    p.X := MoveTo * 10;
    TestList.CaretFull[MoveIdx] := p;

    AssertSorted('Modify, with dup '+s, TestList, Len, MoveTo * 10);



    // Modify, with dups remove
    CreateList(TestList, Len, False);

    p := TestList.CaretFull[MoveIdx];
    p.X := MoveTo * 10;
    TestList.CaretFull[MoveIdx] := p;

    AssertSorted('Modify, with dup '+s, TestList, L2);



    // Iterator Up, no dups
    CreateList(TestList, Len, True);

    TestList.StartIteratorAtFirst;
    for i := 1 to Len do begin
      AssertTrue('Iterator Up Continues '+s, TestList.IterateNextUp);
      p := TestList.CurrentCaretFull;
      AssertEquals('Iterator Ordered '+s+IntToStr(i), i * 10, TestList.CurrentCaretFull.x);

      if i-1 = MoveIdx then begin
        p.X := MoveTo * 10 + 1;
        TestList.CurrentCaretFull := p;
      end;
    end;
    AssertTrue('Iterator Up Finished '+s, not TestList.IterateNextUp);
    AssertSorted('Iterate up, no dup '+s, TestList, Len);

    TestList.StartIteratorAtFirst; // Iterate again / must get all entries
    for i := 1 to Len do
      AssertTrue('Iterator 2 Up Continues '+s, TestList.IterateNextUp);
    AssertTrue('Iterator 2 Up Finished '+s, not TestList.IterateNextUp);



    // Iterator Up, with dups
    CreateList(TestList, Len, True);

    TestList.StartIteratorAtFirst;
    for i := 1 to Len do begin
      AssertTrue('Iterator Up (dups) Continues '+s, TestList.IterateNextUp);
      p := TestList.CurrentCaretFull;
      AssertEquals('Iterator Ordered '+s+IntToStr(i), i * 10, TestList.CurrentCaretFull.x);

      if i-1 = MoveIdx then begin
        p.X := MoveTo * 10;
        TestList.CurrentCaretFull := p;
      end;

    end;
    AssertTrue('Iterator Up (dups) Finished '+s, not TestList.IterateNextUp);
    AssertSorted('Iterate up (dups) '+s, TestList, Len, MoveTo * 10);

    TestList.StartIteratorAtFirst; // Iterate again / must get all entries
    for i := 1 to Len do
      AssertTrue('Iterator 2 Up  (dups) Continues '+s, TestList.IterateNextUp);
    AssertTrue('Iterator 2 Up (dups) Finished '+s, not TestList.IterateNextUp);


(* Not implemented
    // Iterator Up, with dups remove
    CreateList(TestList, Len, False);

    TestList.StartIteratorAtFirst;
    for i := 1 to Len do begin
      AssertTrue('Iterator Up (dups) Continues '+s, TestList.IterateNextUp);
      p := TestList.CurrentCaretFull;
      AssertEquals('Iterator Ordered '+s+IntToStr(i), i * 10, TestList.CurrentCaretFull.x);

      if i-1 = MoveIdx then begin
        p.X := MoveTo * 10;
        TestList.CurrentCaretFull := p;
      end;

    end;
    AssertTrue('Iterator Up (dups) Finished '+s, not TestList.IterateNextUp);
    AssertSorted('Iterate up (dups) '+s, TestList, L2);

    TestList.StartIteratorAtFirst; // Iterate again / must get all entries
    for i := 1 to L2 do
      AssertTrue('Iterator 2 Up  (dups) Continues '+s, TestList.IterateNextUp);
    AssertTrue('Iterator 2 Up (dups) Finished '+s, not TestList.IterateNextUp);
*)


    // Iterator Down, no dups
    CreateList(TestList, Len, True);

    TestList.StartIteratorAtLast;
    for i := Len downto 1 do begin
      AssertTrue('Iterator Down Continues '+s, TestList.IterateNextDown);
      p := TestList.CurrentCaretFull;
      AssertEquals('Iterator Ordered '+s+IntToStr(i), i * 10, TestList.CurrentCaretFull.x);

      if i-1 = MoveIdx then begin
        p.X := MoveTo * 10 + 1;
        TestList.CurrentCaretFull := p;
      end;
    end;
    AssertTrue('Iterator Down Finished '+s, not TestList.IterateNextDown);
    AssertSorted('Iterate Down, no dup '+s, TestList, Len);

    TestList.StartIteratorAtLast; // Iterate again / must get all entries
    for i := 1 to Len do
      AssertTrue('Iterator 2 Down   Continues '+s, TestList.IterateNextDown);
    AssertTrue('Iterator 2 Down  Finished '+s, not TestList.IterateNextDown);



    // Iterator Down, with dups
    CreateList(TestList, Len, True);

    TestList.StartIteratorAtLast;
    for i := Len downto 1 do begin
      AssertTrue('Iterator Down (dups) Continues '+s, TestList.IterateNextDown);
      p := TestList.CurrentCaretFull;
      AssertEquals('Iterator Ordered '+s+IntToStr(i), i * 10, TestList.CurrentCaretFull.x);

      if i-1 = MoveIdx then begin
        p.X := MoveTo * 10;
        TestList.CurrentCaretFull := p;
      end;
    end;
    AssertTrue('Iterator Down (dups) Finished '+s, not TestList.IterateNextDown);
    AssertSorted('Iterate Down (dups) '+s, TestList, Len, MoveTo * 10);

    TestList.StartIteratorAtLast; // Iterate again / must get all entries
    for i := 1 to Len do
      AssertTrue('Iterator 2 Down  (dups) Continues '+s, TestList.IterateNextDown);
    AssertTrue('Iterator 2 Down (dups) Finished '+s, not TestList.IterateNextDown);


(* Not implemented
    // Iterator Down, with dups remove
    CreateList(TestList, Len, False);

    TestList.StartIteratorAtLast;
    for i := Len downto 1 do begin
      AssertTrue('Iterator Down (dups) Continues '+s, TestList.IterateNextDown);
      p := TestList.CurrentCaretFull;
      AssertEquals('Iterator Ordered '+s+IntToStr(i), i * 10, TestList.CurrentCaretFull.x);

      if i-1 = MoveIdx then begin
        p.X := MoveTo * 10;
        TestList.CurrentCaretFull := p;
      end;
    end;
    AssertTrue('Iterator Down (dups) Finished '+s, not TestList.IterateNextDown);
    AssertSorted('Iterate Down (dups) '+s, TestList, L2);

    TestList.StartIteratorAtLast; // Iterate again / must get all entries
    for i := 1 to L2 do
      AssertTrue('Iterator 2 Down  (dups) Continues '+s, TestList.IterateNextDown);
    AssertTrue('Iterator 2 Down (dups) Finished '+s, not TestList.IterateNextDown);
*)

  finally
    TestList.Free;
  end;
end;



initialization

  RegisterTest(TTestSynMultCaret);
end.

