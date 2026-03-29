unit TestNotebook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  // FPCUnit
  FPCUnit, TestRegistry,
  // LCL
  Forms, Controls, ComCtrls, ExtCtrls;

type
  TTestNotebook = class(TTestCase)
  protected
    FNotebook: TNotebook;
    procedure SetUp; override;
    procedure TearDown; override;
  protected
    FTryMoveFrom: integer;
    FTryMoveTo: integer;
    procedure TryMove;
  protected
    procedure SetNames(aPages: TStringList; aCount: integer);
    procedure Init(aPages: TStringList; aIndex: integer);
    procedure AssertPages(aPages: TStringList; aIndex: integer);
  published
    procedure TestPagesMove;
  end;

implementation

procedure TTestNotebook.SetUp;
begin
  inherited SetUp;
  FNotebook := TNotebook.Create(nil);
end;

procedure TTestNotebook.TearDown;
begin
  FreeAndNil(FNotebook);
  inherited TearDown;
end;

procedure TTestNotebook.TryMove;
begin
  FNotebook.Pages.Move(FTryMoveFrom, FTryMoveTo);
end;

// Assigns page names to test (just numbers corresponding to the index)
procedure TTestNotebook.SetNames(aPages: TStringList; aCount: integer);
var
  i: integer;
begin
  aPages.Clear;
  for i := 0 to aCount-1 do
    aPages.Add(i.ToString);
end;

procedure TTestNotebook.Init(aPages: TStringList; aIndex: integer);
var
  s: string;
begin
  // clear
  FNotebook.Pages.Clear;
  AssertEquals(0, FNotebook.Pages.Count); // extra
  // create
  for s in aPages do
    FNotebook.Pages.Add(s);
  FNotebook.PageIndex := aIndex;
  AssertPages(aPages, aIndex); // extra
end;

procedure TTestNotebook.AssertPages(aPages: TStringList; aIndex: integer);
var
  i: integer;
begin
  AssertEquals(aPages.Count, FNotebook.Pages.Count);
  for i := 0 to aPages.Count-1 do
  begin
    AssertEquals(aPages[i], FNotebook.Page[i].Caption);
    // TODO: AssertEquals(aPages[i], FNotebook.Pages[i]);
  end;
  AssertEquals(aIndex, FNotebook.PageIndex);
  if InRange(aIndex, 0, aPages.Count-1) then
  begin
    AssertEquals(aIndex, FNotebook.PageIndex);
    AssertNotNull(FNotebook.ActivePageComponent);
    AssertSame(FNotebook.Page[aIndex], FNotebook.ActivePageComponent);
    AssertEquals(aIndex, FNotebook.ActivePageComponent.PageIndex);
  end else begin
    AssertEquals(-1, FNotebook.PageIndex);
    // TODO: AssertNull(FNotebook.ActivePageComponent);
  end;
end;

procedure TTestNotebook.TestPagesMove;

  procedure Test(
    aSuccess: boolean;                               // test type (success/failure)
    aInitPages: TStringList; aInitIndex: integer;    // init
    aMoveFrom, aMoveTo: integer;                     // action
    aExpectPages: TStringList; aExpectIndex: integer // expected
  );
  const
    CExceptionMsg = 'List index (%d) out of bounds';
  var
    lExpectIndexError: integer;
  begin
    // init
    Init(aInitPages, aInitIndex);
    // move
    if aSuccess then
      FNotebook.Pages.Move(aMoveFrom, aMoveTo)
    else begin
      FTryMoveFrom := aMoveFrom;
      FTryMoveTo   := aMoveTo;
      lExpectIndexError := IfThen(InRange(aMoveFrom, 0, aInitPages.Count-1), aMoveTo, aMoveFrom);
      AssertException(EListError, @TryMove, Format(CExceptionMsg, [lExpectIndexError]));
    end;
    // check
    AssertPages(aExpectPages, aExpectIndex);
  end;

var
  l1: TStringList;   // source pages
  l2: TStringList;   // expected pages
  n: integer;        // pages count
  a: integer;        // move from
  b: integer;        // move to
  s1: integer;       // source selected page
  s2: integer;       // expected selected page
  lSuccess: boolean; // test type (success/failure)
begin
  l1 := TStringList.Create;
  l2 := TStringList.Create;
  try
    for n  :=  0 to   4 do // pages count (be sure to check 0)
    for a  := -2 to n+2 do // move from (be sure to check out of range)
    for b  := -2 to n+2 do // move to (be sure to check out of range)
    for s1 := -1 to n-1 do // selected (be sure to check -1)
    begin
      // test type (success/failure)
      lSuccess := InRange(a, 0, n-1) and InRange(b, 0, n-1);
      // source pages
      SetNames(l1, n);
      // expected pages (no change if an error is expected)
      l2.Text := l1.Text;
      if lSuccess then
        l2.Move(a, b);
      // expected selected page (no change if an error is expected)
      s2 := IfThen(lSuccess, l2.IndexOf(s1.ToString), s1);
      // test
      Test(lSuccess, {init:} l1, s1, {move:} a, b, {expect:} l2, s2);
    end;
  finally
    FreeAndNil(l1);
    FreeAndNil(l2);
  end;
end;

initialization
  RegisterTest(TTestNotebook);
end.

