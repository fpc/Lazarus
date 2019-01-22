program BreakPointPrg;

uses sysutils, Classes;

var
  x, BreakDummy: Integer;

procedure Foo1;
begin
  BreakDummy:= 1; // TEST_BREAKPOINT=Foo1
end;

procedure Foo2;
var
  i: Integer;
begin
  for i := 0 to 1 do
    BreakDummy:= 1; // TEST_BREAKPOINT=Foo2
end;


label
testasmlbl1, testasmlbl2;

begin
  x := 1;
  x := 1; BreakDummy:= 1; // TEST_BREAKPOINT=PrgStep1
  x := 1; BreakDummy:= 1; // TEST_BREAKPOINT=PrgStep2

  x := 1; BreakDummy:= 1; // TEST_BREAKPOINT=PrgRun1
  x := 1; BreakDummy:= 1; // TEST_BREAKPOINT=PrgRun2

  asm
    nop  // TEST_BREAKPOINT=AsmStep1
    nop  // TEST_BREAKPOINT=AsmStep2
    nop  // TEST_BREAKPOINT=AsmRun1
    nop  // TEST_BREAKPOINT=AsmRun2
    nop
    jmp testasmlbl1 // TEST_BREAKPOINT=AsmBranch1
    nop
    nop  // TEST_BREAKPOINT=AsmNever1
testasmlbl1:
    nop   // TEST_BREAKPOINT=AsmBranchEnd1
    nop
    jmp testasmlbl2 // TEST_BREAKPOINT=AsmBranch2
    nop
    nop  // TEST_BREAKPOINT=AsmNever2
testasmlbl2:
    nop   // TEST_BREAKPOINT=AsmBranchEnd2
    nop
  end;

  x := 1;
  Foo1;
  Foo1;
  Foo1;

  x := 1;
  Foo2;
  BreakDummy:= 1; // TEST_BREAKPOINT=PrgAfterFoo2

  Foo2;
  BreakDummy:= 1; // TEST_BREAKPOINT=PrgAfterFoo2B

  // TODO; stepping over ignored breakpoint / actually that is stepping test
  // edit line / move breakpoint

end.

