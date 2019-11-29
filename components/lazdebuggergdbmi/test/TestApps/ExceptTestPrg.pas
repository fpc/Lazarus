program ExceptTestPrg;
uses sysutils;

{off $DEFINE With_Implicit_Finally}

type
  MyExceptionIgnore = class(Exception) end;

var
  TestVal: integer;
  ControlRecurseStep3Outer, ControlRecurseRaise: Integer;

procedure Nop;
begin
  Freemem(GetMem(1));
end;

procedure MyRaise;
begin
  if ControlRecurseRaise >= 0 then begin
    ControlRecurseRaise := 0;
    raise Exception.create('a');  // TEST_BREAKPOINT=BrkMyRaise
  end;

  Freemem(GetMem(1));
end;

procedure Step3Finally; forward;
procedure Step3FinallyOuter; forward;


procedure Step3FinallyOuterRecurse;
begin
  ControlRecurseStep3Outer := 0; // only one recursion
  if ControlRecurseRaise > 0 then ControlRecurseRaise := -1;
  Step3FinallyOuter;
end;

procedure Step3Finally;
{$IFDEF With_Implicit_Finally}
var
  a: Ansistring;  // currently stepping into implicit finally handlers
{$ENDIF}
begin
  try
    try
      Nop;
      try
        Nop;
        {$IFDEF With_Implicit_Finally}
        a := 'a';
        {$ENDIF}
        MyRaise;
        Nop;  // TEST_BREAKPOINT=BrkDeadCode3Fin


      finally
        Nop;    // TEST_BREAKPOINT=BrkStep3Fin_A
        {$IFDEF With_Implicit_Finally}
        a := a + 'b';
        {$ENDIF}
        TestVal := TestVal + 1;
        if ControlRecurseStep3Outer = 1 then
          Step3FinallyOuterRecurse;
      nop; end;    // TEST_BREAKPOINT=BrkStep3Fin_A_END


    finally
      Nop;    // TEST_BREAKPOINT=BrkStep3Fin_B
      TestVal := TestVal + 1;
      //if ControlRecurseStep3Outer = XXX then
      //  Step3FinallyOuterRecurse;
    nop; end;    // TEST_BREAKPOINT=BrkStep3Fin_B_END


  finally
    Nop;    // TEST_BREAKPOINT=BrkStep3Fin_C
    TestVal := TestVal + 1;
    if ControlRecurseStep3Outer = 2 then
      Step3FinallyOuterRecurse;
  nop; end;    // TEST_BREAKPOINT=BrkStep3Fin_C_END


  Nop;
end;

procedure Step3FinallyOuter;
{$IFDEF With_Implicit_Finally}
var
  a: Ansistring;
{$ENDIF}
begin
  try
    try
      Nop;
      try
        Nop;
        {$IFDEF With_Implicit_Finally}
        a := 'a';
        {$ENDIF}
        Step3Finally;
        Nop;  // TEST_BREAKPOINT=BrkDeadCode3FinOuter


      finally
        Nop;    // TEST_BREAKPOINT=BrkStep3FinOuter_A
        {$IFDEF With_Implicit_Finally}
        a := a + 'b';
        {$ENDIF}
        TestVal := TestVal + 1;
        if ControlRecurseStep3Outer = 3 then
          Step3FinallyOuterRecurse;
      nop; end;    // TEST_BREAKPOINT=BrkStep3FinOuter_A_END


    finally
      Nop;    // TEST_BREAKPOINT=BrkStep3FinOuter_B
      TestVal := TestVal + 1;
      //if ControlRecurseStep3Outer = XXX then
      //  Step3FinallyOuterRecurse;
    nop; end;    // TEST_BREAKPOINT=BrkStep3FinOuter_B_END


  finally
    Nop;    // TEST_BREAKPOINT=BrkStep3FinOuter_C
    TestVal := TestVal + 1;
    if ControlRecurseStep3Outer = 4 then
      Step3FinallyOuterRecurse;
  nop; end;    // TEST_BREAKPOINT=BrkStep3FinOuter_C_END


  Nop;
end;

procedure NestedExcept(a: integer = 0);
begin
  try
    try
      MyRaise;
      nop;    // TEST_BREAKPOINT=BrkStepNestedExcept_DEAD

    finally
      nop;    // TEST_BREAKPOINT=BrkStepNestedExcept_Finally
      nop;
      if a = 0 then
        NestedExcept(1);   // TEST_BREAKPOINT=BrkStepNestedExcept_Finally_BEFORE
      nop;     // TEST_BREAKPOINT=BrkStepNestedExcept_Finally_AFTER
      nop;
    nop; end;    // TEST_BREAKPOINT=BrkStepNestedExcept_Finally_END

  nop; except  // some fpc versions put debug line for except, at the end of previous statement
      nop;    // TEST_BREAKPOINT=BrkStepNestedExcept
      nop;
  nop; end;     // TEST_BREAKPOINT=BrkStepNestedExcept_END

end;

var
  RecStep, RecRaise: Integer;
begin
  for RecRaise := 0 to 2 do // ignore or break at recurse
  for RecStep := 0 to 4 do
  if (RecRaise = 0) or (RecStep in [0,1,4]) then
  begin
    try
      ControlRecurseStep3Outer := RecStep;
      ControlRecurseRaise := 0;
      if RecRaise = 2 then
        ControlRecurseRaise := 1; // do not raise in recurse, but enter all the finally without stopping
      TestVal := 1;
      Step3FinallyOuter;
      nop;    // TEST_BREAKPOINT=BrkMainDeadCode1


    nop; except  // some fpc versions put debug line for except, at the end of previous statement
      nop;    // TEST_BREAKPOINT=BrkStepMainExcept1
      nop;
      TestVal := TestVal + 1;
    end;


    Nop;    // TEST_BREAKPOINT=BrkStepMainAfterExcept1
  end;

  ControlRecurseRaise := 0;
  NestedExcept;
  Nop;

  ControlRecurseRaise := 0;
  NestedExcept;
  Nop;


  // Do NOT step to finally, but set a breakpoint in it. Then step to next finally
  nop;
  nop;  // TEST_BREAKPOINT=BrkMain1
  ControlRecurseStep3Outer := 0;
  ControlRecurseRaise := 0;
  TestVal := 1;
  try
    Step3FinallyOuter;
  except
  end;


end.
