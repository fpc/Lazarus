program StepTryBlocksPrg;
{$mode objfpc}{$H+}
{$Inline off}
uses
  sysutils, Classes;

var
  a, b: integer;

procedure Nop;
begin
  Freemem(GetMem(1));
end;

procedure Test(DoExit1, DoExit2, DoRaise1, DoRaise2, DoRaise3: boolean);
begin
  nop;       // TEST_BREAKPOINT=BEGIN
  try nop;   // TEST_BREAKPOINT=TRY_A_1
    nop;
    nop;     // TEST_BREAKPOINT=TRY_A_1_BEFORE_TRY_B
      try                             // TEST_BREAKPOINT=TRY_B_1_EARLY
        nop;                          // TEST_BREAKPOINT=TRY_B_1
        nop;                          // TEST_BREAKPOINT=TRY_B_1_BEFORE_TRY_C
          try                             // TEST_BREAKPOINT=TRY_C_EARLY
            nop;                          // TEST_BREAKPOINT=TRY_C_1
            nop;
            if DoRaise1 then
              raise Exception.Create('');  // TEST_BREAKPOINT=TRY_C_RAISE_1
            nop;                           // TEST_BREAKPOINT=TRY_C_AFTER_RAISE_1
            nop;                           // TEST_BREAKPOINT=TRY_C_BEFORE_EXCEPT_1
          except                           // TEST_BREAKPOINT=TRY_C_EXCEPT_1_EARLY
            nop;                           // TEST_BREAKPOINT=TRY_C_EXCEPT_1
            nop;
          nop end;                         // TEST_BREAKPOINT=TRY_C_EXCEPT_1_END
        nop;                          // TEST_BREAKPOINT=TRY_B_AFTER_EXCEPT_1
        nop;
        if DoRaise2 then
          raise Exception.Create(''); // TEST_BREAKPOINT=TRY_B_RAISE_2
        nop;                          // TEST_BREAKPOINT=TRY_B_AFTER_RAISE_2
        nop;                          // TEST_BREAKPOINT=TRY_B_BEFORE_FINALLY
      finally                         // TEST_BREAKPOINT=TRY_B_FINALLY
        nop;                          // TEST_BREAKPOINT=TRY_B_FINALLY_1
        nop;
      nop end;                        // TEST_BREAKPOINT=TRY_B_FINALLY_END
    nop;     // TEST_BREAKPOINT=TRY_A_AFTER_FIN_B
    nop;     // TEST_BREAKPOINT=TRY_A_BEFORE_FINALLY
  finally    // TEST_BREAKPOINT=TRY_A_FINALLY_EARLY
    nop;     // TEST_BREAKPOINT=TRY_A_FINALLY_1
    nop;
      try nop;          // TEST_BREAKPOINT=TRY_D_1
        nop;
        nop;
          try nop;             // TEST_BREAKPOINT=TRY_E_1
            nop;
            nop;               // TEST_BREAKPOINT=TRY_E_BEFORE_FINALLY
          finally nop;         // TEST_BREAKPOINT=TRY_E_FINALLY_1
            nop;
            nop;
              try nop;                             // TEST_BREAKPOINT=TRY_F_1
                nop;
                nop;                               // TEST_BREAKPOINT=TRY_F_3
{$IF FPC_FULLVERSION >= 030000}{$IF (FPC_FULLVERSION >= 030200) OR defined(CPUX86_64)}
                if DoExit1 and (a <> b) then exit; // TEST_BREAKPOINT=TRY_F_EXIT_1
{$ENDIF}{$ENDIF}
                nop;                               // TEST_BREAKPOINT=TRY_F_AFTER_EXIT_1
                if DoRaise3 then
                  raise Exception.Create('');      // TEST_BREAKPOINT=TRY_F_RAISE_3
                nop;                               // TEST_BREAKPOINT=TRY_F_AFTER_RAISE_3
                nop;                               // TEST_BREAKPOINT=TRY_F_BEFORE_FINALLY
              finally nop;                         // TEST_BREAKPOINT=TRY_F_FINALLY
                nop;                               // TEST_BREAKPOINT=TRY_F_FINALLY_1
                nop;
{$IF FPC_FULLVERSION >= 030000}{$IF (FPC_FULLVERSION >= 030200) OR defined(CPUX86_64)}
                if DoExit2 and (a <> b) then exit;  // TEST_BREAKPOINT=TRY_F_FIN_EXIT_2
{$ENDIF}{$ENDIF}
                nop;                                // TEST_BREAKPOINT=TRY_F_AFTER_FIN_EXIT_2
                nop;                                // TEST_BREAKPOINT=TRY_F_BEFORE_TRY_G
                  try nop;                               // TEST_BREAKPOINT=TRY_G_1
                    nop;
                    nop;                                 // TEST_BREAKPOINT=TRY_G_2
                  finally nop;                           // TEST_BREAKPOINT=TRY_G_FINALLY
                    nop;                                 // TEST_BREAKPOINT=TRY_G_FINALLY_1
                    nop;
                  nop end;                              // TEST_BREAKPOINT=TRY_G_FINALLY_END
                nop;                               // TEST_BREAKPOINT=TRY_F_FINALLY_AFTER_FIN_G
                nop;
              nop end;                             // TEST_BREAKPOINT=TRY_F_FINALLY_END
            nop;                // TEST_BREAKPOINT=TRY_E_FINALLY_AFTER_FIN_F
            nop;                // TEST_BREAKPOINT=TRY_E_FINALLY_AFTER_FIN_F_2
          nop end;              // TEST_BREAKPOINT=TRY_E_FINALLY_END
      except nop;       // TEST_BREAKPOINT=TRY_D_EXCEPT
        nop;
        nop;
      end;              // TEST_BREAKPOINT=TRY_D_EXCEPT_END
    nop;    // TEST_BREAKPOINT=TRY_A_FINALLY_AFTER_EXCEPT_D
    nop;
  nop end;  // TEST_BREAKPOINT=TRY_A_FINALLY_END

  try             // TEST_BREAKPOINT=TRY_X_EARLY
    nop;          // TEST_BREAKPOINT=TRY_X_1
  finally nop;    // TEST_BREAKPOINT=TRY_X_FINALLY
    nop;          // TEST_BREAKPOINT=TRY_X_FINALLY_1
  end;
  nop;     // TEST_BREAKPOINT=BEFORE_END
end;     // TEST_BREAKPOINT=END

procedure Bar1(DoExit1, DoExit2, DoRaise1, DoRaise2, DoRaise3: boolean);
begin
  nop;
  nop;
  Test(DoExit1, DoExit2, DoRaise1, DoRaise2, DoRaise3);  // TEST_BREAKPOINT=BAR1_CALL
  nop;  // TEST_BREAKPOINT=BAR1_RET
  nop;
end;

procedure Bar2(DoExit1, DoExit2, DoRaise1, DoRaise2, DoRaise3: boolean);
begin
  nop;
  nop;
  nop; try
    nop;
    nop;
    Test(DoExit1, DoExit2, DoRaise1, DoRaise2, DoRaise3);  // TEST_BREAKPOINT=BAR2_CALL
    nop;  // TEST_BREAKPOINT=BAR2_RET
    nop;
  finally nop;   // TEST_BREAKPOINT=BAR1_RET_FIN
    nop;
    nop;
  end;
end;

procedure Bar3(DoExit1, DoExit2, DoRaise1, DoRaise2, DoRaise3: boolean);
begin
  nop;
  nop;
  nop; try
    nop;
    nop;
    Test(DoExit1, DoExit2, DoRaise1, DoRaise2, DoRaise3);  // TEST_BREAKPOINT=BAR3_CALL
    nop;  // TEST_BREAKPOINT=BAR3_RET
    nop;
  except nop;  // TEST_BREAKPOINT=BAR1_RET_EXCEPT
    nop;
    nop;
  end;
end;

procedure Foo(a: Integer; DoExit1, DoExit2, DoRaise1, DoRaise2, DoRaise3: boolean);
begin
  nop;
  nop; try
    nop;
    case a of
    1: Bar1(DoExit1, DoExit2, DoRaise1, DoRaise2, DoRaise3);
    2: Bar2(DoExit1, DoExit2, DoRaise1, DoRaise2, DoRaise3);
    3: Bar3(DoExit1, DoExit2, DoRaise1, DoRaise2, DoRaise3);
    end; nop;  // TEST_BREAKPOINT=FOO_RET
    nop;
  except nop;  // TEST_BREAKPOINT=FOO_EXCEPT
    nop;
  end;
  nop;
end;

begin
  a := 1;
  b := 2;
  Foo(1, False, False, False, False, False);  // TEST_BREAKPOINT=CALL_TestSimpleStepOver
  nop;
  nop;
  Foo(1, False, False, False, False, False);  // TEST_BREAKPOINT=CALL_TestSimpleRunAndStepOver
  nop;
  nop;
  Foo(1, True, False, False, False, False);  // TEST_BREAKPOINT=CALL_TestSimpleRunAndStepOverExit
  Foo(1, False, True, False, False, False);
  nop;
  nop;
  // Bar2 with finally
  Foo(2, False, False, False, False, False);  // TEST_BREAKPOINT=CALL_TestSimpleRunAndStepOut
  Foo(2, False, False, False, False, False);
  nop;
  nop;
  Foo(1, False, False, False, False, False);  // TEST_BREAKPOINT=CALL_xx
  nop;
  nop;
end.

