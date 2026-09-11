(* Tests for the FpDebug by-name procedure lookup, FindNamedProcSymbol.

   Nothing inside the Lazarus tree exercised this path before: there was no
   testcase, and LazDebuggerFp does not call it. The fixtures it runs against
   are in lazdebugtestbase/testapps/WatchesScopePrg.pas, and each of them is a
   deliberate name collision - the interesting question is not whether a name
   can be found, but which of two things answering to that name comes back.
*)
unit TestFpDebugApi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestDbgControl, TestDbgTestSuites, TestCommonSources,
  TestDbgConfig, LazDebuggerIntfBaseTypes, DbgIntfDebuggerBase,
  DbgIntfBaseTypes, FpDebugDebugger, FPDbgController, FpDbgClasses, FpDbgInfo,
  FpdMemoryTools, LazLoggerBase;

type

  { TTestFpDebugApi }

  TTestFpDebugApi = class(TDBGTestCase)
  private
    FDbgProcess: TDbgProcess;

    (* All four helpers release the symbol they take; none of them returns one,
       so no test body has to get the reference counting right. *)
    procedure AssertNotFound(const ATestName, AName: String;
                             AFlags: TFpProcSearchFlags = []);
    (* AnExpectedName = '' means "do not check the name". Pass it to assert
       that the symbol reports the spelling the debug info or the link table
       HOLDS, rather than the one the caller asked with. *)
    function  AssertFoundProc(const ATestName, AName: String;
                             AFlags: TFpProcSearchFlags = [];
                             const AnExpectedName: String = ''): TDBGPtr;
    procedure AssertNameAtAddress(const ATestName: String; AnAddress: TDBGPtr;
                             const AnExpectedName: String);
  published
    procedure TestFindNamedProcSymbol;
  end;

implementation

var
  ControlTest, ControlTestFindNamedProcSymbol: Pointer;

{ TTestFpDebugApi }

procedure TTestFpDebugApi.AssertNotFound(const ATestName, AName: String;
  AFlags: TFpProcSearchFlags);
var
  Sym: TFpSymbol;
begin
  Sym := FDbgProcess.FindNamedProcSymbol(AName, AFlags);
  TestTrue(ATestName + ': "' + AName + '" must not be found', Sym = nil);
  Sym.ReleaseReference;
end;

function TTestFpDebugApi.AssertFoundProc(const ATestName, AName: String;
  AFlags: TFpProcSearchFlags; const AnExpectedName: String): TDBGPtr;
var
  Sym: TFpSymbol;
begin
  Result := 0;
  Sym := FDbgProcess.FindNamedProcSymbol(AName, AFlags);
  try
    TestTrue(ATestName + ': "' + AName + '" must be found', Sym <> nil);
    if Sym = nil then
      exit;
    (* skProcedure and not skUnit: the by-name PROC lookup passes
       fsfOnlySubroutines, which suppresses the unit-name match. This is the
       assertion that the own-CU-name fixture exists for. *)
    TestTrue(ATestName + ': "' + AName + '" must be a procedure',
             Sym.Kind = skProcedure);
    TestTrue(ATestName + ': "' + AName + '" must have a target address',
             IsTargetNotNil(Sym.Address));
    if AnExpectedName <> '' then
      TestEquals(ATestName + ': reported name', AnExpectedName, Sym.Name);
    Result := Sym.Address.Address;
  finally
    Sym.ReleaseReference;
  end;
end;

procedure TTestFpDebugApi.AssertNameAtAddress(const ATestName: String;
  AnAddress: TDBGPtr; const AnExpectedName: String);
var
  Sym: TFpSymbol;
begin
  Sym := FDbgProcess.FindProcSymbol(AnAddress);
  try
    TestTrue(ATestName + ': address must resolve back to a symbol', Sym <> nil);
    if Sym = nil then
      exit;
    TestEquals(ATestName + ': name at address', AnExpectedName, Sym.Name);
  finally
    Sym.ReleaseReference;
  end;
end;

procedure TTestFpDebugApi.TestFindNamedProcSymbol;
var
  ExeName, ExpFooBarName: String;
  (* Src is NOT inherited. TDBGTestCase does not declare it - TTestBreakPoint
     declares its own field (testbreakpoint.pas:58), which is easy to misread
     as a base-class member when copying a test from there. *)
  Src: TCommonSource;
  Ctrl: TDbgController;
  AddrFooBar, AddrFooBarUp, AddrFooBarMixed: TDBGPtr;
  AddrOwnCuName, AddrUserBreak, AddrRtlBreak, AddrRtlBreakCi: TDBGPtr;
begin
  ExeName := '';
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestFindNamedProcSymbol) then exit;

  (* A BARE name, not AppDir + name. WatchesScopePrg is a SHARED testapp: it
     lives in lazdebugtestbase and reaches the suite through that package's
     embedded RCDATA resource, not from this suite's testapps directory, which
     does not contain it. TestCommonSources takes the resource branch only when
     the name has no path separator - a full path makes it read from disk and
     fail here. testwatches.pas calls it exactly this way. *)
  Src := GetCommonSourceFor('WatchesScopePrg.pas');
  TestCompile(Src, ExeName);

  TestTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));
  try
    Debugger.SetBreakPoint(Src, 'Prg');
    Debugger.RunToNextPause(dcRun);
    AssertDebuggerState(dsPause);

    Ctrl := (Debugger.LazDebugger as TFpDebugDebugger).DbgController;
    FDbgProcess := Ctrl.CurrentProcess;

    (* Under DWARF 2 FPC stores the name uppercased, so what the symbol reports
       differs by the version under test. The suite runs against all of them. *)
    if Compiler.SymbolType in stDwarf2 then
      ExpFooBarName := 'FOOBAR'
    else
      ExpFooBarName := 'FooBar';

    (* 1. A name that is not in the program at all. *)
    AssertNotFound('absent', 'ZzNoSuchNameHere');

    (* 2. A plain global procedure. *)
    AddrFooBar := AssertFoundProc('plain', 'FooBar');

    (* 3. Case matching: the same procedure, asked for in upper case. *)
    AddrFooBarUp := AssertFoundProc('upper case', 'FOOBAR');
    TestTrue('upper case: same address as the source spelling',
             AddrFooBar = AddrFooBarUp);

    (* 3b. A spelling that matches NEITHER the source form nor the stored one.
          Under DWARF 2 the stored name is FOOBAR and under later versions it
          is FooBar, so this request is wrong in both - which is the point.
          The comparison is CompareUtf8BothCase (fpdbgutil.pp), reached on the
          proc path through GoNamedChildEx and on the unit-name branch
          directly: it accepts each character of the STORED name against
          either the upper or the lower form of the REQUESTED one. So no
          casing of an ASCII identifier can miss, whichever way FPC wrote it.
          That is correct for Pascal, where identifiers are case-insensitive,
          and it is what an exact-match "optimisation" here would break.
          ONLY the DWARF namespace behaves this way: the link table is a
          case-SENSITIVE dictionary, which is what psfIgnoreCase relaxes. *)
    AddrFooBarMixed := AssertFoundProc('mixed case', 'fOobAR');
    TestTrue('mixed case: same address as the source spelling',
             AddrFooBar = AddrFooBarMixed);

    (* 4. A procedure whose name is its own compilation unit's name.
          Without fsfOnlySubroutines this answers the unit; the proc lookup
          must answer the procedure. AssertFoundProc checks the kind. *)
    AddrOwnCuName := AssertFoundProc('own CU name', 'WatchesScopePrg');
    TestTrue('own CU name: a different procedure from FooBar',
             AddrOwnCuName <> AddrFooBar);

    (* 5. Round trip. The by-address lookup must come back to the same
          procedure. Martin's caveat on the reverse direction - going from a
          name to an address and back may not land on the same address for
          every symbol - is why only this direction is asserted. *)
    AssertNameAtAddress('round trip', AddrFooBar, ExpFooBarName);

    (* 6. The two namespaces, on a name that exists in BOTH. The user's
          FPC_BREAK_ERROR is in the debug info; the RTL's is in the link
          table. Each flag must find its own and they are different code. *)
    AddrUserBreak := AssertFoundProc('dwarf namespace', 'FPC_BREAK_ERROR',
                                     [psfDwarfName]);
    AddrRtlBreak  := AssertFoundProc('link table namespace', 'FPC_BREAK_ERROR',
                                     [psfLinkTableSym]);
    TestTrue('namespaces: the two FPC_BREAK_ERRORs are different addresses',
             AddrUserBreak <> AddrRtlBreak);

    (* And the converse: a source-level name is not a link-table name. *)
    AssertNotFound('link table has no source-level name', 'WatchesScopePrg',
                   [psfLinkTableSym]);

    (* 6b. The link table is where case DOES matter, and it is the only place
          in this test where it does. The lookup is TWO steps
          (TfpSymbolList.GetInfo, fpdbgsymtable.pas):

            1. the dictionary is keyed on UpperCase(name)  - case-INsensitive
            2. the collision chain is then walked for p^.Name = AName, exactly

          psfIgnoreCase skips step 2, NOT step 1. So the miss below and the hit
          after it are the same lookup with one flag changed - which is what
          stops the miss being a "there is no such symbol" artefact: the very
          next assertion finds that same string.

          The DWARF namespace behaves the opposite way (see 3b), so a caller
          that gets case wrong is served by one namespace and not the other.
          That asymmetry is the reason both halves are asserted here. *)
    AssertNotFound('link table case', 'fpc_break_error', [psfLinkTableSym]);

    (* And step 2 being the only thing relaxed means the symbol still reports
       the spelling the TABLE holds - GetInfo assigns AFoundName from p^.Name
       in both modes, never from the requested string. That is the link-table
       counterpart of the by-name DWARF lookup echoing its caller. *)
    AddrRtlBreakCi := AssertFoundProc('link table ignore case',
                                      'fpc_break_error',
                                      [psfLinkTableSym, psfIgnoreCase],
                                      'FPC_BREAK_ERROR');
    TestTrue('link table ignore case: same address as the exact spelling',
             AddrRtlBreak = AddrRtlBreakCi);

    (* 7. Scoped-enum collision. With SCOPEDENUMS on, the enumerator "bar" is
          not in the enclosing scope, so the procedure of that name is legal.
          The lookup must return the procedure. *)
    AssertFoundProc('scoped enum', 'bar');

    (* REQUIRED. TestTrue/TestEquals/TestFalse do NOT fail the test - they
       accumulate into FTestErrors via AddTestError. AssertTestErrors is the
       only thing that raises, and every other test in this suite calls it
       (testbreakpoint 8 times, teststepping 5, testwatches 18). Without this
       line the test passes no matter what the assertions saw. *)
    AssertTestErrors;

  finally
    Debugger.RunToNextPause(dcStop);
  end;
end;

initialization
  ControlTest := TestControlRegisterTest('TTestFpDebugApi');
  ControlTestFindNamedProcSymbol :=
    TestControlRegisterTest('TestFindNamedProcSymbol', ControlTest);
  RegisterDbgTest(TTestFpDebugApi);
end.
