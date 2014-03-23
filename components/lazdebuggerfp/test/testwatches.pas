unit TestWatches;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TestGDBMIControl, DbgIntfBaseTypes,
  DbgIntfDebuggerBase, TestBase, FpGdbmiDebugger, LCLProc, SynRegExpr, TestWatchUtils,
  GDBMIDebugger;

const
  BREAK_LINE_TestWatchesUnitSimple = 82;

type

  { TTestWatches }

  TTestWatches = class(TTestWatchesBase)
  private
    FWatches: TWatches;

    ExpectBreakSimple1: TWatchExpectationArray;
    FCurrentExpArray: ^TWatchExpectationArray; // currently added to

    FDbgOutPut: String;
    FDbgOutPutEnable: Boolean;

    procedure DoDbgOutput(Sender: TObject; const AText: String); override;
    procedure ClearAllTestArrays;
    function  HasTestArraysData: Boolean;

    function Add(AnExpr:  string; AFmt: TWatchDisplayFormat; AMtch: string;
                 AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags): PWatchExpectation;
    function Add(AnExpr:  string; AFmt: TWatchDisplayFormat; AEvalFlags: TDBGEvaluateFlags; AMtch: string;
                 AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags): PWatchExpectation;
    function AddFmtDef        (AnExpr, AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags=[]): PWatchExpectation;
    function AddFmtDef        (AnExpr: String; AEvalFlags: TDBGEvaluateFlags; AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags=[]): PWatchExpectation;

    function AddSimpleInt(AnExpr, AMtch: string; ATpNm: string): PWatchExpectation;

    procedure AddExpectSimple;
    procedure RunTestWatches(NamePreFix: String;
                             TestExeName, ExtraOpts: String;
                             UsedUnits: array of TUsesDir
                            );
  published
    procedure TestWatches;
  end;

implementation

const
  RNoPreQuote  = '(^|[^''])'; // No open qoute (Either at start, or other char)
  RNoPostQuote = '($|[^''])'; // No close qoute (Either at end, or other char)
  Match_Pointer = '\$[0-9A-F]+';
  M_Int = 'Integer|LongInt';

  {%region    * Classes * }
  // _vptr$TOBJECt on older gdb e.g. mac 6.3.50
  Match_ArgTFoo = '<TFoo> = \{.*(<|vptr\$)TObject>?.+ValueInt = -11';
  Match_ArgTFoo1 = '<TFoo> = \{.*(<|vptr\$)TObject>?.+ValueInt = 31';
  {%ebdregion    * Classes * }
  // Todo: Dwarf fails with dereferenced var pointer types

function MatchPointer(TypeName: String=''): String;
begin
  if TypeName = ''
  then Result := '\$[0-9A-F]+'
  else Result := TypeName+'\(\$[0-9A-F]+';
end;

function MatchRecord(TypeName: String; AContent: String = ''): String;
begin
  Result := 'record '+TypeName+' .+'+AContent;
end;
function MatchRecord(TypeName: String; AValInt: integer; AValFoo: String = ''): String;
begin
  Result := 'record '+TypeName+' .+ valint = '+IntToStr(AValInt);
  If AValFoo <> '' then Result := Result + ',.* valfoo = '+AValFoo;
end;

function MatchClass(TypeName: String; AContent: String = ''): String;
begin
  Result := '<'+TypeName+'> = \{.*(vptr\$|<TObject>).+'+AContent;
end;

function MatchClassNil(TypeName: String): String;
begin
  Result := '<'+TypeName+'> = nil';
end;




{ TTestWatches }

procedure TTestWatches.DoDbgOutput(Sender: TObject; const AText: String);
begin
  inherited DoDbgOutput(Sender, AText);
  if FDbgOutPutEnable then
    FDbgOutPut := FDbgOutPut + AText;
end;

procedure TTestWatches.ClearAllTestArrays;
begin
  //SetLength(ExpectBreakFooGdb, 0);
  //SetLength(ExpectBreakSubFoo, 0);
end;

function TTestWatches.HasTestArraysData: Boolean;
begin
  //Result := (Length(ExpectBreakFooGdb) > 0) or
  //          (Length(ExpectBreakSubFoo) > 0) or
  //          (Length(ExpectBreakFoo) > 0) or
  //          (Length(ExpectBreakFooArray) >0 );
  //

end;

function TTestWatches.Add(AnExpr: string; AFmt: TWatchDisplayFormat; AMtch: string;
  AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  Result := AddWatchExp(FCurrentExpArray^, AnExpr, AFmt, AMtch, AKind, ATpNm, AFlgs );
end;

function TTestWatches.Add(AnExpr: string; AFmt: TWatchDisplayFormat;
  AEvalFlags: TDBGEvaluateFlags; AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
  AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  Result := AddWatchExp(FCurrentExpArray^, AnExpr, AFmt, AEvalFlags, AMtch, AKind, ATpNm, AFlgs );
end;

function TTestWatches.AddFmtDef(AnExpr, AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
  AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  Result := Add(AnExpr, wdfDefault, AMtch, AKind, ATpNm, AFlgs );
end;

function TTestWatches.AddFmtDef(AnExpr: String; AEvalFlags: TDBGEvaluateFlags; AMtch: string;
  AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  Result := Add(AnExpr, wdfDefault, AEvalFlags, AMtch, AKind, ATpNm, AFlgs );
end;

function TTestWatches.AddSimpleInt(AnExpr, AMtch: string; ATpNm: string): PWatchExpectation;
begin
  AddFmtDef(AnExpr, AMtch, skSimple, ATpNm, [fTpMtch]);
end;

procedure TTestWatches.AddExpectSimple;
begin
  FCurrentExpArray := @ExpectBreakSimple1;
  //
  AddSimpleInt('SimpleArg_Int1',    '^-1902', M_Int);
  AddSimpleInt('SimpleVArg_Int1',   '^-1901', M_Int);
  AddSimpleInt('SimpleLocal_Int1',  '^3901', M_Int);
  AddSimpleInt('SimpleGlob_Int1',   '^2901', M_Int);
  AddSimpleInt('SimpleGlob_Int2',   '^0', M_Int);
  AddSimpleInt('SimpleGlob_Int3',   '^-1', M_Int);
  AddSimpleInt('SimpleGlob_Int4',   '^2147483647', M_Int);
  AddSimpleInt('SimpleGlob_Int5',   '^-2147483648', M_Int);
end;

procedure TTestWatches.RunTestWatches(NamePreFix: String; TestExeName, ExtraOpts: String;
  UsedUnits: array of TUsesDir);
var
  dbg: TGDBMIDebugger;
  Only: Integer;
  OnlyName, OnlyNamePart: String;


var
  i: Integer;
  WListSimple1: TTestWatchArray;

begin
  TestBaseName := NamePreFix;
  if not HasTestArraysData then exit;
  Only := StrToIntDef(TestControlForm.EdOnlyWatch.Text, -1);
  OnlyNamePart := '';OnlyName := '';
  if Only < 0
  then begin
    OnlyName := TestControlForm.EdOnlyWatch.Text;
    if (OnlyName <> '') and (OnlyName[1]='*') then begin
      OnlyNamePart := copy(OnlyName, 2, length(OnlyName));
      OnlyName := '';
    end;
  end;


  try
    TestCompile(AppDir + 'TestWatchesProg.pas', TestExeName, UsedUnits, '', ExtraOpts);
  except
    on e: Exception do begin
      TestTrue('Compile error: ' + e.Message, False);
      exit;
    end;
  end;

  try
    dbg := StartGDB(AppDir, TestExeName);
    FWatches := Watches.Watches;

    with dbg.BreakPoints.Add('TestWatchesUnitSimple.pas', BREAK_LINE_TestWatchesUnitSimple) do begin
      InitialEnabled := True;
      Enabled := True;
    end;

    if dbg.State = dsError then
      Fail(' Failed Init');

    AddWatches(ExpectBreakSimple1, WListSimple1, FWatches, Only, OnlyName, OnlyNamePart);

    (* Start debugging *)
    dbg.ShowConsole := True;
    dbg.Run;



    if TestTrue('State=Pause', dbg.State = dsPause)
    then begin
      (* Hit first breakpoint: BREAK_LINE_FOOFUNC_NEST SubFoo -- (1st loop) Called with none nil data *)

      TestWatchList('Simple1',ExpectBreakSimple1, WListSimple1, dbg, Only, OnlyName, OnlyNamePart);

      dbg.Run;
    end
    else TestTrue('Hit BREAK_LINE_FOOFUNC_NEST', False);





    dbg.Stop;
  except
    on e: Exception do begin
      TestTrue('Error: ' + e.Message, False);
      exit;
    end;
  end;
  dbg.Done;
  CleanGdb;
  dbg.Free;
end;

procedure TTestWatches.TestWatches;
var
  TestExeName: string;
begin
  if SkipTest then exit;
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('TTestWatches')] then exit;

  ClearTestErrors;

  ClearAllTestArrays;
  AddExpectSimple;

  RunTestWatches('', TestExeName,  '', []);

  AssertTestErrors;
end;

initialization

  RegisterDbgTest(TTestWatches);
  RegisterTestSelectors(['TTestWatches'
                        ]);

end.

