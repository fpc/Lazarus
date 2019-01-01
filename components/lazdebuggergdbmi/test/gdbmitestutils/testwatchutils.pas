unit TestWatchUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, TestBase, LazLoggerBase,
  DbgIntfBaseTypes, DbgIntfDebuggerBase, TestDbgConfig, TTestDebuggerClasses;

type

  TWatchExpectationFlag =
    (IgnDwrf,            // ignore error for dwarf at all
     IgnDwrf2,           // ignore error for dwarf 2
     IgnDwrf2IfNoSet,    // ignore error for dwarf2 (-gw) without set
     IgnDwrf3,           // ignore error for dwarf 3
     IgnStabs,
     //IgnDwrfSet,   // no dwarf2 with set // no dwarf3

     IgnData,           // Ignore the data part
     IgnDataDw,         // Ignore the data part, if dwarf
     IgnDataDw2,        // Ignore the data part, if dwarf 2
     IgnDataDw3,        // Ignore the data part, if dwarf 3
     IgnDataSt,         // Ignore the data part, if Stabs

     IgnKind,           // Ignore skSimple, ....
     IgnKindDw,
     IgnKindDw2,
     IgnKindDw3,
     IgnKindSt,

     IgnKindPtr,           // Ignore skSimple, ONLY if got kind=skPointer
     IgnKindPtrDw,
     IgnKindPtrDw2,
     IgnKindPtrDw3,
     IgnKindPtrSt,

     IgnTpName,           // Ignore the typename
     IgnTpNameDw,
     IgnTpNameDw2,
     IgnTpNameDw3,
     IgnTpNameSt,

     fTstSkip,       // Do not run test
     fTstSkipDwarf3,
     fTpMtch,
     fTExpectNotFound,
     fTExpectError
    );
  TWatchExpectationFlags = set of TWatchExpectationFlag;

const
  WatchExpFlagMask: array[TSymbolType] of TWatchExpectationFlags
  = ( {stNone}     [],
      {stStabs}    [IgnStabs,
                    IgnData,    IgnDataSt,
                    IgnKind,    IgnKindSt,
                    IgnKindPtr, IgnKindPtrSt,
                    IgnTpName,  IgnTpNameSt
                   ],
      {stDwarf}    [IgnDwrf, IgnDwrf2, IgnDwrf2IfNoSet,
                    IgnData,    IgnDataDw, IgnDataDw2,
                    IgnKind,    IgnKindDw, IgnKindDw2,
                    IgnKindPtr, IgnKindPtrDw, IgnKindPtrDw2,
                    IgnTpName,  IgnTpNameDw, IgnTpNameDw2
                   ],
      {stDwarfSet} [IgnDwrf, IgnDwrf2,
                    IgnData,    IgnDataDw, IgnDataDw2,
                    IgnKind,    IgnKindDw, IgnKindDw2,
                    IgnKindPtr, IgnKindPtrDw, IgnKindPtrDw2,
                    IgnTpName,  IgnTpNameDw, IgnTpNameDw2
                   ],
      {stDwarf3}   [IgnDwrf, IgnDwrf3,
                    IgnData,    IgnDataDw, IgnDataDw3,
                    IgnKind,    IgnKindDw, IgnKindDw3,
                    IgnKindPtr, IgnKindPtrDw, IgnKindPtrDw3,
                    IgnTpName,  IgnTpNameDw, IgnTpNameDw3
                   ],
                   []
    );

  WatchExpFlagSIgnAll     = [IgnStabs, IgnDwrf, IgnDwrf2, IgnDwrf2IfNoSet, IgnDwrf3];
  WatchExpFlagSIgnData    = [IgnStabs, IgnDwrf, IgnDwrf2, IgnDwrf2IfNoSet, IgnDwrf3,  IgnData,    IgnDataDw, IgnDataDw2, IgnDataDw3, IgnDataSt];
  WatchExpFlagSIgnKind    = [IgnStabs, IgnDwrf, IgnDwrf2, IgnDwrf2IfNoSet, IgnDwrf3,  IgnKind,    IgnKindDw, IgnKindDw2, IgnKindDw3, IgnKindSt];
  WatchExpFlagSIgnKindPtr = [IgnStabs, IgnDwrf, IgnDwrf2, IgnDwrf2IfNoSet, IgnDwrf3,  IgnKindPtr, IgnKindPtrDw, IgnKindPtrDw2, IgnKindPtrDw3, IgnKindPtrSt];
  WatchExpFlagSIgnTpName  = [IgnStabs, IgnDwrf, IgnDwrf2, IgnDwrf2IfNoSet, IgnDwrf3,  IgnTpName,  IgnTpNameDw, IgnTpNameDw2, IgnTpNameDw3, IgnTpNameSt];

type
  PWatchExpectation= ^TWatchExpectation;
  TWatchExpectOnBeforeTest = procedure(AWatchExp: PWatchExpectation) of object;

  TFullTypeMemberExpectationResult = record
    Name: string;
    ExpTypeName: string;
    ExpKind: TDbgSymbolKind;
    Flgs: TWatchExpectationFlags;
  end;
  TFullTypeMemberExpectationResultArray = array of TFullTypeMemberExpectationResult;

  TWatchExpectationResult = record
    ExpMatch: string;
    ExpKind: TDBGSymbolKind;
    ExpTypeName: string;
    Flgs: TWatchExpectationFlags;
    MinGdb, MinFpc: Integer;
    FullTypesExpect: TFullTypeMemberExpectationResultArray;
  end;

  TWatchExpectation = record
    TestName: String;
    Expression:  string;
    DspFormat: TWatchDisplayFormat;
    RepeatCount: Integer;
    EvaluateFlags: TDBGEvaluateFlags;
    StackFrame: Integer;
    Result: Array [TSymbolType] of TWatchExpectationResult;

    TheWatch: TTestWatch;
    UserData, UserData2: Pointer;
    OnBeforeTest: TWatchExpectOnBeforeTest;
  end;
  TWatchExpectationArray = array of TWatchExpectation;
  PWatchExpectationArray = ^TWatchExpectationArray;

  { TTestWatchesBase }

  TTestWatchesBase = class(TGDBTestCase)
  protected
    procedure TestWatch(Name: String; ADbg: TDebuggerIntf;
                        AWatch: TTestWatch; Data: TWatchExpectation; WatchValue: String = '');
    procedure AddWatches(ExpectList: TWatchExpectationArray;
                         AWatches: TWatches;
                         Only: Integer; OnlyName, OnlyNamePart: String);
    procedure TestWatchList(AName: String; ExpectList: TWatchExpectationArray;
                          ADbg: TDebuggerIntf;
                          Only: Integer; OnlyName, OnlyNamePart: String);
  end;


function AddWatchExp(var ExpArray: TWatchExpectationArray;
  AnExpr:  string; AFmt: TWatchDisplayFormat;
  AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
  AFlgs: TWatchExpectationFlags = [];
  AStackFrame: Integer = 0;
  AMinGdb: Integer = 0; AMinFpc: Integer = 0
): PWatchExpectation;
function AddWatchExp(var ExpArray: TWatchExpectationArray;
  AnExpr:  string; AFmt: TWatchDisplayFormat; AEvaluateFlags: TDBGEvaluateFlags;
  AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
  AFlgs: TWatchExpectationFlags = [];
  AStackFrame: Integer = 0;
  AMinGdb: Integer = 0; AMinFpc: Integer = 0
): PWatchExpectation;
function AddWatchExp(var ExpArray: TWatchExpectationArray; ATestName: String;
  AnExpr:  string; AFmt: TWatchDisplayFormat;
  AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
  AFlgs: TWatchExpectationFlags = [];
  AStackFrame: Integer = 0;
  AMinGdb: Integer = 0; AMinFpc: Integer = 0
): PWatchExpectation;
function AddWatchExp(var ExpArray: TWatchExpectationArray; ATestName: String;
  AnExpr:  string; AFmt: TWatchDisplayFormat; AEvaluateFlags: TDBGEvaluateFlags;
  AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
  AFlgs: TWatchExpectationFlags = [];
  AStackFrame: Integer = 0;
  AMinGdb: Integer = 0; AMinFpc: Integer = 0
): PWatchExpectation;

procedure UpdExpRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags;
  AMinGdb: Integer; AMinFpc: Integer
);
procedure UpdExpRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags = []
);
procedure UpdExpRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  AMtch: string; AKind: TDBGSymbolKind
);
procedure UpdExpRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  AKind: TDBGSymbolKind
);
procedure UpdExpRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  ATpNm: string; AFlgs: TWatchExpectationFlags
);
procedure UpdResMinGdb(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType; AMinGdb: Integer);
procedure UpdResMinFpc(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType; AMinFpc: Integer);

procedure UpdExpRes(AWatchExp: PWatchExpectation; ASymbolTypes: TSymbolTypes;
  ATpNm: string; AFlgs: TWatchExpectationFlags
);
procedure UpdResMinGdb(AWatchExp: PWatchExpectation; ASymbolTypes: TSymbolTypes; AMinGdb: Integer);
procedure UpdResMinFpc(AWatchExp: PWatchExpectation; ASymbolTypes: TSymbolTypes; AMinFpc: Integer);

procedure AddMemberExpect(AWatchExp: PWatchExpectation;
  AName, ATpNm: string; AFlgs: TWatchExpectationFlags; AnExpKind: TDBGSymbolKind;
  ASymbolTypes: TSymbolTypes = stSymAll
);

implementation

function AddWatchExp(var ExpArray: TWatchExpectationArray; AnExpr: string;
  AFmt: TWatchDisplayFormat; AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
  AFlgs: TWatchExpectationFlags; AStackFrame: Integer = 0; AMinGdb: Integer = 0;
  AMinFpc: Integer = 0): PWatchExpectation;
begin
  Result := AddWatchExp(ExpArray,
    AnExpr + ' (' + TWatchDisplayFormatNames[AFmt] + ', []',
    AnExpr, AFmt, [], AMtch, AKind, ATpNm, AFlgs, AStackFrame, AMinGdb, AMinFpc);
end;

function AddWatchExp(var ExpArray: TWatchExpectationArray; AnExpr: string;
  AFmt: TWatchDisplayFormat; AEvaluateFlags: TDBGEvaluateFlags; AMtch: string;
  AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags; AStackFrame: Integer;
  AMinGdb: Integer; AMinFpc: Integer): PWatchExpectation;
begin
  Result := AddWatchExp(ExpArray,
    AnExpr + ' (' + TWatchDisplayFormatNames[AFmt] + ', ' + dbgs(AEvaluateFlags) + ')',
    AnExpr, AFmt, AEvaluateFlags, AMtch, AKind, ATpNm, AFlgs, AStackFrame, AMinGdb, AMinFpc);
end;

function AddWatchExp(var ExpArray: TWatchExpectationArray; ATestName: String;
  AnExpr: string; AFmt: TWatchDisplayFormat; AMtch: string; AKind: TDBGSymbolKind;
  ATpNm: string; AFlgs: TWatchExpectationFlags; AStackFrame: Integer; AMinGdb: Integer = 0;
  AMinFpc: Integer = 0): PWatchExpectation;
begin
  Result := AddWatchExp(ExpArray, ATestName, AnExpr, AFmt, [], AMtch, AKind, ATpNm,
    AFlgs, AStackFrame, AMinGdb, AMinFpc);
end;

function AddWatchExp(var ExpArray: TWatchExpectationArray; ATestName: String;
  AnExpr: string; AFmt: TWatchDisplayFormat; AEvaluateFlags: TDBGEvaluateFlags; AMtch: string;
  AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags; AStackFrame: Integer;
  AMinGdb: Integer; AMinFpc: Integer): PWatchExpectation;
var
  i: TSymbolType;
begin
  SetLength(ExpArray, Length(ExpArray)+1);
  with ExpArray[Length(ExpArray)-1] do begin
    TestName     := ATestName;
    Expression   := AnExpr;
    DspFormat    := AFmt;
    RepeatCount  := 0;
    EvaluateFlags := AEvaluateFlags;
    TheWatch := nil;
    OnBeforeTest := nil;
    UserData := nil;
    for i := low(TSymbolType) to high(TSymbolType) do begin
      Result[i].ExpMatch     := AMtch;
      Result[i].ExpKind      := AKind;
      Result[i].ExpTypeName  := ATpNm;
      Result[i].Flgs         := AFlgs;
      Result[i].MinGdb := AMinGdb;
      Result[i].MinFpc := AMinFpc;
    end;
    StackFrame   := AStackFrame;
  end;
  Result := @ExpArray[Length(ExpArray)-1];
end;

procedure UpdExpRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags;
  AMinGdb: Integer; AMinFpc: Integer);
begin
  with AWatchExp^ do begin
    Result[ASymbolType].ExpMatch     := AMtch;
    Result[ASymbolType].ExpKind      := AKind;
    Result[ASymbolType].ExpTypeName  := ATpNm;
    Result[ASymbolType].Flgs         := AFlgs;
    Result[ASymbolType].MinGdb := AMinGdb;
    Result[ASymbolType].MinFpc := AMinFpc;
  end;
end;

procedure UpdExpRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags);
begin
  with AWatchExp^ do begin
    Result[ASymbolType].ExpMatch     := AMtch;
    Result[ASymbolType].ExpKind      := AKind;
    Result[ASymbolType].ExpTypeName  := ATpNm;
    Result[ASymbolType].Flgs         := AFlgs;
  end;
end;

procedure UpdExpRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  AMtch: string; AKind: TDBGSymbolKind);
begin
  with AWatchExp^ do begin
    Result[ASymbolType].ExpMatch     := AMtch;
    Result[ASymbolType].ExpKind      := AKind;
  end;
end;

procedure UpdExpRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  AKind: TDBGSymbolKind);
begin
  with AWatchExp^ do begin
    Result[ASymbolType].ExpKind      := AKind;
  end;
end;

procedure UpdExpRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  ATpNm: string; AFlgs: TWatchExpectationFlags);
begin
  with AWatchExp^ do begin
    Result[ASymbolType].ExpTypeName  := ATpNm;
    Result[ASymbolType].Flgs         := AFlgs;
  end;
end;

procedure UpdExpRes(AWatchExp: PWatchExpectation; ASymbolTypes: TSymbolTypes;
  ATpNm: string; AFlgs: TWatchExpectationFlags);
var
  i: TSymbolType;
begin
  for i := low(TSymbolType) to high(TSymbolType) do
    if i in ASymbolTypes then
      UpdExpRes(AWatchExp, i, ATpNm, AFlgs);
end;

procedure UpdResMinGdb(AWatchExp: PWatchExpectation; ASymbolTypes: TSymbolTypes;
  AMinGdb: Integer);
var
  i: TSymbolType;
begin
  for i := low(TSymbolType) to high(TSymbolType) do
    if i in ASymbolTypes then
      UpdResMinGdb(AWatchExp, i, AMinGdb);
end;

procedure UpdResMinFpc(AWatchExp: PWatchExpectation; ASymbolTypes: TSymbolTypes;
  AMinFpc: Integer);
var
  i: TSymbolType;
begin
  for i := low(TSymbolType) to high(TSymbolType) do
    if i in ASymbolTypes then
      UpdResMinFpc(AWatchExp, i, AMinFpc);
end;

procedure AddMemberExpect(AWatchExp: PWatchExpectation; AName, ATpNm: string;
  AFlgs: TWatchExpectationFlags; AnExpKind: TDBGSymbolKind; ASymbolTypes: TSymbolTypes);
var
  i: TSymbolType;
  l: Integer;
begin
  for i := low(TSymbolType) to high(TSymbolType) do
    if i in ASymbolTypes then begin
      l := length(AWatchExp^.Result[i].FullTypesExpect);
      SetLength(AWatchExp^.Result[i].FullTypesExpect, l + 1);
      AWatchExp^.Result[i].FullTypesExpect[l].Name := AName;
      AWatchExp^.Result[i].FullTypesExpect[l].ExpTypeName := ATpNm;
      AWatchExp^.Result[i].FullTypesExpect[l].ExpKind := AnExpKind;
      AWatchExp^.Result[i].FullTypesExpect[l].Flgs := AFlgs;
    end;
end;

procedure UpdResMinGdb(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  AMinGdb: Integer);
begin
  with AWatchExp^ do begin
    Result[ASymbolType].MinGdb := AMinGdb;
  end;
end;

procedure UpdResMinFpc(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  AMinFpc: Integer);
begin
  with AWatchExp^ do begin
    Result[ASymbolType].MinFpc := AMinFpc;
  end;
end;


var
  Frx: TRegExpr;

{ TTestWatchesBase }

procedure TTestWatchesBase.TestWatch(Name: String; ADbg: TDebuggerIntf; AWatch: TTestWatch;
  Data: TWatchExpectation; WatchValue: String);
var
  rx: TRegExpr;
  s, s2: String;
  flag, IsValid, HasTpInfo, f2: Boolean;
  WV: TWatchValue;
  Stack: Integer;
  n: String;
  DataRes: TWatchExpectationResult;
  IgnoreFlags: TWatchExpectationFlags;
  IgnoreAll, IgnoreData, IgnoreKind, IgnoreKindPtr, IgnoreTpName: boolean;
  IgnoreText: String;
  i, j: Integer;
  fld: TDBGField;
  MemberTests: TFullTypeMemberExpectationResultArray;

  function CmpNames(TestName, Exp, Got: String; Match: Boolean): Boolean;
  begin
    if Match then begin
      if Frx = nil then Frx := TRegExpr.Create;
      Frx.ModifierI := true;
      Frx.Expression := Exp;
      TestTrue(TestName + ' matches '+Exp+' but was '+Got,  Frx.Exec(Got), DataRes.MinGdb, DataRes.MinFpc, IgnoreText);
     end
     else TestEquals(TestName + ' equals ',  LowerCase(Exp), LowerCase(Got), DataRes.MinGdb, DataRes.MinFpc, IgnoreText);
  end;

begin
  if not TestTrue('Dbg did NOT enter dsError', ADbg.State <> dsError) then exit;
  if Data.OnBeforeTest <> nil then Data.OnBeforeTest(@Data);

  rx := nil;
  Stack := Data.StackFrame;
  DataRes := Data.Result[SymbolType];
  IgnoreFlags := DataRes.Flgs * WatchExpFlagMask[SymbolType];
  IgnoreAll     := IgnoreFlags * WatchExpFlagSIgnAll <> [];
  IgnoreData    := IgnoreFlags * WatchExpFlagSIgnData <> [];
  IgnoreKind    := IgnoreFlags * WatchExpFlagSIgnKind <> [];
  IgnoreKindPtr := IgnoreFlags * WatchExpFlagSIgnKindPtr <> [];
  IgnoreTpName  := IgnoreFlags * WatchExpFlagSIgnTpName <> [];

  // Get Value
  n := Data.TestName;
  if n = '' then n := Data.Expression + ' (' + TWatchDisplayFormatNames[Data.DspFormat] + ', ' + dbgs(Data.EvaluateFlags) + ' RepCnt=' + dbgs(Data.RepeatCount) + ')';
  Name := Name + ' ' + n + ' ::: '+adbg.GetLocation.SrcFile+' '+IntToStr(ADbg.GetLocation.SrcLine);
  LogToFile('###### ' + Name + '###### '+LineEnding);
  flag := AWatch <> nil; // test for typeinfo/kind  // Awatch=nil > direct gdb command
  IsValid := True;
  HasTpInfo := True;
  if flag then begin;
    WV := AWatch.Values[1, Stack];// trigger read
    s := WV.Value;
    IsValid := WV.Validity = ddsValid;
    HasTpInfo := IsValid and (WV.TypeInfo <> nil);
//      flag := flag and IsValid;
  end
  else
    s := WatchValue;

  if not TestTrue('ADbg did NOT enter dsError', ADbg.State <> dsError) then exit;

  // Check Data
  f2 := True;
  IgnoreText := '';    if IgnoreData then IgnoreText := 'Ignored by flag';
  if IsValid = not(fTExpectError in DataRes.Flgs) then begin
    rx := TRegExpr.Create;
    rx.ModifierI := true;
    rx.Expression := DataRes.ExpMatch;
    if DataRes.ExpMatch <> ''
    then f2 := TestTrue(Name + ' Matches "'+DataRes.ExpMatch + '", but was "' + s + '"', rx.Exec(s), DataRes.MinGdb, DataRes.MinFpc, IgnoreText);
    FreeAndNil(rx);
  end else begin
     f2 := TestTrue(Name + ' Matches "'+DataRes.ExpMatch + '", but STATE was <'+dbgs(WV.Validity)+'> Val="'+s+'"', False, DataRes.MinGdb, DataRes.MinFpc, IgnoreText);
     //exit; // failed Data, do not list others as potential unexpected success
  end;

  if (not f2) and IgnoreAll then exit; // failed Data, do not list others as potential unexpected success

  // TypeInfo checks ?
  if (not flag) or (DataRes.ExpTypeName = '') then exit;

  // Check TypeInfo
  s:='';
  if HasTpInfo then WriteStr(s, WV.TypeInfo.Kind);
  WriteStr(s2, DataRes.ExpKind);
  IgnoreText := '';    if IgnoreKind then IgnoreText := 'Ignored by flag';
  if IsValid and HasTpInfo then begin
    if (not IgnoreKind) and IgnoreKindPtr and (WV.TypeInfo.Kind = skPointer) then IgnoreText := 'Ignored by flag (Kind may be Ptr)';
    f2 := TestEquals(Name + ' Kind',  s2, s, DataRes.MinGdb, DataRes.MinFpc, IgnoreText);
    if ((s2='skClass') and (s = 'skRecord')) or ((s='skClass') and (s2 = 'skRecord')) then begin
      TotalClassVsRecord := TotalClassVsRecord + 1;
    end;
  end else begin
    f2 := TestTrue(Name + ' Kind is "'+s2+'", failed: STATE was <'+dbgs(WV.Validity)+'>, HasTypeInfo='+dbgs(HasTpInfo)+' Val="'+s+'"', False, DataRes.MinGdb, DataRes.MinFpc, IgnoreText);
  end;

  if (not f2) and IgnoreAll then exit; // failed Data, do not list others as potential unexpected success

  // Check TypeName
  IgnoreText := '';    if IgnoreTpName then IgnoreText := 'Ignored by flag';
  if IsValid and HasTpInfo then begin
    s:='';
    if HasTpInfo then s := WV.TypeInfo.TypeName;
    CmpNames(Name+' TypeName', DataRes.ExpTypeName, s, fTpMtch  in DataRes.Flgs);
    //if fTpMtch  in DataRes.Flgs
    //then begin
    //  rx := TRegExpr.Create;
    //  rx.ModifierI := true;
    //  rx.Expression := DataRes.ExpTypeName;
    //  TestTrue(Name + ' TypeName matches '+DataRes.ExpTypeName+' but was '+s,  rx.Exec(s), DataRes.MinGdb, DataRes.MinFpc, IgnoreText);
    //  FreeAndNil(rx);
    // end
    // else TestEquals(Name + ' TypeName',  LowerCase(DataRes.ExpTypeName), LowerCase(s), DataRes.MinGdb, DataRes.MinFpc, IgnoreText);
  end else begin
      TestTrue(Name + ' TypeName matches '+DataRes.ExpTypeName+' but STATE was <'+dbgs(WV.Validity)+'> HasTypeInfo='+dbgs(HasTpInfo)+' Val="'+s+'"',  False, DataRes.MinGdb, DataRes.MinFpc, IgnoreText);
  end;


  MemberTests := DataRes.FullTypesExpect;
  if Length(MemberTests) > 0 then begin
    if HasTpInfo then begin
      for i := 0 to Length(MemberTests) - 1 do begin
        j := WV.TypeInfo.Fields.Count - 1;
        while (j >= 0) and (uppercase(WV.TypeInfo.Fields[j].Name) <> UpperCase(MemberTests[i].Name)) do dec(j);
        TestTrue(Name + ' no members with name ' +  MemberTests[i].Name,
                 (fTExpectNotFOund  in MemberTests[i].Flgs) <> (j >= 0),
                 DataRes.MinGdb, DataRes.MinFpc, IgnoreText);;
        if j >= 0 then begin
          fld := WV.TypeInfo.Fields[j];
          WriteStr(s, MemberTests[i].ExpKind);
          WriteStr(s2, fld.DBGType.Kind);
          if fld.DBGType <> nil then begin
            TestTrue(Name + ' members with name ' +  MemberTests[i].Name + ' type='
            + s + ' but was ' + s2,
                MemberTests[i].ExpKind = fld.DBGType.Kind, DataRes.MinGdb, DataRes.MinFpc, IgnoreText);;
            CmpNames(Name + ' members with name ' +  MemberTests[i].Name + 'TypeName',
                     MemberTests[i].ExpTypeName, fld.DBGType.TypeName, fTpMtch  in MemberTests[i].Flgs);
          end
          else
            TestTrue(Name + ' no dbgtype for members with name' +  MemberTests[i].Name, False, DataRes.MinGdb, DataRes.MinFpc, IgnoreText);;
        end;
      end;
    end
    else
      TestTrue(Name + ' no typeinfo for members' , False, DataRes.MinGdb, DataRes.MinFpc, IgnoreText);
  end;

end;

procedure TTestWatchesBase.AddWatches(ExpectList: TWatchExpectationArray; AWatches: TWatches;
  Only: Integer; OnlyName, OnlyNamePart: String);

  function SkipTest(const Data: TWatchExpectation): Boolean;
  begin
    Result := True;
    if Data.Result[SymbolType].Flgs * [fTstSkip, fTstSkipDwarf3] <> [] then exit;
    Result := False;
  end;

  function MatchOnly(const Data: TWatchExpectation; Idx: Integer): Boolean;
  begin
    Result := True;
    if ((Only >=0) and (Only <> Idx)) or
       ((OnlyName<>'') and (OnlyName <> Data.TestName)) or
       ((OnlyNamePart<>'') and (pos(OnlyNamePart, Data.TestName)<1))
    then Result := False;
  end;
var
  i: Integer;
begin
  for i := low(ExpectList) to high(ExpectList) do begin
    ExpectList[i].TheWatch := nil;
    if not MatchOnly(ExpectList[i], i) then continue;
    if not SkipTest(ExpectList[i]) then begin
      ExpectList[i].TheWatch := TTestWatch.Create(AWatches);
      ExpectList[i].TheWatch.Expression := ExpectList[i].Expression;
      ExpectList[i].TheWatch.DisplayFormat := ExpectList[i].DspFormat;
      ExpectList[i].TheWatch.RepeatCount := ExpectList[i].RepeatCount;
      ExpectList[i].TheWatch.EvaluateFlags:= ExpectList[i].EvaluateFlags;
      ExpectList[i].TheWatch.enabled := True;
    end;
  end;
end;

procedure TTestWatchesBase.TestWatchList(AName: String; ExpectList: TWatchExpectationArray;
  ADbg: TDebuggerIntf; Only: Integer; OnlyName,
  OnlyNamePart: String);

  function SkipTest(const Data: TWatchExpectation): Boolean;
  begin
    Result := True;
    if Data.Result[SymbolType].Flgs * [fTstSkip, fTstSkipDwarf3] <> [] then exit;
    Result := False;
  end;

  function MatchOnly(const Data: TWatchExpectation; Idx: Integer): Boolean;
  begin
    Result := True;
    if ((Only >=0) and (Only <> Idx)) or
       ((OnlyName<>'') and (OnlyName <> Data.TestName)) or
       ((OnlyNamePart<>'') and (pos(OnlyNamePart, Data.TestName)<1))
    then Result := False;
  end;
var
  i: Integer;
begin
  for i := low(ExpectList) to high(ExpectList) do begin
    if not MatchOnly(ExpectList[i], i) then continue;
    if ExpectList[i].TheWatch = nil then continue;
    if not SkipTest(ExpectList[i]) then
      TestWatch(AName + ' '+IntToStr(i)+' ', ADbg, ExpectList[i].TheWatch, ExpectList[i]);
  end;

end;


finalization
  FreeAndNil(Frx);

end.

