unit TestGdbType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, DebugUtils, GDBTypeInfo, strutils, LCLProc;

type

  { TTestGdbType }

  TTestGdbType = class(TTestCase)
  private
    FIgnoreBaseDeclaration: Boolean;
  published
    procedure TestPTypeParser;
    procedure TestExpressionBreaker;
    procedure TestUnEscape;
    procedure TestConvertPascalExpression;
  end; 

implementation
const
  LN = LineEnding;

procedure TTestGdbType.TestPTypeParser;
const
  KindNames: Array [TGDBPTypeResultKind] of String =
    ('ptprkNotEvaluated', 'ptprkError', 'ptprkSimple', 'ptprkClass', 'ptprkRecord', 'ptprkEnum', 'ptprkSet', 'ptprkArray', 'ptprkProcedure', 'ptprkFunction');
  AllFlags = [low(TGDBPTypeResultFlags)..high(TGDBPTypeResultFlags)];

  function FlagsToString(f : TGDBPTypeResultFlags): string;
  var
    i: TGDBPTypeResultFlag;
  begin
    Result := '';
    for i := low(TGDBPTypeResultFlag) to high(TGDBPTypeResultFlag) do begin
      if i in f then
        case ord(i) of
          0: Result := Result + 'ptprfParamByRef, ';
          1: Result := Result + 'ptprfPointer, ';
          2: Result := Result + 'ptprfNoStructure, ';
          3: Result := Result + 'ptprfDynArray, ';
          4: Result := Result + 'ptprfNoBounds, ';
          5: Result := Result + 'ptprfEmpty, ';
          6: ; // ignore /decl-in-brackets
          else Result := Result + IntToStr(ord(i)) + ', ';
        end
    end;
  end;

  procedure CheckFlags(TestName: string; TestRes, ExpHasFlags, ExpIgnoreFlags: TGDBPTypeResultFlags);
  begin
    AssertEquals(TestName + ' Has Flags',     FlagsToString(ExpHasFlags), FlagsToString(TestRes * (AllFlags - ExpIgnoreFlags)));
  end;
  procedure CheckPCharWLen(TestName: string; TestRes: TPCharWithLen; ExpText: String);
  begin
    AssertEquals(TestName + ' Values', ExpText, PCLenToString(TestRes));
  end;

  procedure CheckResult(TestName: string; TestRes: TGDBPTypeResult;
    ExpKind: TGDBPTypeResultKind;
    ExpHasFlags, ExpIgnoreFlags: TGDBPTypeResultFlags;
    ExpName, ExpDecl: string;
    ExpSubName: String = '';
    ExpLow: String = '';
    ExpHigh: String = '';
    ExpBaseDecl: String = '';
    ExpSubKind: TGDBPTypeResultKind = ptprkSimple;
    ExpSubHasFlags: TGDBPTypeResultFlags = [];
    ExpSubIgnoreFlags: TGDBPTypeResultFlags = []
    );
  begin
    TestName := AnsiReplaceStr(TestName, #10, '#10');
    TestName := AnsiReplaceStr(TestName, #13, '#13');
    AssertEquals(TestName + ' Kind',          KindNames[ExpKind], KindNames[TestRes.Kind]);
    AssertEquals(TestName + ' Has Flags',     FlagsToString(ExpHasFlags), FlagsToString(TestRes.Flags * (AllFlags - ExpIgnoreFlags)));
    AssertEquals(TestName + ' Name',          ExpName, PCLenToString(TestRes.Name));
    AssertEquals(TestName + ' SubName',       ExpSubName, PCLenToString(TestRes.SubName));
    AssertEquals(TestName + ' Decl',          ExpDecl, PCLenToString(TestRes.Declaration));
    AssertEquals(TestName + ' Low',           ExpLow, PCLenToString(TestRes.BoundLow));
    AssertEquals(TestName + ' High',          ExpHigh, PCLenToString(TestRes.BoundHigh));
    if ExpSubName <> '' then begin
      AssertEquals(TestName + ' SubKind',          KindNames[ExpSubKind], KindNames[TestRes.SubKind]);
      AssertEquals(TestName + ' Has SubFlags',     FlagsToString(ExpSubHasFlags), FlagsToString(TestRes.SubFlags * (AllFlags - ExpSubIgnoreFlags)));
    end;

    while (ExpName <> '') and (ExpName[1] in ['^', '&']) do Delete(ExpName, 1, 1);
    while (ExpSubName <> '') and (ExpSubName[1] in ['^', '&']) do Delete(ExpSubName, 1, 1);
    while (ExpDecl <> '') and (ExpDecl[1] in ['(', '^', '&']) do Delete(ExpDecl, 1, 1);
    while (ExpDecl <> '') and (ExpDecl[length(ExpDecl)] in [')']) do Delete(ExpDecl, length(ExpDecl), 1);
    AssertEquals(TestName + ' BaseName',      ExpName, PCLenToString(TestRes.BaseName));
    AssertEquals(TestName + ' BaseSubName',   ExpSubName, PCLenToString(TestRes.BaseSubName));
    if FIgnoreBaseDeclaration then exit;
    if ExpBaseDecl <> '' then ExpDecl := ExpBaseDecl;
    AssertEquals(TestName + ' BaseDecl',      ExpDecl, PCLenToString(TestRes.BaseDeclaration));
  end;

  function CheckResult(TestName: string;
    ExpKind: TGDBPTypeResultKind;
    ExpHasFlags, ExpIgnoreFlags: TGDBPTypeResultFlags;
    ExpName, ExpDecl: string;
    ExpSubName: String = '';
    ExpLow: String = '';
    ExpHigh: String = '';
    ExpBaseDecl: String = '';
    ExpSubKind: TGDBPTypeResultKind = ptprkSimple;
    ExpSubHasFlags: TGDBPTypeResultFlags = [];
    ExpSubIgnoreFlags: TGDBPTypeResultFlags = []
    ): TGDBPTypeResult;
  begin
    Result := ParseTypeFromGdb(TestName);
    CheckResult(TestName, Result, ExpKind, ExpHasFlags, ExpIgnoreFlags, ExpName, ExpDecl,
      ExpSubName, ExpLow, ExpHigh, ExpBaseDecl, ExpSubKind, ExpSubHasFlags,
      ExpSubIgnoreFlags);
  end;

  function CheckArrayResult(TestName: string;
    ExpHasFlags, ExpIgnoreFlags: TGDBPTypeResultFlags;
    ExpName, ExpDecl: string;
    ExpSubName: String = '';
    ExpLow: String = '';
    ExpHigh: String = '';
    ExpBaseDecl: String = '';
    ExpSubKind: TGDBPTypeResultKind = ptprkSimple;
    ExpSubHasFlags: TGDBPTypeResultFlags = [];
    ExpSubIgnoreFlags: TGDBPTypeResultFlags = [];
    ExpNestCount: Integer = 0;
    ExpPointerCnt: Integer = 0
    ): TGDBPTypeResult;
  var
    TestRes: TGDBPTypeResult;
  begin
    Result := CheckResult(TestName, ptprkArray, ExpHasFlags, ExpIgnoreFlags, ExpName, ExpDecl,
      ExpSubName, ExpLow, ExpHigh, ExpBaseDecl, ExpSubKind, ExpSubHasFlags,
      ExpSubIgnoreFlags);
    AssertEquals(TestName + ' NestCount',      ExpNestCount, Result.NestArrayCount);
    AssertEquals(TestName + ' PointerCount',   ExpPointerCnt, Result.PointerCount);
  end;

var
  R: TGDBPTypeResult;
  T: String;
begin
  (* Test with data captured from gdb *)
  FIgnoreBaseDeclaration := true;

  // dummy data
  T := 'type = char';
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkSimple, [], [], 'char', 'char');

  // dummy data
  T := 'type = ^char';
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkSimple, [ptprfPointer], [], '^char', '^char');


  (* simple type / aliases *)

  // <whatis        type = TDYNINTARRAY
  T := 'type = TDYNINTARRAY';
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkSimple, [], [], 'TDYNINTARRAY', 'TDYNINTARRAY');

  // <whatis        type = TDYNINTARRAY+LN  trailing +LN
  T := 'type = TDYNINTARRAY'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkSimple, [], [], 'TDYNINTARRAY', 'TDYNINTARRAY');

  // <whatis         type = ^TDYNINTARRAY
  T := 'type = ^TDYNINTARRAY'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkSimple, [ptprfPointer], [], '^TDYNINTARRAY', '^TDYNINTARRAY');

  // <whatis         type = &TDYNINTARRAY
  // NOTE: the & is still present in the type-name
  T := 'type = &TDYNINTARRAY'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkSimple, [ptprfParamByRef], [], '&TDYNINTARRAY', '&TDYNINTARRAY');

  // <whatis         type = ^&TDYNINTARRAY
  // NOTE: the & is still present in the type-name
  T := 'type = ^&TDYNINTARRAY'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkSimple, [ptprfPointer, ptprfParamByRef], [], '^&TDYNINTARRAY', '^&TDYNINTARRAY');


  (* array *)
  FIgnoreBaseDeclaration := False;


  CheckArrayResult('type = array [0..-1] of LONGINT'+LN,                        // <whatis VarDynIntArrayA>        type = array [0..-1] of LONGINT
                   [ptprfDynArray], [],
                   '', 'array [0..-1] of LONGINT', 'LONGINT', '0', '-1');

  CheckArrayResult('type = array [0..0] of LONGINT'+LN,
                   [], [], '',
                   'array [0..0] of LONGINT', 'LONGINT', '0', '0');

  CheckArrayResult('type = array [0..2] of LONGINT'+LN,
                   [], [], '',
                   'array [0..2] of LONGINT', 'LONGINT', '0', '2');

  CheckArrayResult('type = array [2..2] of TFOO'+LN,
                   [], [], '',
                   'array [2..2] of TFOO', 'TFOO', '2', '2');


  CheckArrayResult('type = array of LONGINT'+LN,
                   [ptprfDynArray, ptprfNoBounds], [], '',
                   'array of LONGINT', 'LONGINT', '', '');


  CheckArrayResult('type = ^(array [0..-1] of LONGINT)'+LN,                       // <whatis VarDynIntArrayA>        type = ^(array [0..-1] of LONGINT)
                   [ptprfPointer, ptprfDynArray], [],
                   '', 'array [0..-1] of LONGINT', 'LONGINT', '0', '-1',
                   '', ptprkSimple, [], [], 0, 1);

  CheckArrayResult('type = ^(array [0..1] of LONGINT)'+LN,
                   [ptprfPointer], [], '',
                   'array [0..1] of LONGINT', 'LONGINT', '0', '1',
                   '', ptprkSimple, [], [], 0, 1);

  CheckArrayResult('type = ^(array of LONGINT)'+LN,
                   [ptprfPointer, ptprfDynArray, ptprfNoBounds], [],
                   '', 'array of LONGINT', 'LONGINT', '', '',
                   '', ptprkSimple, [], [], 0, 1);


  //// Element type => pointer
  CheckArrayResult('type = array [0..-1] of ^LONGINT'+LN,
                   [ptprfDynArray], [],
                   '', 'array [0..-1] of ^LONGINT', '^LONGINT', '0', '-1',
                   '', ptprkSimple, [ptprfPointer],[]);

  CheckArrayResult('type = array [0..0] of ^LONGINT'+LN,
                   [], [], '',
                   'array [0..0] of ^LONGINT', '^LONGINT', '0', '0',
                   '', ptprkSimple, [ptprfPointer],[]);

  CheckArrayResult('type = array [0..2] of ^LONGINT'+LN,
                   [], [], '',
                   'array [0..2] of ^LONGINT', '^LONGINT', '0', '2',
                   '', ptprkSimple, [ptprfPointer],[]);

  CheckArrayResult('type = array [2..2] of ^TFOO'+LN,
                   [], [], '',
                   'array [2..2] of ^TFOO', '^TFOO', '2', '2',
                   '', ptprkSimple, [ptprfPointer],[]);


  CheckArrayResult('type = array of ^LONGINT'+LN,
                   [ptprfDynArray, ptprfNoBounds], [], '',
                   'array of ^LONGINT', '^LONGINT', '', '',
                   '', ptprkSimple, [ptprfPointer],[]);


  CheckArrayResult('type = ^(array [0..-1] of ^LONGINT)'+LN,
                   [ptprfPointer, ptprfDynArray], [],
                   '', 'array [0..-1] of ^LONGINT', '^LONGINT', '0', '-1',
                   '', ptprkSimple, [ptprfPointer],[], 0, 1);

  CheckArrayResult('type = ^(array [0..1] of ^LONGINT)'+LN,
                   [ptprfPointer], [], '',
                   'array [0..1] of ^LONGINT', '^LONGINT', '0', '1',
                   '', ptprkSimple, [ptprfPointer],[], 0, 1);

  CheckArrayResult('type = ^(array of ^LONGINT)'+LN,
                   [ptprfPointer, ptprfDynArray, ptprfNoBounds], [],
                   '', 'array of ^LONGINT', '^LONGINT', '', '',
                   '', ptprkSimple, [ptprfPointer],[], 0, 1);


  //// Element type => RECORD
  T := 'record '+LN + '    VALINT : LONGINT;'+LN + 'end';
  CheckArrayResult('type = array [0..-1] of ' + T + LN,
                   [ptprfDynArray], [],
                   '', 'array [0..-1] of record', '', '0', '-1',
                   '', ptprkRecord, [],[]);

  CheckArrayResult('type = array [0..0] of '+T+LN,
                   [], [], '',
                   'array [0..0] of record', '', '0', '0',
                   '', ptprkRecord, [],[]);

  CheckArrayResult('type = array [0..2] of '+T+LN,
                   [], [], '',
                   'array [0..2] of record', '', '0', '2',
                   '', ptprkRecord, [],[]);

  CheckArrayResult('type = array [2..2] of '+T+LN,
                   [], [], '',
                   'array [2..2] of record', '', '2', '2',
                   '', ptprkRecord, [],[]);


  CheckArrayResult('type = array of '+T+LN,
                   [ptprfDynArray, ptprfNoBounds], [], '',
                   'array of record', '', '', '',
                   '', ptprkRecord, [],[]);


  CheckArrayResult('type = ^(array [0..-1] of '+T+')'+LN,
                   [ptprfPointer, ptprfDynArray], [],
                   '', 'array [0..-1] of record', '', '0', '-1',
                   '', ptprkRecord, [],[],0 ,1);

  CheckArrayResult('type = ^(array [0..1] of '+T+')'+LN,
                   [ptprfPointer], [], '',
                   'array [0..1] of record', '', '0', '1',
                   '', ptprkRecord, [],[], 0, 1);

  CheckArrayResult('type = ^(array of '+T+')'+LN,
                   [ptprfPointer, ptprfDynArray, ptprfNoBounds], [],
                   '', 'array of record', '', '', '',
                   '', ptprkRecord, [],[], 0, 1);

  // nested array
  R := CheckArrayResult('type = ^(array [0..-1] of ^(array [0..-1] of ^(array [0..-1] of TRECFORARRAY1)))'+LN,
                   [ptprfPointer, ptprfDynArray], [],
                   '', 'array [0..-1] of ^(array [0..-1] of ^(array [0..-1] of TRECFORARRAY1))',
                   'TRECFORARRAY1', '0', '-1',
                   'array [0..-1] of ^(array [0..-1] of ^(array [0..-1] of TRECFORARRAY1))',
                   //'', 'array of record', '', '', '',
                   ptprkSimple, [],[],
                   2, 1);

  CheckFlags('NestArr1 :[1]' , R.NestArray[1].Flags, [ptprfPointer, ptprfDynArray], []);
  CheckPCharWLen('NestArr1 :[1] low' ,  R.NestArray[1].BoundLow, '0');
  CheckPCharWLen('NestArr1 :[1] high' , R.NestArray[1].BoundHigh, '-1');

  CheckFlags('NestArr1 :[0]' , R.NestArray[0].Flags, [ptprfPointer, ptprfDynArray], []);
  CheckPCharWLen('NestArr1 :[0] low' ,  R.NestArray[0].BoundLow, '0');
  CheckPCharWLen('NestArr1 :[0] high' , R.NestArray[0].BoundHigh, '-1');


  R := CheckArrayResult('type = ^(array [0..-1] of array [0..3] of ^(array [0..-1] of TRECFORARRAY1))'+LN,
                   [ptprfPointer, ptprfDynArray], [],
                   '', 'array [0..-1] of array [0..3] of ^(array [0..-1] of TRECFORARRAY1)',
                   'TRECFORARRAY1', '0', '-1',
                   'array [0..-1] of array [0..3] of ^(array [0..-1] of TRECFORARRAY1)',
                   //'', 'array of record', '', '', '',
                   ptprkSimple, [],[],
                   2, 1);

  CheckFlags('NestArr2 :[1]' , R.NestArray[1].Flags, [], []);
  CheckPCharWLen('NestArr2 :[1] low' ,  R.NestArray[1].BoundLow, '0');
  CheckPCharWLen('NestArr2 :[1] high' , R.NestArray[1].BoundHigh, '3');

  CheckFlags('NestArr2 :[0]' , R.NestArray[0].Flags, [ptprfPointer, ptprfDynArray], []);
  CheckPCharWLen('NestArr2 :[0] low' ,  R.NestArray[0].BoundLow, '0');
  CheckPCharWLen('NestArr2 :[0] high' , R.NestArray[0].BoundHigh, '-1');


  R := CheckArrayResult('type = array [3..5] of ^(array of TRECFORARRAY1)'+LN,
                   [], [],
                   '', 'array [3..5] of ^(array of TRECFORARRAY1)',
                   'TRECFORARRAY1', '3', '5',
                   'array [3..5] of ^(array of TRECFORARRAY1)',
                   ptprkSimple, [],[],
                   1, 0);

  CheckFlags('NestArr3 :[0]' , R.NestArray[0].Flags, [ptprfPointer, ptprfDynArray, ptprfNoBounds], []);
  CheckPCharWLen('NestArr3 :[0] low' ,  R.NestArray[0].BoundLow, '');
  CheckPCharWLen('NestArr3 :[0] high' , R.NestArray[0].BoundHigh, '');
  AssertEquals('NestArr3 : PointerCount',   1, R.NestArray[0].PointerCount);



  CheckArrayResult('type = ^^(array [0..-1] of TRECFORARRAY1)'+LN,
                   [ptprfPointer, ptprfDynArray], [],
                   '', 'array [0..-1] of TRECFORARRAY1', 'TRECFORARRAY1', '0', '-1',
                   'array [0..-1] of TRECFORARRAY1', ptprkSimple, [],[], 0, 2);


  (* enum *)

  // <whatis ArgEnum>      type = TENUM  = (ONE, TWO, THREE)
  T := 'type = TENUM  = (ONE, TWO, THREE)'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkEnum, [], [],
              'TENUM', '(ONE, TWO, THREE)', '', '' ,'', '(ONE, TWO, THREE)');

  // <whatis ArgEnum>      type = ^TENUM  = (ONE, TWO, THREE)
  T := 'type = ^TENUM  = (ONE, TWO, THREE)'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkEnum, [ptprfPointer], [],
              '^TENUM', '(ONE, TWO, THREE)', '', '' ,'', '(ONE, TWO, THREE)');

  // <whatis VarEnumA>     type =  = (E1, E2, E3)
  T := 'type =  = (E1, E2, E3)'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkEnum, [], [],
              '', '(E1, E2, E3)', '', '' ,'', '(E1, E2, E3)');

  // <whatis VarEnumA>     type = ^ = (E1, E2, E3)
  T := 'type = ^ = (E1, E2, E3)'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkEnum, [ptprfPointer], [],
              '', '(E1, E2, E3)', '', '' ,'', '(E1, E2, E3)');


  (* set *)

  // type = set of TENUM
  T := 'type = set of TENUM'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkSet, [], [], '', 'set of TENUM', 'TENUM');
  // type = ^set of TENUM
  T := 'type = ^set of TENUM'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkSet, [ptprfPointer], [], '', '^set of TENUM', 'TENUM');

  // type = set of  = (...)
  T := 'type = set of  = (...)'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkSet, [], [],
              '', 'set of  = (...)', '(...)', '', '', 'set of  = (...)');
  // type = ^set of  = (...)
  T := 'type = ^set of  = (...)'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkSet, [ptprfPointer], [],
              '', '^set of  = (...)', '(...)', '', '', 'set of  = (...)');

  // type = set of  = (ALPHA, BETA, GAMMA)
  T := 'type = set of  = (ALPHA, BETA, GAMMA)'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkSet, [], [],
              '', 'set of  = (ALPHA, BETA, GAMMA)', '(ALPHA, BETA, GAMMA)', '', '', 'set of  = (ALPHA, BETA, GAMMA)');
  // type = ^set of  = (ALPHA, BETA, GAMMA)
  T := 'type = ^set of  = (ALPHA, BETA, GAMMA)'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkSet, [ptprfPointer], [],
              '', '^set of  = (ALPHA, BETA, GAMMA)', '(ALPHA, BETA, GAMMA)', '', '', 'set of  = (ALPHA, BETA, GAMMA)');

  // type = <invalid unnamed pascal type code 8>   ## Dwarf no sets
  T := 'type = <invalid unnamed pascal type code 8>'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkSet, [], [], '', '');
  // type = ^<invalid unnamed pascal type code 8>
  T := 'type = ^<invalid unnamed pascal type code 8>'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkSet, [ptprfPointer], [], '', '');

  // 'type = set of ONE..THREE'+LN   ## Dwarf gdb 7.0
  T := 'type = set of ONE..THREE'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T+LN, R, ptprkSet, [], [], '', 'set of ONE..THREE', 'ONE..THREE');
  // 'type = ^set of ONE..THREE'+LN
  T := 'type = ^set of ONE..THREE'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T+LN, R, ptprkSet, [ptprfPointer], [], '', '^set of ONE..THREE', 'ONE..THREE');
  // 'type = &set of ONE..THREE'+LN
  T := 'type = &set of ONE..THREE'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T+LN, R, ptprkSet, [ptprfParamByRef], [], '', '&set of ONE..THREE', 'ONE..THREE');
  // 'type = ^&set of ONE..THREE'+LN
  T := 'type = ^&set of ONE..THREE'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T+LN, R, ptprkSet, [ptprfPointer, ptprfParamByRef], [], '', '^&set of ONE..THREE', 'ONE..THREE');


  (* record *)
  FIgnoreBaseDeclaration := true;

  // type = TREC = record
  T := 'type = TREC = record '+LN + '    VALINT : LONGINT;'+LN + '    VALFOO : TFOO;'+LN + 'end'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkRecord, [], [], 'TREC', 'record');
  // type = ^TREC = record
  T := 'type = ^TREC = record '+LN + '    VALINT : LONGINT;'+LN + '    VALFOO : TFOO;'+LN + 'end'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkRecord, [ptprfPointer], [], '^TREC', 'record');
  // type = &TREC = record
  T := 'type = &TREC = record '+LN + '    VALINT : LONGINT;'+LN + '    VALFOO : TFOO;'+LN + 'end'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkRecord, [ptprfParamByRef], [], '&TREC', 'record');
  // type = ^&TREC = record
  T := 'type = ^&TREC = record '+LN + '    VALINT : LONGINT;'+LN + '    VALFOO : TFOO;'+LN + 'end'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkRecord, [ptprfPointer, ptprfParamByRef], [], '^&TREC', 'record');

  // type = record {...}
  T := 'type = record {...}'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T+LN, R, ptprkRecord, [ptprfNoStructure], [], '', 'record');
  // type = ^record {...}
  T := 'type = ^record {...}'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T+LN, R, ptprkRecord, [ptprfPointer, ptprfNoStructure], [], '', '^record');
  // type = record {...}+LN  trailing +LN
  T := 'type = record {...}'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkRecord, [ptprfNoStructure], [], '', 'record');

  // type = record \n + struct
  T := 'type = record '+LN + '    VAL : LONGINT;'+LN + 'end'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkRecord, [], [], '', 'record');
  // type = ^record \n + struct
  T := 'type = ^record '+LN + '    VAL : LONGINT;'+LN + 'end'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkRecord, [ptprfPointer], [], '', '^record');


  (* class *)

  // 'type = TFOO = class : public TOBJECT '+LN + '  public'+LN + '    VALUEINT : LONGINT;'+LN + 'end'+LN
  T := 'type = TFOO = class : public TOBJECT '+LN + '  public'+LN + '    VALUEINT : LONGINT;'+LN + 'end'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T+LN, R, ptprkClass, [], [], 'TFOO', 'class : public TOBJECT');

  // 'type = ^TFOO = class : public TOBJECT '+LN + '  public'+LN + '    VALUEINT : LONGINT;'+LN + 'end'+LN
  T := 'type = ^TFOO = class : public TOBJECT '+LN + '  public'+LN + '    VALUEINT : LONGINT;'+LN + 'end'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T+LN, R, ptprkClass, [ptprfPointer], [], '^TFOO', 'class : public TOBJECT');

  // 'type = &TFOO = class : public TOBJECT '+LN + '  public'+LN + '    VALUEINT : LONGINT;'+LN + 'end'+LN
  T := 'type = &TFOO = class : public TOBJECT '+LN + '  public'+LN + '    VALUEINT : LONGINT;'+LN + 'end'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T+LN, R, ptprkClass, [ptprfParamByRef], [], '&TFOO', 'class : public TOBJECT');

  // 'type = ^&TFOO = class : public TOBJECT '+LN + '  public'+LN + '    VALUEINT : LONGINT;'+LN + 'end'+LN
  T := 'type = ^&TFOO = class : public TOBJECT '+LN + '  public'+LN + '    VALUEINT : LONGINT;'+LN + 'end'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T+LN, R, ptprkClass, [ptprfPointer, ptprfParamByRef], [], '^&TFOO', 'class : public TOBJECT');

  // type = ^TOBJECT = class
  T := 'type = ^TOBJECT = class '+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkClass, [ptprfPointer, ptprfNoStructure], [], '^TOBJECT', 'class');

  (* empty happens with @ArgProcedure *)

  // type = ^
  T := 'type = ^'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkError, [ptprfPointer, ptprfEmpty], [], '', '');
  // type = &
  T := 'type = &'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkError, [ptprfParamByRef, ptprfEmpty], [], '', '');
  // type = ^&
  T := 'type = ^&'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkError, [ptprfPointer, ptprfParamByRef, ptprfEmpty], [], '', '');

  (* procedure *)

  // type = procedure
  T := 'type = procedure '+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkProcedure, [], [], '', 'procedure');
  // type = ^(procedure )
  T := 'type = ^(procedure )'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkProcedure, [ptprfPointer], [], '', 'procedure');

  // type = procedure (LONGINT, SHORTSTRING)
  T := 'type = procedure (LONGINT, SHORTSTRING)'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkProcedure, [], [], '', 'procedure (LONGINT, SHORTSTRING)');


  (* function *)

  // type = function
  T := 'type = function '+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkFunction, [], [], '', 'function');
  // type = ^(function )
  T := 'type = ^(function )'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkFunction, [ptprfPointer], [], '', 'function');

  // type = ^(function  ) : LONGINT
  T := 'type = ^(function  ) : LONGINT'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkFunction, [ptprfPointer], [], '', 'function  ) : LONGINT');

  // type = function  (LONGINT, SHORTSTRING) : LONGINT
  T := 'type = function  (LONGINT, SHORTSTRING) : LONGINT'+LN;
  R := ParseTypeFromGdb(T);
  CheckResult(T, R, ptprkFunction, [], [], '', 'function  (LONGINT, SHORTSTRING) : LONGINT');

end;

procedure DumpGExp(e: TGDBExpressionPart; pre: string = '');
var
  i: Integer;
begin
  debugln([pre, e.ClassName, ' cnt=', e.PartCount, ' txt="', e.Text, '"']);
  for i := 0 to e.PartCount - 1 do
    DumpGExp(e.Parts[i], pre+'  ');
end;

procedure DumpReq(r: PGDBPTypeRequest);
begin
  while r <> nil do begin
    debugln([' Req=', dbgs(r^)]);
    r := r^.Next;
  end;
end;

procedure TTestGdbType.TestExpressionBreaker;
  procedure AssertEqualsLc(n,s1,s2: string);
  begin
    AssertEquals(n, LowerCase(s1), LowerCase(s2));
  end;

  procedure InitExpr(e: String; var b: TGDBExpression; out r: PGDBPTypeRequest; out v: Boolean; rewritten: string = '');
  begin
    FreeAndNil(b);
    r := nil;
    b := TGDBExpression.Create(e);
    debugln('##### '+e);
    DumpGExp(b);
    if rewritten <> ''
    then AssertEqualsLc(e+' as text', rewritten, b.Text)
    else AssertEqualsLc(e+' as text', e, b.Text);
    v := b.NeedValidation(r);
    if r <> nil then DumpReq(r);
    debugln;

  end;
  procedure ContinueExpr(b: TGDBExpression; out r: PGDBPTypeRequest; out v: Boolean);
  begin
    r := nil;
    v := b.NeedValidation(r);
    if r <> nil then DumpReq(r);
    debugln;
  end;

var
  b: TGDBExpression;
  r: PGDBPTypeRequest;
  v: Boolean;
  r2: PGDBPTypeRequest;
  n: String;
begin
  b := nil;
  // with need for typecast
  n := 'abc[123].x';
  InitExpr(n, b, r, v);
  AssertTrue(n + ' is array', b.Parts[0] is TGDBExpressionPartArray);
  AssertTrue(n + ' ptype', (r <> nil) and (LowerCase(r^.Request) = 'ptype abc'));

  r^.Result := ParseTypeFromGdb('type = ^(array [0..-1] of TFoo)');
  ContinueExpr(b, r, v);
  AssertTrue(n + ' Needexp after dyn array', not v);
  AssertEqualsLc(n + ' text after dyn array', 'TFoo(abc^[123]).x', b.Text);

  InitExpr(n, b, r, v);
  r^.Result := ParseTypeFromGdb('type = array [0..1000] of TFoo');
  ContinueExpr(b, r, v);
  AssertTrue(n + ' Needexp after stat array', not v);
  AssertEqualsLc(n + ' text after stat array', 'TFoo(abc[123]).x', b.Text);

  InitExpr(n, b, r, v);
  r^.Result := ParseTypeFromGdb('type = ^TFoo');
  ContinueExpr(b, r, v);
  AssertTrue(n + ' Needexp after dyn array(dwarf)', v);
  r^.Result := ParseTypeFromGdb('type = array of TFoo');
  ContinueExpr(b, r, v);
  AssertTrue(n + ' Needexp after dyn array(dwarf)', not v);
  AssertEqualsLc(n + ' text after dyn array(dwarf)', 'TFoo(abc^[123]).x', b.Text);


  InitExpr(n, b, r, v);
  r^.Result := ParseTypeFromGdb('type = array [0..1000] of ^TFoo');
  ContinueExpr(b, r, v);
  AssertTrue(n + ' Needexp after stat array ^TFoo', not v);
  AssertEqualsLc(n + ' text after stat array ^TFoo', '^TFoo(abc[123]).x', b.Text);

  // with NO need for typecast
  n := 'abc[123]';
  InitExpr(n, b, r, v);
  r^.Result := ParseTypeFromGdb('type = ^(array [0..-1] of TFoo)');
  ContinueExpr(b, r, v);
  AssertTrue(n + ' Needexp after dyn array', not v);
  AssertEqualsLc(n + ' text after dyn array', 'abc^[123]', b.Text);

  n := 'a(abc[123])';
  InitExpr(n, b, r, v);
  r^.Result := ParseTypeFromGdb('type = ^(array [0..-1] of TFoo)');
  ContinueExpr(b, r, v);
  AssertTrue(n + ' Needexp after dyn array', not v);
  AssertEqualsLc(n + ' text after dyn array', 'a(abc^[123])', b.Text);

  n := 'abc[123]+1';
  InitExpr(n, b, r, v);
  r^.Result := ParseTypeFromGdb('type = ^(array [0..-1] of TFoo)');
  ContinueExpr(b, r, v);
  AssertTrue(n + ' Needexp after dyn array', not v);
  AssertEqualsLc(n + ' text after dyn array', 'abc^[123]+1', b.Text);

  // multi index

  n := 'abc[123][456]';
  InitExpr(n, b, r, v);
  AssertTrue(n + ' ptype 1', (r <> nil) and (LowerCase(r^.Request) = 'ptype abc'));
  r^.Result := ParseTypeFromGdb('type = ^(array [0..-1] of TFooA)');
  ContinueExpr(b, r, v);
  AssertTrue(n + ' Needexp after dyn array 1', v);
  //AssertTrue(n + ' ptype 2', (r <> nil) and (r^.Request = 'ptype TFooA(abc^[123])'));
  AssertTrue(n + ' ptype 2', (r <> nil) and (LowerCase(r^.Request) = 'ptype abc^[0]'));
  r^.Result := ParseTypeFromGdb('type = ^(array [0..-1] of TFoo)');
  ContinueExpr(b, r, v);
  AssertTrue(n + ' Needexp after dyn array 2', not v);
  //AssertEqualsLc(n + ' text after dyn array 2', 'TFooA(abc^[123])^[456]', b.Text);
  AssertEqualsLc(n + ' text after dyn array 2', 'abc^[123]^[456]', b.Text);

  n := 'abc[123][456]';
  InitExpr(n, b, r, v);
  r^.Result := ParseTypeFromGdb('type = array [3..1000] of TFooA');
  ContinueExpr(b, r, v);
  AssertTrue(n + ' Needexp after stat array 1', v);
  //AssertTrue(n + ' ptype 2a', (r <> nil) and (LowerCase(r^.Request) = 'ptype TFooA(abc[123])'));
  AssertTrue(n + ' ptype 2a', (r <> nil) and (LowerCase(r^.Request) = 'ptype abc[3]'));
  r^.Result := ParseTypeFromGdb('type = ^(array [0..-1] of TFoo)');
  ContinueExpr(b, r, v);
  AssertTrue(n + ' Needexp after stat,dyn array 2', not v);
  //AssertEqualsLc(n + ' text after dyn array 2', 'TFooA(abc[123])^[456]', b.Text);
  AssertEqualsLc(n + ' text after dyn array 2', 'abc[123]^[456]', b.Text);

  n := 'abc[123][456]';
  InitExpr(n, b, r, v);
  r^.Result := ParseTypeFromGdb('type = ^(array [0..-1] of TFooA)');
  ContinueExpr(b, r, v);
  AssertTrue(n + ' Needexp after dyn array 1', v);
  //AssertTrue(n + ' ptype 2b', (r <> nil) and (LowerCase(r^.Request) = 'ptype TFooA(abc^[123])'));
  AssertTrue(n + ' ptype 2b', (r <> nil) and (LowerCase(r^.Request) = 'ptype abc^[0]'));
  r^.Result := ParseTypeFromGdb('type = array [0..1000] of TFoo');
  ContinueExpr(b, r, v);
  AssertTrue(n + ' Needexp after dyn,stat array 2', not v);
  //AssertEqualsLc(n + ' text after dyn array 2', 'TFooA(abc^[123])[456]', b.Text);
  AssertEqualsLc(n + ' text after dyn array 2', 'abc^[123][456]', b.Text);



  //TODO
//CheckArrayResult('type = ^^(array [0..-1] of TRECFORARRAY1)'+LN,
  n := 'qwe[1][2]';
  InitExpr(n, b, r, v);
  r^.Result := ParseTypeFromGdb('type = &^(array [0..-1] of TRECFORARRAY1)');
  ContinueExpr(b, r, v);
  AssertTrue(n + 'Need expr, after ^^ dyn array 1', v);
  r^.Result := ParseTypeFromGdb('type = ^^(array [0..-1] of TRECFORARRAY1)');
  ContinueExpr(b, r, v);
  AssertTrue(n + 'No Need expr, after ^^ dyn array 1', not v);
  AssertEqualsLc(n + ' text after ^^ dyn array', '^^TRECFORARRAY1(qwe)[1][2]', b.Text);

  n := 'qwe[3][2].a';
  InitExpr(n, b, r, v);
  AssertTrue(n + 'Need expr, after stat dyn array 1', v);
  r^.Result := ParseTypeFromGdb('type = array [3..5] of ^(array of TRECFORARRAY1)');
  ContinueExpr(b, r, v);
  AssertTrue(n + 'No Need expr, after stat dyn array 1', not v);
  AssertEqualsLc(n + ' text after stat dyn array', 'TRECFORARRAY1(qwe[3]^[2]).a', b.Text);


  n := 'qwe[3]';
  InitExpr(n, b, r, v);
  AssertTrue(n + 'Need expr 1', v);
  r^.Result := ParseTypeFromGdb('type = ^char');
  ContinueExpr(b, r, v);
  AssertTrue(n + 'Need expr 2', v);
  r^.Result := ParseTypeFromGdb('type = char');
  ContinueExpr(b, r, v);
  AssertTrue(n + 'No Need expr', not v);
  AssertEqualsLc(n + ' text after', 'qwe[3]', b.Text);
  AssertEquals(n + ' maybe string ', True, b.MayNeedStringFix);
  AssertEqualsLc(n + ' text after as string', 'qwe[3-1]', b.TextStrFixed);


  n := 'abc()[123]';
  InitExpr(n, b, r, v);
  AssertTrue(n + ' is array', b.Parts[0] is TGDBExpressionPartArray);
  AssertTrue(n + ' ptype', (r <> nil) and (LowerCase(r^.Request) = 'ptype abc()'));


  n := 'abc(x[1])[123]';
  InitExpr(n, b, r, v);
  AssertTrue(n + ' is array', b.Parts[0] is TGDBExpressionPartArray);
  AssertTrue(n + ' r.next', (r <> nil) and (r^.Next = nil));
  AssertTrue(n + ' ptype', (r <> nil) and (LowerCase(r^.Request) = 'ptype x')); // cant eval the outer array yet

  r^.Result := ParseTypeFromGdb('type = array [0..1000] of TFoo');
  ContinueExpr(b, r, v);
  AssertTrue(n + ' ptype outer ', (r <> nil) and (LowerCase(r^.Request) = 'ptype abc(x[0])'));


  n := 'abc()[x[123]]';
  InitExpr(n, b, r, v);
  AssertTrue(n + ' is array', b.Parts[0] is TGDBExpressionPartArray);
  AssertTrue(n + ' r.next', (r <> nil)); // and (r^.Next <> nil));
  //r2 := r^.Next;
  //AssertTrue(n + ' ptype', (r <> nil) and ((r^.Request = 'ptype abc()') or (r2^.Request = 'ptype abc()')));
  //AssertTrue(n + ' ptype2', (r <> nil) and ((r^.Request = 'ptype x') or (r2^.Request = 'ptype x')));


  n := 'Cast(foo^.bar[1][foo[2]]+Call[x()]((f+1)^))+bar(1).z.x[1](m)(n)';
  InitExpr(n, b, r, v);

  n := 'abc[1,2,3].x[1]';
  InitExpr(n, b, r, v, 'abc[1][2][3].x[1]');
  AssertEqualsLc(n+' toSkipArrayIdx', 'abc[0][0][0].x[0]', b.TextEx[[toSkipArrayIdx]]);

  n := 'abc[1,2,3].x and abc[1,2][3].y';
  InitExpr(n, b, r, v, 'abc[1][2][3].x and abc[1][2][3].y');

  n := '1+abc[1,2,3].x and b . cc [ 1 , 2 or x ]';
  InitExpr(n, b, r, v, '1+abc[1][2][3].x and b . cc [ 1 ][ 2 or x ]');


  b.Free;
end;

procedure TTestGdbType.TestUnEscape;
begin
  AssertEquals('a\102c', 'aBc',            UnEscapeBackslashed('a\102c', [uefOctal]));
  AssertEquals('4:a\tc', 'a   c',         UnEscapeBackslashed('a\tc',    [uefTab], 4));
  AssertEquals('4:a\t\tc', 'a       c',   UnEscapeBackslashed('a\t\tc',  [uefTab], 4));
  AssertEquals('6:a\tc', 'a     c',       UnEscapeBackslashed('a\tc',    [uefTab], 6));
  AssertEquals('4:a\102\tc\\d', 'aB  c\d', UnEscapeBackslashed('a\102\tc\\d', [uefOctal, uefTab],4));
  AssertEquals('a\102\tc\\d', 'aB\tc\d',   UnEscapeBackslashed('a\102\tc\\d', [uefOctal],4));
  AssertEquals('4:a\102\tc\\d (no oct)', 'a\102   c\d', UnEscapeBackslashed('a\102\tc\\d', [uefTab],4));
end;

procedure TTestGdbType.TestConvertPascalExpression;
  procedure AssertEqualsConv(n, s1, s2: string);
  begin
    AssertTrue(n, ConvertPascalExpression(s2));
    AssertEquals(n, s1, s2);
  end;
begin
  AssertEqualsConv('', 'a=1', 'a=1');
  AssertEqualsConv('', 'a="11"', 'a=''11''');
  AssertEqualsConv('', 'a="1\0471"', 'a=''1''''1''');
  AssertEqualsConv('', 'a="11"', 'a="11"');
  AssertEqualsConv('', 'a="11\061"', 'a=''11''#49');
  AssertEqualsConv('', 'a="1\061"', 'a=''1''#49');

  AssertEqualsConv('', 'a=''1''', 'a=''1''');
end;



initialization

  RegisterTest(TTestGdbType); 
end.

