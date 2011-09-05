unit TestGdbType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, DebugUtils, GDBTypeInfo;

type

  { TTestGdbType }

  TTestGdbType = class(TTestCase)
  private
    FIgnoreBaseDeclaration: Boolean;
  published
    procedure TestPTypeParser;
    procedure TestUnEscape;
  end; 

implementation
const
  LN = LineEnding;

procedure TTestGdbType.TestPTypeParser;
const
  KindNames: Array [TGDBPTypeResultKind] of String =
    ('ptprkError', 'ptprkSimple', 'ptprkClass', 'ptprkRecord', 'ptprkEnum', 'ptprkSet', 'ptprkArray', 'ptprkProcedure', 'ptprkFunction');
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
          4: Result := Result + 'ptprNoBounds, ';
          else Result := Result + IntToStr(ord(i)) + ', ';
        end
    end;
  end;

  procedure CheckResult(TestName: string; TestRes: TGDBPTypeResult;
    ExpKind: TGDBPTypeResultKind;
    ExpHasFlags, ExpIgnoreFlags: TGDBPTypeResultFlags;
    ExpName, ExpDecl: string;
    ExpSubName: String = '';
    ExpLow: String = '';
    ExpHigh: String = '';
    ExpBaseDecl: String = ''
    );
  begin
    AssertEquals(TestName + ' Kind',          KindNames[ExpKind], KindNames[TestRes.Kind]);
    AssertEquals(TestName + ' Has Flags',     FlagsToString(ExpHasFlags), FlagsToString(TestRes.Flags * (AllFlags - ExpIgnoreFlags)));
    AssertEquals(TestName + ' Name',          ExpName, PCLenToString(TestRes.Name));
    AssertEquals(TestName + ' SubName',       ExpSubName, PCLenToString(TestRes.SubName));
    AssertEquals(TestName + ' Decl',          ExpDecl, PCLenToString(TestRes.Declaration));
    AssertEquals(TestName + ' Low',           ExpLow, PCLenToString(TestRes.BoundLow));
    AssertEquals(TestName + ' High',          ExpHigh, PCLenToString(TestRes.BoundHigh));
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

var
  R: TGDBPTypeResult;
begin
  (* Test with data captured from gdb *)
  FIgnoreBaseDeclaration := true;

  // dummy data
  R := ParseTypeFromGdb('type = char');
  CheckResult('Dumy: type=char', R, ptprkSimple, [], [], 'char', '');

  // dummy data
  R := ParseTypeFromGdb('type = ^char');
  CheckResult('Dumy: type=char', R, ptprkSimple, [ptprfPointer], [], '^char', '');


  (* simple type / aliases *)

  // <whatis        type = TDYNINTARRAY
  r := ParseTypeFromGdb('type = TDYNINTARRAY'+LN);
  CheckResult('type = TDYNINTARRAY', R, ptprkSimple, [], [], 'TDYNINTARRAY', '');

  // <whatis        type = TDYNINTARRAY+LN  trailing +LN
  r := ParseTypeFromGdb('type = TDYNINTARRAY'+LN);
  CheckResult('type = TDYNINTARRAY+LN', R, ptprkSimple, [], [], 'TDYNINTARRAY', '');

  // <whatis         type = ^TDYNINTARRAY
  r := ParseTypeFromGdb('type = ^TDYNINTARRAY'+LN);
  CheckResult('type = ^TDYNINTARRAY', R, ptprkSimple, [ptprfPointer], [], '^TDYNINTARRAY', '');

  // <whatis         type = &TDYNINTARRAY
  // NOTE: the & is still present in the type-name
  r := ParseTypeFromGdb('type = &TDYNINTARRAY'+LN);
  CheckResult('type = &TDYNINTARRAY', R, ptprkSimple, [ptprfParamByRef], [], '&TDYNINTARRAY', '');

  // <whatis         type = ^&TDYNINTARRAY
  // NOTE: the & is still present in the type-name
  r := ParseTypeFromGdb('type = ^&TDYNINTARRAY'+LN);
  CheckResult('type = ^&TDYNINTARRAY', R, ptprkSimple, [ptprfPointer, ptprfParamByRef], [], '^&TDYNINTARRAY', '');


  (* array *)
  FIgnoreBaseDeclaration := False;

  // <whatis VarDynIntArrayA>        type = array [0..-1] of LONGINT
  r := ParseTypeFromGdb('type = array [0..-1] of LONGINT'+LN);
  CheckResult('type = array [0..-1] of LONGINT', R, ptprkArray, [ptprfDynArray], [],
              '', 'array [0..-1] of LONGINT', 'LONGINT', '0', '-1');

  r := ParseTypeFromGdb('type = array [0..0] of LONGINT'+LN);
  CheckResult('type = array [0..0] of LONGINT', R, ptprkArray, [], [], '',
              'array [0..0] of LONGINT', 'LONGINT', '0', '0');

  r := ParseTypeFromGdb('type = array [0..2] of LONGINT'+LN);
  CheckResult('type = array [0..2] of LONGINT', R, ptprkArray, [], [], '',
              'array [0..2] of LONGINT', 'LONGINT', '0', '2');

  r := ParseTypeFromGdb('type = array [2..2] of TFOO'+LN);
  CheckResult('type = array [2..2] of TFOO', R, ptprkArray, [], [], '',
              'array [2..2] of TFOO', 'TFOO', '2', '2');

  r := ParseTypeFromGdb('type = array of LONGINT'+LN);
  CheckResult('type = array of LONGINT', R, ptprkArray, [ptprfDynArray, ptprfNoBounds], [], '',
              'array of LONGINT', 'LONGINT', '', '');

  // <whatis VarDynIntArrayA>        type = ^(array [0..-1] of LONGINT)
  r := ParseTypeFromGdb('type = ^(array [0..-1] of LONGINT)'+LN);
  CheckResult('type = ^(array [0..-1] of LONGINT)', R, ptprkArray, [ptprfPointer, ptprfDynArray], [],
              '', '^(array [0..-1] of LONGINT)', 'LONGINT', '0', '-1');

  r := ParseTypeFromGdb('type = ^(array [0..1] of LONGINT)'+LN);
  CheckResult('type = ^(array [0..1] of LONGINT)', R, ptprkArray, [ptprfPointer], [], '',
              '^(array [0..1] of LONGINT)', 'LONGINT', '0', '1');

  r := ParseTypeFromGdb('type = ^(array of LONGINT)'+LN);
  CheckResult('type = ^(array of )', R, ptprkArray, [ptprfPointer, ptprfDynArray, ptprfNoBounds], [],
              '', '^(array of LONGINT)', 'LONGINT', '', '');


  (* enum *)

  // <whatis ArgEnum>      type = TENUM  = (ONE, TWO, THREE)
  r := ParseTypeFromGdb('type = TENUM  = (ONE, TWO, THREE)'+LN);
  CheckResult('type = TENUM  = (ONE, TWO, THREE)', R, ptprkEnum, [], [],
              'TENUM', '(ONE, TWO, THREE)', '', '' ,'', '(ONE, TWO, THREE)');

  // <whatis ArgEnum>      type = ^TENUM  = (ONE, TWO, THREE)
  r := ParseTypeFromGdb('type = ^TENUM  = (ONE, TWO, THREE)'+LN);
  CheckResult('type = ^TENUM  = (ONE, TWO, THREE)', R, ptprkEnum, [ptprfPointer], [],
              '^TENUM', '(ONE, TWO, THREE)', '', '' ,'', '(ONE, TWO, THREE)');

  // <whatis VarEnumA>     type =  = (E1, E2, E3)
  r := ParseTypeFromGdb('type =  = (E1, E2, E3)'+LN);
  CheckResult('type =  = (E1, E2, E3)', R, ptprkEnum, [], [],
              '', '(E1, E2, E3)', '', '' ,'', '(E1, E2, E3)');

  // <whatis VarEnumA>     type = ^ = (E1, E2, E3)
  r := ParseTypeFromGdb('type = ^ = (E1, E2, E3)'+LN);
  CheckResult('type = ^ = (E1, E2, E3)', R, ptprkEnum, [ptprfPointer], [],
              '', '(E1, E2, E3)', '', '' ,'', '(E1, E2, E3)');


  (* set *)

  // type = set of TENUM
  r := ParseTypeFromGdb('type = set of TENUM'+LN);
  CheckResult('type = set of TENUM', R, ptprkSet, [], [], '', 'set of TENUM', 'TENUM');
  // type = ^set of TENUM
  r := ParseTypeFromGdb('type = ^set of TENUM'+LN);
  CheckResult('type = ^set of TENUM', R, ptprkSet, [ptprfPointer], [], '', '^set of TENUM', 'TENUM');

  // type = set of  = (...)
  r := ParseTypeFromGdb('type = set of  = (...)'+LN);
  CheckResult('type = set of  = (...)', R, ptprkSet, [], [],
              '', 'set of  = (...)', '(...)', '', '', 'set of  = (...)');
  // type = ^set of  = (...)
  r := ParseTypeFromGdb('type = ^set of  = (...)'+LN);
  CheckResult('type = ^set of  = (...)', R, ptprkSet, [ptprfPointer], [],
              '', '^set of  = (...)', '(...)', '', '', 'set of  = (...)');

  // type = set of  = (ALPHA, BETA, GAMMA)
  r := ParseTypeFromGdb('type = set of  = (ALPHA, BETA, GAMMA)'+LN);
  CheckResult('type = set of  = (ALPHA, BETA, GAMMA)', R, ptprkSet, [], [],
              '', 'set of  = (ALPHA, BETA, GAMMA)', '(ALPHA, BETA, GAMMA)', '', '', 'set of  = (ALPHA, BETA, GAMMA)');
  // type = ^set of  = (ALPHA, BETA, GAMMA)
  r := ParseTypeFromGdb('type = ^set of  = (ALPHA, BETA, GAMMA)'+LN);
  CheckResult('type = ^set of  = (ALPHA, BETA, GAMMA)', R, ptprkSet, [ptprfPointer], [],
              '', '^set of  = (ALPHA, BETA, GAMMA)', '(ALPHA, BETA, GAMMA)', '', '', 'set of  = (ALPHA, BETA, GAMMA)');

  // type = <invalid unnamed pascal type code 8>   ## Dwarf no sets
  r := ParseTypeFromGdb('type = <invalid unnamed pascal type code 8>'+LN);
  CheckResult('type = <invalid unnamed pascal type code 8>', R, ptprkSet, [], [], '', '');
  // type = ^<invalid unnamed pascal type code 8>
  r := ParseTypeFromGdb('type = ^<invalid unnamed pascal type code 8>'+LN);
  CheckResult('type = ^<invalid unnamed pascal type code 8>', R, ptprkSet, [ptprfPointer], [], '', '');

  // 'type = set of ONE..THREE'+LN   ## Dwarf gdb 7.0
  r := ParseTypeFromGdb('type = set of ONE..THREE'+LN);
  CheckResult('type = set of ONE..THREE'+LN, R, ptprkSet, [], [], '', 'set of ONE..THREE', 'ONE..THREE');
  // 'type = ^set of ONE..THREE'+LN
  r := ParseTypeFromGdb('type = ^set of ONE..THREE'+LN);
  CheckResult('type = ^set of ONE..THREE'+LN, R, ptprkSet, [ptprfPointer], [], '', '^set of ONE..THREE', 'ONE..THREE');
  // 'type = &set of ONE..THREE'+LN
  r := ParseTypeFromGdb('type = &set of ONE..THREE'+LN);
  CheckResult('type = &set of ONE..THREE'+LN, R, ptprkSet, [ptprfParamByRef], [], '', '&set of ONE..THREE', 'ONE..THREE');
  // 'type = ^&set of ONE..THREE'+LN
  r := ParseTypeFromGdb('type = ^&set of ONE..THREE'+LN);
  CheckResult('type = ^&set of ONE..THREE'+LN, R, ptprkSet, [ptprfPointer, ptprfParamByRef], [], '', '^&set of ONE..THREE', 'ONE..THREE');


  (* record *)
  FIgnoreBaseDeclaration := true;

  // type = TREC = record
  r := ParseTypeFromGdb('type = TREC = record '+LN + '    VALINT : LONGINT;'+LN + '    VALFOO : TFOO;'+LN + 'end'+LN);
  CheckResult('type = TREC = record \n + struct', R, ptprkRecord, [], [], 'TREC', 'record');
  // type = ^TREC = record
  r := ParseTypeFromGdb('type = ^TREC = record '+LN + '    VALINT : LONGINT;'+LN + '    VALFOO : TFOO;'+LN + 'end'+LN);
  CheckResult('type = ^TREC = record \n + struct', R, ptprkRecord, [ptprfPointer], [], '^TREC', 'record');
  // type = &TREC = record
  r := ParseTypeFromGdb('type = &TREC = record '+LN + '    VALINT : LONGINT;'+LN + '    VALFOO : TFOO;'+LN + 'end'+LN);
  CheckResult('type = &TREC = record \n + struct', R, ptprkRecord, [ptprfParamByRef], [], '&TREC', 'record');
  // type = ^&TREC = record
  r := ParseTypeFromGdb('type = ^&TREC = record '+LN + '    VALINT : LONGINT;'+LN + '    VALFOO : TFOO;'+LN + 'end'+LN);
  CheckResult('type = ^&TREC = record \n + struct', R, ptprkRecord, [ptprfPointer, ptprfParamByRef], [], '^&TREC', 'record');

  // type = record {...}
  r := ParseTypeFromGdb('type = record {...}'+LN);
  CheckResult('type = record {...}'+LN, R, ptprkRecord, [ptprfNoStructure], [], '', 'record');
  // type = ^record {...}
  r := ParseTypeFromGdb('type = ^record {...}'+LN);
  CheckResult('type = ^record {...}'+LN, R, ptprkRecord, [ptprfPointer, ptprfNoStructure], [], '', '^record');
  // type = record {...}+LN  trailing +LN
  r := ParseTypeFromGdb('type = record {...}'+LN);
  CheckResult('type = record {...}+LN', R, ptprkRecord, [ptprfNoStructure], [], '', 'record');

  // type = record \n + struct
  r := ParseTypeFromGdb('type = record '+LN + '    VAL : LONGINT;'+LN + 'end'+LN);
  CheckResult('type = record \n + struct', R, ptprkRecord, [], [], '', 'record');
  // type = ^record \n + struct
  r := ParseTypeFromGdb('type = ^record '+LN + '    VAL : LONGINT;'+LN + 'end'+LN);
  CheckResult('type = ^record \n + struct', R, ptprkRecord, [ptprfPointer], [], '', '^record');


  (* class *)

  // 'type = TFOO = class : public TOBJECT '+LN + '  public'+LN + '    VALUEINT : LONGINT;'+LN + 'end'+LN
  r := ParseTypeFromGdb('type = TFOO = class : public TOBJECT '+LN + '  public'+LN + '    VALUEINT : LONGINT;'+LN + 'end'+LN);
  CheckResult('type = TFOO = class : public TOBJECT '+LN, R, ptprkClass, [], [], 'TFOO', 'class : public TOBJECT');

  // 'type = ^TFOO = class : public TOBJECT '+LN + '  public'+LN + '    VALUEINT : LONGINT;'+LN + 'end'+LN
  r := ParseTypeFromGdb('type = ^TFOO = class : public TOBJECT '+LN + '  public'+LN + '    VALUEINT : LONGINT;'+LN + 'end'+LN);
  CheckResult('type = ^TFOO = class : public TOBJECT '+LN, R, ptprkClass, [ptprfPointer], [], '^TFOO', 'class : public TOBJECT');

  // 'type = &TFOO = class : public TOBJECT '+LN + '  public'+LN + '    VALUEINT : LONGINT;'+LN + 'end'+LN
  r := ParseTypeFromGdb('type = &TFOO = class : public TOBJECT '+LN + '  public'+LN + '    VALUEINT : LONGINT;'+LN + 'end'+LN);
  CheckResult('type = &TFOO = class : public TOBJECT '+LN, R, ptprkClass, [ptprfParamByRef], [], '&TFOO', 'class : public TOBJECT');

  // 'type = ^&TFOO = class : public TOBJECT '+LN + '  public'+LN + '    VALUEINT : LONGINT;'+LN + 'end'+LN
  r := ParseTypeFromGdb('type = ^&TFOO = class : public TOBJECT '+LN + '  public'+LN + '    VALUEINT : LONGINT;'+LN + 'end'+LN);
  CheckResult('type = ^&TFOO = class : public TOBJECT '+LN, R, ptprkClass, [ptprfPointer, ptprfParamByRef], [], '^&TFOO', 'class : public TOBJECT');

  // type = ^TFOO = class
  r := ParseTypeFromGdb('type = ^TFOO = class '+LN);
  CheckResult('type = ^TFOO = class ', R, ptprkClass, [ptprfPointer, ptprfNoStructure], [], '^TFOO', 'class');

  (* empty happens with @ArgProcedure *)

  // type = ^
  r := ParseTypeFromGdb('type = ^'+LN);
  CheckResult('type = ^', R, ptprkError, [ptprfPointer, ptprfEmpty], [], '', '');
  // type = &
  r := ParseTypeFromGdb('type = &'+LN);
  CheckResult('type = &', R, ptprkError, [ptprfParamByRef, ptprfEmpty], [], '', '');
  // type = ^&
  r := ParseTypeFromGdb('type = ^&'+LN);
  CheckResult('type = ^&', R, ptprkError, [ptprfPointer, ptprfParamByRef, ptprfEmpty], [], '', '');

  (* procedure *)

  // type = procedure
  r := ParseTypeFromGdb('type = procedure '+LN);
  CheckResult('type = procedure ', R, ptprkProcedure, [], [], '', 'procedure');
  // type = ^(procedure )
  r := ParseTypeFromGdb('type = ^(procedure )'+LN);
  CheckResult('type = ^(procedure )', R, ptprkProcedure, [ptprfPointer], [], '', '^(procedure )');

  // type = procedure (LONGINT, SHORTSTRING)
  r := ParseTypeFromGdb('type = procedure (LONGINT, SHORTSTRING)'+LN);
  CheckResult('type = procedure (LONGINT, SHORTSTRING)', R, ptprkProcedure, [], [], '', 'procedure (LONGINT, SHORTSTRING)');


  (* function *)

  // type = function
  r := ParseTypeFromGdb('type = function '+LN);
  CheckResult('type = function ', R, ptprkFunction, [], [], '', 'function');
  // type = ^(function )
  r := ParseTypeFromGdb('type = ^(function )'+LN);
  CheckResult('type = ^(function )', R, ptprkFunction, [ptprfPointer], [], '', '^(function )');

  // type = ^(function  ) : LONGINT
  r := ParseTypeFromGdb('type = ^(function  ) : LONGINT'+LN);
  CheckResult('type = ^(function  ) : LONGINT', R, ptprkFunction, [ptprfPointer], [], '', '^(function  ) : LONGINT');

  // type = function  (LONGINT, SHORTSTRING) : LONGINT
  r := ParseTypeFromGdb('type = function  (LONGINT, SHORTSTRING) : LONGINT'+LN);
  CheckResult('type = function  (LONGINT, SHORTSTRING) : LONGINT', R, ptprkFunction, [], [], '', 'function  (LONGINT, SHORTSTRING) : LONGINT');

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



initialization

  RegisterTest(TTestGdbType); 
end.

