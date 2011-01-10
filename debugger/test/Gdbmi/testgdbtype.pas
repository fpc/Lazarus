unit TestGdbType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, DebugUtils, GDBTypeInfo;

type

  TTestGdbType = class(TTestCase)
  published
    procedure TestPTypeParser;
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
          else Result := Result + IntToStr(ord(i)) + ', ';
        end
    end;
  end;

  procedure CheckResult(TestName: string; TestRes: TGDBPTypeResult;
    ExpKind: TGDBPTypeResultKind;
    ExpHasFlags, ExpIgnoreFlags: TGDBPTypeResultFlags;
    ExpName, ExpDecl: string);
  begin
    AssertEquals(TestName + ' Kind',          KindNames[ExpKind], KindNames[TestRes.Kind]);
    AssertEquals(TestName + ' Has Flags',     FlagsToString(ExpHasFlags), FlagsToString(TestRes.Flags * (AllFlags - ExpIgnoreFlags)));
    AssertEquals(TestName + ' Name',          ExpName, PCLenToString(TestRes.Name));
    AssertEquals(TestName + ' Decl',          ExpDecl, PCLenToString(TestRes.Declaration));
    while (ExpName <> '') and (ExpName[1] in ['^', '&']) do Delete(ExpName, 1, 1);
    AssertEquals(TestName + ' BaseName',      ExpName, PCLenToString(TestRes.BaseName));
  end;

var
  R: TGDBPTypeResult;
begin
  (* Test with data captured from gdb *)

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

  // <whatis VarDynIntArrayA>        type = array [0..-1] of LONGINT
  r := ParseTypeFromGdb('type = array [0..-1] of LONGINT'+LN);
  CheckResult('type = array [0..-1] of LONGINT', R, ptprkArray, [], [], '', 'array [0..-1] of LONGINT');

  // <whatis VarDynIntArrayA>        type = ^(array [0..-1] of LONGINT)
  r := ParseTypeFromGdb('type = ^(array [0..-1] of LONGINT)'+LN);
  CheckResult('type = ^(array [0..-1] of LONGINT)', R, ptprkArray, [ptprfPointer], [], '', '^(array [0..-1] of LONGINT)');


  (* enum *)

  // <whatis ArgEnum>      type = TENUM  = (ONE, TWO, THREE)
  r := ParseTypeFromGdb('type = TENUM  = (ONE, TWO, THREE)'+LN);
  CheckResult('type = TENUM  = (ONE, TWO, THREE)', R, ptprkEnum, [], [], 'TENUM', '(ONE, TWO, THREE)');

  // <whatis ArgEnum>      type = ^TENUM  = (ONE, TWO, THREE)
  r := ParseTypeFromGdb('type = ^TENUM  = (ONE, TWO, THREE)'+LN);
  CheckResult('type = ^TENUM  = (ONE, TWO, THREE)', R, ptprkEnum, [ptprfPointer], [], '^TENUM', '(ONE, TWO, THREE)');

  // <whatis VarEnumA>     type =  = (E1, E2, E3)
  r := ParseTypeFromGdb('type =  = (E1, E2, E3)'+LN);
  CheckResult('type =  = (E1, E2, E3)', R, ptprkEnum, [], [], '', '(E1, E2, E3)');

  // <whatis VarEnumA>     type = ^ = (E1, E2, E3)
  r := ParseTypeFromGdb('type = ^ = (E1, E2, E3)'+LN);
  CheckResult('type = ^ = (E1, E2, E3)', R, ptprkEnum, [ptprfPointer], [], '', '(E1, E2, E3)');


  (* set *)

  // type = set of TENUM
  r := ParseTypeFromGdb('type = set of TENUM'+LN);
  CheckResult('type = set of TENUM', R, ptprkSet, [], [], '', 'set of TENUM');
  // type = ^set of TENUM
  r := ParseTypeFromGdb('type = ^set of TENUM'+LN);
  CheckResult('type = ^set of TENUM', R, ptprkSet, [ptprfPointer], [], '', '^set of TENUM');

  // type = set of  = (...)
  r := ParseTypeFromGdb('type = set of  = (...)'+LN);
  CheckResult('type = set of  = (...)', R, ptprkSet, [], [], '', 'set of  = (...)');
  // type = ^set of  = (...)
  r := ParseTypeFromGdb('type = ^set of  = (...)'+LN);
  CheckResult('type = ^set of  = (...)', R, ptprkSet, [ptprfPointer], [], '', '^set of  = (...)');

  // type = set of  = (ALPHA, BETA, GAMMA)
  r := ParseTypeFromGdb('type = set of  = (ALPHA, BETA, GAMMA)'+LN);
  CheckResult('type = set of  = (ALPHA, BETA, GAMMA)', R, ptprkSet, [], [], '', 'set of  = (ALPHA, BETA, GAMMA)');
  // type = ^set of  = (ALPHA, BETA, GAMMA)
  r := ParseTypeFromGdb('type = ^set of  = (ALPHA, BETA, GAMMA)'+LN);
  CheckResult('type = ^set of  = (ALPHA, BETA, GAMMA)', R, ptprkSet, [ptprfPointer], [], '', '^set of  = (ALPHA, BETA, GAMMA)');

  // type = <invalid unnamed pascal type code 8>   ## Dwarf no sets
  r := ParseTypeFromGdb('type = <invalid unnamed pascal type code 8>'+LN);
  CheckResult('type = <invalid unnamed pascal type code 8>', R, ptprkSet, [], [], '', '');
  // type = ^<invalid unnamed pascal type code 8>
  r := ParseTypeFromGdb('type = ^<invalid unnamed pascal type code 8>'+LN);
  CheckResult('type = ^<invalid unnamed pascal type code 8>', R, ptprkSet, [ptprfPointer], [], '', '');

  // 'type = set of ONE..THREE'+LN   ## Dwarf gdb 7.0
  r := ParseTypeFromGdb('type = set of ONE..THREE'+LN);
  CheckResult('type = set of ONE..THREE'+LN, R, ptprkSet, [], [], '', 'set of ONE..THREE');
  // 'type = ^set of ONE..THREE'+LN
  r := ParseTypeFromGdb('type = ^set of ONE..THREE'+LN);
  CheckResult('type = ^set of ONE..THREE'+LN, R, ptprkSet, [ptprfPointer], [], '', '^set of ONE..THREE');
  // 'type = &set of ONE..THREE'+LN
  r := ParseTypeFromGdb('type = &set of ONE..THREE'+LN);
  CheckResult('type = &set of ONE..THREE'+LN, R, ptprkSet, [ptprfParamByRef], [], '', '&set of ONE..THREE');
  // 'type = ^&set of ONE..THREE'+LN
  r := ParseTypeFromGdb('type = ^&set of ONE..THREE'+LN);
  CheckResult('type = ^&set of ONE..THREE'+LN, R, ptprkSet, [ptprfPointer, ptprfParamByRef], [], '', '^&set of ONE..THREE');


  (* record *)

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



initialization

  RegisterTest(TTestGdbType); 
end.

