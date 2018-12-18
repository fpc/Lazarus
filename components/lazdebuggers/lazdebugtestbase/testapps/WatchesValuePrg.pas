// TEST_USES=WatchesValuePrgIdent.inc

program WatchesValuePrg;
{$Macro on}
{$LONGSTRINGS ON}

uses sysutils, Classes;

type
  ShortStr1 = String[1];
  ShortStr10 = String[10];
  ShortStr255 = String[255];

  TShortRec = record // looks like shortstring
    length: byte;
    st: array [1..5] of char;
  end;
  TCharStatArray = array [1..5] of char;
  TCharDynArray = array [1..5] of char;

  TStrA = AnsiString;
  TStrTA = type AnsiString;
  TPChr = ^Char;

  TWStrA = WideString;
  TWStrTA = type WideString;
  TPWChr = ^WideChar;

const
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gc, ADD=0, CHR1='A', _OP_==, _O2_=:, _EQ_==,"(nil)=nil")

var
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, _OP_=:, (=;//, _O2_=:, _EQ_= )


procedure Foo(
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=arg, _OP_=:, (=;//, _O2_=:, _EQ_= , //ARG=)
  Dummy: Integer
);
begin
end;

var
  BreakDummy: Integer;
begin
  // access constant that are not passed as function arg
  // so every constant is accessed, and they can not be optimized away
  BreakDummy := ord(gcCharStatArray[1]);
  BreakDummy := ord(gcWCharStatArray[1]);

  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gv, ADD=1, CHR1='B', _OP_=:=, _O2_={, _EQ_=}:=, {P2}={, //_pre2_=}gc)

  Foo(
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gc, "_OP_=,//", "_O2_=,//", //ARG=)
    0
  );

  BreakDummy:= 1; // TEST_BREAKPOINT=Prg
end.

