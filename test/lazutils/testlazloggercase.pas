unit TestLazLoggerCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, LazLogger;

type

  { TTestLazLogger }

  TTestLazLogger = class(TTestCase)
  protected
    FTheLogger: TLazLogger;
    FOnDbgOutCount, FOnDebugLnCount: Integer;
    FOnDbgOutText, FOnDebugLnText: string;
    procedure TestOnDbgOut(Sender: TObject; S: string; var Handled: Boolean);
    procedure TestOnDebugln(Sender: TObject; S: string; var Handled: Boolean);
    procedure InitLogger;
    procedure AssertDbgOut(Name: string; ExpCount: integer; ExpText: String);
    procedure AssertDebugLn(Name: string; ExpCount: integer; ExpText: String);
  published
    procedure TestEvent;
  end;

implementation

procedure TTestLazLogger.TestOnDbgOut(Sender: TObject; S: string; var Handled: Boolean);
begin
  inc(FOnDbgOutCount);
  FOnDbgOutText := FOnDbgOutText + s;
  Handled := True;
end;

procedure TTestLazLogger.TestOnDebugln(Sender: TObject; S: string; var Handled: Boolean);
begin
  inc(FOnDebugLnCount);
  FOnDebugLnText := FOnDebugLnText + s + LineEnding;
  Handled := True;
end;

procedure TTestLazLogger.InitLogger;
begin
  FreeAndNil(FTheLogger);
  FTheLogger := TLazLogger.Create;
  FTheLogger.OnDebugLn  := @TestOnDebugln;
  FTheLogger.OnDbgOut  := @TestOnDbgOut;
end;

procedure TTestLazLogger.AssertDbgOut(Name: string; ExpCount: integer; ExpText: String);
begin
  AssertEquals(Name + ' DbgOut call count', ExpCount, FOnDbgOutCount);
  AssertEquals(Name + ' DbgOut text', ExpText, FOnDbgOutText);
  FOnDbgOutCount := 0;
  FOnDbgOutText := '';
end;

procedure TTestLazLogger.AssertDebugLn(Name: string; ExpCount: integer; ExpText: String);
begin
  AssertEquals(Name + ' DebugLn call count', ExpCount, FOnDebugLnCount);
  AssertEquals(Name + ' DebugLn text', ExpText, FOnDebugLnText);
  FOnDebugLnCount := 0;
  FOnDebugLnText := '';
end;

procedure TTestLazLogger.TestEvent;
begin
  InitLogger;

  FTheLogger.DebugLn('a');
  AssertDebugLn('debugln a', 1, 'a'+LineEnding);
  AssertDbgOut('debugln a', 0, '');

  FTheLogger.DebugLn('b', 'c');
  AssertDebugLn('debugln b,c', 1, 'bc'+LineEnding);
  AssertDbgOut('debugln b,c', 0, '');

  FTheLogger.DebugLn(['d', 1]);
  AssertDebugLn('debugln d,1', 1, 'd1'+LineEnding);
  AssertDbgOut('debugln d,1', 0, '');

  FTheLogger.DebugLn('e %d', [1]);
  AssertDebugLn('debugln e,1', 1, 'e 1'+LineEnding);
  AssertDbgOut('debugln e,1', 0, '');


  FTheLogger.DbgOut('a');
  AssertDbgOut('DbgOut a', 1, 'a');
  AssertDebugLn('DbgOut a', 0, '');

  FTheLogger.DbgOut('b', 'c');
  AssertDbgOut('DbgOut b,c', 1, 'bc');
  AssertDebugLn('DbgOut b,c', 0, '');

  FTheLogger.DbgOut(['d', 1]);
  AssertDbgOut('DbgOut d,1', 1, 'd1');
  AssertDebugLn('DbgOut d,1', 0, '');

  FTheLogger.DbgOut('e %d', [1]);
  AssertDbgOut('DbgOut e,1', 1, 'e 1');
  AssertDebugLn('DbgOut e,1', 0, '');

  FTheLogger.DebugLnEnter('a');
  AssertDebugLn('DebugLnEnter() a', 1, 'a'+LineEnding);
  AssertDbgOut('DebugLnEnter() a', 0, '');

  FTheLogger.DebugLn('in enter');
  AssertDebugLn('debugln in enter', 1, '  in enter'+LineEnding);
  AssertDbgOut('debugln in enter', 0, '');

  FTheLogger.DebugLnExit('b');
  AssertDebugLn('DebugLnExit() b', 1, 'b'+LineEnding);
  AssertDbgOut('DebugLnExit() b', 0, '');

  FTheLogger.DebugLn('after exit');
  AssertDebugLn('debugln after exit', 1, 'after exit'+LineEnding);
  AssertDbgOut('debugln after exit', 0, '');


  FreeAndNil(FTheLogger);
end;


initialization

  RegisterTest(TTestLazLogger);
end.

