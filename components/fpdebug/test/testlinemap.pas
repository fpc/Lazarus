unit TestLineMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, FpDbgDwarfDataClasses, FpDbgInfo, DbgIntfBaseTypes, fpcunit, testutils,
  testregistry;

type

  { TTestLineMap }

  TTestLineMap = class(TTestCase)
  private
    LMap: TDWarfLineMap;

    procedure InitMap(l: array of integer);
    procedure CheckNotFound(ASearch: Integer; AFindSibling: TGetLineAddrFindSibling; AMaxSiblingDistance: integer = 0);
    procedure CheckFound(ASearch, AExp: Integer; AFindSibling: TGetLineAddrFindSibling; AMaxSiblingDistance: integer = 0);
  published
    procedure TestLineMapFsNone;
    procedure TestLineMapFsBefore;
    procedure TestLineMapFsNext;
  end;

implementation

procedure TTestLineMap.InitMap(l: array of integer);
var
  i: Integer;
begin
  LMap := Default(TDWarfLineMap);
  LMap.Init;
  for i := 0 to Length(l) - 1 do
    LMap.SetAddressForLine(l[i], l[i]);
end;

procedure TTestLineMap.CheckNotFound(ASearch: Integer; AFindSibling: TGetLineAddrFindSibling;
  AMaxSiblingDistance: integer);
var
  a: TDBGPtrArray;
  r: Boolean;
  fl: Integer;
begin
  fl := -1;
  r := LMap.GetAddressesForLine(ASearch, a, False, AFindSibling, @fl, AMaxSiblingDistance);
  AssertFalse('not found '+IntToStr(ASearch), r);
end;

procedure TTestLineMap.CheckFound(ASearch, AExp: Integer; AFindSibling: TGetLineAddrFindSibling;
  AMaxSiblingDistance: integer);
var
  a: TDBGPtrArray;
  r: Boolean;
  fl: Integer;
begin
  fl := -1;
  r := LMap.GetAddressesForLine(ASearch, a, False, AFindSibling, @fl, AMaxSiblingDistance);
  AssertTrue('found '+IntToStr(ASearch), r);
  AssertTrue('found (data) for '+IntToStr(ASearch), Length(a) = 1);
  AssertEquals('found '+IntToStr(ASearch), AExp, fl);
  AssertEquals('found (addr) '+IntToStr(ASearch), AExp, a[0]);
end;

procedure TTestLineMap.TestLineMapFsNone;
begin
  InitMap([10]);

  CheckFound(10, 10, fsNone);
  CheckNotFound( 9, fsNone);
  CheckNotFound(11, fsNone);

  InitMap([1000]);
  CheckNotFound(1, fsNone);
  CheckNotFound(99999, fsNone);


  InitMap([10, 20]);

  CheckFound(10, 10, fsNone);
  CheckFound(20, 20, fsNone);
  CheckNotFound( 9, fsNone);
  CheckNotFound(19, fsNone);
  CheckNotFound(21, fsNone);

  InitMap([10, 2000]);

  CheckFound(10, 10, fsNone);
  CheckFound(2000, 2000, fsNone);
  CheckNotFound( 9, fsNone);
  CheckNotFound(1999, fsNone);
  CheckNotFound(2001, fsNone);
end;

procedure TTestLineMap.TestLineMapFsBefore;
begin
  InitMap([10]);

  CheckFound(10, 10, fsBefore);
  CheckFound(11, 10, fsBefore);
  CheckFound(19, 10, fsBefore);
  CheckFound(19, 10, fsBefore, 9);
  CheckNotFound(19,  fsBefore, 8);
  CheckNotFound( 9,  fsBefore);

  InitMap([910, 920]);

  CheckFound(910, 910, fsBefore);
  CheckFound(911, 910, fsBefore);
  CheckFound(919, 910, fsBefore);
  CheckFound(919, 910, fsBefore, 9);
  CheckNotFound(919,  fsBefore, 8);

  CheckFound(920, 920, fsBefore);
  CheckFound(921, 920, fsBefore);
  CheckFound(929, 920, fsBefore);
  CheckFound(929, 920, fsBefore, 9);
  CheckNotFound(929,  fsBefore, 8);

  CheckFound(2920, 920, fsBefore);
  CheckNotFound(909,  fsBefore);
  CheckNotFound(9,  fsBefore);

  InitMap([511]);
  CheckFound(2920, 511, fsBefore);
  InitMap([512]);
  CheckFound(2920, 512, fsBefore);

end;

procedure TTestLineMap.TestLineMapFsNext;
begin
  InitMap([10]);

  CheckFound(10, 10, fsNext);
  CheckFound( 9, 10, fsNext);
  CheckFound( 1, 10, fsNext);
  CheckFound( 1, 10, fsNext, 9);
  CheckNotFound(11,  fsNext, 8);
  CheckNotFound(11,  fsNext);

  InitMap([910, 920]);

  CheckFound(910, 910, fsNext);
  CheckFound(909, 910, fsNext);
  CheckFound(901, 910, fsNext);
  CheckFound(901, 910, fsNext, 9);
  CheckNotFound(901,  fsNext, 8);

  CheckFound(920, 920, fsNext);
  CheckFound(919, 920, fsNext);
  CheckFound(911, 920, fsNext);
  CheckFound(911, 920, fsNext, 9);
  CheckNotFound(918,  fsNext, 1);
  CheckNotFound(911,  fsNext, 8);

  CheckFound(1, 910, fsNext);

  CheckNotFound(921,  fsNext);
  CheckNotFound(1921,  fsNext);

  InitMap([10, 2000]);
  CheckFound(11, 2000, fsNext);
  CheckFound(311, 2000, fsNext);
  CheckNotFound(11,  fsNext, 500);

  InitMap([255]);
  CheckFound(11, 255, fsNext);
  InitMap([256]);
  CheckFound(11, 256, fsNext);
end;


initialization

  RegisterTest(TTestLineMap);
end.

