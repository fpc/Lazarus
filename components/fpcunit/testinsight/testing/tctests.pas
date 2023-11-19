unit tcTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  Suite1 = class(TTestCase)
  published
    procedure Test1;
    procedure Test2;
  end;

  { Suite2 }

  Suite2 = class(TTestCase)
  published
    procedure Test1;
    procedure Test2;
    procedure Test3;
  end;

implementation

{ Suite2 }

procedure Suite2.Test1;
begin
  Fail('Test 1 fails');
end;

procedure Suite2.Test2;
begin
  AssertTrue('Test 2 OK',True);
end;

procedure Suite2.Test3;
begin
  Raise Exception.Create('test 3 errors');
end;


procedure Suite1.Test1;
begin
  Fail('Test 1 fails');
end;

procedure Suite1.Test2;
begin
  AssertTrue('Test 2 OK',True);
end;



initialization
  RegisterTests([Suite1,Suite2]);
end.

