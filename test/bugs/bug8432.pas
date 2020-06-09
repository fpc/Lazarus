unit bug8432;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, testglobals,
  maps;

type

  { TTestBug8432 }

  TTestBug8432= class(TTestCase)
  private
    FMap: TMap;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_itu4_1;
    procedure Test_itu4_2;
    procedure Test_itu8_1;
    procedure Test_itu8_2;
  end;

implementation

procedure TTestBug8432.SetUp;
begin
  inherited SetUp;
end;

procedure TTestBug8432.TearDown;
begin
  FMap.Free;
  inherited TearDown;
end;

procedure TTestBug8432.Test_itu4_1;
var
  Key: Integer;
  AInt: Integer;
begin
  FMap:=TMap.Create(itu4,SizeOf(Integer));
  with FMap do
  begin
    for Key := 1 to 3 do
    begin
      AInt:=Key* 10;
      Add(Key,AInt);
    end;
    Key := 1;
    GetData(Key,AInt);
    AssertEquals('Wrong entry for 1', 10, AInt);
    Key := 2;
    GetData(Key,AInt);
    AssertEquals('Wrong entry for 2', 20, AInt);
    Key := 3;
    GetData(Key,AInt);
    AssertEquals('Wrong entry for 3', 30, AInt);
  end;
end;

procedure TTestBug8432.Test_itu4_2;
var
  i: integer;
  AInt: Integer;
  ID: DWord;
begin
  FMap:=TMap.Create(itu4,SizeOf(Integer));
  with FMap do
  begin
    for i := 0 to 255 do begin
      ID := dword(i) shl 24;
      AInt:=i;
      Add(ID,AInt);
    end;
    for i := 0 to 255 do begin
      AInt:= 0;
      ID := dword(i) shl 24;
      GetData(ID,AInt);
      AssertEquals('Wrong entry for '+ IntToStr(i), i, AInt);
    end;
  end;
end;

procedure TTestBug8432.Test_itu8_1;
var
  AInt: Integer;
  ID1, ID2, ID3: QWord;
begin
  FMap:=TMap.Create(itu8,SizeOf(Integer));
  with FMap do
  begin
    ID1 := 1; AInt:=10;
    Add(ID1,AInt);
    ID2 := 2; AInt:=20;
    Add(ID2,AInt);
    ID3 := 3; AInt:=30;
    Add(ID3,AInt);
    GetData(ID1,AInt);
    AssertEquals('Wrong entry for 1', 10, AInt);
    GetData(ID2,AInt);
    AssertEquals('Wrong entry for 2', 20, AInt);
    GetData(ID3,AInt);
    AssertEquals('Wrong entry for 3', 30, AInt);
  end;
end;

procedure TTestBug8432.Test_itu8_2;
var
  AInt: Integer;
  ID1, ID2, ID3: QWord;
begin
  FMap:=TMap.Create(itu8,SizeOf(Integer));
  with FMap do
  begin
    ID1 := 1 shl 32; AInt:=10;
    Add(ID1, AInt);
    AssertEquals('Wrong ID1', QWord($100000000), ID1);
    ID2 := 2 shl 32; AInt:=20;
    Add(ID2, AInt);
    ID3 := 3 shl 32; AInt:=30;
    Add(ID3, AInt);
    GetData(ID1,AInt);
    AssertEquals('Wrong entry for 1', 10, AInt);
    GetData(ID2,AInt);
    AssertEquals('Wrong entry for 2', 20, AInt);
    GetData(ID3,AInt);
    AssertEquals('Wrong entry for 3', 30, AInt);
  end;
end;

initialization
  AddToBugsTestSuite(TTestSuite.Create(TTestBug8432, '8432'));
end.

