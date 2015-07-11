{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}

unit DbTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, DB, MemDS, FPCUnit, TestRegistry, TADbSource;

type

  TDbSourceTest = class(TTestCase)
  strict private
    FMem: TMemDataset;
    FDS: TDataSource;
    FSource: TDbChartSource;

    procedure AddRecord(AId: Integer; AX, AY: Double; AText: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Basic;
    procedure Fields;
  end;

implementation

uses
  SysUtils;

{ TDbSourceTest }

procedure TDbSourceTest.AddRecord(AId: Integer; AX, AY: Double; AText: String);
begin
  FMem.AppendRecord([AId, AX, AY, AText]);
end;

procedure TDbSourceTest.Basic;
begin
  AddRecord(1, 1.0, 2.0, 'test');
  AssertEquals(1, FSource.Count);
  AssertEquals(2.0, FSource.Item[0]^.Y);
end;

procedure TDbSourceTest.Fields;
begin
  FSource.FieldY := 'X,a b';
  AssertEquals(2, FSource.YCount);
  FSource.FieldY := 'Y';
  AssertEquals(1, FSource.YCount);
end;

procedure TDbSourceTest.SetUp;
begin
  inherited SetUp;
  FMem := TMemDataset.Create(nil);
  FMem.FieldDefs.Add('id', ftInteger);
  FMem.FieldDefs.Add('x', ftFloat);
  FMem.FieldDefs.Add('y', ftFloat);
  FMem.FieldDefs.Add('text', ftString);
  FDS := TDataSource.Create(nil);
  FDs.DataSet := FMem;
  FSource := TDbChartSource.Create(nil);
  FSource.DataSource := FDS;
  FSource.FieldX := 'X';
  FSource.FieldY := 'Y';
  FMem.Open;
end;

procedure TDbSourceTest.TearDown;
begin
  FreeAndNil(FSource);
  FreeAndNil(FDS);
  FreeAndNil(FMem);
  inherited TearDown;
end;

initialization

  RegisterTest(TDbSourceTest);

end.

