unit fdt_with;

{$mode objfpc}{$H+}

interface

type
  TTest = class
  public
    I: string;
  end;

  TContainer = class
  public
    Sub: TTest;
    I: string;
  end;

implementation

procedure Test;
var
  Test: TTest{declaration:fdt_with.TTest};
  A: array[0..9] of string;
  X: string;
  aContainer: TContainer{declaration:fdt_with.TContainer};
  I: integer;
begin
  with Test{declaration:Test.Test} do
  begin
    I{declaration:fdt_with.TTest.I} := 'hello';
    A{declaration:Test.A}[0] := 'abc';
  end;

  aContainer:=TContainer.Create;
  with aContainer{declaration:Test.aContainer}, Sub{declaration:fdt_with.TContainer.Sub} do begin
    I{declaration:fdt_with.TTest.I} := 'tool';
  end;

  for I{guesstype:Integer} := Low(A{declaration:Test.A}) to High(A) do
  begin
    Test.I{declaration:fdt_with.TTest.I} := 'bye';
    X{declaration:Test.X} := A[I];
  end;
  if X='' then ;
end;

end.

