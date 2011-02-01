program NestedClasses;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords} // {$mode delphi} has it automatically

// 1. advanced classes/objects

type
  TClass1 = class
  type
    Int = Integer;
    TInnerClass = class
    public
      F: Int;
      procedure Test;
    end;
  const
    CInt: Int = 5;
  end;

procedure TClass1.TInnerClass.Test;
begin
  WriteLn(F);
end;

// 2. advanced records
type
  TRec1 = record
    hWnd : HWND;
  private
    F1: Integer;
    F2: Byte;
  public
    type
      TBar = Integer;
    const
      C: TBar = 1;
    var
      F3: TBar;
      F4: Byte;
    class var
      F5: TBar;
  private
    type
      Int = Integer;
    var
      F: Int;
    const
      DefaultF: Int = 1;
  public
    function GetF: Int;
    procedure SetF(const Value: Int);
    // full list of operators see in tests/test/terecs6.pp
    class operator Inc(Rec: TRec1): TRec1;
  end;

function TRec1.GetF: Int;
begin
  Result := F;
end;

procedure TRec1.SetF(const Value: Int);
begin
  F := Value;
end;

class operator TRec1.Inc(Rec: TRec1): TRec1;
begin
  Result.F := Rec.F + 1;
end;

// 3. generics
type
  generic TArr<T> = array[0..2] of T;
  generic TProc<T> = procedure(Arg: T);
  generic TRecG<T> = record
    F: T;
  end;
// delphi generic please see in:
// tgeneric28.pp, tgeneric29.pp, tgeneric32.pp, tgeneric34.pp

begin
end.

