program Project1;

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

