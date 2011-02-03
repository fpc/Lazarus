program RecordsExample;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords} // {$mode delphi} has it automatically

// advanced records
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


begin
end.

