program RecordsExample;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords} // {$mode delphi} has it automatically

uses
  Classes, SysUtils;

// advanced records
type
  TRec1 = record
    hWnd : integer;
  private
    F1: Integer;
    F2: Byte;
  public
   { type
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
      DefaultF: Int = 1;}
  public
    function GetF: integer;
    procedure SetF(const Value: integer);
    // full list of operators see in tests/test/terecs6.pp
    class operator Inc(Rec: TRec1): TRec1;
  public
    case y: integer of
      0: (a: integer);
      1,2,3: (b: array[char] of char; c: char);
      3: ( d: record
                case byte of
                  10: (i: integer; );
                  11: (y: byte);
              end; );
      4: (e: integer;
            case byte of
            8: (f: integer)
          );
  end;

function TRec1.GetF: integer;
begin
  Result := F1;
end;

procedure TRec1.SetF(const Value: integer);
begin
  F1 := Value;
end;

class operator TRec1.Inc(Rec: TRec1): TRec1;
begin
  Result.F1 := Rec.F1 + 1;
end;

begin
end.

