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

  tvardata = packed record
     vtype : tvartype;
     case integer of
        0:(res1 : word;
           case integer of
              0:
                (res2,res3 : word;
                 case word of
                    varstring : (vstring : pointer);
                    varany :  (vany : pointer);
               );
              1:
                (vlongs : array[0..2] of longint);
          );
        1:(vwords : array[0..6] of word);
        2:(vbytes : array[0..13] of byte);
     end;

function TRec1.GetF: integer;
begin
  Result := F;
end;

procedure TRec1.SetF(const Value: integer);
begin
  F := Value;
end;

class operator TRec1.Inc(Rec: TRec1): TRec1;
begin
  Result.F := Rec.F + 1;
end;

begin
end.

