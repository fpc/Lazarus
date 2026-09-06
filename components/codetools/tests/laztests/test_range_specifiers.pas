program test_range_specifiers;
{$mode objfpc}
uses SysUtils;
type
  d = class
    function f(i: double): integer; virtual; abstract;
    property p(.i: double]: integer read f; default;
  end;

  TRecord = record
    FField: integer;
  end;
var
  l: array [0..1.) of TRecord;
begin
  l[0]  .FField{declaration:TRecord.FField} := 0;
  l(.0.).FField{declaration:TRecord.FField} := 0;
  l(.0] .FField{declaration:TRecord.FField} := 0;
  l[0.) .FField{declaration:TRecord.FField} := 0;

  d(0).p{declaration:d.p}(.0.0-0.0.);
  d(0).p{declaration:d.p}(.0.0-0.0-0.0.);

end.
