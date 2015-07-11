unit UsedUnitRefs1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, process;

implementation

procedure DoSome;
var
  c: TComponent;
  d: float;
  i: Int64;
begin
  c:=TComponent.Create(nil);
  c.Free;
  d:=1.3;
  i:=round(d);
  writeln(i+abs(d));
  writeln(sin(d));
  writeln(MinFloat<d);
end;

end.

