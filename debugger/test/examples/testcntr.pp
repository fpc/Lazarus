program testcntr;

uses
  TestWait,
  SysUtils;
  
var
  m, n, x : Cardinal;
begin
  m :=0;
  x := 0;
  while x < 3 do
  begin
    repeat 
      Write(Format('[%.10d] ', [m]));
      Inc(m);
      for n := 0 to 79 do Write('.');
      WriteLN;
    until m mod 10 = 0;
    Wait(10);
    inc(x);
  end;
end.  