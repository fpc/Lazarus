program testcntr;

uses
  SysUtils;
  
var
  m, n, x : Cardinal;
  time: TDateTime;
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
    time := now;
    while (now - time) * SecsPerDay < 10  do;
    inc(x);
  end;
end.  