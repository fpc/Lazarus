program testcntr;
{$mode objfpc}{$H+}
uses
  TestWait,
  SysUtils;
  
var
  m, n, x : Cardinal;  
  w: TWait;
begin
  m :=0;
  x := 0;
  w := TWait.Create(2);
  while x < 3 do
  begin
    repeat 
      Write(Format('[%.10d] ', [m]));
      Inc(m);
      for n := 0 to 79 do 
      begin
        Write('*');
      end;
      WriteLN;
    until m mod 10 = 0;
    W.Wait(10);
    inc(x);
  end;
end.  