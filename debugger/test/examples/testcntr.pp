program testcntr;
{$mode objfpc}{$H+}
uses
  TestWait,
  SysUtils;
  
var
  m, n, x : Cardinal;  
  w: TWait;
  S: String;
begin
  m :=0;
  x := 0;
  w := TWait.Create(2);
  while x < 3 do
  begin
    repeat  
      S := Format('[%.10d] ', [m]);
      Write(S);
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
