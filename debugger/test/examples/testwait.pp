unit TestWait;

interface

procedure Wait(const ATime: Integer);

implementation

uses
  SysUtils; 

procedure Wait(const ATime: Integer);
var
  time: TDateTime;
begin
  time := now;
  while (now - time) * SecsPerDay < ATime  do;
end;
           
  
var
  n: Integer;
begin
  n := 0;
  while n < 1001 do Inc(n); //something useles 
end.