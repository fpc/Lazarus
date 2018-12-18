program EnvPrg;

uses sysutils;

var
  s: String;
begin
  s := GetEnvironmentVariable('ETEST1');
  if s = 'ab123c' then
    Freemem(GetMem(1))
  else
    Freemem(GetMem(2));
end.
