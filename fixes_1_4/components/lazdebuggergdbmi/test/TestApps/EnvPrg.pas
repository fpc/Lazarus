program EnvPrg;

uses sysutils;

var
  s: String;
begin
  s := GetEnvironmentVariable('ETEST1');
  if s = 'ab123c' then
    writeln(1)
  else
    writeln(2);
end.
