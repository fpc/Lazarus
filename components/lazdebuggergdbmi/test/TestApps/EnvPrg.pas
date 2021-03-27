program EnvPrg;
{$H-}

uses
  {$IFDEF UNIX} cwstring, {$ENDIF}
  sysutils;

var
  u: UnicodeString;
  e, S: string;
  i: Integer;
begin
  {$IFDEF UNIX}
  e := GetEnvironmentVariable(AnsiString('ETEST1'));
  {$ELSE}
  u := GetEnvironmentVariable(UnicodeString('ETEST1'));
  e := UTF8Encode(u);
  {$ENDIF}
  s := '';
  for i := 1 to length(e) do
      s := s + IntToHex(ord(e[i]), 2);

  while false do  ;
  while false do  ;
  while false do  ;
  while false do  ;
  while false do  ;
  while false do  ;
  while false do  ;
  while false do  ;
end.
