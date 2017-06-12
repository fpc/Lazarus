program fdt_guesstype1;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

procedure DoIt(s: string);
begin
  i{guesstype:integer}:=length(s);
  d{guesstype:string}:=copy(s,1,3);
  i{guesstype:integer}:=default(integer);
end;

begin
end.

