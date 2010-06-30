unit system;
(* Test unit for the parser.
  It has been name "system" to prevent a search for the system unit.
  Later the search for related units must be dropped from the compiler,
  in order to make it parse any source file.
*)

{$mode objfpc}

interface

procedure Hello;

implementation

var
  s: string;
procedure Hello;
begin
  //WriteLn('Hello world');
  //s := 'Hello world';
end;

end.

