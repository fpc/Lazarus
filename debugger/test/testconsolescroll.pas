program TestConsoleScroll;

(* This console-mode program for Linux or other unix implementations outputs	*)
(* 100 numbered lines, followed by all 256 8-bit characters as a block. The	*)
(* lines should be presented without intervening blanks, the character block	*)
(* should make sense provided that a formatted console style is selected.	*)
(*										*)
(* It DOES NOT attempt any formatted output using escape sequences etc. MarkMLl	*)

uses
  SysUtils;

var
  i, j: integer;

begin
  for i := 1 to 100 do
    WriteLn(i);
  WriteLn;
  for i := 0 to 15 do begin
    for j := 1 to 15 do
      Write(Chr(16 * i + j));
    WriteLn
  end;
  WriteLn 
end.
  
