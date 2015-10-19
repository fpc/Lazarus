{%norun}
program tudots.prog;

{$mode delphi}

uses
  tudots;

begin
  // this should not fail although we have a namespace tudots and a unit tudots
  tudots.dot.test{ todo declaration:tudots.dot.test} := 1;
end.


