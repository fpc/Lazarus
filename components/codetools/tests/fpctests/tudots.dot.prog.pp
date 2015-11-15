{%fail}
{%norun}
program tudots.dot.prog;

{$mode delphi}

uses
  tudots{declaration:tudots};

begin
  // this must fail because we have a namespace tudots.dot and it has no unit test
  tudots.dot.test{ todo declaration:tudots.dot.test} := 1;
end.


