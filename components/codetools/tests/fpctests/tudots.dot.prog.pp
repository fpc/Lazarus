{%fail}
{%norun}
program tudots.dot.prog;

{$mode delphi}

uses
  tudots{ todo declaration:tudots};

begin
  // this must fail because we have a namespace tudots.dot and it has no unit test
  tudots.dot.test{ todo } := 1;
end.


