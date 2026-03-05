unit test_ref_enum_helper;

{$mode ObjFPC}{$H+}
{$ModeSwitch typehelpers}
interface

type
  TLetter = (a, b, c);
  TEnumHelper = type helper for TLetter
    procedure Print;
  end;

var
  l: TLetter;
implementation
procedure TEnumHelper.Print{findrefs:15,10;23,16;13,21;13,22;13,23;5,24;5,25;5,26};
begin
  writeln(self);
end;
begin
  l := a; l.Print;
  l := b; l.Print;
  l := c; l.Print;
  a.Print;
  b.Print;
  c.Print;
end.

