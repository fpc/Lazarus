program Project1;
{$Mode objfpc}
{$ModeSwitch typehelpers}
{$ModeSwitch advancedrecords}
type
  generic TFoo<T> = record a: T; end;

  TBar = specialize TFoo<byte>;

  THelp = record helper for TBar
    procedure Add(x: integer);
  end;

procedure THelp.Add(x: integer);
begin
  a := a + x;
end;

var
  b: TBar;
begin
  b.{completion:Add}
  Add(1);
end.

