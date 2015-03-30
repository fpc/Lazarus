{type helper}
program pt_thlp2;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

type
  TTest = type helper for LongInt
    procedure Test;
  end;

procedure TTest.Test;
begin

end;

begin
end.

