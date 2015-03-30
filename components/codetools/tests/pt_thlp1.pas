{fail:type helper}
program pt_thlp1;

{$mode objfpc}{$H+}
{$modeswitch typehelpers-}

type
  TTest = type helper {fail}for LongInt
    procedure Test;
  end;

procedure TTest.Test;
begin

end;

begin
end.

