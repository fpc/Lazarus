unit fdt_for_in;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCol = (red, blue, green);
const
  tcRedBlue = [Low(TCol)..blue];

procedure DoIt;

implementation

procedure DoIt;
begin
  for e{guesstype:TCol} in tcRedBlue do ;
end;

end.

