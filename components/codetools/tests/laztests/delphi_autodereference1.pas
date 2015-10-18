program delphi_autodereference1;

{$mode Delphi}

type
  TRec = record
    v: char;
  end;
  PRec = ^TRec;

var
  p: PRec;
begin
  p.v{declaration:TRec.v};
end.

