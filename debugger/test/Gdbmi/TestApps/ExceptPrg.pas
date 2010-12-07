program ExceptPrg;
{$H-}

uses sysutils;

var
  p: pointer; // ensure pointer is in symbol info
  s: string[100];
  x: Exception;
begin
  p := nil;
  try
    x := Exception.Create('foo');
    raise x;
  except
    on e: Exception do begin
      s := IntToStr(PtrInt(p));
      writeln(e.Message + s);
    end;
  end;
end.
