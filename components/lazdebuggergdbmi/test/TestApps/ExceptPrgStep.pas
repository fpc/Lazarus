program ExceptPrgStep;
uses sysutils;

var
  i: integer;

procedure foo;
begin
  raise Exception.create('a');
  writeln(1);
end;

begin
  try
    foo;
    writeln(1);
    foo;
    foo;
  except
    writeln(1);
  end;
  writeln(2);

  try
    try
      foo;
      writeln(1);
      foo;
      foo;
    except
      writeln(1);
    end;
  writeln(2);
  except
    writeln(1);
  end;
  writeln(2);
  writeln(2);
  writeln(2);

end.
