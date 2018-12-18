program ExceptPrgStep;
uses sysutils;

var
  i: integer;

procedure foo;
begin
  raise Exception.create('a');
  Freemem(GetMem(1));
end;

begin
  try
    foo;
    Freemem(GetMem(1));
    foo;
    foo;
  except
    Freemem(GetMem(1));
  end;
  Freemem(GetMem(2));

  try
    try
      foo;
      Freemem(GetMem(1));
      foo;
      foo;
    except
      Freemem(GetMem(1));
    end;
  Freemem(GetMem(2));
  except
    Freemem(GetMem(1));
  end;

  try
    try
      raise Exception.create('a');
      Freemem(GetMem(1));
      Freemem(GetMem(1));
      Freemem(GetMem(1));
    finally
      Freemem(GetMem(1));
    end;
  raise Exception.create('xxx');
  Freemem(GetMem(2));
  except
    Freemem(GetMem(1));
  end;

  Freemem(GetMem(2));
  Freemem(GetMem(2));
  Freemem(GetMem(2));

end.
