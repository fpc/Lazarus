program ExceptPrgStepOver;
uses sysutils;

procedure foo;
begin
  raise Exception.create('a');
  Freemem(GetMem(1));
end;

procedure bar;
begin
  try
    foo();
    Freemem(GetMem(88));
    Freemem(GetMem(88));
  except
    Freemem(GetMem(99));
  end;
end;

///////////////
procedure ffoo;
begin
  try
    raise Exception.create('a');
    Freemem(GetMem(88));
    Freemem(GetMem(88));
  finally
    Freemem(GetMem(1));
  end;
  Freemem(GetMem(1));
end;

procedure fbar;
begin
  try
    ffoo();
    Freemem(GetMem(88));
    Freemem(GetMem(88));
  except
    Freemem(GetMem(99));
  end;
end;

///////////////
procedure abc;
begin
  try
    raise Exception.create('a'); // 3) line 49  // step over: handled at same level
    Freemem(GetMem(88));
    Freemem(GetMem(88));
  except
    Freemem(GetMem(99));  // 3) after step, line 53 (or 51 to 53)
  end;
end;

///////////////
procedure xyz;
begin
  raise Exception.create('a');    // 4) line 60 // step over: handled by outer
  Freemem(GetMem(1));
end;


///////////////
begin
  bar; // 1) line 67  // step over: handled in nested
  Freemem(GetMem(1));

  fbar; // 2) line 70  // step over: handled in nested / with finally
  Freemem(GetMem(1));

  abc; // 3)
  Freemem(GetMem(1));

  try
    xyz;  // 4)
    Freemem(GetMem(1));
    Freemem(GetMem(1));
  except
    Freemem(GetMem(1));  // 4) after step, line 81 (or 79 to 81)
  end;

  Freemem(GetMem(1));
  Freemem(GetMem(1));

end.
