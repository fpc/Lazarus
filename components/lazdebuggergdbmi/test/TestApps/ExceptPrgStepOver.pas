program ExceptPrgStepOver;
uses sysutils;

procedure foo;
begin
  raise Exception.create('a');
  writeln(1);
end;

procedure bar;
begin
  try
    foo();
    writeln(88);
    writeln(88);
  except
    writeln(99);
  end;
end;

///////////////
procedure ffoo;
begin
  try
    raise Exception.create('a');
    writeln(88);
    writeln(88);
  finally
    writeln(55);
  end;
  writeln(1);
end;

procedure fbar;
begin
  try
    ffoo();
    writeln(88);
    writeln(88);
  except
    writeln(99);
  end;
end;

///////////////
procedure abc;
begin
  try
    raise Exception.create('a'); // 3) line 49  // step over: handled at same level
    writeln(88);
    writeln(88);
  except
    writeln(99);  // 3) after step, line 53 (or 51 to 53)
  end;
end;

///////////////
procedure xyz;
begin
  raise Exception.create('a');    // 4) line 60 // step over: handled by outer
  writeln(1);
end;


///////////////
begin
  bar; // 1) line 67  // step over: handled in nested
  writeln(1);

  fbar; // 2) line 70  // step over: handled in nested / with finally
  writeln(1);

  abc; // 3)
  writeln(2);

  try
    xyz;  // 4)
    writeln(3);
    writeln(3);
  except
    writeln(77);  // 4) after step, line 81 (or 79 to 81)
  end;

  writeln(2);
  writeln(2);

end.
