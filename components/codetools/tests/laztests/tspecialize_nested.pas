program tspecialize_nested;
{$Mode objfpc}

type

  generic G1<_B1: TObject> = class(_B1)
    procedure p1;
  end;

  generic G2<_B2: TObject> = class(_B2)
    procedure p2;
  end;

  C1 = class
    procedure f1;
  end;

  C2 = class(
    specialize G2<
      specialize G1<C1>
    >
  )
    procedure f2;
  end;


procedure C1.f1;
begin
  {completion:!p1,!p2,f1,!f2}
  f1{declaration:C1.f1};
end;

procedure C2.f2;
begin
  {completion:p1,p2,f1,f2}
  p2{declaration:G2.p2};
  p1{declaration:G1.p1};
  f1{declaration:C1.f1};
  f2{declaration:C2.f2};
end;

procedure G1.p1;
begin
  {completion:p1,!p2,!f1,!f2}
  p1{declaration:G1.p1};
end;

procedure G2.p2;
begin
  {completion:!p1,p2,!f1,!f2}
  p2{declaration:G2.p2};
end;

begin
end.
