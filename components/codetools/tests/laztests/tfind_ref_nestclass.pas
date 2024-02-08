program Project1{ TODO findrefs:C,...};
uses Classes;
type
  TFoo{findrefs:C,3,4;17,17;11,18;6,19;15,20;17,23;3,30;12,31;26,32;17,33} = class // TODO: misses 15,3
  type

    { TBar }

    TBar{findrefs:5,9;22,17;22,23;8,30;17,31;31,32;22,33} = class
      class procedure abc{findrefs:23,10;27,17;13,30;22,31};
      class procedure def{findrefs:C,23,11;21,18;27,23;36,32;27,33};
    end;
  end;

{ TFoo.TBar }

class procedure TFoo.TBar.abc;
begin  // TFoo.TBar.def
  // TFoo.TBar.abc
  // Project1.TFoo.TBar.abc
end;

class procedure TFoo.TBar.def;
begin
end;

var
  a: TMethod;
begin
  TFoo.TBar.abc;
  Project1.TFoo.TBar.abc;
  a := TMethod(@Project1.TFoo.TBar.def);
  a := TMethod(@TFoo.TBar.def);
end.

