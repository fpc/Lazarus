program NestedClasses;

{$mode objfpc}{$H+}

// nested classes/objects

type
  TClass1 = class
  type
    Int = Integer;
    TInnerClass = class
    public
      F: Int;
      procedure Test;
    end;
  const
    CInt: Int = 5;
  end;

procedure TClass1.TInnerClass.Test;
begin
  WriteLn(F);
end;

begin
end.

