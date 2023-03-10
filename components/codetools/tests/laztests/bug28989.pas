program bug28989;
{$Mode objfpc}
type
  T3 = record
    Var1: Integer;
    Var2: Integer;
  end;

  generic T1<T> = object
    type T2 = record
      TestVar2: T;
    end;
  var
    TestVar1: T;
  end;

  TTest1 = specialize T1<T3>;

var
  testing: TTest1;
  testing2: TTest1.T2;

begin
  testing.TestVar1.{completion:var1,var2};  //CTRL-Space shows Var1 and Var2
  testing2.TestVar2.{completion:var1,var2}; //CTRL-Space shows error: illegal qualifier . found
end.


