program GenericExamples;

{$mode objfpc}{$H+}

// generics
type
  generic TArr<T> = array[0..2] of T;
  generic TProc<T> = procedure(Arg: T);
  generic TRecG<T> = record
    F: T;
  end;
  TGenericArray<T> = array of T;
  IGenericInterface<T> = interface
    function DoSomething(Arg: T): T;
  end;
  TGenericClass<T1,T2> = class(TInterfacedObject, IGenericInterface<T1>)
    F: T21;
    type
      Intf = IGenericInterface<Integer>;
    function DoSomething(Arg: T1): T2;
    function Test(Arg: Intf): Intf;
  end;
const
  RecLongInt: TRecG<longint> = (F:0);

var
  ArraySpecialize: TGenericArray<Integer>;
  ClassSpecialize: TGenericClass<Integer,String>;

function TGenericClass.DoSomething(Arg: T1): T2;
begin

end;

begin
  FooInt := TGenericClass<Integer,String>.Create;
end.

