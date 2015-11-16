{
  ./testcodetools --format=plain --suite=TestFindDeclaration_Generics
}
unit fdt_generics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  generic TGenBaseAction<T> = class
    BaseName: T{declaration:fdt_generics.TGenBaseAction.T};
  end;

  generic TGenCustomAction<T> = class(specialize TGenBaseAction{declaration:fdt_generics.TGenBaseAction}<T>)
    CustomName: T;
  end;

  generic TGenAction<T> = class(specialize TGenCustomAction{declaration:fdt_generics.TGenCustomAction}<T>)
    ActionName: T;
  end;

implementation

procedure DoSomething;
begin

end;

end.

