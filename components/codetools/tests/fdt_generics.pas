{
  ./testcodetools --format=plain --suite=TestFindDeclaration_Generics
}
unit fdt_generics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  generic TGenBaseAction<GenParam1> = class
    BaseName: GenParam1{declaration:fdt_generics.TGenBaseAction.GenParam1};
  end;

  generic TGenCustomAction<GenParam2> = class(specialize TGenBaseAction{declaration:fdt_generics.TGenBaseAction}<GenParam2>)
    CustomName: GenParam2;
  end;

  generic TGenAction<GenParam3> = class(specialize TGenCustomAction{declaration:fdt_generics.TGenCustomAction}<GenParam3>)
    ActionName: GenParam3;
  end;

implementation

procedure DoSomething;
begin

end;

end.

