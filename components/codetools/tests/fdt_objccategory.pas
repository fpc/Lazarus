unit fdt_objccategory;

{$mode objfpc}{$H+}
{$ModeSwitch objectivec1}

interface

uses
  Classes, SysUtils;

type

  { TMyClass }

  TMyClass = objcclass(NSObject)
  end;

  { TCategoryA }

  TCategoryA = objccategory(NSObject)
    procedure categoryAmethod; message 'categoryAmethod';
  end;

  { TCategoryB }

  TCategoryB = objccategory(TMyClass)
    procedure categoryBmethod; message 'categoryBmethod';
  end;

  { TCategoryC }

  TCategoryC = objccategory(TMyClass)
    // contrary to helpers there can be multiple ObjCCategory active for a class
    procedure categoryCmethod; message 'categoryCmethod';
  end;

procedure DoIt;

implementation

procedure DoIt;
var
  a: TMyClass;
begin
  a:=TMyClass(TMyClass.alloc).init;
  a.categoryAmethod{declaration:fdt_objccategory.TCategoryA.categoryAmethod};
  a.categoryBmethod{declaration:fdt_objccategory.TCategoryB.categoryBmethod};
  a.categoryCmethod{declaration:fdt_objccategory.TCategoryC.categoryCmethod};
end;

{ TCategoryA }

procedure TCategoryA.categoryAmethod;
begin

end;

{ TCategoryB }

procedure TCategoryB.categoryBmethod;
begin

end;

{ TCategoryC }

procedure TCategoryC.categoryCmethod;
begin

end;

end.

