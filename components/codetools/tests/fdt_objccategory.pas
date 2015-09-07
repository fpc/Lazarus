{
 Test with:
   ./finddeclarationtest --format=plain --suite=TestFindDeclaration_ObjCCategory
}
unit fdt_objccategory;

{$mode objfpc}{$H+}
{$ModeSwitch objectivec1}

interface

uses
  Classes, SysUtils;

type

  { TMyObjCClassB1 }

  TMyObjCClassB1 = objcclass(NSObject)
  end;

  { TCategoryA }

  TCategoryA = objccategory(NSObject)
    procedure categoryAmethod; message 'categoryAmethod';
  end;

  { TCategoryB }

  TCategoryB = objccategory(TMyObjCClassB1)
    procedure categoryBmethod; message 'categoryBmethod';
  end;

  { TCategoryC }

  TCategoryC = objccategory(TMyObjCClassB1)
    // contrary to helpers there can be multiple ObjCCategory active for a class
    procedure categoryCmethod; message 'categoryCmethod';
  end;

procedure DoIt;

implementation

procedure DoIt;
var
  a: TMyObjCClassB1;
begin
  a:=TMyObjCClassB1.alloc.init{declaration:objcbase.NSObject.init};
  a.categoryAmethod{declaration:fdt_objccategory.TCategoryA.categoryAmethod};
  a.categoryBmethod{declaration:fdt_objccategory.TCategoryB.categoryBmethod};
  a.categoryCmethod{declaration:fdt_objccategory.TCategoryC.categoryCmethod};
  a.dealloc;
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

