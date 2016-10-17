{
 Test with:
   ./finddeclarationtest --format=plain --suite=TestFindDeclaration_ObjCClass
}
unit fdt_objcclass;

{$mode objfpc}{$H+}
{$ModeSwitch objectivec1}

interface

uses
  Classes, SysUtils;

type
  TMyObjCClassA1 = objcclass(NSObject) // ObjCClass with implicit ancestor NSObject

  end;

procedure DoIt;

implementation

procedure DoIt;
var
  a: TMyObjCClassA1;
begin
  a:=TMyObjCClassA1.alloc{declaration:objcbase.NSObject.alloc}.init{declaration:objcbase.NSObject.init};
  a.dealloc;
end;

end.

