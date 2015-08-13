unit fdt_objccategory;

{$mode objfpc}{$H+}
{$ModeSwitch objectivec1}

interface

uses
  Classes, SysUtils;

type

  { ta }

  ta = objcclass(NSObject)
  end;

  { ca }

  ca = objccategory(NSObject)
    procedure categoryAmethod; message 'categoryAmethod';
  end;

  { cb }

  cb = objccategory(ta)
    procedure categoryBmethod; message 'categoryBmethod';
  end;

procedure DoIt;

implementation

{ ca }

procedure ca.categoryAmethod;
begin

end;

{ cb }

procedure cb.categoryBmethod;
begin

end;

procedure DoIt;
var
  a: NSObject;
begin
  a:=ta(ta.alloc).init;
  a.categoryAmethod{declaration:fdt_objccategory.ca.categoryAmethod};
end;

end.

