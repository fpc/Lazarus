unit fdt_classof;

{$mode objfpc}{$H+}

interface

type
  TClassOfMy = class of TMy{declaration:fdt_classof.TMy};
  TClass1OfMy = class of TMy{declaration:fdt_classof.TMy} deprecated 'abc' experimental;

  { TMy }

  TMy = class(TObject)
  public
    class procedure Run;
  end;

  TClass2OfMy = class of fdt_classof.TMy{declaration!:fdt_classof.TMy};

procedure DoIt;

implementation

procedure DoIt;
var
  c: TClassOfMy{declaration:fdt_classof.TClassOfMy};
begin
  c:=nil;
  c.Run{declaration:fdt_classof.TMy.Run};
end;

{ TMy }

class procedure TMy.Run;
begin

end;

end.

