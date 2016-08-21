unit fdt_classof;

{$mode objfpc}{$H+}

interface

type
  TClassOfMy = class of TMy{declaration:fdt_classof.TMy};

  { TMy }

  TMy = class(TObject)
  public
    class procedure Run;
  end;

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

