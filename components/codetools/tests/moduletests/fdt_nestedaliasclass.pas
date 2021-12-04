{
  ./testcodetools --format=plain --suite=TestFindDeclaration_NestedClasses
}
unit fdt_nestedaliasclass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TBird = class
  public type
    TBirdWing = class
    public
      Span: word;
      procedure Fly; virtual;
    end;
  end;

  TEagle = class(TBird)
  public type
    TAliasBirdWing = TBirdWing{declaration:fdt_nestedaliasclass.TBird.TBirdWing};
    TEagleWing = class(TAliasBirdWing{declaration:fdt_nestedaliasclass.TEagle.TAliasBirdWing})
    public
      procedure Fly; override;
    end;
  end;

implementation

{ TBird.TBirdWing }

procedure TBird.TBirdWing.Fly;
begin

end;

{ TEagle.TEagleWing }

procedure TEagle.TEagleWing.Fly;
begin
  //Span{declaration:fdt_nestedaliasclass.TBird.TBirdWing.Span}:=3;
end;

var
  Wing: TEagle.TEagleWing;
begin
  Wing:=TEagle.TEagleWing{declaration:fdt_nestedaliasclass.TEagle.TEagleWing}.Create;
  // Wing.Fly{declaration:fdt_nestedaliasclass.TEagle.TEagleWing.Fly};
end.

