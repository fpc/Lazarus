unit fdt_nestedclasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TBaseClass = class
  public type
    TBaseSubClass = class
      procedure DoSomething; virtual;
    end;
  end;

  TCustomClass = class(TBaseClass)
  public type
    TCustomSubClass = class(TBaseSubClass{declaration:fdt_nestedclasses.TBaseClass.TBaseSubClass})
      procedure DoSomething; override;
    end;
  end;

implementation

{ TCustomClass.TCustomSubClass }

procedure TCustomClass.TCustomSubClass.DoSomething;
begin
  if TBaseSubClass{declaration:fdt_nestedclasses.TBaseClass.TBaseSubClass}.ClassName<>'' then ;
end;

{ TBaseClass.TBaseSubClass }

procedure TBaseClass.TBaseSubClass.DoSomething;
begin

end;


end.

