unit fdt_basic;

{$mode objfpc}{$H+}

interface

uses
  Classes{declaration:Classes}, SysUtils;

type
  TMyClass2 = class;

  { TMyClass1 }

  TMyClass1 = class
  public
    constructor Create{declaration:System.TObject.Create};
    procedure DefaultHandler{declaration:System.TObject.DefaultHandler}(var message); override;
  end;

  { TMyClass2 }

  TMyClass2 = class(TMyClass1{declaration:fdt_basic.TMyClass1})
  public
    procedure DefaultHandler(var message); override;
  end;

implementation

{ TMyClass1 }

constructor TMyClass1{declaration:fdt_basic.TMyClass1}.Create{declaration:fdt_basic.TMyClass1.Create};
begin

end;

procedure TMyClass1.DefaultHandler(var message);
begin
  inherited DefaultHandler{declaration:System.TObject.DefaultHandler}(
    message{declaration:TMyClass1.DefaultHandler.message});
end;

{ TMyClass2 }

procedure TMyClass2.DefaultHandler(var message);
begin
  inherited DefaultHandler{declaration:fdt_basic.TMyClass1.DefaultHandler}(message);
end;

end.

