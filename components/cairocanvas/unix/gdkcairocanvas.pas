unit gdkcairocanvas;

{$mode objfpc}{$H+}

interface

uses
  gdk2, Gtk2Def, CairoGraphics, LCLType;

type
{ TGdkCairoCanvas }

  TGdkCairoCanvas = class(TCairoControlCanvas)
  protected
    procedure SetHandle(NewHandle: HDC); override;
    function CreateCairoHandle: HDC; override;
  end;

implementation

{ TGdkCairoCanvas }

function TGdkCairoCanvas.CreateCairoHandle: HDC;
begin
  Result := 0; //Fake handle, right Handle is setted in SetHandle func
end;

procedure TGdkCairoCanvas.SetHandle(NewHandle: HDC);
begin
  if NewHandle <> 0 then
    NewHandle := {%H-}HDC(gdk_cairo_create(TGtk2DeviceContext(NewHandle).Drawable));
  inherited SetHandle(NewHandle);
end;

initialization
  CairoGraphicControlCanvasClass := TGdkCairoCanvas;

end.


