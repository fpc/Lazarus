unit gdkcairocanvas;

{$mode objfpc}{$H+}

interface

uses
  gdk2, Gtk2Def, CairoGraphics, LCLType;

type
{ TGdkCairoCanvas }

  TGdkCairoCanvas = class(TCairoControlCanvas)
  protected
    procedure CreateCairoHandle(BaseHandle: HDC); override;
  end;

implementation

{ TGdkCairoCanvas }

procedure TGdkCairoCanvas.CreateCairoHandle(BaseHandle: HDC);
begin
  inherited;
  cr := gdk_cairo_create(TGtk2DeviceContext(BaseHandle).Drawable);
end;

initialization
  CairoGraphicControlCanvasClass := TGdkCairoCanvas;

end.


