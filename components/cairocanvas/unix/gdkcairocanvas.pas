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

uses
  Classes;


{ TGdkCairoCanvas }

function TGdkCairoCanvas.CreateCairoHandle: HDC;
begin
  Result := 0; //Fake handle, right Handle is setted in SetHandle func
end;

procedure TGdkCairoCanvas.SetHandle(NewHandle: HDC);
begin
  if NewHandle <> 0 then begin
    NewHandle := {%H-}HDC(gdk_cairo_create(TGtkDeviceContext(NewHandle).Drawable));
    SetLazClipRect(Rect(Control.Left, Control.Top, Control.Left + Control.Width, Control.Top + Control.Height));
  end;
  inherited SetHandle(NewHandle);
end;

initialization
  CairoGraphicControlCanvasClass := TGdkCairoCanvas;

end.


