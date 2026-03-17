unit gdkcairocanvas;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF LCLGtk2}
  gdk2, Gtk2Def,
  {$ENDIF}
  {$IFDEF LCLGtk3}
  LazCairo1, gtk3objects,
  {$ENDIF}
  CairoGraphics, LCLType;

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
    {$IFDEF LCLGtk2}
    NewHandle := {%H-}HDC(gdk_cairo_create(TGtkDeviceContext(NewHandle).Drawable));
    SetLazClipRect(Rect(Control.Left, Control.Top, Control.Left + Control.Width, Control.Top + Control.Height));
    {$ENDIF}
    {$IFDEF LCLGtk3}
    //Create a new owned Cairo context on the same surface as the existing GTK3 DC.
    //The existing pcr already has the correct coordinate space for the widget,
    //so no SetLazClipRect offset is needed. zeljan.
    NewHandle := {%H-}HDC(cairo_create(cairo_get_target(TGtk3DeviceContext(NewHandle).pcr)));
    {$ENDIF}
  end;
  inherited SetHandle(NewHandle);
end;

initialization
  CairoGraphicControlCanvasClass := TGdkCairoCanvas;

end.
