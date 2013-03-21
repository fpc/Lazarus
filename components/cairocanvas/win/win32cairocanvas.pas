unit Win32CairoCanvas;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Cairo, CairoWin32, CairoGraphics;

type
  { TWin32CairoCanvas }

  TWin32CairoCanvas = class(TCairoControlCanvas)
  protected
    procedure CreateCairoHandle(BaseHandle: HDC); override;
  end;

implementation

uses
  SysUtils;

{ TWin32CairoCanvas }

procedure TWin32CairoCanvas.CreateCairoHandle(BaseHandle: HDC);
begin
  inherited;
  sf := cairo_win32_surface_create(BaseHandle);
  cr := cairo_create(sf);
end;

initialization
  CairoGraphicControlCanvasClass := TWin32CairoCanvas;

end.
