unit Win32CairoCanvas;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Cairo, CairoWin32, CairoGraphics;

type
  { TWin32CairoCanvas }

  TWin32CairoCanvas = class(TCairoControlCanvas)
  protected
    function CreateCairoHandle: HDC; override;
    procedure SetHandle(NewHandle: HDC); override;
  end;

implementation

uses
  SysUtils;

{ TWin32CairoCanvas }

function TWin32CairoCanvas.CreateCairoHandle: HDC;
begin
  Result := 0; //Fake handle, right Handle is setted in SetHandle func
end;

procedure TWin32CairoCanvas.SetHandle(NewHandle: HDC);
begin
  if NewHandle <> 0 then begin
    sf := cairo_win32_surface_create(NewHandle);
    NewHandle := {%H-}HDC(cairo_create(sf));
  end;
  inherited SetHandle(NewHandle);
end;

initialization
  CairoGraphicControlCanvasClass := TWin32CairoCanvas;

end.
