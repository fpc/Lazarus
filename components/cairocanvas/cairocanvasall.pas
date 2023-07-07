{ The CairoCanvas package can be compiled on all platforms, except for WinCE.
  This unit defines what units are available on which platform.
}
unit CairoCanvasAll;

{$mode objfpc}{$H+}

interface

{$IF not defined(WinCE)}
uses
  {%H-}CairoCanvas, {%H-}CairoPrinter;
{$ENDIF}

implementation

end.

