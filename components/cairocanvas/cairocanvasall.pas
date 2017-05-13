{ The CairoCanvas package can be compiled on all platforms, except for WinCE.
  This unit defines what units are available on which platform.
}
unit CairoCanvasAll;

{$mode objfpc}{$H+}

interface

{$IF ((FPC_FULLVERSION>=20701) or not defined(win64)) and not defined(WinCE)}
uses
  {%H-}CairoCanvas, {%H-}CairoPrinter;
{$ENDIF}

implementation

end.

