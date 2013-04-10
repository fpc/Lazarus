{ The CairoCanvas package can be compiled on all platforms.
  This unit defines what units are available on which platform.
}
unit CairoCanvasAll;

{$mode objfpc}{$H+}

interface

{$IFNDEF win64}
uses
  {%H-}CairoCanvas, {%H-}CairoPrinter;
{$ENDIF}

implementation

end.

